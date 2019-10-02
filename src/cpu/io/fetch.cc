//-----------------------------------------------------------------------------
// fetch.cc
//-----------------------------------------------------------------------------
//
// Author: Tuan Ta
// Date:   19/08/19

#include "cpu/io/fetch.hh"

#include "arch/utility.hh"
#include "cpu/io/cpu.hh"
#include "debug/Fetch.hh"

//-----------------------------------------------------------------------------
// Fetch::FetchTranslation
//-----------------------------------------------------------------------------

Fetch::FetchTranslation::FetchTranslation(Fetch* _fetch_p)
    : m_fetch_p(_fetch_p)
{ }

void
Fetch::FetchTranslation::markDelayed()
{ }

void
Fetch::FetchTranslation::finish(const Fault& fault, const RequestPtr& req,
                                ThreadContext* tc, BaseTLB::Mode mode)
{
  assert(mode == BaseTLB::Execute);
  m_fetch_p->finishTranslation(fault, req);
  delete this;
}

//-----------------------------------------------------------------------------
// Fetch::FinishTranslationEvent
//-----------------------------------------------------------------------------

Fetch::FinishTranslationEvent::FinishTranslationEvent(Fetch* _fetch_p)
    : m_fetch_p(_fetch_p), m_fault(NoFault), m_req_p(nullptr)
{ }

void
Fetch::FinishTranslationEvent::setFault(Fault _fault)
{
  m_fault = _fault;
}

void
Fetch::FinishTranslationEvent::setReq(const RequestPtr& _req)
{
  m_req_p = _req;
}

void
Fetch::FinishTranslationEvent::process()
{
  m_fetch_p->finishTranslation(m_fault, m_req_p);
}

const char*
Fetch::FinishTranslationEvent::description() const
{
  return "IOCPU FetchFinishTranslation";
}

//-----------------------------------------------------------------------------
// Fetch
//-----------------------------------------------------------------------------

Fetch::Fetch(IOCPU* _cpu_p, IOCPUParams* params)
  : m_cpu_p(_cpu_p),
    m_num_threads(params->numThreads),
    m_is_active(false),
    m_max_num_credits(params->decodeBufferSize),
    m_num_credits(m_max_num_credits),
    m_fetch_buffer_size(m_cpu_p->getCacheLineSize()), // 1-cache-line L0 buffer
    m_fetch_buffers(m_num_threads, new uint8_t[m_fetch_buffer_size]),
    m_fetch_buffer_vaddrs(m_num_threads, 0),
    m_fetch_buffer_valid(m_num_threads, false),
    m_pcs(m_num_threads),
    m_fetch_offsets(m_num_threads, 0),
    m_macro_insts(m_num_threads, nullptr),
    m_mem_reqs(m_num_threads, nullptr),
    m_retry_pkt(nullptr),
    m_thread_status(m_num_threads, Inactive),
    m_branch_pred_p(params->branchPred),
    m_last_thread(0),
    m_decoders(m_num_threads, nullptr)
{
  for (ThreadID tid = 0; tid < m_num_threads; ++tid)
    m_decoders[tid] = new TheISA::Decoder(params->isa[tid]);
}

Fetch::~Fetch()
{
  for (ThreadID tid = 0; tid < m_num_threads; ++tid) {
    delete[] m_fetch_buffers[tid];
    delete m_decoders[tid];
  }
}

void
Fetch::init()
{

}

std::string
Fetch::name() const
{
  return m_cpu_p->name() + ".fetch";
}

void
Fetch::regStats()
{

}

void
Fetch::setCommBuffers(TimeBuffer<InstComm>& inst_buffer,
                      TimeBuffer<CreditComm>& credit_buffer,
                      TimeBuffer<SquashComm>& squash_buffer,
                      TimeBuffer<InfoComm>& info_buffer)
{
  m_outgoing_inst_wire   = inst_buffer.getWire(0);
  m_incoming_credit_wire = credit_buffer.getWire(-1);
  m_incoming_squash_wire = squash_buffer.getWire(-1);
  m_incoming_info_wire   = info_buffer.getWire(-1);
}

TheISA::Decoder*
Fetch::getDecoderPtr(ThreadID tid)
{
  return m_decoders[tid];
}

void
Fetch::wakeup()
{
  assert(!m_is_active);
  m_is_active = true;
}

void
Fetch::suspend()
{
  assert(m_is_active);
  // make sure we suspend only when no thread is active
  assert(getNumActiveThreads() == 0);
  m_is_active = false;
}

void
Fetch::activateThread(ThreadID tid)
{
  assert(m_thread_status[tid] == Inactive);
  m_thread_status[tid] = Active;
}

void
Fetch::deactivateThread(ThreadID tid)
{
  assert(m_thread_status[tid] != Inactive);
  m_thread_status[tid] = Inactive;

  // if this thread has any pending packet, we drop it.
  if (m_retry_pkt != nullptr) {
    delete m_retry_pkt;
    m_retry_pkt = nullptr;
  }

  // reset mem_req for this thread
  m_mem_reqs[tid] = nullptr;
}

void
Fetch::resetStates(ThreadID tid)
{
  m_fetch_buffer_vaddrs[tid] = 0;
  m_fetch_buffer_valid[tid] = false;
  m_pcs[tid] = TheISA::PCState(0);
  m_macro_insts[tid] = nullptr;
  m_mem_reqs[tid] = nullptr;
  m_decoders[tid]->reset();
  if (m_retry_pkt) {
    delete m_retry_pkt;
    m_retry_pkt = nullptr;
  }

  // remove any pending instruction going to the next stage
  auto& inst_list = m_outgoing_inst_wire->to_decode_insts;
  size_t old_size = inst_list.size();
  auto inst_it = std::remove_if(inst_list.begin(), inst_list.end(),
                                [&](const IODynInstPtr& inst)
                                      { return inst->thread_id == tid; } );
  inst_list.erase(inst_it, inst_list.end());

  // give back credits to this stage if some insts were removed
  m_num_credits += (old_size - inst_list.size());
  assert(m_num_credits <= m_max_num_credits);
}

void
Fetch::pcState(const TheISA::PCState& new_pc, ThreadID tid)
{
  DPRINTF(Fetch, "[tid:%d] Update pc to %s\n", tid, new_pc);

  m_pcs[tid] = new_pc;

  // If there was a trap pending, the caller of this function must be finishing
  // up a trap. We can switch to Active status now since the trap if finishing
  // this cycle.
  if (m_thread_status[tid] == TrapPending)
    m_thread_status[tid] = Active;
}

void
Fetch::tick()
{
  // sanity check
  assert(m_is_active);

  // check squash
  bool is_squashed = checkSquash();

  // read credits coming from Decode
  readCredits();

  // read info coming from Commit
  readInfo();

  // do actual fetch if we're not being squashed this cycle and we still have
  // at least one credit to the next stage.
  if (!is_squashed && m_num_credits > 0) {
    // Select a thread to fetch
    ThreadID tid = getNextThread();

    // Break out early if no thread is ready to fetch this cycle
    if (tid == InvalidThreadID)
      return;

    // Otherwise, start fetching for the selected thread
    doFetch(tid);
  }
#ifdef DEBUG
  else if (is_squashed) {
    m_stage_status.set(FetchStatus::Squashed);
  } else if (m_num_credits == 0) {
    m_stage_status.set(FetchStatus::Stalled);
  }
#endif
}

bool
Fetch::checkSquash()
{
  // check all possible squash signals coming from subsequent stages. It's
  // important to do this in a reversed order since earlier stages may squash
  // younger instructions.

  // check squash coming from Commit
  if (m_incoming_squash_wire->commit_squash.squash) {
    IODynInstPtr fault_inst = m_incoming_squash_wire->commit_squash.fault_inst;
    assert(fault_inst);
    DPRINTF(Fetch, "Squash from Commit: squash inst [tid:%d] [sn:%d]\n",
                    fault_inst->thread_id, fault_inst->seq_num);

    doSquash(m_incoming_squash_wire->commit_squash.next_pc, fault_inst);

    // update branch predictor
    m_branch_pred_p->squash(fault_inst->seq_num, fault_inst->thread_id);

    // XXX: a hack that delays progressing Fetch with the updated PC until a
    // trap is completed. It's necessary because a trap in SE mode can update
    // PC functionally by calling RiscvFault::invoke ->
    // ThreadContext::pcState(newPC). Without this hack, we can end up with
    // executing instructions after a fault twice (i.e., once by the new PC
    // passed from Commit and another one by the new PC updated functionally by
    // the trap in SE mode). We temporarily change the state of this thread to
    // TrapPending so that it does not get scheduled to fetch until the trap
    // completes.
    if (m_incoming_squash_wire->commit_squash.is_trap_pending)
      m_thread_status[fault_inst->thread_id] = TrapPending;

    return true;
  }

  // check squash coming from IEW (due to branch misprediction)
  if (m_incoming_squash_wire->iew_squash.squash) {
    IODynInstPtr mispred_inst =
                    m_incoming_squash_wire->iew_squash.mispred_inst;
    assert(mispred_inst);
    DPRINTF(Fetch, "Squash from IEW: squash inst [tid:%d] [sn:%d]\n",
                    mispred_inst->thread_id, mispred_inst->seq_num);

    doSquash(m_incoming_squash_wire->iew_squash.next_pc, mispred_inst);

    // update branch predictor
    m_branch_pred_p->squash(mispred_inst->seq_num,
                            m_incoming_squash_wire->iew_squash.next_pc,
                            m_incoming_squash_wire->iew_squash.branch_taken,
                            mispred_inst->thread_id);
    return true;
  }

  // check squash comming from Decode (due to branch misprediction)
  if (m_incoming_squash_wire->decode_squash.squash) {
    IODynInstPtr mispred_inst =
                    m_incoming_squash_wire->decode_squash.mispred_inst;
    assert(mispred_inst);
    DPRINTF(Fetch, "Squash from Decode: squash inst [tid:%d] [sn:%d]\n",
                    mispred_inst->thread_id, mispred_inst->seq_num);

    doSquash(m_incoming_squash_wire->decode_squash.next_pc, mispred_inst);

    // update branch predictor
    m_branch_pred_p->squash(mispred_inst->seq_num,
                            m_incoming_squash_wire->decode_squash.next_pc,
                            m_incoming_squash_wire->decode_squash.branch_taken,
                            mispred_inst->thread_id);
    return true;
  }

  return false;
}

void
Fetch::readCredits()
{
  // read and update my number of credits
  m_num_credits += m_incoming_credit_wire->from_decode;
  assert(m_num_credits <= m_max_num_credits);
}

void
Fetch::readInfo()
{
  if (m_incoming_info_wire->commit_info.committed_inst) {
    // update the branch predictor to reflect all retired/commited instructions
    m_branch_pred_p->update(
                (m_incoming_info_wire->commit_info.committed_inst)->seq_num,
                (m_incoming_info_wire->commit_info.committed_inst)->thread_id);
  }
}

void
Fetch::doSquash(const TheISA::PCState& new_pc, const IODynInstPtr squash_inst)
{
  ThreadID tid = squash_inst->thread_id;
  assert(tid != InvalidThreadID);

  DPRINTF(Fetch, "[tid:%i] Squashing: squash inst [sn:%i], new_pc %s\n",
                 tid, squash_inst->seq_num, new_pc);

  // update the PC to the new_pc
  m_pcs[tid] = new_pc;

  // reset decoder for this thread
  m_decoders[tid]->reset();

  // reset macro_inst
  m_macro_insts[tid] = nullptr;

  if (m_retry_pkt != nullptr) {
    delete m_retry_pkt;
    m_retry_pkt = nullptr;
  }

  // reset mem_req for this thread
  m_mem_reqs[tid] = nullptr;

  // reset pc offset for this thread
  m_fetch_offsets[tid] = 0;
}

ThreadID
Fetch::getNextThread()
{
  // TODO: for now, always select thread 0 if it's active. Later, we'll support
  // multi-threading mode

  // we only try to fetch active threads that are not waiting for a memory
  // response from I-cache
  if (m_thread_status[0] == Active && m_mem_reqs[0] == nullptr)
    return 0;

  if (m_thread_status[0] == TrapPending)
    DPRINTF(Fetch, "Thread 0 is waiting for a trap to complete\n");

  return InvalidThreadID;
}

void
Fetch::doFetch(ThreadID tid)
{
  assert(m_thread_status[tid] == Active);

  // Get the current PC for this thread and its fetch address
  TheISA::PCState cur_pc = m_pcs[tid];
  size_t pc_offset = m_fetch_offsets[tid];
  Addr fetch_addr = (cur_pc.instAddr() + pc_offset) & BaseCPU::PCMask;

  // Align the fetch_addr to cache line address
  Addr fetch_buffer_addr = getCacheLineAlignedAddr(fetch_addr);

  // If we don't have a valid buffer yet or the existing buffer does not
  // contain the fetch_addr, we send a cache-line fetch request to I-cache
  if (!m_fetch_buffer_valid[tid] ||
      fetch_buffer_addr != m_fetch_buffer_vaddrs[tid]) {
    DPRINTF(Fetch, "[tid:%d]: Fetching cache line 0x%x for PC %s\n",
                   tid, fetch_buffer_addr, cur_pc);
    fetchCacheLine(fetch_buffer_addr, tid, cur_pc.instAddr());
    return;
  }

  // get the next static inst
  StaticInstPtr static_inst = nullptr;

  // check if we still have micro-insts left to fetch
  if (m_macro_insts[tid]) {
    static_inst = m_macro_insts[tid]->fetchMicroop(cur_pc.microPC());
    if (static_inst->isLastMicroop())
      m_macro_insts[tid] = nullptr;
  } else {
    // Try to get the current instruction from the fetch buffer
    TheISA::MachInst* cache_insts =
                    reinterpret_cast<TheISA::MachInst*>(m_fetch_buffers[tid]);
    size_t offset = (fetch_addr - fetch_buffer_addr) /
                                            sizeof(TheISA::MachInst);
    TheISA::MachInst inst = TheISA::gtoh(cache_insts[offset]);

    // Feed the inst into the decoder
    m_decoders[tid]->moreBytes(cur_pc, fetch_addr, inst);

    if (!m_decoders[tid]->instReady()) {
      assert(m_decoders[tid]->needMoreBytes());
      DPRINTF(Fetch, "[tid:%d]: PC %s is not fully fetched\n", tid, cur_pc);

      // increment the fetch_addr and check if we can keep fetching more from
      // the existing buffer or we have to get new line from i-cache
      fetch_addr += sizeof(TheISA::MachInst);

      if (getCacheLineAlignedAddr(fetch_addr) == fetch_buffer_addr) {
        DPRINTF(Fetch, "[tid:%d]: Fetching the rest of PC %s "
                      "from inst buffer\n", tid, cur_pc);
        offset = (fetch_addr - fetch_buffer_addr) / sizeof(TheISA::MachInst);
        inst = TheISA::gtoh(cache_insts[offset]);
        m_decoders[tid]->moreBytes(cur_pc, fetch_addr, inst);
      } else {
        DPRINTF(Fetch, "[tid:%d]: Current buffer runs out of data for PC %s, "
                      "so we fetch from i-cache\n", tid, cur_pc);
        m_fetch_buffer_valid[tid] = false;
        fetch_buffer_addr = getCacheLineAlignedAddr(fetch_addr);
        m_fetch_offsets[tid] = fetch_addr - cur_pc.instAddr();
        fetchCacheLine(fetch_buffer_addr, tid, cur_pc.instAddr());
        return;
      }
    }

    // we get here only if we completed fetching successfully the current PC,
    // so we reset m_pc_offsets here
    assert(m_decoders[tid]->instReady());
    m_fetch_offsets[tid] = 0;

    // Create a static instruction
    static_inst = m_decoders[tid]->decode(cur_pc);

    // If the inst is a macro-inst, we need to further break it down into
    // micro-instructions
    if (static_inst->isMacroop()) {
      m_macro_insts[tid] = static_inst;
      static_inst = m_macro_insts[tid]->fetchMicroop(cur_pc.microPC());
      if (static_inst->isLastMicroop())
        m_macro_insts[tid] = nullptr;
    }
  }

  // build dyn_inst
  IODynInstPtr dyn_inst_p =
          std::make_shared<IODynInst>(static_inst, cur_pc,
                                      m_cpu_p->getAndIncrementInstSeq(),
                                      tid, m_cpu_p);
  DPRINTF(Fetch, "[tid:%d]: built inst %s\n", tid, dyn_inst_p->toString(true));

  // Look up the next pc
  TheISA::PCState next_pc = cur_pc;
  bool predict_taken = lookupAndUpdateNextPC(dyn_inst_p, next_pc);
  DPRINTF(Fetch, "[tid:%d]: cur_pc %s -> next_pc %s (predict_taken = %d)\n",
                tid, cur_pc, next_pc, predict_taken);

  // Update dyn_inst
  dyn_inst_p->setPredTarg(next_pc);
  dyn_inst_p->predicted_taken = predict_taken;

  // Update this thread's pc to the next pc
  m_pcs[tid] = next_pc;

  // Send inst to Decode
  sendInstToNextStage(dyn_inst_p);
}

bool
Fetch::fetchCacheLine(Addr line_addr, ThreadID tid, Addr pc)
{
  Fault fault = NoFault;

  assert(!m_cpu_p->switchedOut());

  if (m_retry_pkt != nullptr || m_mem_reqs[tid] != nullptr) {
    DPRINTF(Fetch, "[tid:%d]: Can't fetch a cache line now. Another request is"
                   " waiting to go out to i-cache\n", tid);
    return false;
  }

  DPRINTF(Fetch, "[tid:%d]: Fetching cache line 0x%x\n", tid, line_addr);
  RequestPtr req = std::make_shared<Request>
                                      (tid, line_addr, m_fetch_buffer_size,
                                       Request::INST_FETCH,
                                       m_cpu_p->instMasterId(), pc,
                                       m_cpu_p->tcBase(tid)->contextId());
  req->taskId(m_cpu_p->taskId());

  m_mem_reqs[tid] = req;

  // init translation of the icache block
  //    -> TLB::translateTiming()
  //    -> FetchTranslation::finish()
  //    -> Fetch::finishTranslation(): make pkt and go out to i-cache
  // XXX: we assume translation happens atomically. No timing delay between
  // this translation and when the memory packet is created.
  FetchTranslation* trans = new FetchTranslation(this);
  m_cpu_p->itb->translateTiming(req, m_cpu_p->tcBase(tid), trans,
                                BaseTLB::Execute);
  return true;
}

void
Fetch::finishTranslation(const Fault& fault, const RequestPtr& mem_req)
{
  assert(!m_cpu_p->switchedOut());

  ThreadID tid = m_cpu_p->contextToThread(mem_req->contextId());
  Addr line_vaddr = mem_req->getVaddr();

  // Wake CPU up if it's idle
  if (!m_is_active)
    m_cpu_p->wakeup();

  // sanity check
  assert(m_mem_reqs[tid] == mem_req);

  if (fault == NoFault) {   // translation is successful
    // assert paddr is valid
    assert(m_cpu_p->system->isMemAddr(mem_req->getPaddr()));

    DPRINTF(Fetch, "[tid:%d] Doing instruction fetch\n", tid);

    // build packet
    PacketPtr data_pkt = new Packet(mem_req, MemCmd::ReadReq);
    data_pkt->dataDynamic(new uint8_t[m_fetch_buffer_size]);

    m_fetch_buffer_vaddrs[tid] = line_vaddr;
    m_fetch_buffer_valid[tid] = false;

    if (!m_cpu_p->getInstPort().sendTimingReq(data_pkt)) {
      assert(m_retry_pkt == nullptr);

      DPRINTF(Fetch, "[tid:%d] icache is busy\n", tid);

      // save the pkt, so that we can retry later
      m_retry_pkt = data_pkt;
    } else {
      DPRINTF(Fetch, "[tid:%d]: Sent i-fetch pkt\n", tid);
    }
  } else {  // translation failed
    DPRINTF(Fetch, "[tid:%d] Translation faulted detected at PC %s. Building "
                   "no-op\n", tid, m_pcs[tid]);

    // no request will be sent to I-cache
    m_mem_reqs[tid] = nullptr;

    // build a no-op instruction to send the fault to commit stage. This thread
    // won't do anything until commit handles the fault and possibly wakes it
    // up if a squash comes along and changes the PC.

    // build a no-op instruction
    IODynInstPtr dyn_inst_p =
          std::make_shared<IODynInst>(StaticInst::nopStaticInstPtr, m_pcs[tid],
                                      m_cpu_p->getAndIncrementInstSeq(),
                                      tid, m_cpu_p);
    dyn_inst_p->setPredTarg(m_pcs[tid]);
    dyn_inst_p->predicted_taken = false;

    // Send dyn_inst to the next stage
    sendInstToNextStage(dyn_inst_p);

    // Update fetch status for this thread
    m_thread_status[tid] = TrapPending;
  }
}

bool
Fetch::lookupAndUpdateNextPC(const IODynInstPtr& inst,
                             TheISA::PCState& next_pc)
{
  if (!inst->isControl()) {
    TheISA::advancePC(next_pc, inst->static_inst_p);
    inst->setPredTarg(next_pc);
    inst->setPredTaken();
    return false;
  }

  ThreadID tid = inst->thread_id;
  bool predict_taken = m_branch_pred_p->predict(inst->static_inst_p,
                                                inst->seq_num, next_pc, tid);

  if (predict_taken) {
    DPRINTF(Fetch, "[tid:%d]: [sn:%d]: Branch predicted to be taken to %s\n",
                   tid, inst->seq_num, next_pc);
    inst->setPredTaken();
  } else {
    DPRINTF(Fetch, "[tid:%d]: [sn:%d]: Branch predicted to be not taken\n",
                   tid, inst->seq_num);
  }

  DPRINTF(Fetch, "[tid:%d]: [sn:%d] Branch predicted to go to %s\n",
                 tid, inst->seq_num, next_pc);
  inst->setPredTarg(next_pc);

  return predict_taken;
}

void
Fetch::processCacheCompletion(PacketPtr pkt)
{
  ThreadID tid = m_cpu_p->contextToThread(pkt->req->contextId());

  DPRINTF(Fetch, "[tid:%d] Receiving icache response\n", tid);
//  assert(!m_cpu_p->swichedOut());

  // Check if this response packet is still needed. If not (i.e., probably due
  // to an early squash), just drop and delete it.
  if (pkt->req != m_mem_reqs[tid]) {
    delete pkt;
    return;
  }

  // Otherwise, refill the fetch buffer for tid
  assert(!m_fetch_buffer_valid[tid]);
  memcpy(m_fetch_buffers[tid], pkt->getConstPtr<uint8_t>(),
         m_fetch_buffer_size);
  m_fetch_buffer_valid[tid] = true;

  // clean up
  delete pkt;
  m_mem_reqs[tid] = nullptr;

  // change the thread status back to Active
  m_thread_status[tid] = Active;
}

void
Fetch::completeRetryReq()
{
  if (m_retry_pkt == nullptr) {
    DPRINTF(Fetch, "Failed packet has been squashed\n");
    return;
  }

  if (m_cpu_p->getInstPort().sendTimingReq(m_retry_pkt)) {
    DPRINTF(Fetch, "Retried sending failed packet successfully\n");
    m_retry_pkt = nullptr;
  }
}

Addr
Fetch::getCacheLineAlignedAddr(Addr addr) const
{
  return addr & ~(m_fetch_buffer_size - 1);
}

void
Fetch::sendInstToNextStage(IODynInstPtr inst)
{
  // sanity check: make sure we have enough credit before we sent the inst
  assert(m_num_credits > 0);
  // Place inst into the buffer
  m_outgoing_inst_wire->to_decode_insts.push_back(inst);
  // consume one credit
  m_num_credits--;

#ifdef DEBUG
  // record trace
  m_stage_status.set(FetchStatus::Busy);
  m_fetched_insts.push_back(inst);
#endif
}

size_t
Fetch::getNumActiveThreads() const
{
  return std::count_if(m_thread_status.begin(), m_thread_status.end(),
                        [](const ThreadStatus& s) { return s == Active; });
}

void
Fetch::linetrace(std::stringstream& ss)
{
#ifdef DEBUG
  std::string s = " [F] ";
  if (m_stage_status[FetchStatus::Squashed]) {
    s += "x";
  } else if (m_stage_status[FetchStatus::Stalled]) {
    s += "#";
  } else if (m_stage_status[FetchStatus::Busy]) {
    assert(!m_fetched_insts.empty());
    for (auto inst : m_fetched_insts)
      s += inst->toString(true) + " ";
  }
  ss << std::setw(55) << std::left << s;

  // reset stage status
  m_stage_status.reset();
  m_fetched_insts.clear();
#endif
}
