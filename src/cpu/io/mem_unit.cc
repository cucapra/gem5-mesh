//=============================================================================
// mem_unit.cc
//=============================================================================
//
// Author: Tuan Ta
// Date:   19/09/12

#include "cpu/io/mem_unit.hh"

#include "cpu/io/cpu.hh"
#include "debug/LSQ.hh"

#include "mem/page_table.hh"
#include "mem/ruby/scratchpad/Scratchpad.hh"
#include "debug/Mesh.hh"

MemUnit::MemUnit(const char* _iew_name, const char* _name,
                 IOCPU* _cpu_p, IOCPUParams* params)
    : m_iew_name(_iew_name),
      m_name(_name),
      m_cpu_p(_cpu_p),
      m_num_lq_entries(params->numLoadQueueEntries),
      m_num_sq_entries(params->numStoreQueueEntries),
      m_num_dcache_ports(params->numDcachePorts),
      m_issued_inst(nullptr),
      m_s0_inst(nullptr),
      m_s1_inst(nullptr),
      m_store_diff_reg(0)
{ }

const std::string
MemUnit::name() const
{
  return m_iew_name + "." + m_name;
}

void
MemUnit::setScoreboardPtr(Scoreboard* scoreboard_p)
{
  m_scoreboard_p = scoreboard_p;
}

void
MemUnit::insert(IODynInstPtr inst)
{
  assert(!isBusy());
  // place the incoming inst into m_s0_inst
  m_s0_inst = inst;
  
  // also save in a seperate variable that is gaurenteed to get freed first time tick called
  m_issued_inst = inst;
}

IODynInstPtr
MemUnit::peekIntroInst()
{
  return m_issued_inst;
}

void
MemUnit::functionalExecute() {
  // don't do anything which is fine b/c mem instruction won't update pc
}


IODynInstPtr
MemUnit::removeCompletedInst()
{
  // if both LQ and SQ have instructions to write back, prioritize older inst
  if (isLQReadyToWB() && isSQReadyToWB()) {
    if (m_ld_queue.front()->seq_num < m_st_queue.front()->seq_num) {
      IODynInstPtr inst = m_ld_queue.front();
      m_ld_queue.pop_front();
      return inst;
    }

    IODynInstPtr inst = m_st_queue.front();
    m_st_queue.pop_front();
    return inst;
  }

  if (isLQReadyToWB()) {
    IODynInstPtr inst = m_ld_queue.front();
    m_ld_queue.pop_front();
    return inst;
  }

  if (isSQReadyToWB()) {
    IODynInstPtr inst = m_st_queue.front();
    m_st_queue.pop_front();
    return inst;
  }

  panic("Neither LQ nor SQ is ready to write back\n");
  return nullptr;
}

bool
MemUnit::hasInstsToWriteBack() const
{
  // return true if either load or store queue has instruction that has already
  // finished its execution (i.e., marked as executed)
  return isLQReadyToWB() || isSQReadyToWB();
}

bool
MemUnit::isLQReadyToWB() const
{
  return !m_ld_queue.empty() && m_ld_queue.front()->isExecuted();
}

bool
MemUnit::isSQReadyToWB() const
{
  return !m_st_queue.empty() && m_st_queue.front()->isExecuted();
}

bool
MemUnit::isBusy() const
{
  // check if S0 is being occupied by any inst
  return m_s0_inst != nullptr;
}

void
MemUnit::tick()
{
  // S2 stage
  doMemIssue();
  // S1 stage
  doTranslation();
  // S0 stage
  doAddrCalc();
}

void
MemUnit::doAddrCalc()
{
  m_issued_inst = nullptr;
  
  if (m_s0_inst == nullptr)
    return;   // nothing to do

  if (m_s1_inst != nullptr) {
#ifdef DEBUG
    m_status.set(Status::S0_Stalled);
#endif
    return;   // S1 is busy, so we stall S0
  }

  // otherwise, simply move instruction from S0 to S1
  m_s1_inst = m_s0_inst;
  m_s0_inst = nullptr;

#ifdef DEBUG
  m_addr_calculated_inst = m_s1_inst;
  m_status.set(S0_Busy);
#endif
}

void
MemUnit::doTranslation()
{
  if (m_s1_inst == nullptr)
    return;   // nothing to do

  // sanity check
  assert(m_ld_queue.size() <= m_num_lq_entries);
  assert(m_st_queue.size() <= m_num_sq_entries);

  // check if S1 needs to stall this cycle b/c LQ is full
  if ((m_s1_inst->isLoad() || m_s1_inst->static_inst_p->isSpadPrefetch()) && m_ld_queue.size() == m_num_lq_entries) {
    DPRINTF(LSQ, "LQ is full. Stalling\n");
#ifdef DEBUG
    m_status.set(Status::S1_Stalled);
#endif
    return;   // no available load queue slot for this inst
  }

  // check if S1 needs to stall this cycle b/c SQ is full
  if ((m_s1_inst->isStore() ||
       m_s1_inst->isAtomic() ||
       m_s1_inst->isStoreConditional()) &&
      m_st_queue.size() == m_num_sq_entries) {
    DPRINTF(LSQ, "SQ is full. Stalling\n");
#ifdef DEBUG
    m_status.set(Status::S1_Stalled);
#endif
    return;   // no available store queue slot for this inst
  }

  // Otherwise, initiate a memory access. This is a pretty convoluted way to
  // do address translation
  //    -> IODynInst::initiateAcc()
  //    -> StaticInst::initiateAcc()
  //    -> initiateMemRead(), initiateMemAMO(), writeMemTiming()
  //                                    // in src/arch/generic/memhelpers.hh
  //    -> IODynInst::initiateMemRead(), initiateMemAMO(), writeMem()
  //    -> IOCPU::pushMemReq()
  //    -> MemUnit::pushMemReq(): init translation
  //    -> TLB::translateTiming()
  //    -> MemTranslation::finish()
  //    -> MemUnit::finishTranslation(): this function places inst with
  //                                     translated addresses into LQ/SQ
  m_s1_inst->initiateAcc();

#ifdef DEBUG
  m_translated_inst = m_s1_inst;
  m_status.set(S1_Busy);
#endif

  // reset m_s1_inst
  m_s1_inst = nullptr;
}

void
MemUnit::doMemIssue()
{
  // loop through LQ and SQ (higher priority goes to LQ) and try to issue any
  // ready-to-issue instructions
  size_t num_issued_insts = 0;

  // check the head of the ROB, if it's a store instruction
  // then set that instruction as issuable
  // this is now a combinational signal, so not sure about how timing works
  // could imagine that this happens cycle before moves to head of ROB so still 1 cycle
  // but allows no-ack stores to be committed immedietly
  auto rob = m_cpu_p->getROBPtr(0);
  if (!rob->isEmpty()) {
    auto head_inst = rob->getHead();
    if ((head_inst->isStore() || head_inst->isAtomic() ||
            head_inst->isStoreConditional()) &&
            !head_inst->canIssueToMem()) {
      head_inst->setCanIssueToMem();
    }
  }

  tryLdIssue(num_issued_insts);
  tryStIssue(num_issued_insts);

}

void
MemUnit::tryLdIssue(size_t &num_issued_insts) {
  for (auto& inst : m_ld_queue) {
    // check if we run of cache bandwidth
    if (num_issued_insts == m_num_dcache_ports) {
      return;
    }

    if (!inst->isExecuted() &&
        !inst->isIssuedToMem() &&
        inst->canIssueToMem()
      ) {
      assert(!inst->isFault());

      PacketPtr pkt = Packet::createRead(inst->mem_req_p);
      pkt->dataStatic(inst->mem_data_p);
      pkt->pushSenderState(new MemUnit::SenderState(inst));

      // send request
      if (!m_cpu_p->getDataPort().sendTimingReq(pkt)) {
        
        DPRINTF(LSQ, "dcache is busy\n");
        //if (inst->static_inst_p->isSpadSpeculative() || inst->static_inst_p->isSpadPrefetch()) 
        //  DPRINTF(Mesh, "failed to send [%s]\n", inst->toString(true));
        // delete the pkt and we'll retry later
        delete pkt->popSenderState();
        delete pkt;
#ifdef DEBUG
        m_status.set(S2_Stalled);
#endif
        // exit issue stage early since the dcache is busy
        return;
      } else {
        DPRINTF(LSQ, "Sent request to memory for inst %s with addr %#x\n", inst->toString(true), pkt->getAddr());
        
        if (inst->srcRegIdx(0) == RegId(IntRegClass, RiscvISA::StackPointerReg) && m_cpu_p->getEarlyVector()->getConfigured()) 
          DPRINTF(Mesh, "Send %s to paddr %#x sp vaddr %#x\n", inst->toString(true), pkt->getAddr(), m_cpu_p->readArchIntReg(RiscvISA::StackPointerReg, 0));

        // mark this inst as "issued to memory"
        inst->setIssuedToMem();
        num_issued_insts++;
        
#ifdef DEBUG
        m_issued_insts.push_back(inst);
        m_status.set(S2_Busy);
#endif
      }
    }
#ifdef DEBUG
    else if (!inst->canIssueToMem()){
      m_status.set(S2_Stalled);
    }
#endif
  }
}

void
MemUnit::tryStIssue(size_t &num_issued_insts) {
  for (auto& inst : m_st_queue) {
    // check if we run of cache bandwidth
    if (num_issued_insts == m_num_dcache_ports) {
      return;
    }

    if (!inst->isExecuted() &&
        !inst->isIssuedToMem() &&
        inst->canIssueToMem()) {
      assert(!inst->isFault());

      PacketPtr pkt = Packet::createWrite(inst->mem_req_p);
      pkt->dataStatic(inst->mem_data_p);
      pkt->pushSenderState(new MemUnit::SenderState(inst));

      // send request
      if (!m_cpu_p->getDataPort().sendTimingReq(pkt)) {
        DPRINTF(LSQ, "dcache is busy\n");
        // delete the pkt and we'll retry later
        delete pkt->popSenderState();
        delete pkt;
#ifdef DEBUG
        m_status.set(S2_Stalled);
#endif
        // exit issue stage early since the dcache is busy
        return;
      } else {
        // an outstanding memory request to track
        m_store_diff_reg++;
        DPRINTF(LSQ, "Sent request to memory for inst %s with addr %#x\n", inst->toString(true), pkt->getAddr());
        if (inst->srcRegIdx(0) == RegId(IntRegClass, RiscvISA::StackPointerReg) && m_cpu_p->getEarlyVector()->getConfigured()) 
          DPRINTF(Mesh, "Send %s to paddr %#x sp vaddr %#x\n", inst->toString(true), pkt->getAddr(), m_cpu_p->readArchIntReg(RiscvISA::StackPointerReg, 0));

        // mark this inst as "issued to memory"
        inst->setIssuedToMem();
        num_issued_insts++;

        // if there is any dependent load, mark them as "CanIssueToMem"
        if (m_st_ld_map.count(inst->seq_num) == 1) {
          for (auto& ld_inst : m_st_ld_map[inst->seq_num]) {
            assert(!ld_inst->canIssueToMem());
            assert(ld_inst->seq_num > inst->seq_num);
            ld_inst->setCanIssueToMem();
          }
          m_st_ld_map.erase(inst->seq_num);
        }

        // if this is a normal store don't wait for the eack and mark as done
       if (inst->static_inst_p->isAckFree()) {
        // if (inst->isStore() && !inst->isStoreConditional() && !inst->isAtomic()) {
          if (m_cpu_p->getEarlyVector()->getConfigured()) DPRINTF(Mesh, "set early execute %s\n", inst->toString(true));
          // mark this as executed
          inst->setExecuted();
          // early bypass
          for (int i = 0; i < inst->numDestRegs(); ++i) {
            DPRINTF(IEW, "[sn:%d] Setting dest reg %i (%s) ready\n",
                          inst->seq_num,
                          inst->renamedDestRegIdx(i)->index(),
                          inst->renamedDestRegIdx(i)->className());
            m_scoreboard_p->setReg(inst->renamedDestRegIdx(i));
          }
        }
        
#ifdef DEBUG
        m_issued_insts.push_back(inst);
        m_status.set(S2_Busy);
#endif
        /**
         * TODO If this inst is not atomic and store-conditional, we can mark
         * it as "Executed" so that it can be written back, and we transfer it
         * into a store buffer to wait for its response. For atomic and
         * store-conditional, we must receive their response before writing
         * back the inst, so we keep them in m_st_queue.
         * XXX: Having a store buffer is probably less critical to performance
         * for now since the pipeline is in-order issue.
         */
      }
    }
#ifdef DEBUG
    else if (!inst->canIssueToMem()){
      m_status.set(S2_Stalled);
    }
#endif
  }
}


void
MemUnit::doSquash(IODynInstPtr squash_inst)
{
  // loop through all in-flight instructions in the unit and squash all
  // insts younger than the squash inst

  // squash S0
  if (m_s0_inst &&
      m_s0_inst->thread_id == squash_inst->thread_id &&
      m_s0_inst->seq_num > squash_inst->seq_num) {
    DPRINTF(LSQ, "Squashing %s\n", m_s0_inst->toString());
    m_s0_inst->setSquashed();
    m_s0_inst = nullptr;
  }

  // squash S1
  if (m_s1_inst &&
      m_s1_inst->thread_id == squash_inst->thread_id &&
      m_s1_inst->seq_num > squash_inst->seq_num) {
    DPRINTF(LSQ, "Squashing %s\n", m_s1_inst->toString());
    m_s1_inst->setSquashed();
    m_s1_inst = nullptr;
  }

  int inst_num = 0;
  // squash insts in m_ld_queue
  for (auto& inst : m_ld_queue) {
    assert(inst);
    if (inst->thread_id == squash_inst->thread_id &&
        inst->seq_num > squash_inst->seq_num) {
      DPRINTF(LSQ, "Squashing %s\n", inst->toString());
      inst->setSquashed();
      
      // TODO if this was a lwspec and at the head of the queue we need to clear
      // the pending retry in the spad or else problems
      if (inst_num == 0 && inst->static_inst_p->isSpadSpeculative())
        clearPortRetry();
      
      
    }
    inst_num++;
  }

  // squash insts in m_st_queue
  for (auto& inst : m_st_queue) {
    assert(inst);
    if (inst->thread_id == squash_inst->thread_id &&
        inst->seq_num > squash_inst->seq_num) {
      DPRINTF(LSQ, "Squashing %s\n", inst->toString());
      inst->setSquashed();
    }
  }

  // remove all squashed insts from m_ld_queue and m_st_queue
  auto ld_it = std::remove_if(m_ld_queue.begin(), m_ld_queue.end(),
                 [](const IODynInstPtr& inst) { return inst->isSquashed(); });
  m_ld_queue.erase(ld_it, m_ld_queue.end());

  auto st_it = std::remove_if(m_st_queue.begin(), m_st_queue.end(),
                 [](const IODynInstPtr& inst) { return inst->isSquashed(); });
  m_st_queue.erase(st_it, m_st_queue.end());

  DPRINTF(LSQ, "Load queue after squashing ...\n");
  for (auto& inst : m_ld_queue)
    DPRINTF(LSQ, "\t%s\n", inst->toString());

  DPRINTF(LSQ, "Store queue after squashing ...\n");
  for (auto& inst : m_st_queue)
    DPRINTF(LSQ, "\t%s\n", inst->toString());

#ifdef DEBUG
  m_status.set(Status::Squashed);
#endif
}

void
MemUnit::processCacheCompletion(PacketPtr pkt)
{
  // extract SenderState
  MemUnit::SenderState* ss =
                      safe_cast<MemUnit::SenderState*>(pkt->popSenderState());
  assert(ss && ss->inst);
  // assert(ss);
  // // lazy ack so have already completed the store
  // if (!ss->inst && pkt->isWrite()) {
  //     m_store_diff_reg--;
  //     delete ss;
  //     delete pkt;
  //     return;
  // }
  DPRINTF(LSQ, "Received response pkt for inst %s\n", ss->inst->toString());

  if (ss->inst->isLoad()) {
    // look up ss->inst in m_ld_queue. If it no longer exists, it must have
    // been squashed. If so, we simply drop the packet.
    auto it = std::find_if(m_ld_queue.begin(),
                           m_ld_queue.end(),
                           [&](const IODynInstPtr& inst)
                              { return ss->inst->seq_num == inst->seq_num; } );

    if (it == m_ld_queue.end()) {
      // this inst must have been squashed earlier
      assert(ss->inst->isSquashed());
    } else {
      // complete access
      ss->inst->completeAcc(pkt);
      // mark this as executed
      ss->inst->setExecuted();
      // early bypass
      for (int i = 0; i < ss->inst->numDestRegs(); ++i) {
        DPRINTF(IEW, "[sn:%d] Setting dest reg %i (%s) ready\n",
                      ss->inst->seq_num,
                      ss->inst->renamedDestRegIdx(i)->index(),
                      ss->inst->renamedDestRegIdx(i)->className());
        m_scoreboard_p->setReg(ss->inst->renamedDestRegIdx(i));
      }
    }

    // clean up
    delete ss;
    delete pkt;
    return;
  } else if (ss->inst->isStore() ||
             ss->inst->isAtomic() ||
             ss->inst->isStoreConditional()) {
    // look up ss->inst in m_st_queue. If it no longer exists, it must have
    // been squashed. If so, we simply drop the packet.
    auto it = std::find_if(m_st_queue.begin(),
                           m_st_queue.end(),
                           [&](const IODynInstPtr& inst)
                              { return ss->inst->seq_num == inst->seq_num; } );
    // mark we've recv an outstanding ack
    m_store_diff_reg--;
    if (it == m_st_queue.end()) {
      // this inst must have been squashed earlier
      if (!ss->inst->static_inst_p->isAckFree())
        assert(ss->inst->isSquashed());
    } else if (!ss->inst->static_inst_p->isAckFree()) {
    // } else if (ss->inst->isStoreConditional() || ss->inst->isAtomic()) {
      // complete access
      ss->inst->completeAcc(pkt);
      // mark this as executed
      ss->inst->setExecuted();
      // early bypass
      for (int i = 0; i < ss->inst->numDestRegs(); ++i) {
        DPRINTF(IEW, "[sn:%d] Setting dest reg %i (%s) ready\n",
                      ss->inst->seq_num,
                      ss->inst->renamedDestRegIdx(i)->index(),
                      ss->inst->renamedDestRegIdx(i)->className());
        m_scoreboard_p->setReg(ss->inst->renamedDestRegIdx(i));
      }
    }

    // clean up
    delete ss;
    delete pkt;
    return;
  } else {
    panic("Unrecongnized response pkt\n");
  }
}

void
MemUnit::completeRetryReq()
{
  // no-op. next tick will retry failed request anyway
}

// This is called when do inst->initiateAcc() called during s1 phase
Fault
MemUnit::pushMemReq(IODynInst* inst, bool is_load, uint8_t* data,
                    unsigned int size, Addr addr, Request::Flags flags,
                    uint64_t* res, AtomicOpFunctor* amo_op)
{
  // sanity check: make sure we have space in either LQ or SQ to place the
  // translated instruction in
  assert((is_load && m_ld_queue.size() < m_num_lq_entries) ||
         (!is_load && m_st_queue.size() < m_num_sq_entries));

  // sanity check: make sure m_s1_inst is indeed what is being passed into the
  // function call
  assert(inst == m_s1_inst.get());

  // check if the request is spanning across two cache lines
  size_t line_size = m_cpu_p->getCacheLineSize();
  if ((addr & (line_size - 1)) + size > line_size) {
#if THE_ISA == RISCV_ISA
    DPRINTFN("Fault: inst %s\n", inst->toString());
    if (is_load) {
      m_s1_inst->fault = std::make_shared<RiscvISA::AddressFault>
                                        (addr, RiscvISA::LOAD_ADDR_MISALIGNED);
    } else if (amo_op) {
      m_s1_inst->fault = std::make_shared<RiscvISA::AddressFault>
                                        (addr, RiscvISA::AMO_ADDR_MISALIGNED);
    } else {
      m_s1_inst->fault = std::make_shared<RiscvISA::AddressFault>
                                      (addr, RiscvISA::STORE_ADDR_MISALIGNED);
    }

    // simply mark this inst "Executed" and let it go through LQ or SQ
    m_s1_inst->setExecuted();
#else
    panic("Unsupported misaligned memory accesses\n");
#endif
  } else {
    // make a request
    ThreadID tid = m_s1_inst->thread_id;
    m_s1_inst->mem_req_p =
                std::make_shared<Request>(0,       // address space ID
                                          addr,    // vaddr
                                          size,    // size
                                          flags,   // request flags
                                          m_cpu_p->dataMasterId(),
                                          m_s1_inst->pc.pc(),
                                          m_cpu_p->tcBase(tid)->contextId(),
                                          amo_op);
    m_s1_inst->mem_req_p->taskId(m_cpu_p->taskId());
    
    // a spad prefetch can be turned into a spad reset if in trace mode
    bool spadPrefetch = m_s1_inst->static_inst_p->isSpadPrefetch();
    // TODO depcreate what even is this?
    bool spadReset = spadPrefetch; // always do reset on prefetch && MeshHelper::isVectorSlave(csrVal) && !m_cpu_p->getEarlyVector()->isCurDiverged();
    m_s1_inst->mem_req_p->spadReset = spadReset;
    // give an epoch number as data if this will be a reset instruction
    // included as seperate field, but in practice would send on data lines
    // if (spadPrefetch) {
    //   m_s1_inst->mem_req_p->epoch = m_cpu_p->getMemEpoch();
    // }
    
    // setup prefetch addr
    if (spadPrefetch) {
      // the 'data' register for this instruction is actually an address
      // convert accordingly
      Addr spadVAddr = 0;
      for (int i = size - 1; i >= 0; i--) {
        spadVAddr |= ((uint64_t)data[i]) << (i * 8);
      }
      
      // need to translate the address, do atomically,
      // real hammerblade doesnt have virtual addresses anyway
      Addr spadPAddr = 0;
      assert(m_cpu_p->tcBase(tid)->getProcessPtr()->pTable->translate(spadVAddr, spadPAddr));
      
      m_s1_inst->mem_req_p->prefetchAddr = spadPAddr;
    }
    
    // only an sp load if not a slave
    // Vector* vec = m_cpu_p->getEarlyVector();
    // bool diverged = vec && vec->isCurDiverged();
    // bool master   = vec && vec->isRootMaster();
    // bool solo     = !(vec && vec->getConfigured());
    // m_s1_inst->mem_req_p->isSpLoad = spadPrefetch && ( diverged || master || solo );
    
    Vector* vec = m_cpu_p->getEarlyVector();
    bool diverged = vec && vec->isCurDiverged();
    bool dAccess  = vec && vec->isDecoupledAccess();
    bool master   = vec && vec->isRootMaster();
    bool solo     = !(vec && vec->getConfigured()) || (vec && !vec->hasForwardingPath());
    m_s1_inst->mem_req_p->isSpLoad = spadPrefetch && ( diverged || dAccess || master || solo );



    
    // setup vector memory request... if we're in vector mode and not detached
    // also send memory request as vector request (single request, multiple response)
    // otherwise send as the usual single request, single response
    
    
    // if (spadPrefetch && master) {
    if (spadPrefetch && (dAccess || master)) { 
      m_s1_inst->mem_req_p->xDim = m_cpu_p->getEarlyVector()->getXLen();
      m_s1_inst->mem_req_p->yDim = m_cpu_p->getEarlyVector()->getYLen();
      m_s1_inst->mem_req_p->xOrigin = m_cpu_p->getEarlyVector()->getXOrigin();
      m_s1_inst->mem_req_p->yOrigin = m_cpu_p->getEarlyVector()->getYOrigin();
      m_s1_inst->mem_req_p->fromDecoupledAccess = dAccess;
      DPRINTF(Mesh, "[%s] send vec load %#x to %#x, (%d,%d)\n", m_s1_inst->toString(true), 
          addr, m_s1_inst->mem_req_p->prefetchAddr , m_s1_inst->mem_req_p->xDim, m_s1_inst->mem_req_p->yDim);
    }
    else {
      m_s1_inst->mem_req_p->xDim = 1;
      m_s1_inst->mem_req_p->yDim = 1;
      m_s1_inst->mem_req_p->xOrigin = 0;
      m_s1_inst->mem_req_p->yOrigin = 0;
      m_s1_inst->mem_req_p->fromDecoupledAccess = false;
    }
    
    // allow load to issue to spad without getting any acks the load is there
    m_s1_inst->mem_req_p->spadSpec  = m_s1_inst->static_inst_p->isSpadSpeculative();

    if (spadPrefetch) assert(m_s1_inst->isStore() && !is_load);

    // this memory will be deleted together with the dynamic instruction
    m_s1_inst->mem_data_p = new uint8_t[size];

    if (data)
      memcpy(m_s1_inst->mem_data_p, data, size);
    else
      memset(m_s1_inst->mem_data_p, 0, size);

    // init a translation for this memory request
    // XXX: Note that we assume address translation is functionally done. No
    // timing delay in doing translateTiming. That means this translateTiming
    // function will eventually call MemUnit::finishTranslation in the same
    // cycle.
    MemTranslation* trans = new MemTranslation(this);
    m_cpu_p->dtb->translateTiming(m_s1_inst->mem_req_p,
                                  m_cpu_p->tcBase(tid),
                                  trans,
                                  is_load ? BaseTLB::Read : BaseTLB::Write);

    if (m_s1_inst->isFault())
      m_s1_inst->setExecuted();
  }

  // place the translated inst into LQ or SQ
  if (is_load) {
    m_ld_queue.push_back(m_s1_inst);

    // for LDs, we can speculatively issue them to memory unless the LD has
    // fault, or the LD depends on an older ST.
    if (!m_s1_inst->isExecuted() &&
        m_s1_inst->isEffAddrValid() &&
        !checkLdStDependency(m_s1_inst)) {
      m_s1_inst->setCanIssueToMem();
    }
  } else {
    // For ST, AMO, SC, we cannot speculatively issue them to memory
    m_st_queue.push_back(m_s1_inst);
  }

  return m_s1_inst->fault;
}

void
MemUnit::finishTranslation(const Fault& fault, RequestPtr req)
{
  assert(m_s1_inst);
  if (fault == NoFault)
    m_s1_inst->setEffAddrValid();
  // set instruction fault
  m_s1_inst->fault = fault;
}

bool
MemUnit::checkLdStDependency(IODynInstPtr ld_inst)
{
  Addr ld_paddr = ld_inst->mem_req_p->getPaddr();
  size_t ld_size = ld_inst->mem_req_p->getSize();

  // walk through the SQ from the back (younger insts first) and looks for any
  // older store that has overlap with the load
  for (auto it = m_st_queue.rbegin(); it != m_st_queue.rend(); ++it) {
    if (!(*it)->isExecuted() &&
        !(*it)->isIssuedToMem() &&
        !(*it)->isFault() &&
        (*it)->isEffAddrValid() &&
        (*it)->seq_num < ld_inst->seq_num) {

      InstSeqNum st_seq_num = (*it)->seq_num;
      Addr st_paddr = (*it)->mem_req_p->getPaddr();
      Addr st_size = (*it)->mem_req_p->getSize();

      if ((st_paddr >= ld_paddr && st_paddr <= ld_paddr + ld_size) ||
          (ld_paddr >= st_paddr && ld_paddr <= st_paddr + st_size)) {

        // update m_st_ld_map
        m_st_ld_map[st_seq_num].push_back(ld_inst);

        return true;
      }
    }
  }

  return false;
}

// if the head of either queue squashes and they had a spad retry set 
// for them, then this retry if void... and can actually cause assert fails in
// some cases
// annoying have to define scratchpad here, removing modularity, but watevs
void
MemUnit::clearPortRetry() {
  /*BaseSlavePort *slave_port = &(m_cpu_p->getDataPort().getSlavePort());
  if (CpuPort *slaveSpadPort = dynamic_cast<CpuPort*>(slave_port)) {
    return slaveSpadPort->clearRetry();
  }*/
}

void
MemUnit::linetrace(std::stringstream& ss)
{
#ifdef DEBUG
  std::string s = " [X_" + m_name + "] ";

  // S0
  if (m_status[Status::Squashed])
    s += "x";
  else if (m_status[Status::S0_Stalled])
    s += "#";
  else if (m_status[Status::S0_Busy])
    s += m_addr_calculated_inst->toString();
  ss << std::setw(35) << std::left << s;

  // S1
  s = " -> ";
  if (m_status[Status::Squashed])
    s += "x";
  else if (m_status[Status::S1_Stalled])
    s += "#";
  else if (m_status[Status::S1_Busy])
    s += m_translated_inst->toString();
  ss << std::setw(30) << std::left << s;

  // S2
  s = " -> ";
  if (m_status[Status::Squashed])
    s += "x";
  else if (m_status[Status::S2_Stalled])
    s += "#";
  else if (m_status[Status::S2_Busy])
    for (auto inst : m_issued_insts)
      s += inst->toString() + " ";
  ss << std::setw(30) << std::left << s;

  // reset
  m_addr_calculated_inst = nullptr;
  m_translated_inst = nullptr;
  m_issued_insts.clear();
  m_status.reset();
#endif
}
