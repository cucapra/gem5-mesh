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
#include "debug/RiscvVector.hh"
#include "debug/Frame.hh"

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
  // 3 stage config
  // // S2 stage
  // doMemIssue();
  // // S1 stage
  // doTranslation();
  // // S0 stage
  // doAddrCalc();

  // 1 stage config
  doAddrCalc();
  doTranslation();
  doMemIssue();
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

  if (m_s0_inst->static_inst_p->isSpadSpeculative()) 
    DPRINTF(Mesh, "addr calc %s srcReg %lx\n", m_s0_inst->toString(true),
      m_cpu_p->readIntReg(m_s0_inst->renamedSrcRegIdx(0)));

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
  if (m_s1_inst->isLoad() && m_ld_queue.size() == m_num_lq_entries) {
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

  if (m_s1_inst->static_inst_p->isSpadSpeculative())
    DPRINTF(Mesh, "translate %s\n", m_s1_inst->toString(true));

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
        if (inst->static_inst_p->isSpadSpeculative()) 
          DPRINTF(Mesh, "failed to send [%s]\n", inst->toString(true));
        // delete the pkt and we'll retry later
        delete pkt->popSenderState();
        delete pkt;
#ifdef DEBUG
        m_status.set(S2_Stalled);
#endif
        // exit issue stage early since the dcache is busy
        return;
      } else {
        DPRINTF(LSQ, "Sent request to memory for inst %s with vaddr %#x paddr %#x\n", inst->toString(true), pkt->req->getVaddr(), pkt->getAddr());
        
        if (m_cpu_p->getEarlyVector()->isSlave()) 
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

      // save address b/c the packet can be deleted if noack
      Addr addr = pkt->getAddr();
      int respCnt = pkt->getRespCnt();

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
        m_store_diff_reg+=respCnt;
        DPRINTF(LSQ, "Sent request to memory for inst %s with vaddr %#x paddr %#x\n", inst->toString(true), pkt->req->getVaddr(), addr);
        if (m_cpu_p->getEarlyVector()->getConfigured()) 
          DPRINTF(Mesh, "Send %s to paddr %#x sp vaddr %#x\n", inst->toString(true), addr, m_cpu_p->readArchIntReg(RiscvISA::StackPointerReg, 0));

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

        // record stat about prefetch sent to memory
        // only include left prefetch
        if (inst->static_inst_p->isSpadPrefetch() && inst->static_inst_p->isLeftSide()) {
          auto upper7 = bits((uint32_t)inst->static_inst_p->machInst, 31, 25);
          auto lower5 = bits((uint32_t)inst->static_inst_p->machInst, 11, 7);
          uint32_t imm = (upper7 << 5) | lower5;
          int config = bits(imm, 1, 0);
          size_t count = bits(imm, 11, 2);

          if (count == 1) {
            m_scalar_prefetches++;
          }
          else if (count > 1) {
            if (config == 0) {
              m_horizontal_prefetches++;
            }
            else {
              m_vertical_prefetches++;
            }
          }
          else {
            assert(false);
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


  #ifndef NDEBUG
  DPRINTF(LSQ, "Load queue after squashing ...\n");
  for (auto& inst : m_ld_queue)
    DPRINTF(LSQ, "\t%s\n", inst->toString());

  DPRINTF(LSQ, "Store queue after squashing ...\n");
  for (auto& inst : m_st_queue)
    DPRINTF(LSQ, "\t%s\n", inst->toString());
  #endif

#ifdef DEBUG
  m_status.set(Status::Squashed);
#endif
}

void
MemUnit::processCacheCompletion(PacketPtr pkt)
{
  // lazy ack so have already completed the store
  if (pkt->isStoreNoAck()) {
    m_store_diff_reg-=pkt->getRespCnt();
    delete pkt;
    return;
  }

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
        if (ss->inst->static_inst_p->isSpadSpeculative())
          DPRINTF(Mesh, "%s Setting dest reg %i (%s) ready\n",
                      ss->inst->toString(true),
                      ss->inst->renamedDestRegIdx(i)->index(),
                      ss->inst->renamedDestRegIdx(i)->className());
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
    m_store_diff_reg-=pkt->getRespCnt();
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

  // a prefetch to a scratchpad requires more complex decoding here
  bool spadPrefetch = m_s1_inst->static_inst_p->isSpadPrefetch();

  // adjust where the addr is in the case of a prelw
  if (spadPrefetch) {
    // immediate field used for count so remove that from the address
    auto upper7 = bits((uint32_t)m_s1_inst->static_inst_p->machInst, 31, 25);
    auto lower5 = bits((uint32_t)m_s1_inst->static_inst_p->machInst, 11, 7);
    uint32_t imm = (upper7 << 5) | lower5;
    addr = addr - imm;
  }

  // vector loads have larger full size, but in some case only a subset will be executed
  // make sure that is considered before detecting a misaligned address fault
  // fixed register size is 16 (not something that can be changed in python config)
  size_t effReqSize = size;
  if (m_s1_inst->isVector()) {
    // effReqSize = (size / m_cpu_p->getHardwareVectorLength()) * m_cpu_p->readMiscRegNoEffect(RiscvISA::MISCREG_VL, 0);
    
    // only make sure one word doesnt go off for vector
    // will automatically split words in scratchpad
    // effReqSize = (size / m_cpu_p->getHardwareVectorLength());

    // TODO assume always word sized. above  doesnt work b/c size is hardcoded
    // and not reflective of hardwareVectorLenght param most of the time
    effReqSize = sizeof(uint32_t);

    // for same reason need to figure out size of request when writing to scratchpad
    // (only relevant then)
    if (m_s1_inst->isStore())
      size = effReqSize * m_cpu_p->getHardwareVectorLength();

  }

  // check if the request is spanning across two cache lines
  size_t line_size = m_cpu_p->getCacheLineSize();
  if ((addr & (line_size - 1)) + effReqSize > line_size) {
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
    

    m_s1_inst->mem_req_p->isSpadPrefetch = spadPrefetch;
    m_s1_inst->mem_req_p->isStoreNoAck = m_s1_inst->static_inst_p->isAckFree() && !spadPrefetch;

    // adjust where the addr is in the case of a prelw
    if (spadPrefetch) {
      // the 'data' register for this instruction is actually an address
      // convert accordingly
      Addr spadVAddr = 0;
      for (int i = size - 1; i >= 0; i--) {
        spadVAddr |= ((uint64_t)data[i]) << (i * 8);
      }

      // part of this address encodes the coreOffset we're going to use
      int baseCoreOffset = bits(spadVAddr, 31, 12);

      // fake the virtual scratchpad address for this core
      // TODO this should put the vector group origin on instead of the scratchpad
      Addr spadIdx = bits(spadVAddr, 11, 0);
      spadVAddr = 0x10000000 | (m_cpu_p->cpuId() << 12) | ( spadIdx * size );
      
      // need to translate the address, do atomically,
      // real hammerblade doesnt have virtual addresses anyway
      Addr spadPAddr = 0;
      assert(m_cpu_p->tcBase(tid)->getProcessPtr()->pTable->translate(spadVAddr, spadPAddr));
      m_s1_inst->mem_req_p->prefetchAddr = spadPAddr;

      // immediate field used for count so remove that from the address
      auto upper7 = bits((uint32_t)m_s1_inst->static_inst_p->machInst, 31, 25);
      auto lower5 = bits((uint32_t)m_s1_inst->static_inst_p->machInst, 11, 7);
      uint32_t imm = (upper7 << 5) | lower5;
      // addr = addr - imm;

      // if we are going to go off of a cacheline then cut this load short depending on
      // if prefetch left or prefetch right
      // left  : offset = baseCoreOffset.          leftCount  = min(endOfCacheLine, count)
      // right : offset = coreOffset + leftCount   rightCount = count - leftCount 
      bool prefetchLeft = m_s1_inst->static_inst_p->isLeftSide();
      size_t lineSize = m_cpu_p->getCacheLineSize();
      size_t countPerCore = bits(imm, 11, 2);
      int config = bits(imm, 1, 0);
      int vecDimX = m_cpu_p->getEarlyVector()->getXLen();
      int vecDimY = m_cpu_p->getEarlyVector()->getYLen();

      int numCores = (config == 1) ? vecDimX * vecDimY : 1;
      size_t totalResp = countPerCore * numCores;

      size_t wordOffset = addr & (lineSize - 1);
      size_t wordsRemInLine = (lineSize - wordOffset) / size;
      size_t leftCount = std::min(wordsRemInLine, totalResp);
      m_s1_inst->mem_req_p->prefetchConfig = config;
      if (prefetchLeft) {
        m_s1_inst->mem_req_p->coreOffset = baseCoreOffset;
        m_s1_inst->mem_req_p->subCoreOffset = 0;
        m_s1_inst->mem_req_p->respCnt = leftCount;
        DPRINTF(Frame, "send vec load left global %#x spad %#x core offset %d cpc %d config %d, cnt %d\n", 
          m_s1_inst->mem_req_p->getVaddr(), m_s1_inst->mem_req_p->prefetchAddr,
          m_s1_inst->mem_req_p->coreOffset, countPerCore, config, m_s1_inst->mem_req_p->respCnt);
      }
      else {
        int rightCount = totalResp - wordsRemInLine;
        // don't execute this the right prefetch because no overshoot
        if (rightCount <= 0) {
          m_s1_inst->setExecuted();
          DPRINTF(Frame, "don't send vec load right global %#x spad %#x cnt %d\n", 
            m_s1_inst->mem_req_p->getVaddr(), m_s1_inst->mem_req_p->prefetchAddr,
            rightCount);
        }
        else {
          // bool isVerticalLoad = (config == 1);
          // if (isVerticalLoad) {
          //   m_s1_inst->mem_req_p->coreOffset = baseCoreOffset;
          //   m_s1_inst->mem_req_p->prefetchAddr = spadPAddr + leftCount * sizeof(uint32_t);
          // }
          // else m_s1_inst->mem_req_p->coreOffset = baseCoreOffset + leftCount;
          m_s1_inst->mem_req_p->coreOffset = baseCoreOffset + leftCount / countPerCore;
          m_s1_inst->mem_req_p->subCoreOffset = leftCount % countPerCore;
          // m_s1_inst->mem_req_p->prefetchAddr = spadPAddr + (leftCount ) * sizeof(uint32_t);
          m_s1_inst->mem_req_p->respCnt = rightCount;
          // change vaddr to reflect new baseOffset
          Addr rightVirtAddr = addr + (leftCount * size);
          m_s1_inst->mem_req_p->setVirt(0, rightVirtAddr, size, flags, 
              m_cpu_p->dataMasterId(), m_s1_inst->pc.pc(), amo_op);

          DPRINTF(Frame, "send vec load right %#x spad %#x core offset %d word offset %d cnt %d\n", 
            m_s1_inst->mem_req_p->getVaddr(), m_s1_inst->mem_req_p->prefetchAddr,
            m_s1_inst->mem_req_p->coreOffset, m_s1_inst->mem_req_p->subCoreOffset, m_s1_inst->mem_req_p->respCnt);
        }
      }

      assert (m_cpu_p->getEarlyVector());

      m_s1_inst->mem_req_p->countPerCore = countPerCore;
      m_s1_inst->mem_req_p->xDim = m_cpu_p->getEarlyVector()->getXLen();
      m_s1_inst->mem_req_p->yDim = m_cpu_p->getEarlyVector()->getYLen();

      // to self
      if (config == 2) {
        m_s1_inst->mem_req_p->xOrigin = m_cpu_p->cpuId(); // flattened so just set as full idx
        m_s1_inst->mem_req_p->yOrigin = 0; // flattened so just set to 0
      }
      else { 
        m_s1_inst->mem_req_p->xOrigin = m_cpu_p->getEarlyVector()->getXOrigin();
        m_s1_inst->mem_req_p->yOrigin = m_cpu_p->getEarlyVector()->getYOrigin();
      }

    }
    else {
      m_s1_inst->mem_req_p->countPerCore = 1;
      m_s1_inst->mem_req_p->xDim = 1;
      m_s1_inst->mem_req_p->yDim = 1;
      m_s1_inst->mem_req_p->xOrigin = 0;
      m_s1_inst->mem_req_p->yOrigin = 0;
      m_s1_inst->mem_req_p->respCnt = 1;
      // m_s1_inst->mem_req_p->coreOffset = m_s1_inst->seq_num; // for debugging normal loads and stores
    }
    
    // allow load to issue to spad without getting any acks the load is there
    m_s1_inst->mem_req_p->spadSpec  = m_s1_inst->static_inst_p->isSpadSpeculative();

    // handle packed simd vec request, TODO not sure how to handle, need to serialize similarly to cur vec requests 
    // but wont be noack, and should come directly into register rather than spad
    if (m_s1_inst->isVector()) {
      std::vector<Addr> vecAddrs = m_s1_inst->generateAddresses();

      m_s1_inst->mem_req_p->respCnt = vecAddrs.size(); //;m_cpu_p->readMiscRegNoEffect(RiscvISA::MISCREG_VL, 0);
      // assert(m_s1_inst->mem_req_p->respCnt > 0); // can hit this condition if csr write hasnt happened, ok b/c will be squashed
      // just set resp cnt to 1 b/c will be squashed
      if (m_s1_inst->mem_req_p->respCnt == 0) {
        m_s1_inst->fault = std::make_shared<RiscvISA::AddressFault>
                                        (addr, RiscvISA::INST_ACCESS);
      }
      m_s1_inst->mem_req_p->prefetchConfig = 1; // vertical
      m_s1_inst->mem_req_p->xOrigin = m_cpu_p->cpuId(); // flattened so just set as full idx
      m_s1_inst->mem_req_p->yOrigin = 0; // flattened so just set to 0
      m_s1_inst->mem_req_p->coreOffset = 0;
      m_s1_inst->mem_req_p->subCoreOffset = 0;
      m_s1_inst->mem_req_p->countPerCore = m_s1_inst->mem_req_p->respCnt; // only fetches for one core
      m_s1_inst->mem_req_p->xDim = 1;
      m_s1_inst->mem_req_p->yDim = 1;
      m_s1_inst->mem_req_p->isNormVector = true;

      
      // do functional translation of every address
      for (int i = 0; i < vecAddrs.size(); i++) {
        Addr vAddr = vecAddrs[i];
        Addr pAddr;
        // can prob just do based on relative address of base offset b/c likely
        // in same page, but dont do b/c overoptimization that could mess us up!
        assert(m_cpu_p->tcBase(tid)->getProcessPtr()->
          pTable->translate(vAddr, pAddr));

        vecAddrs[i] = pAddr;
        // DPRINTF(RiscvVector, "vec addr (%d) %#x\n", i, pAddr);
      }
      m_s1_inst->mem_req_p->vecAddrs = vecAddrs;

      // TODO cheap out and just assume words. need to acutally look at instruction
      // or vsew (vtype) to know
      // not valid too use getHardwareVectorLength() here.
      m_s1_inst->mem_req_p->wordSize = sizeof(uint32_t);

      if (vecAddrs.size() > 0)
        DPRINTF(RiscvVector, "%s send vector request %#x of size %d load ? %d\n", m_s1_inst->toString(true), vecAddrs[0], m_s1_inst->mem_req_p->respCnt, is_load);
    }
    else {
      m_s1_inst->mem_req_p->isNormVector = false;
    }


    // this memory will be deleted together with the dynamic instruction
    m_s1_inst->mem_data_p = new uint8_t[size];

    if (data)
      memcpy(m_s1_inst->mem_data_p, data, size);
    else
      memset(m_s1_inst->mem_data_p, 0, size);

    if (!m_s1_inst->isFault()) { 
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

      if (m_s1_inst->isFault()) {
        DPRINTF(Mesh, "fault could not translate %lx for inst %s\n", addr, m_s1_inst->toString(true));
        m_s1_inst->setExecuted();
      }
    }
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

  if (m_s1_inst->mem_req_p->isSpadPrefetch)
    DPRINTF(Frame, "pf addr trans %#x -> %#x\n", m_s1_inst->mem_req_p->getVaddr(),
      m_s1_inst->mem_req_p->getPaddr());
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
MemUnit::regStats() {
  m_vertical_prefetches
    .name(name() + ".vertical_prefetches")
    .desc("number of vertical prefetches (left) with length greater than one")
  ;
  m_horizontal_prefetches
    .name(name() + ".horizontal_prefetches")
    .desc("number of horizontal prefetches (left) with length greater than one")
  ;
  m_scalar_prefetches
    .name(name() + ".scalar_prefetches")
    .desc("number of prefetches with length one")
  ;
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
