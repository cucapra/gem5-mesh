#include "custom/vec_inst_sel.hh"
#include "custom/vector.hh"
#include "cpu/io/cpu.hh"

#include "debug/Mesh.hh"

VecInstSel::VecInstSel(IOCPU *_cpu_p, IOCPUParams *params) :
  Named(_cpu_p->name() + ".vec_cmd"),
  m_cpu_p(_cpu_p),
  _uopPC(0),
  _uopIssueLen(0),
  _uopCnt(0),
  _maxVecCmds(params->meshBufferSize),
  _lastICacheResp(nullptr),
  _pendingICacheReqAddr(0),
  _pendingICacheReq(false),
  _toEnqueue(nullptr),
  _enqueueEvent([this] { enqueueCmd(); }, name()),
  _tempREMEMS(0),
  _tempBlocksRecv(0),
  _tempBlocksPopped(0),
  _lastSendTick(0),
  _stallUntilJumpPC(false),
  _waitingForTerminator(false),
  _terminatorFound(false),
  _pendingFailedReq(false),
  _failedReqVirtAddr(0)
{
}

// enqueue an instruction or packet from port
// wait 1 cycle to place into the queue
// but if it's a macro op command allow icache req to be issued this cycle
bool
VecInstSel::enqueueTiming(PacketPtr pkt) {
  // push if we can recv this cycle
  if (getRdy()) {
    // get veccmd and schedule to be added at the beginning of the next cycle
    Vector::SenderState* ss =
              safe_cast<Vector::SenderState*>(pkt->popSenderState());
    auto msg = ss->master_data;
    assert(!_toEnqueue); // structural hazard
    _toEnqueue = msg;
    m_cpu_p->schedule(_enqueueEvent, m_cpu_p->clockEdge(Cycles(1)));
    if (!_toEnqueue->isInst) {
      _tempBlocksRecv++;
      DPRINTF(Mesh, "recv block %d\n", _tempBlocksRecv);
    }
    _toEnqueue->recvCnt = _tempBlocksRecv;
    // also if this is a macro op command schedule
    processHead();

    // cleanup
    delete ss;
    delete pkt;

    return true;
  }

  return false;
}

bool
VecInstSel::getRdy() {
  // we can safely push onto the queue for two of the following cases
  // 1) there is space now
  // 2) there is not space now, but we are guarenteed to consume a slot this cycle due to vec pipe state -- CHEATING!!!
  return (_vecCmds.size() < _maxVecCmds /* || willHaveOpening()*/);
}

// actually enqueue at beginnign of the cycle to be usable
void
VecInstSel::enqueueCmd() {
  DPRINTF(Mesh, "enqueue inst %s\n", _toEnqueue->inst->toString(true));
  _vecCmds.push(_toEnqueue);
  _toEnqueue = nullptr;
}

bool
VecInstSel::getVal() {
  if (!_vecCmds.empty() && !_vecCmds.front()->isInst && !_lastICacheResp && lastReqNoInst != m_cpu_p->curCycle()) {
    NoFetchedInst++;
    lastReqNoInst = m_cpu_p->curCycle();
  }
  // not empty with inst cmd or a uop is available
  return (!_vecCmds.empty() && _vecCmds.front()->isInst) ||
    (_lastICacheResp);
}

// new slot will become available
bool
VecInstSel::willHaveOpening() {
  // if not going to pull off b/c this is instruction then def won't
  // be a free spot even if the vec stage doesn't stall
  if (isPCGenActive()) return false;

  // otherwise a slot will open up at b/c we're going to pull the vec cmd off this cycle
  Vector *vec = m_cpu_p->getEarlyVector();
  return vec->canPullMesh();
}

// dequeue an instruction (either from icache or mesh, but unknown to caller and frankly does not matter)
// this should be called every cycle kind of like a tick function
IODynInstPtr
VecInstSel::dequeueInst() {
  IODynInstPtr ret = nullptr;

  DequeueReqs++;

  // // profile no instruction, want call this if no instruction
  // if (!_lastICacheResp && !_vecCmds.empty() && !_vecCmds.front()->isInst) {
  //   NoFetchedInst++;
  // }

  // if we have an icache resp, then prioritize that
  if (_lastICacheResp) {
    // this assumes icache response will be recv at the beginning of the cycle
    ret = _lastICacheResp;
    _lastICacheResp = nullptr;

    if (ret) {
      DPRINTF(Mesh, "pop %s\n", ret->toString(true));
    }

    if (ret && ret->static_inst_p->isRemem()) {
      _tempREMEMS++;
      // DPRINTF(Mesh, "create remem %s count %d\n", ret->toString(true), _tempREMEMS);
    }


    // ok so if the last instruction is a jump (we should never finish a block on a jump)
    // we need to stop the next instruction from issuing until we get a PC
    // you can imagine this is a feedback path from opcode in decode (cheap) and we will get
    // this at the beginning of the next cycle but we might have already sent an instruction
    // out to another core, so we need to do early
    // then we wait for PC to come in via a squash as the usual core would do
    //
    // careful if jump is the last instruction then don't want to stall for it? just don't allow this scenario for now
    // but if you want then need to not stall on it and just send off a nop instead
    if (ret->isUncondCtrl() || ret->isCondCtrl()) {
      // TODO not sure if this if condition is required anymore
      if ((_uopIssueLen - _uopCnt > 0) || (_waitingForTerminator)) {
        // DPRINTF(Mesh, "stalling due to jump %s\n", ret->toString(true));
        // _stallUntilJumpPC = true;
      }
      // TODO not sure this is needed anymore
      else {
        // when making a nop that will get copied, can't actually use nopStaticInstPtr?
        RiscvISA::Decoder dec;
        ret->static_inst_p = dec.decode(0x00000033, 0);
        // ret =
        //   std::make_shared<IODynInst>(StaticInst::nopStaticInstPtr, ret->pc,
        //                               ret->seq_num,
        //                               ret->thread_id, m_cpu_p);
      }
    }
    // TODO this will happen in decode so should still log an additional icache access that will be squashed
    else if (ret->static_inst_p->isTerminator()) {
      _terminatorFound = true;
    }


  }

  // otherwise tyr to get instruction at the front of queue
  else if (!_vecCmds.empty()) {
    // look at head of queue
    auto& cmd = _vecCmds.front();

    // if its a single inst and we can just pull that off
    if (cmd->isInst) {
      ret = cmd->inst;

      DPRINTF(Mesh, "pop inst %s\n", ret->toString(true));

      // TODO move this pop into process head?
      _vecCmds.pop();
    }
  }

  // remove the block if its a devec
  // TODO should prob pass flag into process head
  if (ret && ret->isSquashAfter()) {
    cleanCurIssueBlock();
    _vecCmds.pop();
  }
  
  // handle the next operation if there is an element on the queue
  processHead();

  return ret;

}

// do any actions for the head of the queue or the incoming cmd if it would be 
// the head
void
VecInstSel::processHead() {
  // DPRINTF(Mesh, "process head rem uops %d\n", _uopIssueLen - _uopCnt);
  // pop off pc head if finished
  // TODO need uopIssueLen > 0 b/c not popping inst cmd here
  if (!_vecCmds.empty() && !_vecCmds.front()->isInst && !isPCGenActive() && /* TODO is next part needed?*/(_uopIssueLen > 0 || _waitingForTerminator)) {
    DPRINTF(Mesh, "pop pc block %d\n", _vecCmds.front()->recvCnt);
    cleanCurIssueBlock();
    _vecCmds.pop();
  }

  // see if we can issue an instruction from either the next vec cmd or incoming
  if (!_vecCmds.empty() ||
      (_vecCmds.empty() && _toEnqueue)) {
    auto cmd = _toEnqueue;
    if (!_vecCmds.empty()) {
      cmd = _vecCmds.front();
    }
    if (!cmd->isInst) {
      // set new pc
      if (!isPCGenActive()) {
        int cnt = extractInstCntFromVissue(cmd->inst);
        setPCGen(cmd->pc, cnt);
      }
      // try to fetch
      tryReqNextUop();
    }
  }
}

void
VecInstSel::doSquash(SquashComm::BaseSquash &squashInfo, StageIdx initiator) {
  // allow to continue fetching after recving branch resolution from either decode or writeback
  // don't do anything about commit stalls (i.e. devec)?
  if (initiator == StageIdx::DecodeIdx || initiator == StageIdx::IEWIdx) {
    // if (_stallUntilJumpPC) DPRINTF(Mesh, "resolve pending jump %s %s\n", squashInfo.next_pc, squashInfo.trig_inst->toString(true));
    // _stallUntilJumpPC = false;
    _uopPC = squashInfo.next_pc;
    
    _lastICacheResp = nullptr;

    // send out a request now b/c vector stage won't tick this if there is no uop present...
    tryReqNextUop();
  }
}

// reset by cleaning queue and removing any uop tracking state
void
VecInstSel::reset() {
  cleanCurIssueBlock();
  while(!_vecCmds.empty()) {
    _vecCmds.pop();
  }
}

void
VecInstSel::cleanCurIssueBlock() {
  _uopIssueLen = 0;
  _uopCnt = 0;
  _waitingForTerminator = false;
  _terminatorFound = false;
}

// recv response from the icache and store in buffer
// called from cpu seletively when vec is configured
// NOTE we don't support compressed instructions so don't have to wait for two 
// reqs across the cacheline
// NOTE in hardware this would be the same logic as fetch, but we seperate in gem5
// because easier to manage and think about
void
VecInstSel::recvIcacheResp(PacketPtr pkt) {
    DPRINTF(Mesh, "recv icache pkt %#x expecting %#x\n", pkt->getAddr(), _pendingICacheReqAddr);
  // make sure this was to us and not a stale fetch icache packet
  if (pkt->getAddr() != _pendingICacheReqAddr || !_pendingICacheReq) {
    // potentially we were waiting for this to finish, check if so and issue req for pending
    if (_pendingFailedReq) {
      _pendingFailedReq = false;
      sendICacheReq(0, _failedReqVirtAddr);
    }
    return;
  }

  // build the fetched instruction
  TheISA::MachInst* cache_insts =
                    reinterpret_cast<TheISA::MachInst*>(pkt->getPtr<uint8_t>());

  // we really request 32bits but the whole line was returned by ICache, so get the 32bits we want
  // NOTE make sure these are both physical addresses
  size_t offset = 0; //(_uopPC.instAddr() - pkt->getAddr()) / sizeof(TheISA::MachInst);
  TheISA::MachInst mach_inst = TheISA::gtoh(cache_insts[offset]);

  // decode 32bit instruction
  RiscvISA::Decoder decoder;
  StaticInstPtr static_inst = decoder.decode(mach_inst, 0x0);

  int tid = 0;
  IODynInstPtr inst =
          std::make_shared<IODynInst>(static_inst, _uopPC,
                                      m_cpu_p->getAndIncrementInstSeq(),
                                      tid, m_cpu_p);
  DPRINTF(Mesh, "create inst %s\n", inst->toString(true));
  // increment the pc and uops
  _uopPC.pc(_uopPC.instAddr() + sizeof(RiscvISA::MachInst));
  _uopPC.npc(_uopPC.pc() + sizeof(RiscvISA::MachInst));
  _uopCnt++;

  // record stat
  m_cpu_p->getFetch()->m_32bit_icache_accesses++;

  // mark we've recv instruction
  _lastICacheResp = inst;

  // mark inst no longer pending (to allow another instruction to be sent)
  _pendingICacheReq = false;

  delete pkt;
}


// bypass instruction decoder to get IMM5 field for vissue instruction
int
VecInstSel::extractInstCntFromVissue(IODynInstPtr inst) {
  // extract count from the instructions, bits 11,7
  RiscvISA::MachInst machInst = inst->static_inst_p->machInst;
  int imm5 = bits(machInst, 11, 7);
  DPRINTF(Mesh, "%#x imm5 %d\n", machInst, imm5);
  return imm5;
}

void
VecInstSel::setPCGen(TheISA::PCState issuePC, int cnt) {
  _tempBlocksPopped++;
  assert(_tempBlocksPopped <= _tempBlocksRecv);
  DPRINTF(Mesh, "set pc gen pc %#x cnt %d blks popped %d\n", issuePC.instAddr(), cnt, _tempBlocksPopped);
  _uopPC = issuePC;
  _uopPC.npc(_uopPC.pc() + sizeof(RiscvISA::MachInst));
  _uopCnt = 0;
  _uopIssueLen = cnt;
  if (_uopIssueLen == 0) { // do based on terminating instruction rather than cnt
    _waitingForTerminator = true;
  }
}

bool
VecInstSel::isPCGenActive() {
  return ((_uopCnt < _uopIssueLen) && (_uopIssueLen > 0)) || (_waitingForTerminator && !_terminatorFound);
}


void
VecInstSel::sendICacheReq(int tid, Addr instAddr) {
  // Addr lineAddr = instAddr & ~(m_cpu_p->getCacheLineSize() - 1);
  // int fetchSize = m_cpu_p->getCacheLineSize();
  int fetchSize = sizeof(RiscvISA::MachInst);

  RequestPtr req = std::make_shared<Request>
                                  (tid, instAddr, fetchSize,
                                    Request::INST_FETCH,
                                    m_cpu_p->instMasterId(), instAddr,
                                    m_cpu_p->tcBase(tid)->contextId());

  // translate instruction addr atomically (right now!)
  Fault fault = m_cpu_p->itb->translateAtomic(req, m_cpu_p->tcBase(tid), BaseTLB::Execute);
  assert(fault == NoFault);
  
  // Addr lineAddr = instAddr & ~(m_cpu_p->getCacheLineSize() - 1);
  // DPRINTF(Mesh, "request lineAddr %#x addr %#x\n", lineAddr, instAddr);

  // send the req to the cache
  PacketPtr inst_pkt = new Packet(req, MemCmd::ReadReq);
  inst_pkt->dataDynamic(new uint8_t[fetchSize]);



  
  // might be busy do transition from fetch stage, need to keep delaying as long as can't send
  if (!m_cpu_p->getInstPort().sendTimingReq(inst_pkt)) {
    DPRINTF(Mesh, "fail to send req for %#x. save for later\n", instAddr);
    delete inst_pkt;
    
    // save this to retry when we recv the packet thats causing the stall
    // this is a gem5 specific thing I believe, but timing should be realistic 
    // (and this happens at most once in the whole runtime so doesn't really matter if a couple cycles off)
    _pendingFailedReq = true;
    _failedReqVirtAddr = instAddr;

  }
  else {
    DPRINTF(Mesh, "send icache req for %#x\n", instAddr);

    _pendingICacheReq = true;
    _pendingICacheReqAddr = inst_pkt->getAddr();
  }
}

// try to send a request for the next uop addr
void
VecInstSel::tryReqNextUop() {

  // record reason can't issue next
  if (_lastICacheResp) {
    AlreadyInstruction++;
  }
  else if (_pendingICacheReq) {
    TryFetchAgain++;
  }
  else if (_stallUntilJumpPC) {
    StallsOnControl++;
  }

  // TODO in the case of stallUntilJUmpPC should we still issue the reqs and then drop them
  // to make a fair comparison with how the normal fetch stage does this
  if (_lastICacheResp || _pendingICacheReq || _stallUntilJumpPC) return;
  
  if (isPCGenActive()) {
    assert(_lastSendTick != curTick());
    _lastSendTick = curTick();
    sendICacheReq(0, _uopPC.instAddr());
  }

}

void
VecInstSel::regStats(std::string parentName) {
  MeshQueueSize
    .init(1, _maxVecCmds + 1)
    .name(parentName + "." + name() + ".occupancy")
    .desc("number of cycles mesh queue has a certain occupancy while vec unit is configured")
    .flags(Stats::total | Stats::pdf | Stats::dist);
  // iew_dep_insts.ysubnames(Enums::OpClassStrings);

  NoFetchedInst
    .name(parentName + "." + name() + ".no_fetched_inst")
    .desc("vissue block but no instruction rdy")
  ;  

  TryFetchAgain
    .name(parentName + "." + name() + ".fetch_multiple")
    .desc("tried to fetch while already pending inst")
  ;  

  StallsOnControl
    .name(parentName + "." + name() + ".stall_control")
    .desc("stall due to waiting for control to resolve")
  ;  

  AlreadyInstruction
    .name(parentName + "." + name() + ".already_inst")
    .desc("cant fetch because already instruction rdy")
  ;   

  DequeueReqs
    .name(parentName + "." + name() + ".dequeue_reqs")
    .desc("number of cycles there was a dequeue request from vec stage")
  ;   

}

void
VecInstSel::profile() {
  MeshQueueSize[0][_vecCmds.size()]++;
}