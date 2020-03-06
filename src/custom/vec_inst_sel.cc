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
  _maxVecCmds(2), // TODO should have in params!
  _lastICacheResp(nullptr),
  _pendingICacheReq(false),
  _toEnqueue(nullptr),
  _enqueueEvent([this] { enqueueCmd(); }, name())
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

    // also if this is a macro op command schedule
    processHead(_toEnqueue);

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
  // 2) there is not space now, but we are guarenteed to consume a slot this cycle due to vec pipe state
  return (_vecCmds.size() < _maxVecCmds || willHaveOpening());
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
  return !vec->canPullMesh();
}

// dequeue an instruction (either from icache or mesh, but unknown to caller and frankly does not matter)
// this should be called every cycle kind of like a tick function
IODynInstPtr
VecInstSel::dequeueInst() {
  IODynInstPtr ret = nullptr;

  // get instruction at the front of queue
  if (!_vecCmds.empty()) {
    // look at head of queue
    auto& cmd = _vecCmds.front();

    // if its a single inst and we can just pull that off
    if (cmd->isInst) {
      ret = cmd->inst;
      _vecCmds.pop();
    }
  }

  // otherwise its a pc uop fetch is at head of vector cmds (or nothing), try to give the icache resp
  if (_lastICacheResp) {
    // this assumes icache response will be recv at the beginning of the cycle
    ret = _lastICacheResp;
    _lastICacheResp = nullptr;

    // pop if we've finished this macro op
    if (!isPCGenActive()) {
      setPCGen(0, 0); // reset pc gen
      _vecCmds.pop();
    }
  }
  
  // handle the next operation if there is an element on the queue
  if (!_vecCmds.empty()) {
    processHead(_vecCmds.front());
  }

  return ret;

}

// do any actions for the head of the queue
void
VecInstSel::processHead(std::shared_ptr<MasterData> cmd) {
  if (!cmd->isInst) {
    // if not currently active we need to setup the uop pc
    if (!isPCGenActive()) {
      int cnt = extractInstCntFromVissue(cmd->inst);
      setPCGen(cmd->pc, cnt);
    }

    // issue icache req and hope to recv next cycle
    tryReqNextUop();
  }
}

// reset by cleaning queue and removing any uop tracking state
void
VecInstSel::reset() {
  setPCGen(0, 0);
  while(!_vecCmds.empty()) {
    _vecCmds.pop();
  }
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
  if (pkt->getAddr() != _pendingICacheReqAddr) return;

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
  _uopCnt++;

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
  DPRINTF(Mesh, "set pc gen pc %#x cnt %d\n", issuePC.instAddr(), cnt);
  _uopPC = issuePC;
  _uopCnt = 0;
  _uopIssueLen = cnt;
}

bool
VecInstSel::isPCGenActive() {
  return (_uopCnt < _uopIssueLen) && (_uopIssueLen > 0);
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
  m_cpu_p->getInstPort().sendTimingReq(inst_pkt);

  _pendingICacheReq = true;
  _pendingICacheReqAddr = inst_pkt->getAddr();
  DPRINTF(Mesh, "send icache req for %#x\n", instAddr);
}

// try to send a request for the next uop addr
void
VecInstSel::tryReqNextUop() {
  if (_lastICacheResp || _pendingICacheReq) return;
  
  if (isPCGenActive()) {
    sendICacheReq(0, _uopPC.instAddr());
  }

}