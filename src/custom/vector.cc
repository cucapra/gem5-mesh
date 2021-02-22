#include "arch/registers.hh"
#include "base/bitfield.hh"
#include "custom/vector.hh"
#include "cpu/io/cpu.hh"
//#include "arch/utility.hh"

#include "debug/Mesh.hh"

Vector::Vector(IOCPU *_cpu_p, IOCPUParams *p, size_t in_size, size_t out_size,
    StageIdx stageType, bool canRootSend, bool canRecv) : 
    Stage(_cpu_p, in_size, out_size, stageType, false),
    _numInPortsActive(0),
    _numOutPortsActive(0),
    _stage(FETCH),
    _curCsrVal(0),
    _stolenCredits(0),
    _canRootSend(canRootSend),
    _canRecv(canRecv),
    _numInstructions(0),
    _vecPassThrough(false),
    _meshRevecId(-1),
    _pipeRevecId(-1),
    _vecUops(_cpu_p, p)
{
}

void
Vector::tick() {

  Stage::tick();
  
  // TODO this awkwardly calls doSquash as well, would like to decouple
  bool squashed = checkSquash();
  if (squashed) return;

  // if not configured just pass the instruction through
  // not needed, still seems to work w/o, but will go faster through non-vec code
  if (!getConfigured()) {
    passInstructions();
    return;
  }
  
  // profile any stalling
  profile();
  
  // figure out the sources (parallel muxes) for the mesh net and pipeline respectively
  auto pipeSrc = getOutPipeSource();
  auto meshSrc = getOutMeshSource();

  // get stalling statuses at the beginning b/c may change over this func call
  bool meshPull = canPullMesh();
  bool pipePull = canPullPipe();
  bool meshPush = canPushMesh();
  bool pipePush = canPushPipe();

  // pull instructions for both sources if no stalls
  IODynInstPtr meshInst = nullptr;
  if (meshPull) {
    pullMeshInstruction(meshInst);
  }
  
  IODynInstPtr pipeInst = nullptr;
  if (pipePull) {
    pullPipeInstruction(pipeInst);
  }

  // if possible push instruction to next pipe stage and/or mesh network
  if (meshPush) {
     
    // forward instruction to other neighbors potentially
    if (meshSrc == Pipeline) {
      // DPRINTF(Mesh, "push pipe inst %s to mesh\n", pipeInst->toString(true));
      forwardInstruction(pipeInst);
    }
    else if (meshSrc == Mesh) {
      // DPRINTF(Mesh, "push mesh inst %s to mesh\n", meshInst->toString(true));
      forwardInstruction(meshInst);
    }
  }
  
  // give instruction to the local decode stage if present
  if (pipePush) {
    if (pipeSrc == Pipeline) {
      // DPRINTF(Mesh, "push pipe inst %s to pipe\n", pipeInst->toString(true));
      pushPipeInstToNextStage(pipeInst);
    
      // only needs to be done here? b/c always happens in late vector
      // MUST BE THE LAST THING TO HAPPEN!
      if (!hasNextStage()) { // actually make sure its the last stage that does this (HACK)
        pipeInst->updateMiscRegs();
      }
    }
    else if (pipeSrc == Mesh) {
      // DPRINTF(Mesh, "push mesh inst %s to pipe\n", meshInst->toString(true));
      pushMeshInstToNextStage(meshInst);
    }
  }

  // if the instruction is a revec we need to handle it
  // IMPORTANT that this updates the config on the next cycle, not the current one
  // so that's why we put this after the stalls have been considered for this cycle
  // also prevents revec from being sent twice
  handleRevec(pipeInst, meshInst);
  
}

bool
Vector::canRecvMeshPkt() {
  return _vecUops.getRdy() && getConfigured();
}

bool
Vector::enqueueMeshPkt(PacketPtr pkt) {
  return _vecUops.enqueueTiming(pkt);
}

void
Vector::recvICacheResp(PacketPtr pkt) {
  _vecUops.recvIcacheResp(pkt);
}

// TODO would like a refactor with the stuff in tick
bool
Vector::canPullMesh() {
  // figure out the sources (parallel muxes) for the mesh net and pipeline respectively
  auto pipeSrc = getOutPipeSource();
  auto meshSrc = getOutMeshSource();
  
  // these determine whetehr we can push to the respective output buffer
  bool outMeshStall = !canPushMesh();
  bool outPipeStall = !canPushPipe();
  
  // these determine whether we can pull from the respective input buffer
  bool inMeshStall = (meshSrc == Mesh && outMeshStall) || (pipeSrc == Mesh && outPipeStall) 
    // or not using at all
    || (meshSrc != Mesh && pipeSrc != Mesh);
  return !inMeshStall;
}

bool
Vector::canPullPipe() {
    // figure out the sources (parallel muxes) for the mesh net and pipeline respectively
  auto pipeSrc = getOutPipeSource();
  auto meshSrc = getOutMeshSource();
  
  // these determine whetehr we can push to the respective output buffer
  bool outMeshStall = !canPushMesh();
  bool outPipeStall = !canPushPipe();
  
  // these determine whether we can pull from the respective input buffer
  bool inPipeStall = (meshSrc == Pipeline && outMeshStall) || (pipeSrc == Pipeline && outPipeStall)
    // or not using at all
    || (meshSrc != Pipeline && pipeSrc != Pipeline);
  return !inPipeStall;
}

bool
Vector::canPushMesh() {
  // figure out the sources (parallel muxes) for the mesh net and pipeline respectively
  auto pipeSrc = getOutPipeSource();
  auto meshSrc = getOutMeshSource();
  
  // if outputs have different srcs then can decouple stalls
  bool coupledStalls = (pipeSrc == meshSrc);
  
  // these determine whetehr we can push to the respective output buffer
  bool outMeshStall = (meshSrc != None) && 
    // stall if own output is stalled
    (isOutMeshStalled() ||
    // stall if any inputs stalled
    (meshSrc == Pipeline && isInPipeStalled()) ||
    (meshSrc == Mesh     && isInMeshStalled()) ||
    // stall if coupled output is stalled
    (coupledStalls       && isOutPipeStalled()));
  
  return !outMeshStall;
}

bool
Vector::canPushPipe() {
    // figure out the sources (parallel muxes) for the mesh net and pipeline respectively
  auto pipeSrc = getOutPipeSource();
  auto meshSrc = getOutMeshSource();
  
  // if outputs have different srcs then can decouple stalls
  bool coupledStalls = (pipeSrc == meshSrc);

  bool outPipeStall = (pipeSrc != None) &&
    // stall if own output is stalled
    (isOutPipeStalled() ||
    // stall if any inputs stalled
    (pipeSrc == Pipeline && isInPipeStalled()) ||
    (pipeSrc == Mesh     && isInMeshStalled()) ||
    // stall if coupled output is stalled
    (coupledStalls       && isOutMeshStalled()));
  
  return !outPipeStall;

}


std::string
Vector::name() const {
  if (m_stage_idx == EarlyVectorIdx)
    return m_cpu_p->name() + ".vector";
  else
    return m_cpu_p->name() + ".late_vector";
}

void
Vector::init() {
}

void
Vector::regStats() {
    m_revec_stalls
      .name(name() + ".revec_stalls")
      .desc("number of stalls due to revec")
    ;

    m_backpressure_stalls
      .name(name() + ".backpressure_stalls")
      .desc("number of stalls due to backpressure")
    ;

    m_no_mesh_stalls
      .name(name() + ".mesh_input_stalls")
      .desc("number of stalls due to no input from mesh")
    ;

    m_no_pipe_stalls
      .name(name() + ".pipe_input_stalls")
      .desc("number of stalls due to no input from fetch")
    ;

    m_cycles_in_vec
      .name(name() + ".vec_cycles")
      .desc("number of cycles in vector mode")
    ;

    _vecUops.regStats(name());
}

void
Vector::profile() {
  // holding up due to revec not recv by mesh yet
  if (pipeHasRevec() && !meshHasRevec()) {
    m_revec_stalls++;
  }

  // no instruction from mesh and want to get
  else if (canReadMesh() && isInMeshStalled() && !isCurDiverged()){
    m_no_mesh_stalls++;
  }

  // no instruction from pipe and want to get
  else if (isInPipeStalled() && (!canReadMesh() || (canReadMesh() && isCurDiverged()))) {
    m_no_pipe_stalls++;
  }

  // stall due to back pressure
  else if (isOutMeshStalled() && !isInMeshStalled() && canWriteMesh()) {
    m_backpressure_stalls++;
  }

  // only register vec uop when configured
  if (getConfigured()) {
    m_cycles_in_vec++;
    _vecUops.profile();
  }
}

void
Vector::linetrace(std::stringstream& ss) {
  // TODO this can be really useful in debugging!
}

void
Vector::doSquash(SquashComm::BaseSquash &squashInfo, StageIdx initiator) {
  
  IODynInstPtr squash_inst = squashInfo.trig_inst;
  
  if (getConfigured()) {
    if (initiator == StageIdx::CommitIdx)
      DPRINTF(Mesh, "Squash from Commit: squash inst %s\n",
                      squash_inst->toString(true));
    else if (initiator == StageIdx::IEWIdx) {
      DPRINTF(Mesh, "Squash from IEW: squash inst %s\n",
                      squash_inst->toString(true));            
    }
    else if (initiator == StageIdx::DecodeIdx)
      DPRINTF(Mesh, "Squash from Decode: squash inst %s\n", 
                      squash_inst->toString(true));
  }
  
  ThreadID tid = squash_inst->thread_id;

  // walk through all insts in the m_insts queue and remove all instructions
  // belonging to thread tid
  size_t qsize = m_insts.size();
  size_t count = 0;
  IODynInstPtr inst = nullptr;
  while (count < qsize) {
    inst = m_insts.front();
    m_insts.pop();
    if (inst->thread_id != tid || 
      ((m_stage_idx == LateVectorIdx) && inst->seq_num <= squash_inst->seq_num)) {
      m_insts.push(inst);
    } else {
      if (getConfigured()) DPRINTF(Mesh, "Squashing %s\n", inst->toString());
      assert(inst->seq_num > squash_inst->seq_num);
      // increment the number of credits to the previous stage
      outputCredit()++;
    }
    count++;
  }
  
  // do squash in subunit
  _vecUops.doSquash(squashInfo, initiator);

  // if squash due to a traced instruction, then we need to exit trace mode
  // TODO probably want to go to 'transparent' where recv and send as before
  // but still doing own seperate fetch and instruction stream.
  // + Don't run into issue when one core diverges and causes rest to diverge b/c dominates them
  // - Potentially wasted energy on instruction pass throughs (especially if low usage)
  // - This core can potentially stall b/c target is stalled, but that's awkward b/c this is working on diff stream
  // if (initiator == StageIdx::IEWIdx && squash_inst->from_trace) {
  //   DPRINTF(Mesh, "[[INFO]] trace divergence [%s]\n", squash_inst->toString(true));
  //   //m_cpu_p->setMiscReg(RiscvISA::MISCREG_FETCH, 0, tid);
  //   _vecPassThrough = true;
  //   restoreCredits();
  // }
  
  // if this was a config squash and if we are a slave, then takeaway all credits?
  // but not sure we can make this check based on when the csr file is written and credits
  //
  // could have small wire carrying slave bit from commit stage -- ok
  //    a gem5 specific implementation detail of this is can't read the csr until executes (this ends up firing cycle after)
  // IMPORTANT that config is set before the squash comes in, otherwise wont work properly
  // How does -ve credit work? -> use twos complement to send back (currently using some small unsigned int) -- ok
  if (initiator == StageIdx::CommitIdx && !((SquashComm::CommitSquash*)&squashInfo)->is_trap_pending) {
    if (canReadMesh()) {
      stealCredits();
    }
    else {
      // restore credits when go back?, I guess done in setupConfig (b/c no squash in other)
    }
  }
  
}

void
Vector::passInstructions() {
  while (!m_insts.empty() && !checkStall()) {
    IODynInstPtr inst = m_insts.front();
    //ThreadID tid = inst->thread_id;
    //DPRINTF(Mesh, "[tid:%d] Decoding inst [sn:%lli] with PC %s\n",
    //                tid, inst->seq_num, inst->pc);
    // DPRINTF(Mesh, "%s %#x\n", inst->toString(true), m_cpu_p->readArchIntReg(2, 0));
    
    // send out this inst
    sendInstToNextStage(inst);

    // Remove the inst from the queue and increment the credit to the previous
    // stage.
    consumeInst(); // TODO credits need to reflect next queue, not this queue if combinational
    
    if (!hasNextStage()) { // actually make sure its the last stage that does this (HACK)
      inst->updateMiscRegs();
    }
  }
}

void
Vector::forwardInstruction(const IODynInstPtr& inst) {
  // check whether this stage is allowed to forward to the mesh net
  if (!canWriteMesh()) return;

  // find each direction to send a packet to
  std::vector<Mesh_DS_t> out;
  MeshHelper::csrToOutSrcs(RiscvISA::MISCREG_FETCH, _curCsrVal, out);
  // ret if no directions
  if (out.size() == 0) return;

  // if we are now in decoupled access mode, we don't send instructions anymore, just PCs?
  // could potentially send both if have explicit vector and scalar instruction
  VecInstSel::MasterData forwardInst;
  forwardInst = VecInstSel::MasterData(inst);
  if (inst->static_inst_p->isVectorIssue()) {
    forwardInst = VecInstSel::MasterData(inst->branchTarget());

    // also send instruction for ease of access tho
    forwardInst.inst  = inst;
  }
  else if (inst->static_inst_p->isBroadcast()) {
    // create new instruction that does a load immediate of the value computed
    // for not just use immediate in addiw, but could have a load lower immediate custom inst
    uint32_t opcode = 0x1b; //7bits
    uint32_t funct3 = 0x0;
    uint32_t dest_reg = inst->destRegIdx(0).index(); //5bits
    uint32_t src_reg = 0;
    uint32_t imm = inst->broadcast_val; // 20bits -- def bitfield IMM20  <31:12>;
    RiscvISA::MachInst mach_inst = 0x0;
    mach_inst |= opcode;
    mach_inst |= (funct3 << 12);
    mach_inst |= (src_reg << 15);
    mach_inst |= (dest_reg << 7);
    mach_inst |= (imm << 20);
    RiscvISA::Decoder decoder;
    StaticInstPtr static_inst = decoder.decode(mach_inst, 0x0);
    IODynInstPtr forged_inst =
        std::make_shared<IODynInst>(static_inst, inst->pc, inst->seq_num, inst->thread_id, m_cpu_p);
    forwardInst.inst = forged_inst;

    DPRINTF(Mesh, "forged inst %s from machinst %#x imm %#x\n", forged_inst->toString(true), mach_inst, imm);
  }
  else if (inst->isUncondCtrl() || inst->isCondCtrl()) {
    return; // don't send any control instructions
  }
  else if (isDecoupledAccess()) {
    return;
  }
  // if (isDecoupledAccess()) {
  //   if (instInfo.inst->static_inst_p->isVectorIssue()) {
  //     forwardInst = MasterData(instInfo.inst->branchTarget());
  //   }
  //   else {
  //     return;
  //   }
  // }

  // if this is a vector issue instruction then we need to turn this into a PC addr
  // MasterData forwardInst = instInfo;
  // if (instInfo.isInst && instInfo.inst->static_inst_p->isVectorIssue()) {
  //   forwardInst = MasterData(instInfo.inst->branchTarget());
  // }
  
  if (forwardInst.isInst)
    DPRINTF(Mesh, "Forward to mesh net %s\n", forwardInst.inst->toString(true));
  else
    DPRINTF(Mesh, "Forward to mesh net %#x\n", forwardInst.pc.instAddr());
  
  // send a packet in each direction
  for (int i = 0; i < out.size(); i++) {
    Mesh_Dir dir = out[i].outDir;
    //Mesh_Out_Src src = out[i].src;
    
    
    /*if (instInfo.inst->isCondCtrl()) {
      DPRINTF(Mesh, "bne sent pred taken %d target %#x\n", instInfo.inst->predicted_taken, instInfo.inst->readPredTarg());
    }*/
    
    //uint64_t meshData = encodeMeshData(instInfo);
    //DPRINTF(Mesh, "Sending mesh request %d from %d with val %#x %d %d = %#x\n", 
    //    dir, src, instInfo.machInst, instInfo.predictTaken, instInfo.mispredicted, meshData);
    //if (instInfo.predictTaken) DPRINTF(Mesh, "predict taken %d\n", instInfo.predictTaken);
    //if (instInfo.mispredicted) DPRINTF(Mesh, "mispredicted %d\n", instInfo.mispredicted);
    PacketPtr new_pkt = createMeshPacket(forwardInst);
    getMeshMasterPorts()[dir].sendTimingReq(new_pkt);
    
  }

}

void
Vector::pullPipeInstruction(IODynInstPtr &instInfo) {
  // if master, pull from the local fetch
  auto msg = getFetchInput();
  instInfo = msg;
    // instInfo = MasterData(msg);
    
    // pop to free up the space
    consumeInst();
}

// if slave, pull from the mesh
// Recently changed so that there is a single buffer for all of the mesh ports
// there won't be a structural hazard because only one input will be active at once
void
Vector::pullMeshInstruction(IODynInstPtr &instInfo) {
  
  instInfo = _vecUops.dequeueInst();

  // Mesh_Dir recvDir;
  // if (MeshHelper::fetCsrToInSrc(_curCsrVal, recvDir)) {
  //   auto dataPtr = getMeshPortInst(recvDir);
  //   if (dataPtr->isInst) {
  //     instInfo = MasterData(dataPtr->inst);
  //     DPRINTF(Mesh, "get inst %s\n", instInfo.inst->toString(true));
  //   }
  //   else {
  //     instInfo = MasterData(dataPtr->pc);
  //     // also pass the inst information for extra metadata, that will need encode when go to RTL
  //     instInfo.inst = dataPtr->inst;
  //     DPRINTF(Mesh, "get PC %#x\n", instInfo.pc);
  //   }
  // }
}

IODynInstPtr
Vector::createInstruction(const IODynInstPtr &inst) {
  // make a dynamic instruction to pass to the decode stage
  // a slave core will track its seq num (incr on every instruction, i.e. nth dynamic inst), 
  
  // in trace mode don't know pc yet, so just give dummy value
  TheISA::PCState cur_pc = 0;
  
  // if the instruction is from a trace then we might want to convert to a nop
  // 1) vec loads
  // 2) scalar instructions
  StaticInstPtr static_inst;
  // bool force32bit;
  if (inst->static_inst_p->isSpadPrefetch()) {
    // TODO this makes pc + 2, when need pc + 4 (the prefetch is 32 bits)
    // compensate in iew logic
    static_inst = StaticInst::nopStaticInstPtr; 
    // force32bit = true;
  }
  // otherwise copy the sent instruction
  else {
    TheISA::MachInst machInst = (TheISA::MachInst)inst->static_inst_p->machInst;
    static_inst = extractInstruction(machInst, cur_pc);
    // force32bit = false;
  }
  // TheISA::MachInst machInst = (TheISA::MachInst)instInfo.inst->static_inst_p->machInst;
  // static_inst = extractInstruction(machInst, cur_pc);
  // force32bit = false;
  
  int tid = 0;
  IODynInstPtr copied_inst =
          std::make_shared<IODynInst>(static_inst, inst->pc,
                                      m_cpu_p->getAndIncrementInstSeq(),
                                      tid, m_cpu_p);
  DPRINTF(Mesh, "[tid:%d]: built inst %s from %s\n", tid, copied_inst->toString(true), inst->toString(true));  
  
  // // mark instruction as from a stream traced by master core
  // if (!static_inst->isVectorIssue()) {
  //   copied_inst->from_trace = true;
  //   copied_inst->replaced = force32bit;
    
  //   // iew will pass a mispredicted branch forward, we don't want to send 
  //   // this to slave core because it will be wasted work, however you still need
  //   // to check the branch here. if it fails with update then we know there is divergence
    
  //     copied_inst->master_targ  = inst->master_targ;
  //     copied_inst->master_taken = inst->master_taken;
    
  //   if (inst->isControl()) {
  //     DPRINTF(Mesh, "master targed %d %s. pred targ %d %s\n", inst->master_taken, inst->master_targ, inst->predicted_taken, inst->readPredTarg() );
  //   }
  // }  

  return copied_inst;
}

void
Vector::pushPipeInstToNextStage(const IODynInstPtr &inst) {
  sendInstToNextStage(inst);
    
  //DPRINTF(Mesh, "Push inst to decode %s\n", instInfo.inst->toString(true));
  if (m_stage_idx == LateVectorIdx) {
    _numInstructions++;
    // DPRINTF(Mesh, "[%s] num instructions seen %d\n", inst->toString(true), _numInstructions);
  }
}

void
Vector::pushMeshInstToNextStage(const IODynInstPtr &instInfo) {
  IODynInstPtr dynInst = createInstruction(instInfo);
  sendInstToNextStage(dynInst);
}



void
Vector::sendInstToNextStage(IODynInstPtr dynInst) {
  //DPRINTF(Mesh, "push instruction to decode %s\n", dynInst->toString(true));
  
  // TODO add epoch # here, but really shouldn't be because these things can be squashed
  //dynInst->epoch = getRevecEpoch();
  //DPRINTF(Mesh, "revec # %d\n", dynInst->epoch);
  
  Stage::sendInstToNextStage(dynInst);
  
  // if any one of the stages calls this, then the processor will tick
  // on the follwoing cycle
  signalActivity();
}

void
Vector::setupConfig(int csrId, RegVal csrVal) {
  // make sure this is the csr we are looking for
  if (csrId != RiscvISA::MISCREG_FETCH) return;
  
  // cache the csr val for easy lookup later
  _curCsrVal = csrVal;
  
  // clear all ports associated with this csr
  resetActive();
  
  DPRINTF(Mesh, "csrVal %d\n", csrVal);
  
  //int csrId = MeshHelper::stageToCsr(_stage);
  
  // get the internal src to be send of each of the output ports
  std::vector<Mesh_Dir> outDirs;
  MeshHelper::csrToOutDests(csrId, csrVal, outDirs);
    
  for (int j = 0; j < outDirs.size(); j++) {
    getMeshMasterPorts()[outDirs[j]].setActive(_stage);
    _numOutPortsActive++;
  }
  
  std::vector<Mesh_Dir> inDirs;
  MeshHelper::csrToInSrcs(csrId, csrVal, inDirs);
    
  for (int j = 0; j < inDirs.size(); j++) {
    getMeshSlavePorts()[inDirs[j]].setActive(_stage);
    _numInPortsActive++;
  }
  
  // give back stolen credits
  if (!getConfigured()) {
    restoreCredits();
  }
  
  _vecPassThrough = false;
  resetMeshRevec();
  resetPipeRevec();
  
}

StaticInstPtr
Vector::extractInstruction(const TheISA::MachInst inst, TheISA::PCState& cur_pc) {
  // turn instruction bits into higher-level instruction
  RiscvISA::Decoder decoder;
  
    if (decoder.compressed(inst)) {
        cur_pc.npc(cur_pc.instAddr() + sizeof(RiscvISA::MachInst) / 2);
    } else {
        cur_pc.npc(cur_pc.instAddr() + sizeof(RiscvISA::MachInst));
    }
  // need to figure out how long insturction is (emulates the following process that happens in fetch and rv decoder
  //decoder.moreBytes(cur_pc, 0x0, inst);
  //assert(decoder.instReady());
  //StaticInstPtr ret = decoder.decode(cur_pc);
  StaticInstPtr ret = decoder.decode(inst, 0x0);
  return ret;
}



PacketPtr
Vector::createMeshPacket(RegVal payload) {
  // create a packet to send
  // size is numbytes? (8 bytes -- 64 bits)
  int size = sizeof(payload);
  // need to break up payload into bytes
  // assume big endian?
  uint8_t *data = new uint8_t[size];
  for (int i = 0; i < size; i++) {
    // shift off byte at a time and truncate
    data[i] = (uint8_t)(payload >> (i * 8));
  }
  
  // create a packet to send
  Addr addr = 0;
  RequestPtr req = std::make_shared<Request>(addr, size, 0, 0);
  PacketPtr new_pkt = new Packet(req, MemCmd::WritebackDirty, size);
  new_pkt->dataDynamic(data);
  
  return new_pkt;
}

// cheat and let all information about master state be available
// when figure out what we actually need we can stop cheating
PacketPtr
Vector::createMeshPacket(const VecInstSel::MasterData& data) {
  auto copy = std::make_shared<VecInstSel::MasterData>(data);
  // create a packet to send
  Addr addr = 0;
  int size = 0;
  RequestPtr req = std::make_shared<Request>(addr, size, 0, 0);
  PacketPtr new_pkt = new Packet(req, MemCmd::WritebackDirty, size);
  new_pkt->pushSenderState(new Vector::SenderState(copy));
  
  return new_pkt;
}


bool
Vector::getOutRdy() {
  if (!canWriteMesh()) return true;
  
  bool allRdy = true;
  
  for (int i = 0; i < getMeshMasterPorts().size(); i++) {
    if (getMeshMasterPorts()[i].getActive() == _stage) {
      if (!getMeshMasterPorts()[i].getPairRdy()) allRdy = false;
    }
  }
  
  return allRdy;
}

// check if input packets are valid
// in RTL this wuold be a valid signal that is updated every cycle
// however in cycle level simulators, NULL exists so if there's
// a new packet then its valid otherwise its invalid
bool
Vector::getInVal() {
  if (!canReadMesh()) return true;
  
  // just check vecUop, only need a single buffer for all in port 
  // b/c only one will be active at once
  return _vecUops.getVal();

  // bool allVal = true;
  
  // for (int i = 0; i < getMeshSlavePorts().size(); i++) {
  //   if (getMeshSlavePorts()[i].getActive() == _stage) {
  //     if (!getMeshSlavePorts()[i].getPairVal()) allVal = false;
  //     //if (!_fromMeshPort[i].pktExists()) allVal = false;
  //   }
  // }
  
  // return allVal;
}

void
Vector::resetActive() {
  _numInPortsActive = 0;
  _numOutPortsActive = 0;
  
  // TODO need to prevent this reset if this is not the driver?
  // if (!getConfigured() || canWriteMesh()) {
    
    for (int i = 0; i < getMeshMasterPorts().size(); i++) {
      getMeshMasterPorts()[i].setActive(NONE);
    
      // set the stage driving the ports (effectively setting mux select)
      if (canWriteMesh()) {
        if (!getConfigured()) getMeshMasterPorts()[i].setDriver(nullptr);
        else getMeshMasterPorts()[i].setDriver(this);
      }
    }
  // }
  
  // if (!getConfigured() || canReadMesh()) {

    for (int i = 0; i < getMeshSlavePorts().size(); i++) {
      getMeshSlavePorts()[i].setActive(NONE);
     
      // set the stage driving the ports (effectively setting mux select)
      if (canReadMesh()) {
        if (!getConfigured()) getMeshSlavePorts()[i].setDriver(nullptr);
        else getMeshSlavePorts()[i].setDriver(this);
      }
    }
  // }
  
}

bool
Vector::isRootMaster() {
  return isMaster() && !isSlave();
}

bool
Vector::isMaster() {
  return MeshHelper::isVectorMaster(_curCsrVal);
}

bool
Vector::isSlave() {
  return MeshHelper::isVectorSlave(_curCsrVal);
}

bool
Vector::hasForwardingPath() {
  return MeshHelper::hasForwardingPath(_curCsrVal);
}

bool
Vector::canWriteMesh() {
  return ((_canRootSend && isRootMaster()) || (!_canRootSend && !isRootMaster()));
}

bool
Vector::canReadMesh() {
  return (_canRecv && isSlave());
}

bool
Vector::isOutPipeStalled() {
  return checkStall();
}

bool
Vector::isInPipeStalled() {
  return m_insts.empty();
}

bool
Vector::isOutMeshStalled() {
  return getConfigured() && !getOutRdy();
}

bool
Vector::isInMeshStalled() {
  return getConfigured() && (!getInVal() || getRevecStall());
}

bool
Vector::getRevecStall() {
  return (meshHasRevec() && !pipeHasRevec());
}

// inform that there has been an update
void
Vector::neighborUpdate() {
  /*if ((getNumPortsActive(EXECUTE) > 0) || (getNumPortsActive(FETCH) > 0)) {
  
  DPRINTF(Mesh, "to_mesh:\nactive   %d %d %d %d\nself val %d %d %d %d\npair rdy %d %d %d %d\n",
    toMeshPort[0].getActive(), toMeshPort[1].getActive(), toMeshPort[2].getActive(), toMeshPort[3].getActive(),
    toMeshPort[0].getVal(), toMeshPort[1].getVal(), toMeshPort[2].getVal(), toMeshPort[3].getVal(),
    toMeshPort[0].getPairRdy(), toMeshPort[1].getPairRdy(), toMeshPort[2].getPairRdy(), toMeshPort[3].getPairRdy());
    
  DPRINTF(Mesh, "from_mesh:\nactive   %d %d %d %d\nself rdy %d %d %d %d\npair val %d %d %d %d\n",
    fromMeshPort[0].getActive(), fromMeshPort[1].getActive(), fromMeshPort[2].getActive(), fromMeshPort[3].getActive(),
    fromMeshPort[0].getRdy(), fromMeshPort[1].getRdy(), fromMeshPort[2].getRdy(), fromMeshPort[3].getRdy(),
    fromMeshPort[0].getPairVal(), fromMeshPort[1].getPairVal(), fromMeshPort[2].getPairVal(), fromMeshPort[3].getPairVal());
  }*/
  
  // update the statemachines
  //_fsm->neighborEvent();
  
  
  signalActivity();
  
}


void
Vector::informNeighbors() {
  //DPRINTF(Mesh, "notify neighbors\n");
  // go through mesh ports to get tryUnblock function called in neighbor cores
  for (int i = 0; i < getMeshMasterPorts().size(); i++) {
    getMeshMasterPorts()[i].tryUnblockNeighbor();
  }
}

// std::shared_ptr<MasterData>
// Vector::getMeshPortInst(Mesh_Dir dir) {
//   PacketPtr pkt = getMeshPortPkt(dir);
//   Vector::SenderState* ss =
//               safe_cast<Vector::SenderState*>(pkt->popSenderState());
//   auto msg = ss->master_data;
//   delete ss;
//   return msg;
// }

uint64_t
Vector::getMeshPortData(Mesh_Dir dir) {
  PacketPtr pkt = getMeshPortPkt(dir);
  return FromMeshPort::getPacketData(pkt);
  //return fromMeshPort[dir].getPacketData();
}

PacketPtr
Vector::getMeshPortPkt(Mesh_Dir dir) {
  return getMeshSlavePorts()[dir].getPacket();
}

int
Vector::getNumPortsActive() {
  return _numInPortsActive + _numOutPortsActive;
}

IODynInstPtr
Vector::getFetchInput() {
  if (!m_insts.empty()) {
    return m_insts.front();
  }
  else {
    return nullptr;
  }
}

void
Vector::stealCredits() {
  if (m_is_sequential) {
    int remainingCred = m_input_queue_size - outputCredit();
    outputCredit() = -1 * remainingCred;
    _stolenCredits = m_input_queue_size;
  }
  else {
    // HACK to stall previous stage if it is in combinational pair with this one
    // in hardware would just have access to the credit buffer because in same
    // stage, but in this modular c++ model that's not trivial to do 
    //
    // if this is combinational then the previous sequential stages is looking
    // at the stage after us' out credit buffer, not this one, so we need to modify
    // that instead.
    // because this is tick forward we can gauge are assignment based on whats
    // currently there
    // TODO only works with one combinational stage right now
    _stolenCredits = m_max_num_credits;
    auto& pipeline = m_cpu_p->getPipeline();
    pipeline.setPrevStageUnemployed(m_stage_idx, true);
    //m_outgoing_credit_wire->to_prev_stage(m_next_stage_credit_idx) -= _stolenCredits;
    
    // we also need to not stall ourselves so give us a bump in credits
    // for future cycles
    //m_num_credits += _stolenCredits;
  }
  DPRINTF(Mesh, "steal credits %d\n", _stolenCredits); 
  
  
}

void
Vector::restoreCredits() {
  if (m_is_sequential) {
    outputCredit() += _stolenCredits;
  }
  else {
    if (_stolenCredits > 0) {
      // need to get on the same page as combinational pair
      m_outgoing_credit_wire->to_prev_stage(m_next_stage_credit_idx) += m_num_credits;
      m_num_credits = 0; // we are also reading this line so compensate
    
      auto& pipeline = m_cpu_p->getPipeline();
      pipeline.setPrevStageUnemployed(m_stage_idx, false);
    }
  }
    
  DPRINTF(Mesh, "restore credits %d\n", _stolenCredits);
  _stolenCredits = 0;
  
}

bool
Vector::getConfigured() {
  return !MeshHelper::isCSRDefault(_curCsrVal);
}

// TODO should revec be updated in this stage?? can potentially be squashed...
void
Vector::handleRevec(const IODynInstPtr pipeInst, const IODynInstPtr meshInst) {
  // if already in trace mode, or can't be trace mode, then just ignore
  if (!canReadMesh() || !_vecPassThrough || isRootMaster()) {
    return;
  }
  
  if (meshInst && meshInst->static_inst_p->isRevec()) {
    // set revec, TODO how to check value, or it even appropriate to check here?
    setMeshRevec(0);
    
     DPRINTF(Mesh, "[[INFO]] Recv mesh REVEC\n");
    
    // if there was a revec before from pipe, then unstall
    if (pipeHasRevec()) {
      restoreCredits();
    }
  }
  
  // if we recv a revec via pipeline, then store
  if (pipeInst && pipeInst->static_inst_p->isRevec()) {
    setPipeRevec(0);
  
    DPRINTF(Mesh, "[[INFO]] Recv pipe REVEC\n");
  
    // if theres no revec from mesh, then stall
    if (!meshHasRevec()) {
      DPRINTF(Mesh, "[[INFO]] Stall due to REVEC\n");
      stealCredits();
    }
  }
  
  // check if both present now and can continue vec (or not)
  if (pipeHasRevec() && meshHasRevec()) {
    if (getPipeRevec() == getMeshRevec()) {
      _vecPassThrough = false;
      resetPipeRevec();
      resetMeshRevec();
      stealCredits();
      
      DPRINTF(Mesh, "[[INFO]] REVEC\n");
      
    }
  }
  
}

Vector::InstSource
Vector::getOutMeshSource() {
  if (canWriteMesh()) {
    if (isRootMaster()) {
      return Pipeline;
    }
    else {
      return Mesh;
    }
  }
  else {
    return None;
  }
}

Vector::InstSource
Vector::getOutPipeSource() {
  if (canReadMesh() && !_vecPassThrough) {
    return Mesh;
  }
  else {
    return Pipeline;
  }
}

bool
Vector::pipeHasRevec() {
  return _pipeRevecId >= 0;
}

bool
Vector::meshHasRevec() {
  return _meshRevecId >= 0;
}

int
Vector::getPipeRevec() {
  return _pipeRevecId;
}

int
Vector::getMeshRevec() {
  return _meshRevecId;
}

void
Vector::setPipeRevec(int val) {
  _pipeRevecId = val;
}

void
Vector::setMeshRevec(int val) {
  _meshRevecId = val;
}

void
Vector::resetPipeRevec() {
  _pipeRevecId = -1;
}

void
Vector::resetMeshRevec() {
  _meshRevecId = -1;
}

bool
Vector::isCurDiverged() {
  return _vecPassThrough;
}

int
Vector::getXLen() {
  return MeshHelper::getXLen(RiscvISA::MISCREG_FETCH, _curCsrVal);
}

int
Vector::getYLen() {
  return MeshHelper::getYLen(RiscvISA::MISCREG_FETCH, _curCsrVal);
}

int
Vector::getXOrigin() {
  return MeshHelper::getXOrigin(_curCsrVal);
}

int
Vector::getYOrigin() {
  return MeshHelper::getYOrigin(_curCsrVal);
}

/*int
Vector::getPrefetchXLen() {
  if (isRootMaster())
    return getXLen();
  else
    return 1;
}

int
Vector::getPrefetchYLen() {
  if (isRootMaster())
    return getYLen();
  else
    return 1;
}*/

std::vector<ToMeshPort>&
Vector::getMeshMasterPorts() {
  return m_cpu_p->getMeshMasterPorts();
}

std::vector<FromMeshPort>&
Vector::getMeshSlavePorts() {
  return m_cpu_p->getMeshSlavePorts();
}

