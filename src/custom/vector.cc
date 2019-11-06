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
    _vecPassThrough(false)
{
}

void
Vector::tick() {

  Stage::tick();
  
  // TODO this awkwardly calls doSquash as well, would like to decouple
  bool squashed = checkSquash();
  if (squashed) return;

  // if not configured just pass the instruction through
  if (!getConfigured()) {
    passInstructions();
    return;
  }
  
  // if IOCPU implements activity monitor the need to have something like this
  bool update = true; //_fsm->tick();
  if (update) {
    // inform there is local activity
    signalActivity();
    // inform there might be neighbor activity
    informNeighbors();
  }
  
  
  // figure out the sources (parallel muxes) for the mesh net and pipeline respectively
  auto pipeSrc = getOutPipeSource();
  auto meshSrc = getOutMeshSource();
  
  // if outputs have different srcs then can decouple stalls
  bool coupledStalls = (pipeSrc == meshSrc);
  
  // check whether the mesh path should stall
  //bool meshStall = isMeshStalled() || (coupledStalls && isPipeStalled());
  // check whether the pipe path should stall
  //bool pipeStall = isPipeStalled() || (coupledStalls && isMeshStalled());
  
  // these determine whetehr we can push to the respective output buffer
  bool outMeshStall = (meshSrc != None) && 
    // stall if own output is stalled
    (isOutMeshStalled() ||
    // stall if any inputs stalled
    (meshSrc == Pipeline && isInPipeStalled()) ||
    (meshSrc == Mesh     && isInMeshStalled()) ||
    // stall if coupled output is stalled
    (coupledStalls       && isOutPipeStalled()));
  
  bool outPipeStall = (pipeSrc != None) &&
    // stall if own output is stalled
    (isOutPipeStalled() ||
    // stall if any inputs stalled
    (pipeSrc == Pipeline && isInPipeStalled()) ||
    (pipeSrc == Mesh     && isInMeshStalled()) ||
    // stall if coupled output is stalled
    (coupledStalls       && isOutMeshStalled()));
  
  
  // these determine whether we can pull from the respective input buffer
  bool inMeshStall = (meshSrc == Mesh && outMeshStall) || (pipeSrc == Mesh && outPipeStall) 
    // or not using at all
    || (meshSrc != Mesh && pipeSrc != Mesh);
  bool inPipeStall = (meshSrc == Pipeline && outMeshStall) || (pipeSrc == Pipeline && outPipeStall)
    // or not using at all
    || (meshSrc != Pipeline && pipeSrc != Pipeline);
  
  // pull instructions for both sources if no stalls
  MasterData meshInfo;
  if (!inMeshStall) {
    pullMeshInstruction(meshInfo);
  }
  
  MasterData pipeInfo;
  if (!inPipeStall) {
    pullPipeInstruction(pipeInfo);
  }
  
  if (!outMeshStall) {
     
    // forward instruction to other neighbors potentially
    if (meshSrc == Pipeline) {
      forwardInstruction(pipeInfo);
    }
    else if (meshSrc == Mesh) {
      forwardInstruction(meshInfo);
    }
  }
  
  // give instruction to the local decode stage if present
  if (!outPipeStall) {
    if (pipeSrc == Pipeline) {
      pushPipeInstToNextStage(pipeInfo);
    
      // only needs to be done here? b/c always happens in late vector
      // MUST BE THE LAST THING TO HAPPEN!
      if (!hasNextStage()) { // actually make sure its the last stage that does this (HACK)
        pipeInfo.inst->updateMiscRegs();
      }
    }
    else if (pipeSrc == Mesh) {
      pushMeshInstToNextStage(meshInfo);
    }
  }
  
  // old method
  /*
  // check if the current processor state allows us to go
  if (!shouldStall()) {
    //DPRINTF(Mesh, "vector unit going\n");
    // pull instruction from the mesh or from the local fetch stage
    MasterData instInfo;
    pullInstruction(instInfo);
  
    // give instruction to the local decode stage if present
    pushInstToNextStage(instInfo);
    
    // forward instruction to other neighbors potentially
    forwardInstruction(instInfo);
    
    // TODO maybe refactor out to parent
    // prefix()
    // callChildTick()
    // suffix()
    if (!hasNextStage()) { // actually make sure its the last stage that does this (HACK)
      instInfo.inst->updateMiscRegs();
    }
  }
  */
  
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
    DPRINTF(Mesh, "Squash from Commit: squash inst [tid:%d] [sn:%d]\n",
                    squash_inst->thread_id, squash_inst->seq_num);
  else if (initiator == StageIdx::IEWIdx) {
    if (m_stage_idx == LateVectorIdx)
      DPRINTF(Mesh, "[[WARNING]] Squash from IEW: squash inst [tid:%d] [sn:%d]\n",
                    squash_inst->thread_id, squash_inst->seq_num);
    else
      DPRINTF(Mesh, "Squash from IEW: squash inst [tid:%d] [sn:%d]\n",
                    squash_inst->thread_id, squash_inst->seq_num);            
  }
  else if (initiator == StageIdx::DecodeIdx)
    DPRINTF(Mesh, "Squash from Decode: squash inst [tid:%d] [sn:%d]\n",
                    squash_inst->thread_id, squash_inst->seq_num);
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
      ((m_stage_idx == LateVectorIdx) && inst->seq_num <= squash_inst->seq_num) ||
      !inst->decAndCheckSquash()) {
      m_insts.push(inst);
    } else {
      if (getConfigured()) DPRINTF(Mesh, "Squashing %s\n", inst->toString());
      assert(inst->seq_num > squash_inst->seq_num);
      // increment the number of credits to the previous stage
      outputCredit()++;
    }
    count++;
  }
  
  // if squash due to a traced instruction, then we need to exit trace mode
  // TODO probably want to go to 'transparent' where recv and send as before
  // but still doing own seperate fetch and instruction stream.
  // + Don't run into issue when one core diverges and causes rest to diverge b/c dominates them
  // - Potentially wasted energy on instruction pass throughs (especially if low usage)
  // - This core can potentially stall b/c target is stalled, but that's awkward b/c this is working on diff stream
  if (initiator == StageIdx::IEWIdx && squash_inst->from_trace) {
    //m_cpu_p->setMiscReg(RiscvISA::MISCREG_FETCH, 0, tid);
    _vecPassThrough = true;
    DPRINTF(Mesh, "[[WARNING]] trace divergence [%s]\n", squash_inst->toString(true));
  }
  
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
Vector::forwardInstruction(const MasterData& instInfo) {
  // check whether this stage is allowed to forward to the mesh net
  if (!canWriteMesh()) return;
  
  // find each direction to send a packet to
  std::vector<Mesh_DS_t> out;
  MeshHelper::csrToOutSrcs(RiscvISA::MISCREG_FETCH, _curCsrVal, out);
  
  //if (out.size() > 0)
  //  DPRINTF(Mesh, "Forward to mesh net %s %d\n", instInfo.inst->toString(true), instInfo.new_squashes);
  
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
    PacketPtr new_pkt = createMeshPacket(instInfo);
    getMeshMasterPorts()[dir].sendTimingReq(new_pkt);
    
  }

}

void
Vector::pullPipeInstruction(MasterData &instInfo) {
  // if master, pull from the local fetch
  auto msg = getFetchInput();
    
    instInfo.inst = msg;
    instInfo.new_squashes = 0;
    
    // pop to free up the space
    consumeInst();
}

// if slave, pull from the mesh
void
Vector::pullMeshInstruction(MasterData &instInfo) {
  
  Mesh_Dir recvDir;
  if (MeshHelper::fetCsrToInSrc(_curCsrVal, recvDir)) {
    auto dataPtr = getMeshPortInst(recvDir);
    instInfo.inst = dataPtr->inst;
    instInfo.new_squashes = dataPtr->new_squashes;
  }
}

/*
void
Vector::pullInstruction(MasterData &instInfo) {
  
  // if slave, pull from the mesh
  Mesh_Dir recvDir;
  if (MeshHelper::fetCsrToInSrc(_curCsrVal, recvDir) && canReadMesh()) {
    //uint64_t meshData = getMeshPortData(recvDir);
    
    //Mesh_Dir recvDir;
    //MeshHelper::fetCsrToInSrc(_curCsrVal, recvDir);
    
    // decode the msg here
    //instInfo = decodeMeshData(meshData);
    //DPRINTF(Mesh, "decoded %#x -> %#x %d %d\n", meshData, instInfo.machInst, instInfo.predictTaken, instInfo.mispredicted);
    auto dataPtr = getMeshPortInst(recvDir);
    instInfo.inst = dataPtr->inst;
    instInfo.new_squashes = dataPtr->new_squashes;
    
    //DPRINTF(Mesh, "Pull from mesh net %s\n", instInfo.inst->toString(true));
  }
  // if master, pull from the local fetch
  else {
    auto msg = getFetchInput();
    
    instInfo.inst = msg;
    instInfo.new_squashes = 0;
    
    // pop to free up the space
    consumeInst();
    
    //DPRINTF(Mesh, "Pull from fetch %s\n", instInfo.inst->toString(true));
  }
}
*/

IODynInstPtr
Vector::createInstruction(const MasterData &instInfo) {
  // make a dynamic instruction to pass to the decode stage
  // a slave core will track its seq num (incr on every instruction, i.e. nth dynamic inst), 
  // own pc(?), and maybe branch predictor (in fetch)
  int tid = 0;
  TheISA::MachInst machInst = (TheISA::MachInst)instInfo.inst->static_inst_p->machInst;
  TheISA::PCState cur_pc = 0;
  auto static_inst = extractInstruction(machInst, cur_pc);
  IODynInstPtr inst =
          std::make_shared<IODynInst>(static_inst, cur_pc,
                                      m_cpu_p->getAndIncrementInstSeq(),
                                      tid, m_cpu_p);
  //DPRINTF(Mesh, "[tid:%d]: built inst %s\n", tid, dyn_inst->toString(true));  
  
  // mark instruction as from a stream traced by master core
  inst->from_trace = true;
  
  // iew will pass a mispredicted branch forward, we don't want to send 
  // this to slave core because it will be wasted work, however you still need
  // to check the branch here. if it fails with update then we know there is divergence
  
    inst->master_targ  = instInfo.inst->master_targ;
    inst->master_taken = instInfo.inst->master_taken;
  
  if (instInfo.inst->isControl()) {
    DPRINTF(Mesh, "master targed %d %s. pred targ %d %s\n", inst->master_taken, inst->master_targ, inst->predicted_taken, inst->readPredTarg() );
  }

  return inst;
}

void
Vector::pushPipeInstToNextStage(const MasterData &instInfo) {
  sendInstToNextStage(instInfo.inst);
    
  //if (m_stage_idx == LateVectorIdx)
  //DPRINTF(Mesh, "Push inst to decode %s\n", instInfo.inst->toString(true));
  if (m_stage_idx == LateVectorIdx) {
    _numInstructions++;
    DPRINTF(Mesh, "[%s] num instructions seen %d\n", instInfo.inst->toString(true), _numInstructions);
    if (instInfo.inst->m_inst_str == "jal ra, 1530") {
            RegVal ra = m_cpu_p->readArchIntReg(1, 0);
            DPRINTF(Mesh, "[%s] %#x PC=>NPC %s\n", instInfo.inst->toString(true), ra, instInfo.inst->pc);
          }
  }
}

void
Vector::pushMeshInstToNextStage(const MasterData &instInfo) {
  IODynInstPtr dynInst = createInstruction(instInfo);
  sendInstToNextStage(dynInst);
}

/*
void
Vector::pushInstToNextStage(const MasterData &instInfo) {
  
  // create new instruction only if slave
  if (canReadMesh()) {
    IODynInstPtr dynInst = createInstruction(instInfo);
    sendInstToNextStage(dynInst);
    
    //if (m_stage_idx == LateVectorIdx) {
     // DPRINTF(Mesh, "Push inst to decode %s->%s\n", instInfo.inst->toString(true), dynInst->toString(true));
      //_numInstructions++;
      //DPRINTF(Mesh, "num instructions seen %d\n", _numInstructions);
    //}
  }
  // otherwise just pass the given instruction ptr
  else {
    sendInstToNextStage(instInfo.inst);
    
    //if (m_stage_idx == LateVectorIdx)
    //DPRINTF(Mesh, "Push inst to decode %s\n", instInfo.inst->toString(true));
    if (m_stage_idx == LateVectorIdx) {
    _numInstructions++;
    DPRINTF(Mesh, "[%s] num instructions seen %d\n", instInfo.inst->toString(true), _numInstructions);
    if (instInfo.inst->m_inst_str == "jal ra, 1530") {
            RegVal ra = m_cpu_p->readArchIntReg(1, 0);
            DPRINTF(Mesh, "[%s] %#x PC=>NPC %s\n", instInfo.inst->toString(true), ra, instInfo.inst->pc);
          }
  }
  }
  
}
*/ 

void
Vector::sendInstToNextStage(IODynInstPtr dynInst) {
  //DPRINTF(Mesh, "push instruction to decode %s\n", dynInst->toString(true));
  
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
Vector::createMeshPacket(const MasterData& data) {
  auto copy = std::make_shared<MasterData>();
  copy->inst = data.inst;
  copy->new_squashes = data.new_squashes;
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
  
  bool allVal = true;
  
  for (int i = 0; i < getMeshSlavePorts().size(); i++) {
    if (getMeshSlavePorts()[i].getActive() == _stage) {
      if (!getMeshSlavePorts()[i].getPairVal()) allVal = false;
      //if (!_fromMeshPort[i].pktExists()) allVal = false;
    }
  }
  
  return allVal;
}

/*void
Vector::setRdy(bool rdy) {
  for (int i = 0; i < _fromMeshPort.size(); i++) {
    _fromMeshPort[i].setRdyIfActive(rdy, _stage);
  }
}

void
Vector::setVal(bool val) {
  for (int i = 0; i < _toMeshPort.size(); i++) {
    _toMeshPort[i].setValIfActive(val, _stage);
  }
}*/

void
Vector::resetActive() {
  _numInPortsActive = 0;
  _numOutPortsActive = 0;
  
  // TODO need to prevent this reset if this is not the driver?
  if (!getConfigured() || canWriteMesh()) {
    
    for (int i = 0; i < getMeshMasterPorts().size(); i++) {
      getMeshMasterPorts()[i].setActive(NONE);
    
      // set the stage driving the ports (effectively setting mux select)
      if (!getConfigured()) getMeshMasterPorts()[i].setDriver(nullptr);
      else getMeshMasterPorts()[i].setDriver(this);
    }
  }
  
  if (!getConfigured() || canReadMesh()) {

    for (int i = 0; i < getMeshSlavePorts().size(); i++) {
      getMeshSlavePorts()[i].setActive(NONE);
     
      // set the stage driving the ports (effectively setting mux select)
      if (!getConfigured()) getMeshSlavePorts()[i].setDriver(nullptr);
      else getMeshSlavePorts()[i].setDriver(this);
    }
  }
  
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
  return getConfigured() && !getInVal();
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

std::shared_ptr<Vector::MasterData>
Vector::getMeshPortInst(Mesh_Dir dir) {
  PacketPtr pkt = getMeshPortPkt(dir);
  Vector::SenderState* ss =
              safe_cast<Vector::SenderState*>(pkt->popSenderState());
  auto msg = ss->master_data;
  delete ss;
  return msg;
}

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

/*Port&
Vector::getMeshPort(int idx, bool isOut) {
 if (isOut) {
   return _toMeshPort[idx];
 } 
 else {
   return _fromMeshPort[idx];
 }
}

int
Vector::getNumMeshPorts() {
  return _toMeshPort.size();
}*/

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

/*void
Vector::popFetchInput(ThreadID tid) {
  if (!_inputBuffer[tid].empty()) {
    //_inputBuffer[tid].front().freeLine();
    _inputBuffer[tid].pop();
  }
}*/

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

/*
bool
Vector::isPipeStalled() {
  bool nextStageStall = checkStall();
  
  // there was no input from fetch
  bool senderStall = !canReadMesh() && m_insts.empty();
  bool stall = senderStall || nextStageStall;
  //if (stall) {
  //  if (recver) DPRINTF(Mesh, "slave stall\n");
  //  else if (sender) DPRINTF(Mesh, "master stall\n");
  //}
  
  return stall;
}

bool
Vector::isMeshStalled() {
  return !(getConfigured() && getInVal() && getOutRdy());
}*/

/*
bool
Vector::shouldStall() {
  // check for instruction from mesh
  bool meshStall = meshStalled();
  //DPRINTF(Mesh, "config %d inval %d outrdy %d\n", getConfigured(), getInVal(), getOutRdy());
  // check if local decode is open to get new input or is stalled
  bool inStall = isInternallyStalled();
  bool stalled = meshStall || inStall;
  return stalled;
}
*/

std::vector<ToMeshPort>&
Vector::getMeshMasterPorts() {
  return m_cpu_p->getMeshMasterPorts();
}

std::vector<FromMeshPort>&
Vector::getMeshSlavePorts() {
  return m_cpu_p->getMeshSlavePorts();
}

