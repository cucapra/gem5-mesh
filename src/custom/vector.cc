#include "arch/registers.hh"
#include "base/bitfield.hh"
#include "custom/vector.hh"
#include "cpu/io/cpu.hh"

#include "debug/Mesh.hh"

Vector::Vector(IOCPU *_cpu_p, IOCPUParams *p) : 
    Stage(_cpu_p, p->vectorBufferSize, p->decodeBufferSize, StageIdx::VectorIdx, false),
    _numInPortsActive(0),
    _numOutPortsActive(0),
    _stage(FETCH),
    _curCsrVal(0),
    _stolenCredits(0)
{
  // 
  // declare vector ports
  for (int i = 0; i < p->port_to_mesh_port_connection_count; ++i) {
      _toMeshPort.emplace_back(this, m_cpu_p, i);
  }
   
  for (int i = 0; i < p->port_from_mesh_port_connection_count; ++i) {
      _fromMeshPort.emplace_back(this, m_cpu_p, i);
  }
    
  for (int i = 0; i < p->port_from_mesh_port_connection_count; i++) {
    // need to setup anything involving the 'this' pointer in the port
    // class after have moved into vector memory
    
    // alternatively could declare ports as pointers
    _fromMeshPort[i].setupEvents();
  }
  
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
    
  }
  
}

std::string
Vector::name() const {
  return m_cpu_p->name() + ".vector";
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
  else if (initiator == StageIdx::IEWIdx)
    DPRINTF(Mesh, "Squash from IEW: squash inst [tid:%d] [sn:%d]\n",
                    squash_inst->thread_id, squash_inst->seq_num);
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
    if (inst->thread_id != tid) {
      m_insts.push(inst);
    } else {
      if (getConfigured()) DPRINTF(Mesh, "Squashing %s\n", inst->toString());
      assert(inst->seq_num > squash_inst->seq_num);
      // increment the number of credits to the previous stage
      outputCredit()++;
    }
    count++;
  }
  
  // if this was a config squash and if we are a slave, then takeaway all credits?
  // but not sure we can make this check based on when the csr file is written and credits
  //
  // could have small wire carrying slave bit from commit stage -- ok
  //    a gem5 specific implementation detail of this is can't read the csr until executes (this ends up firing cycle after)
  // How does -ve credit work? -> use twos complement to send back (currently using some small unsigned int) -- ok
  if (initiator == StageIdx::CommitIdx && !((SquashComm::CommitSquash*)&squashInfo)->is_trap_pending) {
    if (MeshHelper::isVectorSlave(_curCsrVal)) {
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
  }
}

void
Vector::forwardInstruction(const MasterData& instInfo) {
  
  // find each direction to send a packet to
  std::vector<Mesh_DS_t> out;
  MeshHelper::csrToOutSrcs(RiscvISA::MISCREG_FETCH, _curCsrVal, out);
  
  DPRINTF(Mesh, "Forward to mesh net %s\n", instInfo.inst->toString(true));
  if (instInfo.inst->isCondCtrl()) {
    DPRINTF(Mesh, "bne sent pred taken %d target %#x\n", instInfo.inst->predicted_taken, instInfo.inst->readPredTarg());
  }
  
  // send a packet in each direction
  for (int i = 0; i < out.size(); i++) {
    Mesh_Dir dir = out[i].outDir;
    //Mesh_Out_Src src = out[i].src;
    
    
    //uint64_t meshData = encodeMeshData(instInfo);
    //DPRINTF(Mesh, "Sending mesh request %d from %d with val %#x %d %d = %#x\n", 
    //    dir, src, instInfo.machInst, instInfo.predictTaken, instInfo.mispredicted, meshData);
    //if (instInfo.predictTaken) DPRINTF(Mesh, "predict taken %d\n", instInfo.predictTaken);
    //if (instInfo.mispredicted) DPRINTF(Mesh, "mispredicted %d\n", instInfo.mispredicted);
    PacketPtr new_pkt = createMeshPacket(instInfo.inst);
    _toMeshPort[dir].sendTimingReq(new_pkt);
    
  }
}

void
Vector::pullInstruction(MasterData &instInfo) {
  
  // if slave, pull from the mesh
  Mesh_Dir recvDir;
  if (MeshHelper::fetCsrToInSrc(_curCsrVal, recvDir)) {
    //uint64_t meshData = getMeshPortData(recvDir);
    
    // decode the msg here
    //instInfo = decodeMeshData(meshData);
    //DPRINTF(Mesh, "decoded %#x -> %#x %d %d\n", meshData, instInfo.machInst, instInfo.predictTaken, instInfo.mispredicted);
    instInfo.inst = getMeshPortInst(recvDir);
    
    //DPRINTF(Mesh, "Pull from mesh net %s\n", instInfo.inst->toString(true));
  }
  // if master, pull from the local fetch
  else {
    auto msg = getFetchInput();
    
    instInfo.inst = msg;
    
    // pop to free up the space
    consumeInst();
    
    //DPRINTF(Mesh, "Pull from fetch %s\n", instInfo.inst->toString(true));
  }
}

/*ForwardVectorData
Vector::decodeMeshData(uint64_t data) {
  TheISA::MachInst instWord = (TheISA::MachInst)data;
  // note bits() args are MSB->LSB all inclusive
  bool predictTaken = (bool)bits(data, 32, 32);
  bool mispredicted = (bool)bits(data, 33, 33);
  ForwardVectorData instInfo;
  instInfo.machInst = instWord;
  instInfo.predictTaken = predictTaken;
  instInfo.mispredicted = mispredicted;
  return instInfo;
}

uint64_t
Vector::encodeMeshData(const ForwardVectorData &instInfo) {
  return (((uint64_t)instInfo.mispredicted) << 33) | (((uint64_t)instInfo.predictTaken) << 32) | (uint64_t)instInfo.machInst;
}*/

IODynInstPtr
Vector::createInstruction(const MasterData &instInfo) {
  // make a dynamic instruction to pass to the decode stage
  // a slave core will track its seq num (incr on every instruction, i.e. nth dynamic inst), 
  // own pc(?), and maybe branch predictor (in fetch)
  int tid = 0;
  TheISA::MachInst machInst = (TheISA::MachInst)instInfo.inst->static_inst_p->machInst;
  auto cur_pc = instInfo.inst->pc; // temp?
  auto static_inst = extractInstruction(machInst);
  IODynInstPtr inst =
          std::make_shared<IODynInst>(static_inst, cur_pc,
                                      m_cpu_p->getAndIncrementInstSeq(),
                                      tid, m_cpu_p);
  //DPRINTF(Mesh, "[tid:%d]: built inst %s\n", tid, dyn_inst->toString(true));
  
  // just use branch prediction from the master instruction
  // need to justify part of this in how it would actually work in hardware
  // in the future will prob just determine this locally
  inst->setPredTarg(instInfo.inst->readPredTarg());
  inst->predicted_taken = instInfo.inst->predicted_taken;
  
  // you need to update the branch predictor with the predictions before 
  // a squash can happen, otherwise the predictor will get confused and assert fail
  // we can access this structure here without a structural hazard because 
  // the fetch stage should be inactive if we are here
  TheISA::PCState next_pc = cur_pc; // passed by ref and expected to change
  /*bool pred_taken =*/ m_cpu_p->getBranchPredPtr()->predict(inst->static_inst_p,
                                                inst->seq_num, next_pc, tid);
                                                
  // if there's no divergence expect these to be similar
  // but don't base divergence off prediction, but rather outcome of branch
  //assert(pred_taken == inst->predicted_taken);

  return inst;
}

void
Vector::pushInstToNextStage(const MasterData &instInfo) {
  
  IODynInstPtr dynInst = createInstruction(instInfo);
  sendInstToNextStage(dynInst);
    
  DPRINTF(Mesh, "Push inst to decode %s->%s\n", instInfo.inst->toString(true), dynInst->toString(true));
  
}

void
Vector::sendInstToNextStage(IODynInstPtr dynInst) {
  //DPRINTF(Mesh, "push instruction to decode %s\n", *dynInst);
  
  Stage::sendInstToNextStage(dynInst);
  
  // if any one of the stages calls this, then the processor will tick
  // on the follwoing cycle
  signalActivity();
}

void
Vector::setupConfig(int csrId, RegVal csrVal) {
  // make sure this is the csr we are looking for
  if (csrId != RiscvISA::MISCREG_FETCH) return;
  
  // clear all ports associated with this csr
  
  resetActive();
  
  DPRINTF(Mesh, "csrVal %d\n", csrVal);
  
  //int csrId = MeshHelper::stageToCsr(_stage);
  
  // get the internal src to be send of each of the output ports
  std::vector<Mesh_Dir> outDirs;
  MeshHelper::csrToOutDests(csrId, csrVal, outDirs);
    
  for (int j = 0; j < outDirs.size(); j++) {
    _toMeshPort[outDirs[j]].setActive(_stage);
    _numOutPortsActive++;
  }
  
  std::vector<Mesh_Dir> inDirs;
  MeshHelper::csrToInSrcs(csrId, csrVal, inDirs);
    
  for (int j = 0; j < inDirs.size(); j++) {
    _fromMeshPort[inDirs[j]].setActive(_stage);
    _numInPortsActive++;
  }
  
  // cache the csr val for easy lookup later
  _curCsrVal = csrVal;
  
  // give back stolen credits
  if (!getConfigured()) {
    restoreCredits();
  }
  
}

StaticInstPtr
Vector::extractInstruction(const TheISA::MachInst inst) {
  // turn instruction bits into higher-level instruction
  RiscvISA::Decoder decoder;
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
Vector::createMeshPacket(IODynInstPtr inst) {
  // create a packet to send
  Addr addr = 0;
  int size = 0;
  RequestPtr req = std::make_shared<Request>(addr, size, 0, 0);
  PacketPtr new_pkt = new Packet(req, MemCmd::WritebackDirty, size);
  new_pkt->pushSenderState(new Vector::SenderState(inst));
  
  return new_pkt;
}


bool
Vector::getOutRdy() {
  bool allRdy = true;
  
  for (int i = 0; i < _toMeshPort.size(); i++) {
    if (_toMeshPort[i].getActive() == _stage) {
      if (!_toMeshPort[i].getPairRdy()) allRdy = false;
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
  bool allVal = true;
  
  for (int i = 0; i < _fromMeshPort.size(); i++) {
    if (_fromMeshPort[i].getActive() == _stage) {
      if (!_fromMeshPort[i].getPairVal()) allVal = false;
      //if (!_fromMeshPort[i].pktExists()) allVal = false;
    }
  }
  
  return allVal;
}

void
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
}

void
Vector::resetActive() {
  _numInPortsActive = 0;
  _numOutPortsActive = 0;
  
  for (int i = 0; i < _fromMeshPort.size(); i++) {
    _fromMeshPort[i].setActive(NONE);
  }
  
  for (int i = 0; i < _toMeshPort.size(); i++) {
    _toMeshPort[i].setActive(NONE);
  }
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
  for (int i = 0; i < _toMeshPort.size(); i++) {
    _toMeshPort[i].tryUnblockNeighbor();
  }
}

IODynInstPtr
Vector::getMeshPortInst(Mesh_Dir dir) {
  PacketPtr pkt = getMeshPortPkt(dir);
  Vector::SenderState* ss =
              safe_cast<Vector::SenderState*>(pkt->popSenderState());
  auto msg = ss->master_inst;
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
  return _fromMeshPort[dir].getPacket();
}

Port&
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

/*void
Vector::popFetchInput(ThreadID tid) {
  if (!_inputBuffer[tid].empty()) {
    //_inputBuffer[tid].front().freeLine();
    _inputBuffer[tid].pop();
  }
}*/

void
Vector::stealCredits() {
  int remainingCred = m_input_queue_size - outputCredit();
  outputCredit() = -1 * remainingCred;
  _stolenCredits = m_input_queue_size;
  DPRINTF(Mesh, "steal credits %d\n", _stolenCredits);
}

void
Vector::restoreCredits() {
  outputCredit() = _stolenCredits;
  DPRINTF(Mesh, "restore credits %d\n", _stolenCredits);
  _stolenCredits = 0;
}

bool
Vector::getConfigured() {
  return !MeshHelper::isCSRDefault(_curCsrVal);
}

bool
Vector::isInternallyStalled() {
  bool decodeStall = checkStall();
  
  // the stall depends on whether this is a slave or not (has an indest)
  bool recver = MeshHelper::isVectorSlave(_curCsrVal);
  bool sender = MeshHelper::isVectorMaster(_curCsrVal);
  
  // we don't have the credits to push into the next stage
  bool recverStall = recver && decodeStall;
  
  // there was no input from fetch
  bool senderStall = sender && !recver && m_insts.empty();
  
  // we also can stall when we are not configured if next stage is stalled
  // TODO not sure why this is needed, should not be in this case
  bool normalStall = decodeStall;
  //if (normalStall && !recverStall) DPRINTF(Mesh, "normal stall\n");
  
  // also check squash here?
  bool stall = recverStall || senderStall || normalStall;
  /*if (stall) {
    if (recver) DPRINTF(Mesh, "slave stall\n");
    else if (sender) DPRINTF(Mesh, "master stall\n");
  }*/
  
  return stall;
}

bool
Vector::shouldStall() {
  // check for instruction from mesh
  bool meshOk = getConfigured() && getInVal() && getOutRdy(); //_fsm->isMeshActive();
  
  // check if local decode is open to get new input or is stalled
  bool nextStageOk = !isInternallyStalled();
  bool canGo = meshOk && nextStageOk;
  return !canGo;
}
