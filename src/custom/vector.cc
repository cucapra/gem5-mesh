#include "arch/registers.hh"
#include "base/bitfield.hh"
#include "custom/vector.hh"

#include "debug/Mesh.hh"

Vector::Vector(IOCPU *_cpu_p, IOCPUParams *p) : 

    Stage(_cpu_p, 2, 2, /*StageIdx::VectorIdx*/ StageIdx::FetchIdx, true),
    _numInPortsActive(0),
    _numOutPortsActive(0),
    _stage(FETCH),
    _fsm(std::make_shared<EventDrivenFSM>(this, m_cpu_p, _stage)),
    _curCsrVal(0),
    _wasStalled(false)
    //_internalInputThisCycle(false),
    //_mispredictUpdate([this] { handleMispredict(); }, name)
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
  
  // hack so when we call this, we still have this information on state transition
  //_internalInputThisCycle = !_inputBuffer[0].empty();
  
  // check the state of the fsm (dont need async updates like before)
  // because minor has cycle by cycle ticks unlike TimingSimpleCPU
  bool update = _fsm->tick();
  if (update) {
    // inform there is local activity
    signalActivity();
    // inform there might be neighbor activity
    informNeighbors();
  }
  
  // if there was an internal stall either due to last stage or next stage
  // we need to make sure to schedule state machine update for the next cycle
  processInternalStalls();
  
  // check if the current processor state allows us to go
  bool canGo = !shouldStall();
  
  if (canGo) {
    //DPRINTF(Mesh, "vector unit going\n");
    // pull instruction from the mesh or from the local fetch stage
    MasterData instInfo;
    pullInstruction(instInfo);
  
    // give instruction to the local decode stage if present
    pushInstToNextStage(instInfo);
    
    // forward instruction to other neighbors potentially
    forwardInstruction(instInfo);
    
  } 
  /*else {
    if (getConfigured())
      DPRINTF(Mesh, "vec can't do anything\n");
  }*/
  
  // if not configured just pass the instruction through
  if (!getConfigured()) {
    passInstructions();
    //popFetchInput(0);
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
  
}

void
Vector::passInstructions() {
  while (!m_insts.empty() && nextStageRdy()) {
    IODynInstPtr inst = m_insts.front();
    //ThreadID tid = inst->thread_id;
    //DPRINTF(Mesh, "[tid:%d] Decoding inst [sn:%lli] with PC %s\n",
    //                tid, inst->seq_num, inst->pc);

    // send out this inst
    sendInstToNextStage(inst);

    // Remove the inst from the queue and increment the credit to the previous
    // stage.
    consumeInst(); // TODO credits need to reflect next queue, not this queue
  }
}

void
Vector::forwardInstruction(const MasterData& instInfo) {
  
  // find each direction to send a packet to
  std::vector<Mesh_DS_t> out;
  MeshHelper::csrToOutSrcs(RiscvISA::MISCREG_FETCH, _curCsrVal, out);
  
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
  }
  // if master, pull from the local fetch
  else {
    auto msg = getFetchInput();
    
    //instInfo.machInst = msg->machInst;
    //instInfo.predictTaken = msg->predictTaken;
    //instInfo.mispredicted = msg->mispredicted;
    instInfo.inst = msg;
    
    // pop to free up the space
    consumeInst();
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
  // make a minor dynamic instruction to pass to the decode stage
  // bs most of the fields because the slave wouldn't be keeping track
  // of this
  int tid = 0;
  TheISA::MachInst machInst = (TheISA::MachInst)instInfo.inst->static_inst_p->machInst;
  auto cur_pc = instInfo.inst->pc; // temp?
  auto static_inst = extractInstruction(machInst);
  IODynInstPtr dyn_inst =
          std::make_shared<IODynInst>(static_inst, cur_pc,
                                      m_cpu_p->getAndIncrementInstSeq(),
                                      tid, m_cpu_p);
  DPRINTF(Mesh, "[tid:%d]: built inst %s\n", tid, dyn_inst->toString(true));
  
  
  /* // Look up the next pc
  TheISA::PCState next_pc = cur_pc;
  bool predict_taken = lookupAndUpdateNextPC(dyn_inst_p, next_pc);
  DPRINTF(Fetch, "[tid:%d]: cur_pc %s -> next_pc %s (predict_taken = %d)\n",
                tid, cur_pc, next_pc, predict_taken);

  // Update dyn_inst
  dyn_inst_p->setPredTarg(next_pc);
  dyn_inst_p->predicted_taken = predict_taken;*/
  
  // mark this instruction as being generated by the vector stage
  // this flag will be used by later stages to do unique behavior like
  // turn off the BTB check in execute
  //dyn_inst->fromVector = true;

  return dyn_inst;
}

void
Vector::pushInstToNextStage(const MasterData &instInfo) {
  // only push into decode if we are a slave
  if (MeshHelper::isVectorSlave(_curCsrVal)) {
    // update the stream seq number based on branch hint from master
    //if (instInfo.branchTaken) _lastStreamSeqNum++;
    
    IODynInstPtr dynInst = createInstruction(instInfo);
    sendInstToNextStage(dynInst);
  }
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
  
  // the state machine is sensitive to configure events, so let it know here
  _fsm->configEvent();
  
  // no pending mispredicts to start off with
  //_pendingMispredict = false;
  
  // if the new configuration is slave, we need to push a bs instruction
  // into the pipeline buffer, or when reset need to take instruction out of pipeline buffer
  if (MeshHelper::isVectorSlave(_curCsrVal)) {
    stallFetchInput(0);
  }
  if (!getConfigured()) {
    unstallFetchInput(0);
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
  _fsm->neighborEvent();
  
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

/*std::vector<Minor::InputBuffer<Minor::ForwardVectorData>>&
Vector::getInputBuf() {
  return _inputBuffer;
}*/

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
Vector::stallFetchInput(ThreadID tid) {
  /*if (_inputBuffer[tid].empty()) {
    ForwardVectorData vecData(0);
    vecData.machInst = 0x511; // c_addi (nop)
    _inputBuffer[tid].setTail(vecData);
    _inputBuffer[tid].pushTail();
    DPRINTF(Mesh, "try stall frontend %d?=0\n", _inputBuffer[tid].canReserve());
  }*/
  
  assert(0);
}

void
Vector::unstallFetchInput(ThreadID tid) {
  //popFetchInput(tid);
  assert(0);
}

bool
Vector::getConfigured() {
  return !MeshHelper::isCSRDefault(_curCsrVal);
}

bool
Vector::isInternallyStalled() {
  //int tid = 0;
  bool decodeStall = !nextStageRdy();
  
  // the stall depends on whether this is a slave or not (has an indest)
  // TODO can vector just do all of the forwarding into fetch2?
  // then don't need to distinguish the stalls here
  // note you can be both a master and a slave under defs that master
  // sends at least, once and slave sends at least once
  bool recver = MeshHelper::isVectorSlave(_curCsrVal);
  bool sender = MeshHelper::isVectorMaster(_curCsrVal);
  
  // we care about this if we are going to push into that stage (vec slave)
  bool recverStall = recver && decodeStall;
  
  // there was no input from fetch2
  // employ hack were use the input buffer at the beginning of the eval
  // to be the one used for check
  bool senderStall = sender && !recver; // && !_internalInputThisCycle;
  
  bool stall = recverStall || senderStall;
  /*if (stall) {
    DPRINTF(Mesh, "internal stall: rs %d ss %d r %d s %d dec %d in %d\n", 
      recverStall, senderStall, recver, sender, decodeStall, _internalInputThisCycle);
  }*/
  
  return stall;
}


void
Vector::processInternalStalls() {
  bool justStalled = isInternallyStalled();
  
  if (_wasStalled ^ justStalled) {
    _fsm->stallEvent();
  }
  
  _wasStalled = justStalled;
}

bool
Vector::shouldStall() {
  // check for instruction from mesh
  bool meshOk = _fsm->isMeshActive();
  
  // check if local decode is open to get new input or is stalled
  bool nextStageOk = !isInternallyStalled();
  bool canGo = meshOk && nextStageOk;
  
  return !canGo;
}

/*void
Vector::updateStreamSeqNum(InstSeqNum seqNum) { 
  //if (getConfigured()) DPRINTF(Mesh, "set slave seq num %s\n", seqNum);
  _lastStreamSeqNum = seqNum; 
  // when do isSerializeAfter (a branch), need to hack in an additional +1
  //_lastStreamSeqNum += 1;
}

int
Vector::getStreamSeqNum() {
  return _lastStreamSeqNum;
}

void
Vector::setMispredict() {
  //_pendingMispredict = true;
  //_mispredictTick = curTick();
  _cpu.schedule(_mispredictUpdate, _cpu.clockEdge(Cycles(1)));
}

// TODO no longer used
// this is hack to see if we sent anything this cycle when the state machine needs to update
bool
Vector::sentMsgThisCycle() {
  return _internalInputThisCycle;
}

void
Vector::handleMispredict() {
  //if (_pendingMispredict) {
    //if (_mispredictTick == curTick()) DPRINTF(Mesh, "[[WARNING]] Update due to pending mispredict on same cycle it was set\n");
    //_lastStreamSeqNum++;
    updateStreamSeqNum(getStreamSeqNum() + 1);
    //_pendingMispredict = false;
    
  //}
}*/
