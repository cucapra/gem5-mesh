#include "arch/registers.hh"
#include "base/bitfield.hh"
#include "custom/vector_forward.hh"

#include "debug/Mesh.hh"

using namespace Minor;

VectorForward::VectorForward(const std::string &name,
  MinorCPU &cpu,
  MinorCPUParams &p,
  Minor::Latch<Minor::ForwardInstData>::Input out,
  std::vector<Minor::InputBuffer<Minor::ForwardInstData>> &nextStageReserve,
  std::function<bool()> backStall) : 
        
    Named(name),
    _cpu(cpu),
    _out(out),
    _nextStageReserve(nextStageReserve),
    _numInPortsActive(0),
    _numOutPortsActive(0),
    _stage(FETCH),
    _fsm(std::make_shared<EventDrivenFSM>(this, &_cpu, _stage)),
    _curCsrVal(0),
    _checkExeStall(backStall),
    _wasStalled(false),
    _internalInputThisCycle(false),
    _mispredictUpdate([this] { handleMispredict(); }, name)
    //_pendingMispredict(false),
    //_mispredictTick(0)
{
  // 
  // declare vector ports
  for (int i = 0; i < p.port_to_mesh_port_connection_count; ++i) {
      _toMeshPort.emplace_back(this, &_cpu, i);
  }
   
  for (int i = 0; i < p.port_from_mesh_port_connection_count; ++i) {
      _fromMeshPort.emplace_back(this, &_cpu, i);
  }
    
  for (int i = 0; i < p.port_from_mesh_port_connection_count; i++) {
    // need to setup anything involving the 'this' pointer in the port
    // class after have moved into vector memory
    
    // alternatively could declare ports as pointers
    _fromMeshPort[i].setupEvents();
  }
  
  // Per-thread input buffers
  for (ThreadID tid = 0; tid < p.numThreads; tid++) {
    _inputBuffer.push_back(
      Minor::InputBuffer<Minor::ForwardVectorData>(
        name + ".inputBuffer" + std::to_string(tid), "insts",
        1 /* buf size */));
  }
  
  
}

void
VectorForward::evaluate() {
  
  // hack so when we call this, we still have this information on state transition
  _internalInputThisCycle = !_inputBuffer[0].empty();
  
  // check the state of the fsm (dont need async updates like before)
  // because minor has cycle by cycle ticks unlike TimingSimpleCPU
  bool update = _fsm->tick();
  if (update) {
    // set the mesh ports with the new state output
    // can't do here b/c not guarenteed to be visible to nearby cores
    //EventDrivenFSM::Outputs_t fsmOut = _fsm->stateOutput();
    //setVal(fsmOut.val);
    //setRdy(fsmOut.rdy);
    
    // inform there is local activity
    _cpu.activityRecorder->activity();
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
    ForwardVectorData instInfo;
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
  
  // if not configured just pop the input so fetch doesn't stall
  if (!getConfigured()) {
    popFetchInput(0);
  }
  
  // TODO need to actually stall fetch2 when in slave mode??? why are we not doing that
  
}

/*// Decprecated using buffers now
bool
VectorForward::canIssue() {
  bool ok = _fsm->isMeshActive() || (getNumPortsActive() == 0);
  return ok;
}*/


void
VectorForward::forwardInstruction(const ForwardVectorData& instInfo) {
  
  // find each direction to send a packet to
  std::vector<Mesh_DS_t> out;
  MeshHelper::csrToOutSrcs(RiscvISA::MISCREG_FETCH, _curCsrVal, out);
  
  // send a packet in each direction
  for (int i = 0; i < out.size(); i++) {
    Mesh_Dir dir = out[i].outDir;
    Mesh_Out_Src src = out[i].src;
    
    
    uint64_t meshData = encodeMeshData(instInfo);
    DPRINTF(Mesh, "Sending mesh request %d from %d with val %#x %d %d = %#x\n", 
        dir, src, instInfo.machInst, instInfo.predictTaken, instInfo.mispredicted, meshData);
    if (instInfo.predictTaken) DPRINTF(Mesh, "predict taken %d\n", instInfo.predictTaken);
    if (instInfo.mispredicted) DPRINTF(Mesh, "mispredicted %d\n", instInfo.mispredicted);
    PacketPtr new_pkt = createMeshPacket(meshData);
    _toMeshPort[dir].sendTimingReq(new_pkt);
    
  }
}

void
VectorForward::pullInstruction(ForwardVectorData &instInfo) {
  
  // if slave, pull from the mesh
  Mesh_Dir recvDir;
  if (MeshHelper::fetCsrToInSrc(_curCsrVal, recvDir)) {
    uint64_t meshData = getMeshPortData(recvDir);
    
    // decode the msg here
    instInfo = decodeMeshData(meshData);
    DPRINTF(Mesh, "decoded %#x -> %#x %d %d\n", meshData, instInfo.machInst, instInfo.predictTaken, instInfo.mispredicted);
    //return instWord;
  }
  // if master, pull from the local fetch
  else {
    auto msg = getFetchInput(0);
    
    instInfo.machInst = msg->machInst;
    instInfo.predictTaken = msg->predictTaken;
    instInfo.mispredicted = msg->mispredicted;
    
    // pop to free up the space
    popFetchInput(0);
  }
}

ForwardVectorData
VectorForward::decodeMeshData(uint64_t data) {
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
VectorForward::encodeMeshData(const ForwardVectorData &instInfo) {
  return (((uint64_t)instInfo.mispredicted) << 33) | (((uint64_t)instInfo.predictTaken) << 32) | (uint64_t)instInfo.machInst;
}

Minor::MinorDynInstPtr
VectorForward::createInstruction(const ForwardVectorData &instInfo) {
  // make a minor dynamic instruction to pass to the decode stage
  // bs most of the fields because the slave wouldn't be keeping track
  // of this
  Minor::MinorDynInstPtr dyn_inst = new Minor::MinorDynInst(0);
  //dyn_inst->id.fetchSeqNum = 0;
  //dyn_inst->id.predictionSeqNum = 0;
  //assert(dyn_inst->id.execSeqNum == 0);
  //dyn_inst->pc = 0;
  dyn_inst->staticInst = extractInstruction(instInfo.machInst);
  
  // lie about fetchSeqNum so not considered a bubble
  // alternatviely can try to track own pc here
  dyn_inst->id.fetchSeqNum = 1;
  
  // if there was a branch prediction/taken by the master core, apply this
  // change to the seq number so that the execute stage won't squash the 
  // instruction.
  /*if (instInfo.branchTaken) _lastStreamSeqNum++;
  
  // It's important that seq numbers only change on the next basic block
  // rather than at the end of the current basic block. otherwise the branch
  // will be discarded because we changed seqNum before execute even got result
  
  // Also will eventually need to support divergence and/or predication
  
  if (instInfo.branchTaken && dyn_inst->staticInst->isCondCtrl()) {
    dyn_inst->id.streamSeqNum = _lastStreamSeqNum - 1;
  }
  else {
    dyn_inst->id.streamSeqNum = _lastStreamSeqNum;
  }*/
  
  /*if (instInfo.mispredicted || _pendingMispredict) { 
    if (instInfo.mispredicted) DPRINTF(Mesh, "update due seq misalignment\n");
    if (_pendingMispredict) DPRINTF(Mesh, "Update due to pending mispredict\n");
    //_lastStreamSeqNum++;
    updateStreamSeqNum(getStreamSeqNum() + 1);
    _pendingMispredict = false;
  }*/
  
  //handleMispredict();
  
  dyn_inst->id.streamSeqNum = _lastStreamSeqNum;
  
  // mark this instruction as being generated by the vector stage
  // this flag will be used by later stages to do unique behavior like
  // turn off the BTB check in execute
  dyn_inst->fromVector = true;
  
  // use branch prediction from master to get execute to correctly update
  // seq number
  dyn_inst->predictedTaken = instInfo.predictTaken;
  
  
  //DPRINTF(Mesh, "decoder inst %s\n", *dyn_inst);

  return dyn_inst;
}

void
VectorForward::pushInstToNextStage(const ForwardVectorData& instInfo) {
  // only push into decode if we are a slave
  if (MeshHelper::isVectorSlave(_curCsrVal)) {
    // update the stream seq number based on branch hint from master
    //if (instInfo.branchTaken) _lastStreamSeqNum++;
    
    Minor::MinorDynInstPtr dynInst = createInstruction(instInfo);
    pushToNextStage(dynInst);
  }
}

void
VectorForward::pushToNextStage(const Minor::MinorDynInstPtr dynInst) {
  DPRINTF(Mesh, "push instruction to decode %s\n", *dynInst);
  Minor::ForwardInstData &insts_out = *_out.inputWire;
  insts_out.resize(1);
  
  // Pack the generated dynamic instruction into the output
  insts_out.insts[0] = dynInst;
  
  // if any one of the stages calls this, then the processor will tick
  // on the follwoing cycle
  _cpu.activityRecorder->activity();
    
  // reserve space in the output buffer?
  //Minor::ForwardInstData &insts_out = *_out.inputWire;
  // tid is always 0 when no smt
  int tid = 0;
    
  insts_out.threadId = tid;
  
  assert(!(dynInst->isBubble()));
  assert(!insts_out.isBubble());
  
  _nextStageReserve[tid].reserve();
}

void
VectorForward::setupConfig(int csrId, RegVal csrVal) {
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
VectorForward::extractInstruction(const TheISA::MachInst inst) {
  //TheISA::Decoder *decoder = thread->getDecoderPtr();
  // decoder->reset() sometimes?
  /*TheISA::MachInst inst_word;
  inst_word = TheISA::gtoh(
    *(reinterpret_cast<TheISA::MachInst *>
      (line + fetch_info.inputIndex)));

  if (!decoder->instReady()) {
      decoder->moreBytes(fetch_info.pc,
          line_in->lineBaseAddr + fetch_info.inputIndex,
          inst_word);
        DPRINTF(Fetch, "Offering MachInst to decoder addr: 0x%x\n",
            line_in->lineBaseAddr + fetch_info.inputIndex);
        }

        if (decoder->instReady()) {
         StaticInstPtr decoded_inst = decoder->decode(fetch_info.pc);
        */
        
  // can we just totally fake the actual decoding? probably
  // the fetchPc and addr stuff is generally just used for printouts
  // really only need machInst
  
  // For RISCV decoder, ExtMachInst = MachInst
  // has a decode(ExtMachInst) function
  
  
  // but may also need to provide moreBytes to check alignment
  // this just seems to be when don't have an aligned instruction
  // seems only important for compressed instruction, which we don't care about?
  
  // bool aligned = pc.pc() % sizeof(MachInst) == 0;?
  
  // TODO might need to call decoder.moreBytes() to align MachInst (32 bits)
  // onto ExtMachInst (64 bits). although I thought all RV instructions
  // were 32 bits? Would need to get the PC from master core to do this
  // occasionally instructions can be 16bits (compressed) or more?
  RiscvISA::Decoder decoder;
  StaticInstPtr ret = decoder.decode(inst, 0x0);
  return ret;
  
}



PacketPtr
VectorForward::createMeshPacket(RegVal payload) {
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


bool
VectorForward::getOutRdy() {
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
VectorForward::getInVal() {
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
VectorForward::setRdy(bool rdy) {
  for (int i = 0; i < _fromMeshPort.size(); i++) {
    _fromMeshPort[i].setRdyIfActive(rdy, _stage);
  }
}

void
VectorForward::setVal(bool val) {
  for (int i = 0; i < _toMeshPort.size(); i++) {
    _toMeshPort[i].setValIfActive(val, _stage);
  }
}

void
VectorForward::resetActive() {
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
VectorForward::neighborUpdate() {
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
VectorForward::informNeighbors() {
  //DPRINTF(Mesh, "notify neighbors\n");
  // go through mesh ports to get tryUnblock function called in neighbor cores
  for (int i = 0; i < _toMeshPort.size(); i++) {
    _toMeshPort[i].tryUnblockNeighbor();
  }
}

uint64_t
VectorForward::getMeshPortData(Mesh_Dir dir) {
  PacketPtr pkt = getMeshPortPkt(dir);
  return FromMeshPort::getPacketData(pkt);
  //return fromMeshPort[dir].getPacketData();
}

PacketPtr
VectorForward::getMeshPortPkt(Mesh_Dir dir) {
  return _fromMeshPort[dir].getPacket();
}

Port &
VectorForward::getMeshPort(int idx, bool isOut) {
 if (isOut) {
   return _toMeshPort[idx];
 } 
 else {
   return _fromMeshPort[idx];
 }
}

int
VectorForward::getNumMeshPorts() {
  return _toMeshPort.size();
}

int
VectorForward::getNumPortsActive() {
  return _numInPortsActive + _numOutPortsActive;
}

std::vector<Minor::InputBuffer<Minor::ForwardVectorData>>&
VectorForward::getInputBuf() {
  return _inputBuffer;
}

ForwardVectorData*
VectorForward::getFetchInput(ThreadID tid) {
  // Get a line from the inputBuffer to work with
  if (!_inputBuffer[tid].empty()) {
    auto msg = &(_inputBuffer[tid].front());
    return msg;
  } else {
    return nullptr;
  }
}

void
VectorForward::popFetchInput(ThreadID tid) {
  if (!_inputBuffer[tid].empty()) {
    //_inputBuffer[tid].front().freeLine();
    _inputBuffer[tid].pop();
  }
}

void
VectorForward::stallFetchInput(ThreadID tid) {
  if (_inputBuffer[tid].empty()) {
    ForwardVectorData vecData(0);
    vecData.machInst = 0x511; // c_addi (nop)
    _inputBuffer[tid].setTail(vecData);
    _inputBuffer[tid].pushTail();
    DPRINTF(Mesh, "try stall frontend %d?=0\n", _inputBuffer[tid].canReserve());
  }
  /*else {
    DPRINTF(Mesh, "fetch not empty %d\n", _inputBuffer[tid].canReserve());
  }*/
}

void
VectorForward::unstallFetchInput(ThreadID tid) {
  popFetchInput(tid);
}

bool
VectorForward::getConfigured() {
  return !MeshHelper::isCSRDefault(_curCsrVal);
}

bool
VectorForward::isInternallyStalled() {
  int tid = 0;
  bool decodeStall = !_nextStageReserve[tid].canReserve();
  
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
  bool senderStall = sender && !recver && !_internalInputThisCycle;
  
  bool stall = recverStall || senderStall;
  /*if (stall) {
    DPRINTF(Mesh, "internal stall: rs %d ss %d r %d s %d dec %d in %d\n", 
      recverStall, senderStall, recver, sender, decodeStall, _internalInputThisCycle);
  }*/
  
  return stall;
}


void
VectorForward::processInternalStalls() {
  bool justStalled = isInternallyStalled();
  
  if (_wasStalled ^ justStalled) {
    _fsm->stallEvent();
  }
  
  _wasStalled = justStalled;
}

bool
VectorForward::shouldStall() {
  // check for instruction from mesh
  bool meshOk = _fsm->isMeshActive();
  
  // check if local decode is open to get new input or is stalled
  bool nextStageOk = !isInternallyStalled();
  bool canGo = meshOk && nextStageOk;
  
  return !canGo;
}

void
VectorForward::updateStreamSeqNum(InstSeqNum seqNum) { 
  if (getConfigured()) DPRINTF(Mesh, "set slave seq num %s\n", seqNum);
  _lastStreamSeqNum = seqNum; 
  // when do isSerializeAfter (a branch), need to hack in an additional +1
  //_lastStreamSeqNum += 1;
}

int
VectorForward::getStreamSeqNum() {
  return _lastStreamSeqNum;
}

void
VectorForward::setMispredict() {
  //_pendingMispredict = true;
  //_mispredictTick = curTick();
  _cpu.schedule(_mispredictUpdate, _cpu.clockEdge(Cycles(1)));
}

// TODO no longer used
// this is hack to see if we sent anything this cycle when the state machine needs to update
bool
VectorForward::sentMsgThisCycle() {
  return _internalInputThisCycle;
}

void
VectorForward::handleMispredict() {
  //if (_pendingMispredict) {
    //if (_mispredictTick == curTick()) DPRINTF(Mesh, "[[WARNING]] Update due to pending mispredict on same cycle it was set\n");
    //_lastStreamSeqNum++;
    updateStreamSeqNum(getStreamSeqNum() + 1);
    //_pendingMispredict = false;
    
  //}
}

