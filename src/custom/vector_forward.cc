#include "arch/registers.hh"

#include "custom/vector_forward.hh"

#include "debug/Mesh.hh"

VectorForward::VectorForward(const std::string &name,
  MinorCPU &cpu,
  MinorCPUParams &p,
  Minor::Latch<Minor::ForwardInstData>::Input out,
  std::vector<Minor::InputBuffer<Minor::ForwardInstData>> &nextStageReserve) : 
        
    Named(name),
    _cpu(cpu),
    _out(out),
    _nextStageReserve(nextStageReserve),
    _numInPortsActive(0),
    _numOutPortsActive(0),
    _stage(FETCH),
    _fsm(std::make_shared<EventDrivenFSM>(this, &_cpu, _stage)),
    _curCsrVal(0)
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
  
  // check the state of the fsm (dont need async updates like before)
  // because minor has cycle by cycle ticks unlike TimingSimpleCPU
  bool update = _fsm->tick();
  if (update) {
    // inform there is local activity
    _cpu.activityRecorder->activity();
    // inform there might be neighbor activity
    informNeighbors();
  }
  
  // check for instruction from mesh
  bool canGo = _fsm->isMeshActive();
  
  if (canGo) {
    
    // pull instruction from the mesh or from the local fetch stage
    TheISA::MachInst instWord = pullInstruction();
  
    // give instruction to the local decode stage if present
    pushInstToNextStage(instWord);
    
    // forward instruction to other neighbors potentially
    forwardInstruction(instWord);
    
    // if any one of the stages calls this, then the processor will tick
    // on the follwoing cycle
    _cpu.activityRecorder->activity();
    
    // reserve space in the output buffer?
    Minor::ForwardInstData &insts_out = *_out.inputWire;
    // tid is always 0 when no smt
    int tid = 0;
    
    insts_out.threadId = tid;
    _nextStageReserve[tid].reserve();
  }
  
  // if not configured just pop the input so fetch doesn't stall
  if (MeshHelper::isCSRDefault(_curCsrVal)) {
    popFetchInput(0);
  }
  
}

/*// Decprecated using buffers now
bool
VectorForward::canIssue() {
  bool ok = _fsm->isMeshActive() || (getNumPortsActive() == 0);
  return ok;
}*/


void
VectorForward::forwardInstruction(const TheISA::MachInst inst) {
  
  // find each direction to send a packet to
  std::vector<Mesh_DS_t> out;
  MeshHelper::csrToOutSrcs(RiscvISA::MISCREG_FETCH, _curCsrVal, out);
  
  // send a packet in each direction
  for (int i = 0; i < out.size(); i++) {
    Mesh_Dir dir = out[i].outDir;
    Mesh_Out_Src src = out[i].src;
    
    DPRINTF(Mesh, "Sending mesh request %d from %d with val %#x\n", dir, src, inst);
  
    PacketPtr new_pkt = createMeshPacket((uint64_t)inst);
    _toMeshPort[dir].sendTimingReq(new_pkt);
    
  }
}

TheISA::MachInst
VectorForward::pullInstruction() {
  
  // if slave, pull from the mesh
  Mesh_Dir recvDir;
  if (MeshHelper::fetCsrToInSrc(_curCsrVal, recvDir)) {
    uint64_t meshData = getMeshPortData(recvDir);
    TheISA::MachInst instWord = (TheISA::MachInst) meshData;
    return instWord;
  }
  // if master, pull from the local fetch
  else {
    TheISA::MachInst instWord = getFetchInput(0);
    // pop to free up the space
    popFetchInput(0);
    return instWord;
  }
}

Minor::MinorDynInstPtr
VectorForward::createInstruction(const TheISA::MachInst instWord) {
  // make a minor dynamic instruction to pass to the decode stage
  // bs most of the fields because the slave wouldn't be keeping track
  // of this
  Minor::MinorDynInstPtr dyn_inst = new Minor::MinorDynInst(0);
  //dyn_inst->id.fetchSeqNum = 0;
  //dyn_inst->id.predictionSeqNum = 0;
  //assert(dyn_inst->id.execSeqNum == 0);
  //dyn_inst->pc = 0;
  dyn_inst->staticInst = extractInstruction(instWord);
  
  DPRINTF(Mesh, "decoder inst %s\n", *dyn_inst);

  return dyn_inst;
}

void
VectorForward::pushInstToNextStage(const TheISA::MachInst instWord) {
  Minor::MinorDynInstPtr dynInst = createInstruction(instWord);
  pushToNextStage(dynInst);
}

void
VectorForward::pushToNextStage(const Minor::MinorDynInstPtr dynInst) {
  Minor::ForwardInstData &insts_out = *_out.inputWire;
  insts_out.resize(1);
  
  // Pack the generated dynamic instruction into the output
  insts_out.insts[0] = dynInst;
}

void
VectorForward::setupConfig(RegVal csrVal) {
  // clear all ports associated with this csr
  
  resetActive();
  
  DPRINTF(Mesh, "csrVal %d\n", csrVal);
  
  int csrId = MeshHelper::stageToCsr(_stage);
  
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
  // size is numbytes?
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
      //if (!_fromMeshPort[i].getPairVal()) allVal = false;
      if (!_fromMeshPort[i].pktExists()) allVal = false;
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

TheISA::MachInst
VectorForward::getFetchInput(ThreadID tid) {
  // Get a line from the inputBuffer to work with
  if (!_inputBuffer[tid].empty()) {
    auto msg = &(_inputBuffer[tid].front());
    return msg->machInst;
  } else {
    return 0;
  }
}

void
VectorForward::popFetchInput(ThreadID tid) {
  if (!_inputBuffer[tid].empty()) {
    //_inputBuffer[tid].front().freeLine();
    _inputBuffer[tid].pop();
  }
}



