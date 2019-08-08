#include "custom/mesh_ports.hh"
#include "cpu/simple/timing.hh"

#define PROTOCOL(active, val, rdy) \
  (active && val && rdy) || (!active)

/*----------------------------------------------------------------------
 * Implement base port behavior
 *--------------------------------------------------------------------*/ 

TimingCPUSlavePort::TimingCPUSlavePort(const std::string& _name, int idx, TimingSimpleCPU* _cpu)
        : SlavePort(_name, _cpu), idx(idx), cpu(_cpu)
         //, retryRespEvent([this]{ sendRetryResp(); }, name())
    { }

void
TimingCPUSlavePort::TickEvent::schedule(PacketPtr _pkt, Tick t)
{
    pkt = _pkt;
    cpu->schedule(this, t);
}

TimingCPUMasterPort::TimingCPUMasterPort(const std::string& _name, int idx, TimingSimpleCPU* _cpu) : 
  MasterPort(_name, _cpu), idx(idx), cpu(_cpu), retryRespEvent([this]{ sendRetryResp(); }, name()) {
        
}


void
TimingCPUMasterPort::TickEvent::schedule(PacketPtr _pkt, Tick t)
{
    pkt = _pkt;
    cpu->schedule(this, t);
}

/*----------------------------------------------------------------------
 * Define mesh master port behavior
 *--------------------------------------------------------------------*/ 


ToMeshPort::ToMeshPort(TimingSimpleCPU *_cpu, int idx)
        : TimingCPUMasterPort(
          _cpu->name() + ".mesh_out_port" + csprintf("[%d]", idx), 
          idx, _cpu), tickEvent(_cpu), val(false), active(NONE), 
          stalledPkt(nullptr)
    { }

// if you want to send a packet, used <MasterPort_Inst>.sendTimingReq(pkt);

// after waiting for some delay, we finally do something with the recv
// packet response
void
ToMeshPort::MOTickEvent::process()
{
    //cpu->completeIfetch(pkt);
}

// override what should happen when the packet we sent on this port has returned
bool
ToMeshPort::recvTimingResp(PacketPtr pkt)
{
    DPRINTF(Mesh, "Received mesh out response %#x\n", pkt->getAddr());
    // we should only ever see one response per cycle since we only
    // issue a new request once this response is sunk
    assert(!tickEvent.scheduled());
    // delay processing of returned data until next CPU clock edge
    tickEvent.schedule(pkt, cpu->clockEdge());

    return true;
}

// override what should happen when the packet we sent on this port failed to send
void
ToMeshPort::recvReqRetry()
{
    // we shouldn't get a retry unless we have a packet that we're
    // waiting to transmit
    /*assert(cpu->ifetch_pkt != NULL);
    assert(cpu->_status == IcacheRetry);
    PacketPtr tmp = cpu->ifetch_pkt;
    if (sendTimingReq(tmp)) {
        cpu->_status = IcacheWaitResponse;
        cpu->ifetch_pkt = NULL;
    }*/
}

void
ToMeshPort::setVal(bool val) {
  this->val = val;
}

// if active we want val/rdy, otherwise we don't care
/*bool
ToMeshPort::checkHandshake(){
  bool rdy = getPairRdy();
  return (PROTOCOL(active, val, rdy));
}*/

bool
ToMeshPort::getPairRdy() {
  BaseSlavePort *slavePort = &(getSlavePort());
  if (FromMeshPort *slaveMeshPort = dynamic_cast<FromMeshPort*>(slavePort)) {
    return slaveMeshPort->getRdy();
  }
  // if not connected to a mesh port, then probably an edge harness
  // in this case, its always rdy!
  else {
    return true;
  }
}

void
ToMeshPort::tryUnblockNeighbor() {
  BaseSlavePort *slavePort = &(getSlavePort());
  if (FromMeshPort *slaveMeshPort = dynamic_cast<FromMeshPort*>(slavePort)) {
    slaveMeshPort->tryUnblockCPU();
  }
}

// deprecated
/*void
ToMeshPort::failToSend(PacketPtr pkt) {
  // assert not rdy
  cpu->resetRdy();
  
  // store this packet to be used later
  stalledPkt = pkt;
}

// deprecated
void
ToMeshPort::beginToSend() {
  // if there is a stalled packet lets send that now
  if (stalledPkt) {
    sendTimingReq(stalledPkt);
    stalledPkt = nullptr;
  }
  
  cpu->setVal();
  cpu->setRdy();
}*/

/*----------------------------------------------------------------------
 * Define mesh slave port behavior
 *--------------------------------------------------------------------*/ 

// NEVER give THIS pointer 
FromMeshPort::FromMeshPort(TimingSimpleCPU *_cpu, int idx)
        : TimingCPUSlavePort(
          _cpu->name() + ".mesh_in_port" + csprintf("[%d]", idx), 
          idx, _cpu), recvPkt_d(nullptr), recvEvent([this] { process(); }, name()), 
          wakeupCPUEvent([this] { tryUnblockCPU(); }, name()), 
          recvPkt(nullptr), cyclePktRecv(0), rdy(false), active(NONE)
    { 
      //DPRINTF(Mesh, "init %d %d %ld %ld\n", rdy, active, (uint64_t)recvPkt, (uint64_t)this);
      DPRINTF(Mesh, "init idx %d\n", idx);
      }

// This isn't working when multiple FromMesh ports schedule this event!!!!
// how to handle a request after waiting for some delay
void
FromMeshPort::process(){
  
  assert(cpu != nullptr);
  // crashes the session :(
  if (idx < 4){
    DPRINTF(Mesh, "process idx %d\n", idx);
  }
  else {
    assert(0);
  }
  // save the received packet
  setPacket(recvPkt_d);
}

bool 
FromMeshPort::recvTimingReq(PacketPtr pkt) {
  //DPRINTF(Mesh, "recvresp packet %ld %ld\n", (uint64_t)recvPkt, (uint64_t)this);
  DPRINTF(Mesh, "Received mesh request %#x for idx %d\n", pkt->getAddr(), idx);
    // we should only ever see one response per cycle since we only
    // issue a new request once this response is sunk
    
    //assert(!tickEvent.scheduled());
    assert(!recvEvent.scheduled());
    // delay processing of returned data until next CPU clock edge
    //tickEvent.schedule(pkt, cpu->clockEdge());
    //recvPkt_d = pkt;
    //recvEvent.schedule(cpu->clockEdge());
    recvPkt_d = pkt;
    cpu->schedule(recvEvent, cpu->clockEdge());
    // temp
    //setPacket(pkt);

    // TODO ERROR wrong need to do this on the next cycle!
    // try to unblock when recv a packet
    //cpu->tryUnblock(false);

    return true;
}

void
FromMeshPort::recvRespRetry() {
  // ?
  assert(0);
}

// the this pointer changes after constructing due to being transfered
// to vector mem management
void
FromMeshPort::setupEvents() {
  recvEvent = EventFunctionWrapper([this] { process(); }, name());
  wakeupCPUEvent = EventFunctionWrapper([this] { tryUnblockCPU(); }, name());
}

void
FromMeshPort::recvFunctional(PacketPtr pkt) {
  // ? just call MITickEvent::proccess?
  assert(0);
}

// might want to do bsg_manycore eva treament here for addressing
AddrRangeList 
FromMeshPort::getAddrRanges() const {
  //return cpu->getAddrRanges();
  return std::list<AddrRange>();
}

// get recv pkts
PacketPtr
FromMeshPort::getPacket() {
  if (recvPkt == nullptr) {
    DPRINTF(Mesh, "[[WARNING]] Did not recv packet\n");
    return nullptr;
  }
  
  if (cyclePktRecv >= cpu->clockEdge()) {
    DPRINTF(Mesh, "[[WARNING]] Packet not ready for use\n");
    return nullptr;
  }
  
  PacketPtr curPacket = recvPkt;
  
  // destructive read on packet
  recvPkt = nullptr;
  
  // this might update val/rdy interface
  //cpu->informNeighbors();
  
  return curPacket;
}


// extract data from packet
uint64_t
FromMeshPort::getPacketData(PacketPtr pkt) {
  return pkt->getUintX(LittleEndianByteOrder);
}

/*uint64_t
FromMeshPort::getPacketData() {
  PacketPtr curPacket = getPacket();
  if (curPacket == nullptr) return 0;
  
  // get data from packet
  uint64_t data = curPacket->getUintX(LittleEndianByteOrder);
  
  // destructive read on packet
  recvPkt = nullptr;
  
  // this might update val/rdy interface
  cpu->informNeighbors();
  
  return data;
}*/

void
FromMeshPort::setPacket(PacketPtr pkt) {
  if (recvPkt != nullptr) {
    DPRINTF(Mesh, "[[WARNING]] Overwrite packet %#x in port %d\n", recvPkt->getAddr(), idx);
  }
  
  recvPkt = pkt;
  
  // remember the time we recv the packet
  cyclePktRecv = cpu->clockEdge();
  
  // schedule a wakeup event on the next cycle to try to run with this
  // pkt
  cpu->schedule(wakeupCPUEvent, cpu->clockEdge(Cycles(1)));
}

bool
FromMeshPort::pktExists() { 
  return ((recvPkt != nullptr) && (cyclePktRecv < cpu->clockEdge()));
}

void
FromMeshPort::setRdy(bool rdy) {
  this->rdy = rdy;
}

// if active we want val/rdy, otherwise we don't care
/*bool
FromMeshPort::checkHandshake(){
  bool val = getPairVal();
  return (PROTOCOL(active, val, rdy));
}*/

bool
FromMeshPort::getPairVal() {
  return pktExists();
  /*BaseMasterPort *masterPort = &(getMasterPort());
  if (ToMeshPort *masterMeshPort = dynamic_cast<ToMeshPort*>(masterPort)) {
    return masterMeshPort->getVal();
  }
  else {
    return false;
  }*/
}

void
FromMeshPort::tryUnblockCPU() {
  cpu->tryUnblock(true); 
}

bool
FromMeshPort::getRdy() {
  if (rdy) {
    // if we have a packet but don't have a packet in other ports or
    // we have packets but can't send them anywhere b/c output not rdy
    // then we can't accept anymore packets on this port b/c we can't tick
    
    // should look at next_val?
    // if (next_pairval && !next_pairval, then not rdy?)
    
    if (getPairVal() && 
      cpu->getInVal(active) && cpu->getOutRdy(active)) return true;
    else if (getPairVal() && 
      (!cpu->getInVal(active) || !cpu->getOutRdy(active))) return false;
    else return true;
  }
  else {
    return false;
  }
}
