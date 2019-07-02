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
          idx, _cpu), tickEvent(_cpu), val(false), active(false), 
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
bool
ToMeshPort::checkHandshake(){
  bool rdy = getPairRdy();
  return (PROTOCOL(active, val, rdy));
}

bool
ToMeshPort::getPairRdy() {
  BaseSlavePort *slavePort = &(getSlavePort());
  if (FromMeshPort *slaveMeshPort = dynamic_cast<FromMeshPort*>(slavePort)) {
    return slaveMeshPort->getRdy();
  }
  else {
    return false;
  }
}

void
ToMeshPort::tryUnblockNeighbor() {
  BaseSlavePort *slavePort = &(getSlavePort());
  if (FromMeshPort *slaveMeshPort = dynamic_cast<FromMeshPort*>(slavePort)) {
    slaveMeshPort->tryUnblockCPU();
  }
}

void
ToMeshPort::failToSend(PacketPtr pkt) {
  // assert not rdy
  cpu->resetRdy();
  
  // store this packet to be used later
  stalledPkt = pkt;
}

void
ToMeshPort::beginToSend() {
  // if there is a stalled packet lets send that now
  if (stalledPkt) {
    sendTimingReq(stalledPkt);
    stalledPkt = nullptr;
  }
  
  cpu->setVal();
  cpu->setRdy();
}

/*----------------------------------------------------------------------
 * Define mesh slave port behavior
 *--------------------------------------------------------------------*/ 

FromMeshPort::FromMeshPort(TimingSimpleCPU *_cpu, int idx)
        : TimingCPUSlavePort(
          _cpu->name() + ".mesh_in_port" + csprintf("[%d]", idx), 
          idx, _cpu), tickEvent(_cpu), rdy(false), active(false)
    { }

// how to handle a request after waiting for some delay
void
FromMeshPort::MITickEvent::process(){
  // cpu->dostuff();
}

bool 
FromMeshPort::recvTimingReq(PacketPtr pkt) {
  DPRINTF(Mesh, "Received mesh request %#x with data \n", pkt->getAddr());
    // we should only ever see one response per cycle since we only
    // issue a new request once this response is sunk
    assert(!tickEvent.scheduled());
    // delay processing of returned data until next CPU clock edge
    tickEvent.schedule(pkt, cpu->clockEdge());

    return true;
}

void
FromMeshPort::recvRespRetry() {
  // ?
  assert(0);
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

PacketPtr
FromMeshPort::getPacket() {
  PacketPtr ret = recvPkt;
  recvPkt = nullptr;
  return ret;
}

void
FromMeshPort::setPacket(PacketPtr pkt) {
  if (recvPkt != nullptr) {
    DPRINTF(Mesh, "Overwrite packet %#x in port %d\n", recvPkt->getAddr(), idx);
  }
  
  recvPkt = pkt;
}

void
FromMeshPort::setRdy(bool rdy) {
  this->rdy = rdy;
}

// if active we want val/rdy, otherwise we don't care
bool
FromMeshPort::checkHandshake(){
  bool val = getPairVal();
  return (PROTOCOL(active, val, rdy));
}

bool
FromMeshPort::getPairVal() {
  BaseMasterPort *masterPort = &(getMasterPort());
  if (ToMeshPort *masterMeshPort = dynamic_cast<ToMeshPort*>(masterPort)) {
    return masterMeshPort->getVal();
  }
  else {
    return false;
  }
}

void
FromMeshPort::tryUnblockCPU() { 
  cpu->tryUnblock(); 
}
