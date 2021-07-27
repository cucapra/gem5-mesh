// Authors: Philip Bedoukian

#include "custom/mesh_ports.hh"
#include "cpu/io/cpu.hh"
#include "custom/vector.hh"
#include "debug/Mesh.hh"

#define PROTOCOL(active, val, rdy) \
  (active && val && rdy) || (!active)

#define MESH_QUEUE_SLOTS 2

/*----------------------------------------------------------------------
 * Define mesh master port behavior
 *--------------------------------------------------------------------*/ 


ToMeshPort::ToMeshPort(IOCPU *_cpu, int idx)
        : MasterPort(
          _cpu->name() + ".mesh_out_port" + csprintf("[%d]", idx), _cpu), 
          cpu(_cpu), idx(idx), active(NONE), vec(nullptr)
    { }

// if you want to send a packet, used <MasterPort_Inst>.sendTimingReq(pkt);

// override what should happen when the packet we sent on this port has returned
bool
ToMeshPort::recvTimingResp(PacketPtr pkt)
{
    return true;
}

// override what should happen when the packet we sent on this port failed to send
void
ToMeshPort::recvReqRetry()
{
}

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

/*----------------------------------------------------------------------
 * Define mesh slave port behavior
 *--------------------------------------------------------------------*/ 

// NEVER give THIS pointer 
FromMeshPort::FromMeshPort(IOCPU *_cpu, int idx)
        : SlavePort(
          _cpu->name() + ".mesh_in_port" + csprintf("[%d]", idx), _cpu), 
          cpu(_cpu), idx(idx), recvPkt_d(nullptr), recvEvent([this] { process(); }, name()), 
          wakeupCPUEvent([this] { tryUnblockCPU(); }, name()), 
          active(NONE), _meshQueue(name(), "pkt", MESH_QUEUE_SLOTS), 
          vec(nullptr)
    {}

// This isn't working when multiple FromMesh ports schedule this event!!!!
// how to handle a request after waiting for some delay
void
FromMeshPort::process(){
  // save the received packet
  setPacket(recvPkt_d);
}

bool 
FromMeshPort::recvTimingReq(PacketPtr pkt) {
    if (vec->enqueueMeshPkt(pkt)) {
      return true;
    }

    return false;
}

void
FromMeshPort::recvRespRetry() {
  // this is if reject this cycle, but then space opens up due to combinational delay
  // I think we've worked around this with VecInstSel canRecv which also checks if it will open up
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
  
  PacketPtr pkt = nullptr;
  if (pktExists()) {
    pkt = _meshQueue.front().getPacket();
    _meshQueue.pop();
  }
  else {
    DPRINTF(Mesh, "[[WARNING]] No packet available\n");
  }
  
  return pkt;
}


// extract data from packet
uint64_t
FromMeshPort::getPacketData(PacketPtr pkt) {
  return pkt->getUintX(LittleEndianByteOrder);
}

void
FromMeshPort::setPacket(PacketPtr pkt) {
  
  // push packet onto a 2 element queue to be stall-proof
  if (_meshQueue.canReserve()) {
    auto pktData = MeshPacketData(pkt);
    _meshQueue.push(pktData);
    //DPRINTF(Mesh, "set packet, size now %d\n", _meshQueue.occupiedSpace());
  }
  else {
    DPRINTF(Mesh, "[[WARNING]] Dropping packet %#x in port %d\n", 
      pkt->getAddr(), idx);
  }
}

bool
FromMeshPort::pktExists() { 
  return !_meshQueue.empty();
}

bool
FromMeshPort::getPairVal() {
  return pktExists();
}

void
FromMeshPort::tryUnblockCPU() {
  if (vec)
    vec->neighborUpdate(); 
}

bool
FromMeshPort::getRdy() {
  if (!vec) return false;
  
  return vec->canRecvMeshPkt();

}