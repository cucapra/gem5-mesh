#ifndef __CUSTOM_MESH_PORTS_HH__
#define __CUSTOM_MESH_PORTS_HH__

/*
 * Mesh ports used in TimingSimpleCPU
 *
 * Authors: Philip Bedoukian
 */ 


#include "mem/port.hh"
#include "mem/packet.hh"
#include "sim/clocked_object.hh"
#include "custom/mesh_helper.hh"
#include "cpu/minor/buffers.hh"

//resolve circular deps
class IOCPU;
class Vector;


// TODO need this will using a Minor hardware queue, maybe want to move away?
/** Data for queues/regs between mesh nodes */
class MeshPacketData /* : public ReportIF, public BubbleIF */
{
  public:
    /** mesh packet */
    PacketPtr pkt;

  public:
    explicit MeshPacketData(PacketPtr pkt) { this->pkt = pkt; }

    MeshPacketData(const MeshPacketData &src) { *this = src; }

  public:

    /** Fill with bubbles */
    void bubbleFill() { pkt = nullptr; }

    /** BubbleIF interface */
    bool isBubble() const { return pkt == nullptr; }

    /** ReportIF interface */
    void reportData(std::ostream &os) const {}
    
    /** get the stored packet */
    PacketPtr getPacket() const { return pkt; }
};


// port that goes out over the mesh
class ToMeshPort : public MasterPort {
  public:
    ToMeshPort(IOCPU *_cpu, int idx);

    // set the driving stage
    void setDriver(Vector *vec) { this->vec = vec; }

    //void setVal(bool val);
    //bool getVal();
    
    bool getPairRdy();
    
    void setActive (SensitiveStage active) { this->active = active; }
    SensitiveStage getActive() const { return active; }
    bool isActive() const { return (active != NONE); }
    
    // call tryUnblock() in cpu attach to slave port of this
    void tryUnblockNeighbor();

  protected:

    // important to implement/override these from MasterPort class
    virtual bool recvTimingResp(PacketPtr pkt);
    virtual void recvReqRetry();

    // cpu for callbacks
    IOCPU *cpu;
    
    // idx for debug
    int idx;
    
    // whether this port is used and should assert val when packet 
    // available
    SensitiveStage active;
    
    Vector *vec;
    
};

class FromMeshPort : public SlavePort {
  public:
    FromMeshPort(IOCPU *_cpu, int idx);

    virtual AddrRangeList getAddrRanges() const;

    // set the driving stage
    void setDriver(Vector *vec) { this->vec = vec; }

    // get the packet from the port
    PacketPtr getPacket();
    static uint64_t getPacketData(PacketPtr pkt);

    bool getRdy();
    
    bool getPairVal();
    
    void setActive (SensitiveStage active) { this->active = active; }
    SensitiveStage getActive() const { return active; }
    bool isActive() const { return (active != NONE); }
    
    // call tryUnblock in the cpu
    void tryUnblockCPU();
    
    bool pktExists();
    
    void setupEvents();
    
  protected:

    // important to implement/override these from slave port
    virtual bool recvTimingReq(PacketPtr pkt);
    virtual void recvRespRetry();
    virtual Tick recvAtomic(PacketPtr pkt) { panic("recvAtomic unimpl"); };
    virtual void recvFunctional(PacketPtr pkt);

    // cpu reference, so we can do things in it on response
    IOCPU* cpu;
    
    // id in vector port
    int idx;
  
    // packet to be received after clk edge
    PacketPtr recvPkt_d;
    EventFunctionWrapper recvEvent;
    EventFunctionWrapper wakeupCPUEvent;
    void process();
    
    // setter, if there is an unused packet already present
    // we should drop and inform of the lost packet
    void setPacket(PacketPtr pkt);
    
    // this should go high when the core is rdy
    SensitiveStage active;
    
    // we need to enqueue packets into a two-slot queue to handle
    // stalls in the pipeline. this replaces the single register being
    // used previously
    // if were willing to except long wires, then could have a global
    // stall signal in which wouldn't need the second register slot.
    // if not willing need 2x as many registers in the systolic net...
    // overhead? potentially can have a hybrid approach where have queues
    // every 4x4 tiles and lets wires expand within the 4x4 tile group
    // doesn't change performance just reduces the number of queue registers
    // while not blowing up cycle time
    Minor::Queue<MeshPacketData> _meshQueue;
    
    Vector *vec;
};


#endif
