#ifndef __CUSTOM_MESH_PORTS_HH__
#define __CUSTOM_MESH_PORTS_HH__

/*
 * Mesh ports used in TimingSimpleCPU
 */ 


#include "mem/port.hh"
#include "mem/packet.hh"
#include "sim/clocked_object.hh"
#include "custom/mesh_helper.hh"

//resolve circular deps
class MinorCPU;
class VectorForward;


// port that goes out over the mesh
class ToMeshPort : public MasterPort {
  public:
    ToMeshPort(VectorForward *vec, MinorCPU *_cpu, int idx);
        /*: TimingCPUMasterPort(
          _cpu->name() + ".mesh_out_port" + csprintf("[%d]", idx), 
          idx, _cpu), tickEvent(_cpu), val(false), active(false), 
          stalledPkt(nullptr)
    { }*/

    void setVal(bool val);
    bool getVal() const { return val; }
    
    bool getPairRdy();
    
    void setActive (SensitiveStage active) { this->active = active; }
    SensitiveStage getActive() const { return active; }
    bool isActive() const { return (active != NONE); }
    
    void setValIfActive(bool val, SensitiveStage stage);
    
    // check if this port is rdy and the slave port is valid
    //bool checkHandshake();
    
    // call tryUnblock() in cpu attach to slave port of this
    void tryUnblockNeighbor();
    
    // buffer pkt and inform we're not ready
    //void failToSend(PacketPtr pkt);
    
    // release the pkt and inform we are now ready
    //void beginToSend();

  protected:

    // important to implement/override these from MasterPort class
    virtual bool recvTimingResp(PacketPtr pkt);
    virtual void recvReqRetry();

    // cpu for callbacks
    MinorCPU *cpu;
    
    // idx for debug
    int idx;
    
    // whether this signal is valid over the mesh net
    // we're not going to set a value, rather you need to check from neighbors
    bool val;
    
    // whether this port is used and should assert val when packet 
    // available
    SensitiveStage active;
    
    // packet stored in register that can't be sent yet due to val/rdy
    // requires additional enable reg and 2-1 mux
    PacketPtr stalledPkt;
    
    VectorForward *vec;
    
};

class FromMeshPort : public SlavePort {
  public:
    FromMeshPort(VectorForward *vec, MinorCPU *_cpu, int idx);
        /*: TimingCPUSlavePort(
          _cpu->name() + ".mesh_in_port" + csprintf("[%d]", idx), 
          idx, _cpu), tickEvent(_cpu), rdy(false), active(false)
    { }*/

    virtual AddrRangeList getAddrRanges() const;

    // get the packet from the port
    PacketPtr getPacket();
    static uint64_t getPacketData(PacketPtr pkt);

    void setRdy(bool val);
    bool getRdy();
    
    bool getPairVal();
    
    void setActive (SensitiveStage active) { this->active = active; }
    SensitiveStage getActive() const { return active; }
    bool isActive() const { return (active != NONE); }
    
    void setRdyIfActive(bool rdy, SensitiveStage stage);
    
    // check val rdy interface
    //bool checkHandshake();
    
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
    MinorCPU* cpu;
    
    // id in vector port
    int idx;
  
    // packet to be received after clk edge
    PacketPtr recvPkt_d;
    EventFunctionWrapper recvEvent;
    EventFunctionWrapper wakeupCPUEvent;
    void process();
    
    // store the most recently received packet (in a single register)
    PacketPtr recvPkt;
    
    // the packet can't be used until the cycle after its received
    // but can't communicate this with 1 cycle delay event b/c have
    // to send at the end of the previous clock cycle to be available
    // to every event on the next cycle
    uint64_t cyclePktRecv;
    
    // setter, if there is an unused packet already present
    // we should drop and inform of the lost packet
    void setPacket(PacketPtr pkt);
    
    // whether this port is rdy to recv from the mesh net
    bool rdy;
    
    // this should go high when the core is rdy
    SensitiveStage active;
    
    //std::queue<PacketPtr> _pktQueue;
    
    VectorForward *vec;
};


#endif
