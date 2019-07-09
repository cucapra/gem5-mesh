#ifndef __CUSTOM_MESH_PORTS_HH__
#define __CUSTOM_MESH_PORTS_HH__

/*
 * Mesh ports used in TimingSimpleCPU
 */ 


#include "mem/port.hh"
#include "mem/packet.hh"
#include "sim/clocked_object.hh"

//resolve circular deps
class TimingSimpleCPU;

// pbb implement mesh ports
class TimingCPUMasterPort : public MasterPort
{
  public:

    TimingCPUMasterPort(const std::string& _name, int idx, TimingSimpleCPU* _cpu);
       /* : MasterPort(_name, _cpu), idx(idx), cpu(_cpu),
          retryRespEvent([this]{ sendRetryResp(); }, name())
    { }*/

  protected:
  
    int idx;

    TimingSimpleCPU* cpu;

    struct TickEvent : public Event
    {
        PacketPtr pkt;
        TimingSimpleCPU *cpu;

        TickEvent(TimingSimpleCPU *_cpu) : pkt(NULL), cpu(_cpu) {}
        const char *description() const { return "Timing CPU tick"; }
        void schedule(PacketPtr _pkt, Tick t);
    };

    EventFunctionWrapper retryRespEvent;
};


// port that goes out over the mesh
class ToMeshPort : public TimingCPUMasterPort {
  public:
    ToMeshPort(TimingSimpleCPU *_cpu, int idx);
        /*: TimingCPUMasterPort(
          _cpu->name() + ".mesh_out_port" + csprintf("[%d]", idx), 
          idx, _cpu), tickEvent(_cpu), val(false), active(false), 
          stalledPkt(nullptr)
    { }*/

    void setVal(bool val);
    bool getVal() const { return val; }
    
    bool getPairRdy();
    
    void setActive (bool active) { this->active = active; }
    bool getActive() const { return active; }
    
    void setValIfActive() { if (active) setVal(true); }
    
    // check if this port is rdy and the slave port is valid
    bool checkHandshake();
    
    // call tryUnblock() in cpu attach to slave port of this
    void tryUnblockNeighbor();
    
    // buffer pkt and inform we're not ready
    void failToSend(PacketPtr pkt);
    
    // release the pkt and inform we are now ready
    void beginToSend();

  protected:

    // important to implement/override these from MasterPort class
    virtual bool recvTimingResp(PacketPtr pkt);
    virtual void recvReqRetry();

    // the event to put on the event queue when a resp is received
    struct MOTickEvent : public TickEvent
    {

        MOTickEvent(TimingSimpleCPU *_cpu)
            : TickEvent(_cpu) {}
        void process();
        const char *description() const { return "Timing CPU to mesh tick"; }
    };

    MOTickEvent tickEvent;
    
    // whether this signal is valid over the mesh net
    bool val;
    
    // whether this port is used and should assert val when packet 
    // available
    bool active;
    
    // packet stored in register that can't be sent yet due to val/rdy
    // requires additional enable reg and 2-1 mux
    PacketPtr stalledPkt;
    
};

class FromMeshPort;

// similar purpose as TimingCPUPort (derived from master port)
class TimingCPUSlavePort : public SlavePort {
  public:

    TimingCPUSlavePort(const std::string& _name, int idx, TimingSimpleCPU* _cpu);
        /*: SlavePort(_name, _cpu), idx(idx), cpu(_cpu)
         //, retryRespEvent([this]{ sendRetryResp(); }, name())
    { }*/

  protected:

    // id in vector port
    int idx;

    // cpu reference, so we can do things in it on response
    TimingSimpleCPU* cpu;

    // base for delayed events like processing a request
    struct TickEvent : public Event
    {
        PacketPtr pkt;
        TimingSimpleCPU *cpu;

        TickEvent(TimingSimpleCPU *_cpu) : pkt(NULL), cpu(_cpu) {}
        const char *description() const { return "Timing CPU slave tick"; }
        void schedule(PacketPtr _pkt, Tick t);
    };

    // potentially want to schedule a retry event in the case that
    // two things arrive? in the same cycle ??
    // not sure if would also sched retry in the slave?
    //EventFunctionWrapper retryRespEvent;
};

class FromMeshPort : public TimingCPUSlavePort {
  public:
    FromMeshPort(TimingSimpleCPU *_cpu, int idx);
        /*: TimingCPUSlavePort(
          _cpu->name() + ".mesh_in_port" + csprintf("[%d]", idx), 
          idx, _cpu), tickEvent(_cpu), rdy(false), active(false)
    { }*/

    virtual AddrRangeList getAddrRanges() const;

    // get the packet from the port
    uint64_t getPacketData();

    void setRdy(bool val);
    bool getRdy();
    
    bool getPairVal();
    
    void setActive(bool active) { this->active = active; }
    bool getActive() const { return active; }
    
    void setRdyIfActive() { if (active) setRdy(true); }
    
    // check val rdy interface
    bool checkHandshake();
    
    // call tryUnblock in the cpu
    void tryUnblockCPU();
    
    bool pktExists();
    
  protected:

    // important to implement/override these from slave port
    virtual bool recvTimingReq(PacketPtr pkt);
    virtual void recvRespRetry();
    virtual Tick recvAtomic(PacketPtr pkt) { panic("recvAtomic unimpl"); };
    virtual void recvFunctional(PacketPtr pkt);

  /*
    // the event to put on the event queue when a resp is received
    struct MITickEvent : public TickEvent
    {
      // for some reason the port is changing its base addr after
      // being initialized, so this pointer is instantly bad
        FromMeshPort *port;

        MITickEvent(TimingSimpleCPU *_cpu)
            : TickEvent(_cpu) {}
        void process() override;
        const char *description() const { return "Mesh to timing CPU tick"; }
    };

    MITickEvent tickEvent;
  */
  
  // packet to be received after clk edge
    PacketPtr recvPkt_d;
    EventFunctionWrapper recvEvent;
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
    bool active;
    
    //std::queue<PacketPtr> _pktQueue;
};


#endif
