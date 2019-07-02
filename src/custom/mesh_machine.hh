#ifndef __CUSTOM_MESH_MACHINE_HH__
#define __CUSTOM_MESH_MACHINE_HH__

/*
 * State machine for sending mesh packets and determining val/rdy signal
 * 
 * Should update on every clock edge
 * tickEvent.schedule(pkt, cpu->clockEdge());
 */ 

#include "sim/clocked_object.hh"

// forward declare TimingSimpleCPU so we can circular define
class TimingSimpleCPU;

class MeshMachine {
  public:
    struct StateInputs {
      bool outRdy;
      bool inVal;
      bool storedPkt;
      
      StateInputs(bool outRdy, bool inVal, bool storedPkt) : 
        outRdy(outRdy), inVal(inVal), storedPkt(storedPkt) {}
    };
  
    void setInputs(bool outRdy, bool inVal, bool storedPkt);
    
    void update(StateInputs inputs);
  protected:
    TimingSimpleCPU *cpu;
    
    struct TickEvent : public Event
    {
      StateInputs inputs;
      
      TimingSimpleCPU *cpu;
      MeshMachine *machine;

      TickEvent(TimingSimpleCPU *_cpu, MeshMachine *_mach) : inputs(StateInputs(0, 0, 0)), cpu(_cpu), machine(_mach) {}
      const char *description() const { return "Timing CPU tick"; }
      void schedule(StateInputs in_, Tick t);
      
      // overriden
      void process();
    };
    
    TickEvent machineTick;
    
    enum State {
      Not_Sending = 0,
      Sending
    };
    
    State state;
    State nextState;
    
    void stateTransition();
    
    void computeNextState(StateInputs inputs);
    
    void computeStateOutput(StateInputs inputs);
  
  
  public:
    MeshMachine(TimingSimpleCPU *_cpu)
            : cpu(_cpu), machineTick(_cpu, this),
            state(Not_Sending), nextState(Not_Sending)
        { }
  
  
  
  
};

#endif
