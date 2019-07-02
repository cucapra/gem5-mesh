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
    struct MeshStateInputs {
      bool outRdy;
      bool inVal;
      bool storedPkt;
      
      MeshStateInputs(bool outRdy, bool inVal, bool storedPkt) : 
        outRdy(outRdy), inVal(inVal), storedPkt(storedPkt) {}
    };
  
    struct MeshStateOutputs {
      bool selfRdy;
      bool selfVal;
      
      MeshStateOutputs() :
        selfRdy(0), selfVal(0) {}
      
      MeshStateOutputs(bool selfRdy, bool selfVal) :
        selfRdy(selfRdy), selfVal(selfVal) {}
      
    };
    
    
    // trigger an update of current output and next state
    // also schedule a state update event on the next clk edge
    // ret is the current output
    MeshStateOutputs updateMachine(MeshStateInputs inputs);
    
  protected:
    
  
    TimingSimpleCPU *cpu;
    
    struct TickEvent : public Event
    {
      MeshStateInputs inputs;
      
      TimingSimpleCPU *cpu;
      MeshMachine *machine;

      TickEvent(TimingSimpleCPU *_cpu, MeshMachine *_mach) : inputs(MeshStateInputs(0, 0, 0)), cpu(_cpu), machine(_mach) {}
      const char *description() const { return "Timing CPU tick"; }
      void schedule(MeshStateInputs in_, Tick t);
      
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
    
    void computeNextState(MeshStateInputs inputs);
    
    MeshStateOutputs computeStateOutput(MeshStateInputs inputs);
  
  
  public:
    MeshMachine(TimingSimpleCPU *_cpu)
            : cpu(_cpu), machineTick(_cpu, this),
            state(Not_Sending), nextState(Not_Sending)
        { }
  
  
  
  
};

#endif
