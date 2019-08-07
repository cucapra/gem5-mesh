#ifndef __CUSTOM_EVENT_DRIVEN_FSM_HH__
#define __CUSTOM_EVENT_DRIVEN_FSM_HH__

// fsm that updates on the next cycle or when a packet comes in
// "event triggered"

#include "mem/packet.hh"
#include "custom/mesh_helper.hh"
#include <string>

// or just call functions in the cpu
/*typedef struct FSM_Out_t {
  bool setRdy;
  bool setVal;
  bool resetRdy;
  bool resetVal;
} FSM_Out_t;*/

class TimingSimpleCPU;

class EventDrivenFSM {
  public:
    void tickEvent();
    //void pktEvent(PacketPtr pkt);
    
    // update val or rdy from neighbors
    void neighborEvent();
    
    
    
    EventDrivenFSM(TimingSimpleCPU *cpu, SensitiveStage stage);
  
  private:
    typedef enum State {
      IDLE,
      /*RUNNING_UNBD,
      RUNNING_BIND,
      WAIT_INST_MEM_UNBD,
      WAIT_DATA_MEM_UNBD,
      WAIT_INST_MEM_BIND,
      WAIT_DATA_MEM_BIND,*/
      RUNNING_BIND,
      WAIT_ALL,
      WAIT_VAL,
      WAIT_RDY
    } State;
    
    // update statemachine, if pkt != null, then exists and can use
    State updateState();
    void stateOutput();
    
    bool isWaitState(State state);
    
    bool getInVal();
    bool getOutRdy();
    bool getConfigured();
    void setRdy(bool rdy);
    void setVal(bool val);
    void startLink();
    std::string stateToStr(State state);
    
    
  private:
    // reference to cpu this fsm is apart of
    TimingSimpleCPU *_cpu;
    
    // state of fsm
    State _state;
    State _nextState;
    
    // stage this fsm is associated with
    SensitiveStage _stage;
    
    // tick of the fsm
    EventFunctionWrapper _tickEvent;
    
    
};






#endif
