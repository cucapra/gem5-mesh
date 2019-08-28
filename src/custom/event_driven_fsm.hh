#ifndef __CUSTOM_EVENT_DRIVEN_FSM_HH__
#define __CUSTOM_EVENT_DRIVEN_FSM_HH__

// Moore fsm that updates on the next cycle or when a packet comes in
// "event triggered"

#include "mem/packet.hh"
#include "custom/mesh_helper.hh"
#include <string>

class MinorCPU;
class VectorForward;

class EventDrivenFSM {
  public:
    typedef struct Outputs_t {
      bool rdy;
      bool val;
      
      Outputs_t(bool rdy, bool val)
        : rdy(rdy), val(val)
      {}
    } Outputs_t;
  
  public:
    // constructor
    EventDrivenFSM(VectorForward *vec, MinorCPU *cpu, SensitiveStage stage);
  
    // sensitivity list
    void neighborEvent();
    void configEvent();
    void stallEvent();
    
    // check if can do stuff
    bool isMeshActive();
    
    // update to the next state, ret whether new state
    bool tick();
    
    // get the current output of the moore state machine
    // TICK should have been called before accessing
    Outputs_t stateOutput();
    
  private:
    typedef enum State {
      // not participating in mesh
      IDLE,
      // starting to run, self val next cycle, rdy this cycle
      BEGIN_SEND,
      // actively running, val and rdy
      RUNNING_BIND,
      // waiting on all neighbors
      WAIT_MESH_VALRDY,
      // waiting on neighbors to be val, neighbors are rdy
      WAIT_MESH_VAL,
      // waiting on neighbors to be rdy, neighbors are val
      WAIT_MESH_RDY,
      // we are beginning stall this cycle, b/c active neighbor went !rdy
      // but we still sent a packet last cycle so need to assert val
      BEGIN_STALL_VAL,
      // we stalled due to the internal core pipeline, next packet won't be val
      STALL_NOT_VAL
    } State;
    
  private:
    // make sure that the next state was produced from inputs in the last
    // cycle
    //typedef struct NextState_t {
    //  State nextState;
    //  Cycle cycleUpdated;
    //} NextState_t;
    
    // next state updates
    //State getNextState();
    //void setNextState(State state);
    void stateTransition();
    
    // update the next state outputs after all of the states have consumed
    // the current input
    void stateOutputTransition();
    
    // update the pending next state
    void sensitiveUpdate();
    
    // try to schedule an update for the end of cycle
    void tryScheduleUpdate();
    
    // pool the status of the mesh net
    bool getInVal();
    bool getOutRdy();
    bool getConfigured();
    bool getInternalStall();
    
    // find the next state for the given inptus statemachine
    State pendingNextState();
    State meshState(State onValRdy, bool inVal, bool outRdy);
    
    // debug helper
    std::string stateToStr(State state);
    
  private:
    // pointer to mesh unit this fsm is apart of
    VectorForward *_vec;
  
    // reference to cpu this fsm is apart of
    MinorCPU *_cpu;
    
    // state of fsm
    State _state;
    // remember debug for the old state
    State _oldState;
    // there was an update since the last time we ticked, 
    // so the output might be diff
    bool _didTransition;
    
    // double buffer the next state for weird timing issues
    //int _nextStateIdx;
    //NextState_t _nextState[2];
    
    // stage this fsm is associated with
    SensitiveStage _stage;
    
    // update the state at the end of the current cycle, in order to
    // 1) Use up-to-date inputs from the current cycle to inform next state
    // 2) Have the state ready and consistent for everybody in the next cycle
    // This only works if the fsm is a Moore machine
    EventFunctionWrapper _stateUpdateEvent;
    EventFunctionWrapper _outputUpdateEvent;
    
    //Inputs_t _inputs;
    
    
};






#endif
