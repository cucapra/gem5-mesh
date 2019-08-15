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

class MinorCPU;
class VectorForward;

class EventDrivenFSM {
  public:
    
    //void pktEvent(PacketPtr pkt);
    
    // update val or rdy from neighbors
    void neighborEvent();
    void configEvent();
    void instReq();
    void instResp();
    void dataReq();
    void dataResp();
    
    bool isRunning();
    
    
    EventDrivenFSM(VectorForward *vec, MinorCPU *cpu, SensitiveStage stage);
  
  private:
    typedef enum State {
      IDLE,
      /*RUNNING_UNBD,
      RUNNING_BIND,
      WAIT_INST_MEM_UNBD,
      WAIT_DATA_MEM_UNBD,
      WAIT_INST_MEM_BIND,
      WAIT_DATA_MEM_BIND,*/
      BEGIN_SEND,
      RUNNING_BIND,
      WAIT_MESH_VALRDY,
      WAIT_MESH_VAL,
      WAIT_MESH_RDY,
      //WAIT_MESH_RDY_WITH_INST,
      //WAIT_ON_REQ
      WAIT_INST_RESP,
      WAIT_DATA_RESP
    } State;
    
    // sticky for reqs and resps?
    typedef struct Inputs_t {
      bool dataReq;
      bool instReq;
      bool dataResp;
      bool instResp;
      //bool meshVal;
      //bool meshRdy;
      
      Inputs_t(bool drq, bool irq, bool drp, bool irp) 
        : dataReq(drq), instReq(irq), dataResp(drp), instResp(irp)
      {}
      
      void clear() {
        dataReq = false;
        instReq = false;
        dataResp = false;
        instResp = false;
      }
      
    } Inputs_t;
    
    void tickEvent();
    // update statemachine, if pkt != null, then exists and can use
    State updateState();
    void stateOutput();
    void update();
    
    bool isWaitState(State state);
    
    
    bool getInVal();
    bool getOutRdy();
    bool getConfigured();
    void setRdy(bool rdy);
    void setVal(bool val);
    void startLink();
    std::string stateToStr(State state);
    State meshState(State onValRdy, bool inVal, bool outRdy);
    
    
  private:
    // pointer to mesh unit this fsm is apart of
    VectorForward *_vec;
  
    // reference to cpu this fsm is apart of
    MinorCPU *_cpu;
    
    // state of fsm
    State _state;
    State _nextState;
    
    // stage this fsm is associated with
    SensitiveStage _stage;
    
    // tick of the fsm
    EventFunctionWrapper _tickEvent;
    
    Inputs_t _inputs;
    
    
};






#endif
