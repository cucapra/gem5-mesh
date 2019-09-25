#include "custom/event_driven_fsm.hh"
#include "cpu/minor/cpu.hh"
#include "debug/Mesh.hh"
#include "custom/vector_forward.hh"

EventDrivenFSM::EventDrivenFSM(VectorForward *vec, MinorCPU *cpu, SensitiveStage stage) :
  _vec(vec), _cpu(cpu), _state(IDLE), _oldState(IDLE), _didTransition(false),
  _stage(stage), _stateUpdateEvent([this] { stateTransition(); }, cpu->name()),
  _outputUpdateEvent([this] { stateOutputTransition(); }, cpu->name())
{}


// as soon as a bind takes place, this FSM is in control
// of the execution flow, i.e. it does the ticks and calls functions?

// or when stalled due to mesh this state machine takes over execution


/*----------------------------------------------------------------------
 * This state machine is sensitive to the following events
 * 
 * 1) A new bind/config
 * 2) Neighbor val/rdy updates
 * 3) Inst + data req
 * 4) Inst + data resp
 *--------------------------------------------------------------------*/
void
EventDrivenFSM::neighborEvent() {
  //DPRINTF(Mesh, "%s neighbor update\n", _cpu->name());
  
  sensitiveUpdate();
}

void
EventDrivenFSM::configEvent() {
  //DPRINTF(Mesh, "%s config update\n", _cpu->name());
  
  sensitiveUpdate();
}

void
EventDrivenFSM::stallEvent() {
  //DPRINTF(Mesh, "%s stall update\n", _cpu->name());
  
  sensitiveUpdate();
}

/*----------------------------------------------------------------------
 * 
 *--------------------------------------------------------------------*/


// TODO do we even need a state machine or can just check val rdy before sending?
bool
EventDrivenFSM::isMeshActive() {
  /*if (getConfigured()) {
    DPRINTF(Mesh, "%s in %d out %d\n", _cpu->name(), getInVal(), getOutRdy());
  }*/
  return 
    //((getInVal() && getOutRdy()) && getConfigured())
    (_state == BEGIN_SEND || _state == RUNNING_BIND) && getInVal() && getOutRdy();
    ;
}

// allowed to be ticked multiple times per cycle
// does not modify the current state, but the potential next state
void
EventDrivenFSM::sensitiveUpdate() {
  // try to update the next state
  //_nextState = updateState();
  //setNextState(pendingNextState());
  
  tryScheduleUpdate();
}

// TODO needs to transition at the end of this cycle so ready for any
// early event on the next cycle


// ticked once per cycle, do state update
// state outputs only change once per cycle --> moore machine
bool
EventDrivenFSM::tick() {
  //bool updateOut = (_nextState != _state);
  
  // 
  // _state = _nextState;

  //_state = _nextState;
  
  //_inputs.clear();
  
  // stateOutput();

  // try to update the state
  //_nextState = updateState();

  // handle the state transition
  //stateTransition();

  // if there was a state update, inform caller
  // it will likely want to inform neighbors that something happened
  if (_didTransition) {
    //DPRINTF(Mesh, "%s %d on clk edge state %s -> %s\n", _cpu->name(), _stage, stateToStr(_oldState), stateToStr(_state));
    
    _didTransition = false;
    
    // if there is a state update, going to want to inform neighbors
    return true;
  }
  else {
    return false;
  }
  
  
  //DPRINTF(Mesh, "state %s -> %s\n", stateToStr(_state), stateToStr(_nextState));
  tryScheduleUpdate();
  
}


void 
EventDrivenFSM::tryScheduleUpdate() {
  // do update if there will be a new state
  /*if (_state != pendingNextState()) {
    if (!_tickEvent.scheduled()) {
      _cpu->schedule(_tickEvent, _cpu->clockEdge());
    }
  }*/
  
  // more efficient to reorder branches b/c don't want to waste lookup work?
  if (!_stateUpdateEvent.scheduled()) {
    if (_state != pendingNextState()) {
      _cpu->schedule(_stateUpdateEvent, _cpu->clockEdge(Cycles(1)));
      
      // if we're going to change state on the next cycle we want the cpu
      // to try to tick as it may unstall
      //_cpu->activityRecorder->activity();
    }
  }
}


EventDrivenFSM::Outputs_t
EventDrivenFSM::stateOutput() {
  //bool inVal = getInVal();
  //bool outRdy = getOutRdy();
  //DPRINTF(Mesh, "%s state output\n", _cpu->name());
  switch(_state) {
    case IDLE: {
      return Outputs_t(false, false);
    }
    case BEGIN_SEND: {
      return Outputs_t(true, false);
    }
    case RUNNING_BIND: {
      return Outputs_t(true, true);
    }
    case WAIT_MESH_VALRDY: {
      return Outputs_t(false, false);
    }
    case WAIT_MESH_RDY: {
      return Outputs_t(false, false);
    }
    case WAIT_MESH_VAL: {
      return Outputs_t(true, false);
    }
    case BEGIN_STALL_VAL: {
      return Outputs_t(false, true);
    }
    case STALL_NOT_VAL: {
      return Outputs_t(false, false);
    }
    default: {
      return Outputs_t(false, false);
    }
  }
  
  // inform neighbors about any state change here?
  //_vec->informNeighbors();
}

EventDrivenFSM::State
EventDrivenFSM::meshState(State onValRdy, bool inVal, bool outRdy) {
  //if (_stage == EXECUTE && _inputs.dataReq == true) return WAIT_DATA_RESP;
  //else if (_stage == FETCH && _inputs.instReq == true) return WAIT_INST_RESP;
  if (inVal && outRdy) return onValRdy;
  else if (inVal) return WAIT_MESH_RDY;
  else if (outRdy) return WAIT_MESH_VAL;
  else return WAIT_MESH_VALRDY;
}

EventDrivenFSM::State
EventDrivenFSM::pendingNextState() {
  
  bool inVal = getInVal();
  bool outRdy = getOutRdy();
  bool configured = getConfigured();
  bool stalled = getInternalStall();
  bool sentMsg = _vec->sentMsgThisCycle();
 
  DPRINTF(Mesh, "%s find pending change: currstate %s inval %d, outrdy %d, config %d, stalled %d, sentmsg %d\n",
     _cpu->name(), stateToStr(_state), inVal, outRdy, configured, stalled, sentMsg); 

 
  // fully connected state machine?
  /*if (inVal && outRdy) return SENDING;
  else if (inVal) return WAIT_RDY;
  else if (outRdy) return WAIT_VAL;
  else return WAIT_ALL;*/
  
  if (!configured) return IDLE;
  
  switch(_state) {
    case IDLE: {
      if (configured) return WAIT_MESH_VALRDY;
      else return IDLE;
    }
    case BEGIN_SEND: {
      if (stalled) return BEGIN_STALL_VAL;
      else if (inVal && outRdy) return RUNNING_BIND;
      else if (sentMsg) return BEGIN_STALL_VAL;
      else return STALL_NOT_VAL;
    }
    case RUNNING_BIND: {
      if (stalled) return BEGIN_STALL_VAL;
      else if (inVal && outRdy) return RUNNING_BIND;
      // if you didn't send a packet this cycle you should just stall not valid
      else if (sentMsg) return BEGIN_STALL_VAL;
      else return STALL_NOT_VAL;
    }
    case BEGIN_STALL_VAL: {
      if (stalled) return STALL_NOT_VAL;
      else return meshState(BEGIN_SEND, inVal, outRdy);
    }
    /*case STALL_NOT_VAL: {
      
    }*/
    default: {
      if (stalled) return STALL_NOT_VAL;
      else return meshState(BEGIN_SEND, inVal, outRdy);
    }
  }
  
}

/*State
EventDrivenFSM::getNextState() {
  return _nextState[_nextStateIdx];
}

void
EventDrivenFSM::setNextState(State state) {
  // if this update is on a later cycle dont update the current buffer
  if (_nextState[_nextStateIdx].cycleUpdated < curTick()) {
    
  }
  else {
    _nextState[_nextStateIdx].nextState = state;
  }
}*/

void
EventDrivenFSM::stateTransition() {
  _didTransition = true;
  _oldState = _state;
  _state = pendingNextState();
  
  // schedule the state to update after this transition
  _cpu->schedule(_outputUpdateEvent, _cpu->clockEdge());
  
  DPRINTF(Mesh, "%s %d on clk edge state %s -> %s\n", _cpu->name(), _stage, stateToStr(_oldState), stateToStr(_state));

  // if there was a state transition we should try to update on the next cycle as well
  tryScheduleUpdate();

}

void
EventDrivenFSM::stateOutputTransition() {
  Outputs_t newOutputs = stateOutput();
  _vec->setVal(newOutputs.val);
  _vec->setRdy(newOutputs.rdy);
  
  DPRINTF(Mesh, "%s update output val %d rdy %d\n", _cpu->name(), newOutputs.val, newOutputs.rdy);
}

bool
EventDrivenFSM::getInVal() {
  return _vec->getInVal();
}

bool
EventDrivenFSM::getOutRdy() {
  return _vec->getOutRdy();
}

bool
EventDrivenFSM::getInternalStall() {
  return _vec->isInternallyStalled();
}

bool
EventDrivenFSM::getConfigured() {
  //return _vec->getNumPortsActive() > 0;
  return _vec->getConfigured();
}

std::string
EventDrivenFSM::stateToStr(State state) {
  switch(state) {
    case IDLE: {
      return "IDLE";
    }
    case WAIT_MESH_VALRDY: {
      return "WAIT_MESH_VALRDY";
    }
    case WAIT_MESH_RDY: {
      return "WAIT_MESH_RDY";
    }
    case WAIT_MESH_VAL: {
      return "WAIT_MESH_VAL";
    }
    case RUNNING_BIND: {
      return "RUNNING_BIND";
    }
    case BEGIN_SEND: {
      return "BEGIN_SEND";
    }
    case BEGIN_STALL_VAL: {
      return "BEGIN_STALL_VAL";
    }
    case STALL_NOT_VAL: {
      return "STALL_NOT_VAL";
    }
    default: {
      return "NONE";
    }
  }
}






