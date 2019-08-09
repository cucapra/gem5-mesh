#include "custom/event_driven_fsm.hh"
#include "cpu/simple/timing.hh"
#include "debug/Mesh.hh"

EventDrivenFSM::EventDrivenFSM(TimingSimpleCPU *cpu, SensitiveStage stage) :
  _cpu(cpu), _state(IDLE), _nextState(IDLE), _stage(stage), 
  _tickEvent([this] { tickEvent(); }, cpu->name()), 
  _inputs(Inputs_t(false,false,false,false))
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
  
  update();
}

void
EventDrivenFSM::configEvent() {
  //DPRINTF(Mesh, "%s config update\n", _cpu->name());
  
  update();
}

void
EventDrivenFSM::instReq() {
  if (_state != IDLE) {
    DPRINTF(Mesh, "%s inst req\n", _cpu->name());
  }
  
  _inputs.instReq = true;
  update();
}

void
EventDrivenFSM::dataReq() {
  _inputs.dataReq = true;
  
  if (_state != IDLE) {
    DPRINTF(Mesh, "%s data req\n", _cpu->name());
  }
  
  update();
}

void
EventDrivenFSM::instResp() {
  if (_state != IDLE) {
    DPRINTF(Mesh, "%s inst resp\n", _cpu->name());
  }
  
  _inputs.instResp = true;
  update();
}

void
EventDrivenFSM::dataResp() {
  _inputs.dataResp = true;
  if (_state != IDLE) {
  DPRINTF(Mesh, "%s data resp\n", _cpu->name());
}
  update();
}

/*----------------------------------------------------------------------
 * 
 *--------------------------------------------------------------------*/
 
bool
EventDrivenFSM::isRunning() {
  return 
    (_state == RUNNING_BIND) || 
    (_state == BEGIN_SEND) ||
    //(_state == IDLE && _nextState == IDLE); // this line could problematic, b/c not setup for mealy machine!
    (!getConfigured());
}

// allowed to be ticked multiple times per cycle
// does not modify the current state, but the potential next state
void
EventDrivenFSM::update() {
  // try to update the state
  _nextState = updateState();
  
  // do update if there will be a new state
  if (_state != _nextState) {
    if (!_tickEvent.scheduled()) {
      _cpu->schedule(_tickEvent, _cpu->clockEdge(Cycles(1)));
    }
  }
}

// TODO needs to transition at the end of this cycle so ready for any
// early event on the next cycle


// ticked once per cycle, do state update
// state outputs only change once per cycle --> moore machine
void
EventDrivenFSM::tickEvent() {
  //bool updateOut = (_nextState != _state);
  
  if (_nextState != _state) {
    DPRINTF(Mesh, "%s %d state %s -> %s\n", _cpu->name(), _stage, stateToStr(_state), stateToStr(_nextState));
  }
  
  _state = _nextState;
  
  _inputs.clear();
  
  //if (updateOut) {
    // do any action related to the new state
    stateOutput();
  //}
  
  // try to update the state
  _nextState = updateState();

  
  
  
  //DPRINTF(Mesh, "state %s -> %s\n", stateToStr(_state), stateToStr(_nextState));
  
  // do update if there will be a new state
  if (_state != _nextState) {
    
    // if already schedule, reschedule
    if (!_tickEvent.scheduled()) {/*
      _cpu->reschedule(_tickEvent, _cpu->clockEdge(Cycles(1)));
    }
    else {*/
      _cpu->schedule(_tickEvent, _cpu->clockEdge(Cycles(1)));
    }
  }
}


void
EventDrivenFSM::stateOutput() {
  //bool inVal = getInVal();
  //bool outRdy = getOutRdy();
  //DPRINTF(Mesh, "%s state output\n", _cpu->name());
  switch(_state) {
    case IDLE: {
      setRdy(false);
      setVal(false);
      break;
    }
    case BEGIN_SEND: {
      setRdy(true);
      setVal(false);
      startLink();
      break;
    }
    case RUNNING_BIND: {
      setRdy(true);
      setVal(true);
      // need to have sent a packet on the last cycle!
      break;
    }
    case WAIT_MESH_VALRDY: {
      setRdy(false);
      setVal(false);
      
      // initiate and start sending on the next cycle
      /*if (inVal && outRdy) {
        startLink();
      }*/
      
      break;
    }
    case WAIT_MESH_RDY: {
      setRdy(false);
      setVal(false);
      
      /*if (inVal && outRdy) {
        startLink();
      }*/
      
      break;
    }
    case WAIT_MESH_VAL: {
      setRdy(true);
      setVal(false);
      break;
      /*if (inVal && outRdy) {
        startLink();
      }*/
    }
    default: {
      setRdy(false);
      setVal(false);
      break;
    }
  }
  
  // inform neighbors about any state change here?
  _cpu->informNeighbors();
}

EventDrivenFSM::State
EventDrivenFSM::meshState(State onValRdy, bool inVal, bool outRdy) {
  if (_stage == EXECUTE && _inputs.dataReq == true) return WAIT_DATA_RESP;
  else if (_stage == FETCH && _inputs.instReq == true) return WAIT_INST_RESP;
  else if (inVal && outRdy) return onValRdy;
  else if (inVal) return WAIT_MESH_RDY;
  else if (outRdy) return WAIT_MESH_VAL;
  else return WAIT_MESH_VALRDY;
}

EventDrivenFSM::State
EventDrivenFSM::updateState() {
  
  bool inVal = getInVal();
  bool outRdy = getOutRdy();
  bool configured = getConfigured();
  
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
      return meshState(RUNNING_BIND, inVal, outRdy);
    }
    case RUNNING_BIND: {
      return meshState(RUNNING_BIND, inVal, outRdy);
    }
    case WAIT_DATA_RESP: {
      if (_inputs.dataResp) {
        return meshState(BEGIN_SEND, inVal, outRdy);
      }
      else {
        return WAIT_DATA_RESP;
      }
    }
    // this one needs work, on resp tries the send, so shouldnt
    // do again here
    case WAIT_INST_RESP: {
      if (_inputs.instResp) {
        return meshState(WAIT_MESH_RDY, inVal, outRdy);
      }
      else {
        return WAIT_INST_RESP;
      }
    }
    default: {
      return meshState(BEGIN_SEND, inVal, outRdy);
    }
  }
  
}

void
EventDrivenFSM::startLink() {
  if (_stage == FETCH) {
    _cpu->tryFetch();
  }
  else if (_stage == EXECUTE) {
    _cpu->tryInstruction();
  }
}

bool
EventDrivenFSM::getInVal() {
  return _cpu->getInVal(_stage);
}

bool
EventDrivenFSM::getOutRdy() {
  return _cpu->getOutRdy(_stage);
}

bool
EventDrivenFSM::getConfigured() {
  return _cpu->getNumPortsActive(_stage) > 0;
}

// TODO keep these internally?
void
EventDrivenFSM::setRdy(bool rdy) {
  _cpu->setRdy(rdy, _stage);
}

void
EventDrivenFSM::setVal(bool val) {
  _cpu->setVal(val, _stage);
}

bool
EventDrivenFSM::isWaitState(State state) {
  return false;
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
    case WAIT_DATA_RESP: {
      return "WAIT_DATA_RESP";
    }
    case WAIT_INST_RESP: {
      return "WAIT_INST_RESP";
    }
    case RUNNING_BIND: {
      return "RUNNING_BIND";
    }
    case BEGIN_SEND: {
      return "BEGIN_SEND";
    }
    default: {
      return "NONE";
    }
  }
}






