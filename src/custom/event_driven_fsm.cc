#include "custom/event_driven_fsm.hh"
#include "cpu/simple/timing.hh"
#include "debug/Mesh.hh"

EventDrivenFSM::EventDrivenFSM(TimingSimpleCPU *cpu, SensitiveStage stage) :
  _cpu(cpu), _state(IDLE), _nextState(IDLE), _stage(stage), _tickEvent([this] { tickEvent(); }, cpu->name())
{}


// as soon as a bind takes place, this FSM is in control
// of the execution flow, i.e. it does the ticks and calls functions?

// or when stalled due to mesh this state machine takes over execution


/*----------------------------------------------------------------------
 * This state machine is sensitive to the following events
 *--------------------------------------------------------------------*/
void
EventDrivenFSM::neighborValRdy() {
  tickEvent();
}

void
EventDrivenFSM::instResp() {
  tickEvent();
}

void
EventDrivenFSM::dataResp() {
  tickEvent();
}

void
EventDrivenFSM::tickEvent() {
  bool updateOut = (_nextState != _state);
  
  _state = _nextState;
  
  if (updateOut) {
    // do any action related to the new state
    stateOutput();
  }
  
  // try to update the state
  _nextState = updateState();
  
  DPRINTF(Mesh, "state %s -> %s\n", stateToStr(_state), stateToStr(_nextState));
  
  // do update if there will be a new state
  if (_state != _nextState) {
    
    // if already schedule, reschedule
    if (_tickEvent.scheduled()) {
      _cpu->reschedule(_tickEvent, _cpu->clockEdge(Cycles(1)));
    }
    else {
      _cpu->schedule(_tickEvent, _cpu->clockEdge(Cycles(1)));
    }
  }
}


void
EventDrivenFSM::stateOutput() {
  bool inVal = getInVal();
  bool outRdy = getOutRdy();
  
  switch(_state) {
    case IDLE: {
      setRdy(false);
      setVal(false);
      break;
    }
    case RUNNING_UNBD: {
      
      break;
    }
    case RUNNING_BIND: {
      setRdy(true);
      setVal(true);
      // need to have sent a packet on the last cycle!
      break;
    }
    case WAIT_MESH: {
      setRdy(false);
      setVal(false);
      
      // initiate and start sending on the next cycle
      if (inVal && outRdy) {
        startLink();
      }
      
      break;
    }
    case WAIT_RDY: {
      setRdy(false);
      setVal(false);
      
      if (inVal && outRdy) {
        startLink();
      }
      
      break;
    }
    case WAIT_VAL: {
      setRdy(true);
      setVal(false);
      
      if (inVal && outRdy) {
        startLink();
      }
    }
  }
  
  // inform neighbors about any state change here?
  _cpu->informNeighbors();
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
  
  switch(_state) {
    case IDLE: {
      
      if (configured) return WAIT_ALL;
      else return IDLE;
      
      break;
    }
    default: {
      if (!configured) return IDLE;
      else if (inVal && outRdy) return SENDING;
      else if (inVal) return WAIT_RDY;
      else if (outRdy) return WAIT_VAL;
      else return WAIT_ALL;
      
      break;
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
  return _cpu->getNumPortsActive() > 0;
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
  return (
    (state == WAIT_ALL) ||
    (state == WAIT_VAL) ||
    (state == WAIT_RDY)
  );
}

std::string
EventDrivenFSM::stateToStr(State state) {
  switch(state) {
    case IDLE: {
      return "IDLE";
    }
    case WAIT_ALL: {
      return "WAIT_ALL";
    }
    case WAIT_RDY: {
      return "WAIT_RDY";
    }
    case WAIT_VAL: {
      return "WAIT_VAL";
    }
    case SENDING: {
      return "SENDING";
    }
    default: {
      return "NONE";
    }
  }
}






