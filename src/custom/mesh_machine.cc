#include "custom/mesh_machine.hh"
#include "cpu/simple/timing.hh"


void
MeshMachine::TickEvent::schedule(StateInputs in_, Tick t)
{
    inputs = in_;
    
    // schedule process
    cpu->schedule(this, t); 
}

void
MeshMachine::TickEvent::process() {
  machine->update(inputs);
}

void
MeshMachine::update(StateInputs inputs) {
  // transition on clock edge
  stateTransition();
  
  // get next state and do any state output stuff
  computeNextState(inputs);
  computeStateOutput(inputs);
}

void
MeshMachine::stateTransition() {
  state = nextState;
}

void
MeshMachine::computeNextState(StateInputs inputs) {
  switch(state) {
    case Not_Sending:
      if (inputs.outRdy && inputs.inVal) {
        nextState = Sending;
      }
      else {
        nextState = Not_Sending;
      }
      break;
    case Sending:
      if (!inputs.outRdy || !inputs.inVal) {
        nextState = Not_Sending;
      }
      else {
        nextState = Sending;
      }
      break;
    default:
      nextState = Not_Sending;
      break;
  }
}

void
MeshMachine::computeStateOutput(StateInputs inputs) {
  switch(state) {
    case Not_Sending:
      // if we have a pkt stored we can't accept another one
      if (inputs.storedPkt) {
        cpu->setVal();
        cpu->resetRdy();
      }
      // if we don't have a packet we can accept one
      else {
        cpu->resetVal();
        cpu->setRdy();
      }
      break;
    case Sending:
      cpu->setVal();
      cpu->setRdy();
      break;
    default:
      cpu->resetVal();
      cpu->resetRdy();
      break;
  }
}

void
MeshMachine::setInputs(bool outRdy, bool inVal, bool storedPkt) {
  StateInputs inputs = StateInputs(outRdy, inVal, storedPkt);
  
  if (machineTick.scheduled()) {
    machineTick.squash();
  }
  
  machineTick.schedule(inputs, cpu->clockEdge());
}
