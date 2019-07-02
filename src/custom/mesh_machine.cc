#include "custom/mesh_machine.hh"
#include "cpu/simple/timing.hh"


void
MeshMachine::TickEvent::schedule(MeshStateInputs in_, Tick t)
{
    inputs = in_;
    
    // schedule process
    cpu->schedule(this, t); 
}

void
MeshMachine::TickEvent::process() {
  // update that state
  machine->stateTransition();
}

/*void
MeshMachine::update(StateInputs inputs) {
  // transition on clock edge
  stateTransition();
  
  // get next state and do any state output stuff
  computeNextState(inputs);
  computeStateOutput(inputs);
}*/

void
MeshMachine::stateTransition() {
  state = nextState;
}

void
MeshMachine::computeNextState(MeshStateInputs inputs) {
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

MeshMachine::MeshStateOutputs
MeshMachine::computeStateOutput(MeshStateInputs inputs) {
  MeshStateOutputs out;
  
  switch(state) {
    case Not_Sending:
      // if we have a pkt stored we can't accept another one
      if (inputs.storedPkt) {
        out.selfVal = true;
        out.selfRdy = false;
      }
      // if we don't have a packet we can accept one
      else {
        out.selfVal = false;
        out.selfRdy = true;
      }
      break;
    case Sending:
      out.selfVal = true;
      out.selfRdy = true;
      break;
    default:
      out.selfVal = false;
      out.selfRdy = false;
      break;
  }
  
  return out;
}

MeshMachine::MeshStateOutputs
MeshMachine::updateMachine(MeshStateInputs inputs) {
  
  // remove the old state transition b/c inputs this cycle changed again
  if (machineTick.scheduled()) {
    machineTick.squash();
  }
  
  // get next state and do any state output stuff
  computeNextState(inputs);
  MeshStateOutputs out = computeStateOutput(inputs);
  
  // schedule state to update based on these inputs in the next cycle
  machineTick.schedule(inputs, cpu->clockEdge());
  
  return out;
}
