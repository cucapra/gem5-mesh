#include "custom/config_counter.hh"
#include "cpu/simple/timing.hh"

#include "debug/Mesh.hh"

void
ConfigCounter::setCount(int count) {
  this->requiredCount = count;
}

// should prob schedule an event on the clockEdge, but can we get away with not?
void
ConfigCounter::incrCount() {
  this->currentCount++;
  
  if (currentCount > requiredCount) {
    DPRINTF(Mesh, "[[WARNING]] countdown is negative\n");
  }
}

bool
ConfigCounter::isComplete() {
  if (currentCount == requiredCount) return true;
  else return false;
}
