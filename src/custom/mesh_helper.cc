#include <cassert>

#include "custom/mesh_helper.hh"

Mesh_Dir
MeshHelper::csrToMeshDir(uint64_t csrVal) {
  // 0 is invalid
  assert(csrVal != 0);
  
  int val = (int)csrVal;
  val -= 1;
  
  // only 4 directions!
  assert(val < NUM_DIR);
  
  return (Mesh_Dir)val;
}

bool
MeshHelper::isCSRDefault(uint64_t csrVal) {
  return (csrVal == 0);
}
