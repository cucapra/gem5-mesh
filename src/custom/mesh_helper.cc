#include <cassert>

#include "arch/registers.hh"
#include "custom/mesh_helper.hh"
#include "base/bitfield.hh"


#define RD_HI 2
#define RD_LO 0

#define OP1_HI 5
#define OP1_LO 3

#define OP2_HI 8
#define OP2_LO 6

// need this to get reg names
using namespace RiscvISA;

bool
MeshHelper::csrToRd(uint64_t csrVal, Mesh_Dir &dir) {
  return rangeToMeshDir(csrVal, RD_HI, RD_LO, dir);
}

bool
MeshHelper::csrToOp1(uint64_t csrVal, Mesh_Dir &dir) {
  return rangeToMeshDir(csrVal, OP1_HI, OP1_LO, dir);
}

bool
MeshHelper::csrToOp2(uint64_t csrVal, Mesh_Dir &dir) {
  return rangeToMeshDir(csrVal, OP2_HI, OP2_LO, dir);
}

bool
MeshHelper::isBindCSR(int csrIdx) {
  if (csrIdx == MISCREG_EXE) {
    return true;
  }
  else {
    return false;
  }
}

std::vector<int>
MeshHelper::getCSRCodes() {
  std::vector<int> csrs = { MISCREG_EXE };
  return csrs;
}

bool
MeshHelper::rangeToMeshDir(uint64_t csrVal, int hi, int lo, Mesh_Dir &dir) {
  uint64_t ret = bits(csrVal, hi, lo);
  if (ret == 0) return false;
  else {
    ret--;
    assert(ret < NUM_DIR);
    dir = (Mesh_Dir)ret;
    return true;
  }
}

bool
MeshHelper::isCSRDefault(uint64_t csrVal) {
  return (csrVal == 0);
}
