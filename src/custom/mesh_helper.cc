#include <cassert>

#include "arch/registers.hh"
#include "custom/mesh_helper.hh"
#include "base/bitfield.hh"


// need this to get reg names
using namespace RiscvISA;

bool
MeshHelper::csrToOutSrcs(uint64_t csrVal, Mesh_Dir dir, Mesh_Out_Src &src) {
  if (dir == UP) {
    return rangeToMeshOutSrc(csrVal, UP_HI, UP_LO, src);
  }
  else if (dir == RIGHT) {
    return rangeToMeshOutSrc(csrVal, RIGHT_HI, RIGHT_LO, src);
  }
  else if (dir == DOWN) {
    return rangeToMeshOutSrc(csrVal, DOWN_HI, DOWN_LO, src);
  }
  else if (dir == LEFT) {
    return rangeToMeshOutSrc(csrVal, LEFT_HI, LEFT_LO, src);
  }
  else {
    assert(0);
  }
}


bool
MeshHelper::csrToOp(uint64_t csrVal, int opIdx, Mesh_Dir &dir) {
  assert(opIdx < 2);
  if (opIdx == 0) {
    return rangeToMeshDir(csrVal, OP1_HI, OP1_LO, dir);
  }
  else {
    return rangeToMeshDir(csrVal, OP2_HI, OP2_LO, dir); 
  }
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

/*int
MeshHelper::csrSrcSends(uint64_t csrVal, Mesh_Out_Src src) {
  for (int i = 0; i < Mesh_
}*/

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
MeshHelper::rangeToMeshOutSrc(uint64_t csrVal, int hi, int lo, Mesh_Out_Src &src) {
  uint64_t ret = bits(csrVal, hi, lo);
  if (ret == 0) return false;
  else {
    ret--;
    assert(ret < NUM_DIR);
    src = (Mesh_Out_Src)ret;
    return true;
  }
}

bool
MeshHelper::isCSRDefault(uint64_t csrVal) {
  return (csrVal == 0);
}
