#include <cassert>

#include "arch/registers.hh"
#include "custom/mesh_helper.hh"
#include "base/bitfield.hh"


// need this to get reg names
using namespace RiscvISA;


static std::vector<int> csrs = { MISCREG_EXE, MISCREG_FETCH };

bool
MeshHelper::csrToOutSrcs(uint64_t csrVal, Mesh_Dir dir, Mesh_Out_Src &src) {
  if (dir == UP) {
    return rangeToMeshOutSrc(csrVal, EXE_UP_HI, EXE_UP_LO, src);
  }
  else if (dir == RIGHT) {
    return rangeToMeshOutSrc(csrVal, EXE_RIGHT_HI, EXE_RIGHT_LO, src);
  }
  else if (dir == DOWN) {
    return rangeToMeshOutSrc(csrVal, EXE_DOWN_HI, EXE_DOWN_LO, src);
  }
  else if (dir == LEFT) {
    return rangeToMeshOutSrc(csrVal, EXE_LEFT_HI, EXE_LEFT_LO, src);
  }
  else {
    assert(0);
  }
}


bool
MeshHelper::csrToOp(uint64_t csrVal, int opIdx, Mesh_Dir &dir) {
  assert(opIdx < 2);
  if (opIdx == 0) {
    return rangeToMeshDir(csrVal, EXE_OP1_HI, EXE_OP1_LO, dir);
  }
  else {
    return rangeToMeshDir(csrVal, EXE_OP2_HI, EXE_OP2_LO, dir); 
  }
}



bool
MeshHelper::isBindCSR(int csrIdx) {
  for (int i = 0; i < csrs.size(); i++) {
    if (csrs[i] == csrIdx) return true;
  }
  
  return false;
}

std::vector<int>
MeshHelper::getCSRCodes() {
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


// reproduce the isa file here, b/c not sure how to get the info from the
// weirdo isa language
/*
StaticInstPtr
MeshHelper::nameToStaticInst(Locked_Insts inst) {
  switch(inst) {
    case ADD:
      MachInst mach = 0b00000000000000000000000000110011;
      return new StaticInst("add", mach, IntALUOp);
    case MUL:
      MachInst mach = 0b00000010000000000000000000110011
      return new StaticInst("mul", mach, IntMultOp);
    case SUB:
      MachInst mach = 0b01000000000000000000000000110011
      return new StaticInst("sub", mach, IntALUOp);
    case LW:
      
  }
}
*/
