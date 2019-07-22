#include <cassert>

#include "arch/registers.hh"
#include "custom/mesh_helper.hh"
#include "base/bitfield.hh"


// need this to get reg names
using namespace RiscvISA;

#define NUM_OPERANDS 2

static std::vector<int> csrs = { MISCREG_EXE, MISCREG_FETCH };

bool
MeshHelper::exeCsrToOutSrcs(uint64_t csrVal, Mesh_Dir dir, Mesh_Out_Src &src) {
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
MeshHelper::exeCsrToOp(uint64_t csrVal, int opIdx, Mesh_Dir &dir) {
  assert(opIdx < NUM_OPERANDS);
  if (opIdx == 0) {
    return rangeToMeshDir(csrVal, EXE_OP1_HI, EXE_OP1_LO, dir);
  }
  else {
    return rangeToMeshDir(csrVal, EXE_OP2_HI, EXE_OP2_LO, dir); 
  }
}

// TODO might want to make a unified parent class and overload this method
// also for out src
bool
MeshHelper::exeCsrToInSrc(uint64_t csrVal, std::vector<Mesh_Dir> &dirs) {
  Mesh_Dir dir;
  for (int i = 0 ; i < NUM_OPERANDS; i++) {
    if (exeCsrToOp(csrVal, i, dir)) {
      dirs.push_back(dir);
    }
  }
  
  return (dirs.size() > 0);
}

bool
MeshHelper::exeCsrToOutDests(uint64_t csrVal, std::vector<Mesh_Dir> &dirs) {
  for (int j = 0; j < NUM_DIR; j++) {
    Mesh_Dir dir = (Mesh_Dir)j;
    Mesh_Out_Src src;
    if (exeCsrToOutSrcs(csrVal, dir, src)) {
      dirs.push_back(dir);
    }
  }
  
  return (dirs.size() > 0);
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

bool
MeshHelper::fetCsrToInSrc(uint64_t csrVal, Mesh_Dir &dir) { 
  return rangeToMeshDir(csrVal, FET_IN_SRC_HI, FET_IN_SRC_LO, dir);
}

bool
MeshHelper::fetCsrToOutDests(uint64_t csrVal, std::vector<Mesh_Dir> &dirs) {
  // check each bit region to see if we should send
  for (int i = 0; i < NUM_DIR; i++) {
    if (bits(csrVal, FET_OUT_HI + i, FET_OUT_LO + i) == 1) {
      dirs.push_back((Mesh_Dir)i);
    }
  }
  
  return (dirs.size() > 0);
}

bool
MeshHelper::fetCsrToCount(uint64_t csrVal, int &count) {
  count = bits(csrVal, FET_COUNT_HI, FET_COUNT_LO);
  return (count > 0);
}

bool
MeshHelper::fetCsrToLockedInst(uint64_t csrVal, Locked_Insts &inst) {
  // only does anything if dir set to lock
  inst = (Locked_Insts)bits(csrVal, FET_INST_HI, FET_INST_LO);
  int lockedInt = bits(csrVal, FET_IN_SRC_HI, FET_IN_SRC_LO);
  return (lockedInt == (FET_I_INST_LOCK >> FET_I_INST_SHAMT));
}


bool
MeshHelper::csrToInSrcs(uint64_t csr, uint64_t csrVal, std::vector<Mesh_Dir> &dirs) {
  if (csr == MISCREG_EXE) {
    return exeCsrToInSrc(csrVal, dirs);
  }
  else if (csr == MISCREG_FETCH) {
    Mesh_Dir singleDir;
    bool ret = fetCsrToInSrc(csrVal, singleDir);
    if (ret) dirs.push_back(singleDir);
    return ret;
  }
  else {
    assert(0);
  }
}

// parent class -- noob!
bool
MeshHelper::csrToOutDests(uint64_t csr, uint64_t csrVal, std::vector<Mesh_Dir> &dirs) {
  if (csr == MISCREG_EXE) {
    return exeCsrToOutDests(csrVal, dirs);
  }
  else if (csr == MISCREG_FETCH) {
    return fetCsrToOutDests(csrVal, dirs);
  }
  else {
    assert(0);
  }
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
