// Authors: Philip Bedoukian

#include <cassert>

#include "arch/registers.hh"
#include "custom/mesh_helper.hh"
#include "base/bitfield.hh"


// need this to get reg names
using namespace RiscvISA;

#define NUM_OPERANDS 2

static std::vector<int> csrs = { MISCREG_EXE, MISCREG_FETCH };

bool
MeshHelper::exeCsrToOutSrcs(uint64_t csrVal, std::vector<Mesh_DS_t> &out) {
  Mesh_DS_t ds;
  for (int i = 0; i < NUM_DIR; i++) {
    ds.outDir = (Mesh_Dir)i;
    if (ds.outDir == UP) {
      if (rangeToMeshOutSrc(csrVal, EXE_UP_HI, EXE_UP_LO, ds.src)) {
        out.push_back(ds);
      }
    }
    else if (ds.outDir == RIGHT) {
      if (rangeToMeshOutSrc(csrVal, EXE_RIGHT_HI, EXE_RIGHT_LO, ds.src)) {
        out.push_back(ds);
      }
    }
    else if (ds.outDir == DOWN) {
      if (rangeToMeshOutSrc(csrVal, EXE_DOWN_HI, EXE_DOWN_LO, ds.src)) {
        out.push_back(ds);
      }
    }
    else if (ds.outDir == LEFT) {
      if (rangeToMeshOutSrc(csrVal, EXE_LEFT_HI, EXE_LEFT_LO, ds.src)) {
        out.push_back(ds);
      }
    }
  }
  
  return (out.size() > 0);
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
  std::vector<Mesh_DS_t> ds;
  exeCsrToOutSrcs(csrVal, ds);
  
  for (int i = 0; i < ds.size(); i++) {
    dirs.push_back(ds[i].outDir);
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
MeshHelper::isVectorSlave(uint64_t csrVal) {
  Mesh_Dir dir;
  return (fetCsrToInSrc(csrVal, dir));
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
MeshHelper::isVectorMaster(uint64_t csrVal) {
  std::vector<Mesh_Dir> dirs;
  return (fetCsrToOutDests(csrVal, dirs));
}

bool
MeshHelper::isDecoupledAccess(RegVal csrVal) {
  uint64_t ret = bits(csrVal, FET_DAE_HI, FET_DAE_LO);
  return (ret == 1);
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
    return false;
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
    return false;
  }
}

bool
MeshHelper::csrToOutSrcs(uint64_t csr, uint64_t csrVal, std::vector<Mesh_DS_t> &out) {
  if (csr == MISCREG_EXE) {
    return exeCsrToOutSrcs(csrVal, out);
  }
  else if (csr == MISCREG_FETCH) {
    std::vector<Mesh_Dir> dirs;
    fetCsrToOutDests(csrVal, dirs);
    for (int i = 0; i < dirs.size(); i++) {
      Mesh_DS_t ds;
      ds.outDir = dirs[i];
      ds.src = INST;
      out.push_back(ds);
    }
    
    return (out.size() > 0);
  }
  else {
    return false;
  }
}

int
MeshHelper::getXOrigin(RegVal csrVal) {
  auto val = bits(csrVal, FET_XORIGIN_HI, FET_XORIGIN_LO);
  return val;
}

int
MeshHelper::getYOrigin(RegVal csrVal) {
  auto val = bits(csrVal, FET_YORIGIN_HI, FET_YORIGIN_LO);
  return val;
}

int
MeshHelper::getXLen(uint64_t csr, uint64_t csrVal) {
  auto val = bits(csrVal, FET_XLEN_HI, FET_XLEN_LO);
  if (val == 0) return 1;
  else return val;
}

int
MeshHelper::getYLen(uint64_t csr, uint64_t csrVal) {
  auto val = bits(csrVal, FET_YLEN_HI, FET_YLEN_LO);
  if (val == 0) return 1;
  else return val;
}

SensitiveStage
MeshHelper::csrToStage(uint64_t csr) {
  if (csr == MISCREG_EXE) {
    return EXECUTE;
  }
  else if (csr == MISCREG_FETCH) {
    return FETCH;
  }
  else {
    return NONE;
  }
}

uint64_t
MeshHelper::stageToCsr(SensitiveStage stage) {
  if (stage == EXECUTE) {
    return MISCREG_EXE;
  }
  else if (stage == FETCH) {
    return MISCREG_FETCH;
  }
  else {
    return 0;
  }
}

int
MeshHelper::numPrefetchRegions(RegVal csrVal) {
  return bits(csrVal, PREFETCH_NUM_REGION_HI, PREFETCH_NUM_REGION_LO);
}

int
MeshHelper::prefetchRegionSize(RegVal csrVal) {
  return bits(csrVal, PREFETCH_REGION_SIZE_HI, PREFETCH_REGION_SIZE_LO);
}

bool
MeshHelper::hasForwardingPath(RegVal csrVal) {
  return isVectorMaster(csrVal) || isVectorSlave(csrVal);
}