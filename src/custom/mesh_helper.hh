#ifndef __CUSTOM_MESH_HELPER_HH__
#define __CUSTOM_MESH_HELPER_HH__

// Authors: Philip Bedoukian

#include <cstdint>
#include <vector>

#include "custom/bind_spec.hh"

typedef enum SensitiveStage {
  EXECUTE = 0,
  FETCH,
  NUM_STAGES,
  NONE
} SensitiveStage;

class MeshHelper {
  
  public:
    
    static bool exeCsrToOutSrcs(uint64_t csrVal, std::vector<Mesh_DS_t> &out);
    static bool exeCsrToOp(uint64_t csrVal, int opIdx, Mesh_Dir &dir);
    static bool exeCsrToInSrc(uint64_t csrVal, std::vector<Mesh_Dir> &dirs);
    static bool exeCsrToOutDests(uint64_t csrVal, std::vector<Mesh_Dir> &dirs);
    
    // the csr idx is to a bind one
    static bool isBindCSR(int csrIdx);
    
    // get all bindcsr idx
    static std::vector<int> getCSRCodes();
  
    // the config in the csr means that no special behvaior should occur
    static bool isCSRDefault(uint64_t csrVal);
    
    static bool isVectorMaster(uint64_t csrVal);
    static bool isVectorSlave(uint64_t csrVal);
    static bool hasForwardingPath(RegVal csrVal);
    static bool fetCsrToInSrc(uint64_t csrVal, Mesh_Dir &dir);
    static bool fetCsrToOutDests(uint64_t csrVal, std::vector<Mesh_Dir> &dirs);
    static bool isDecoupledAccess(RegVal csrVal);
    
    static bool csrToInSrcs(uint64_t csr, uint64_t csrVal, std::vector<Mesh_Dir> &dirs);
    static bool csrToOutDests(uint64_t csr, uint64_t csrVal, std::vector<Mesh_Dir> &dirs);
    static bool csrToOutSrcs(uint64_t csr, uint64_t csrVal, std::vector<Mesh_DS_t> &out);

    // get vector origin
    static int getXOrigin(RegVal csrVal);
    static int getYOrigin(RegVal csrVal);

    // get vector lengths
    static int getXLen(uint64_t csr, uint64_t csrVal);
    static int getYLen(uint64_t csr, uint64_t csrVal);
    
    // whether to perform a vector load or not based on current configuration
    static bool doVecLoad(uint64_t csrVal) { return isVectorMaster(csrVal) && !isVectorSlave(csrVal); }
  
    static SensitiveStage csrToStage(uint64_t csr);
    static uint64_t stageToCsr(SensitiveStage stage);

    // get info from prefetch csr
    static int numPrefetchRegions(RegVal csrVal);
    static int prefetchRegionSize(RegVal csrVal);
    
  private:
    static bool rangeToMeshDir(uint64_t csrVal, int hi, int lo, Mesh_Dir &dir);
    static bool rangeToMeshOutSrc(uint64_t csrVal, int hi, int lo, Mesh_Out_Src &src);
};


#endif
