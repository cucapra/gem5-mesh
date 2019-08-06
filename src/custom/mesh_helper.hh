#ifndef __CUSTOM_MESH_HELPER_HH__
#define __CUSTOM_MESH_HELPER_HH__

#include <cstdint>
#include <vector>

#include "custom/bind_spec.hh"

typedef enum SensitiveStage {
  NONE = 0,
  EXECUTE,
  FETCH
} SensitiveStage;

class MeshHelper {
  
  public:
    //static bool csrToRd(uint64_t csrVal, Mesh_Dir &dir);
    
    static bool exeCsrToOutSrcs(uint64_t csrVal, std::vector<Mesh_DS_t> &out);
    //static bool csrSrcActive(uint64_t csrVal, Mesh_Dir dir, Mesh_Out_Src src);
    
    //static int csrSrcSends(uint64_t csrVal, Mesh_Out_Src src);
    
    static bool exeCsrToOp(uint64_t csrVal, int opIdx, Mesh_Dir &dir);
    //static bool csrToOp1(uint64_t csrVal, Mesh_Dir &dir);
    //static bool csrToOp2(uint64_t csrVal, Mesh_Dir &dir);
    
    
    static bool exeCsrToInSrc(uint64_t csrVal, std::vector<Mesh_Dir> &dirs);
    static bool exeCsrToOutDests(uint64_t csrVal, std::vector<Mesh_Dir> &dirs);
    
    // the csr idx is to a bind one
    static bool isBindCSR(int csrIdx);
    
    // get all bindcsr idx
    static std::vector<int> getCSRCodes();
  
    // how many out ports we are going to use
    //static int numOutPorts(uint64_t csrVal);
    
    // how many in ports we are going to use
    //static int numInPorts(uint64_t csrVal);
    
    // no inward recvs
  
    // the config in the csr means that no special behvaior should occur
    static bool isCSRDefault(uint64_t csrVal);
    
    static bool fetCsrToInSrc(uint64_t csrVal, Mesh_Dir &dir);
    static bool fetCsrToOutDests(uint64_t csrVal, std::vector<Mesh_Dir> &dirs);
    static bool fetCsrToCount(uint64_t csrVal, int &count);
    static bool fetCsrToLockedInst(uint64_t csrVal, Locked_Insts &inst);
    
    static bool csrToInSrcs(uint64_t csr, uint64_t csrVal, std::vector<Mesh_Dir> &dirs);
    static bool csrToOutDests(uint64_t csr, uint64_t csrVal, std::vector<Mesh_Dir> &dirs);
    static bool csrToOutSrcs(uint64_t csr, uint64_t csrVal, std::vector<Mesh_DS_t> &out);

  
    static SensitiveStage csrToStage(uint64_t csr);
    
  private:
    static bool rangeToMeshDir(uint64_t csrVal, int hi, int lo, Mesh_Dir &dir);
    static bool rangeToMeshOutSrc(uint64_t csrVal, int hi, int lo, Mesh_Out_Src &src);
    
    //static StaticInst lockedToStaticInst(Locked_Insts inst);
};


#endif
