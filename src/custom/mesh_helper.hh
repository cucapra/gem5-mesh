#ifndef __CUSTOM_MESH_HELPER_HH__
#define __CUSTOM_MESH_HELPER_HH__

#include <cstdint>
#include <vector>

// order from mesh.py reproduced here (and should agree with software bind_defs.h
typedef enum Mesh_Dir {
  RIGHT = 0,
  DOWN = 1,
  LEFT = 2,
  UP = 3,
  NUM_DIR
} Mesh_Dir;

typedef enum Mesh_Out_Src {
  RD = 0,
  RS1 = 1,
  RS2 = 2
} Mesh_Out_Src;

class MeshHelper {
  
  public:
    //static bool csrToRd(uint64_t csrVal, Mesh_Dir &dir);
    
    static bool csrToOutSrcs(uint64_t csrVal, Mesh_Dir dir, Mesh_Out_Src &src);
    //static bool csrSrcActive(uint64_t csrVal, Mesh_Dir dir, Mesh_Out_Src src);
    
    //static int csrSrcSends(uint64_t csrVal, Mesh_Out_Src src);
    
    static bool csrToOp(uint64_t csrVal, int opIdx, Mesh_Dir &dir);
    //static bool csrToOp1(uint64_t csrVal, Mesh_Dir &dir);
    //static bool csrToOp2(uint64_t csrVal, Mesh_Dir &dir);
    
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
    
  private:
    static bool rangeToMeshDir(uint64_t csrVal, int hi, int lo, Mesh_Dir &dir);
    static bool rangeToMeshOutSrc(uint64_t csrVal, int hi, int lo, Mesh_Out_Src &src);
};


#endif
