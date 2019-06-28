#ifndef __CUSTOM_MESH_HELPER_HH__
#define __CUSTOM_MESH_HELPER_HH__

#include <cstdint>

// order from mesh.py reproduced here
typedef enum Mesh_Dir {
  RIGHT = 0,
  DOWN = 1,
  LEFT = 2,
  UP = 3,
  NUM_DIR
} Mesh_Dir;

class MeshHelper {
  
  public:
    static bool csrToRd(uint64_t csrVal, Mesh_Dir &dir);
    
    static bool csrToOp1(uint64_t csrVal, Mesh_Dir &dir);
    static bool csrToOp2(uint64_t csrVal, Mesh_Dir &dir);
  
    // the config in the csr means that no special behvaior should occur
    static bool isCSRDefault(uint64_t csrVal);
    
  private:
    static bool rangeToMeshDir(uint64_t csrVal, int hi, int lo, Mesh_Dir &dir);
};


#endif
