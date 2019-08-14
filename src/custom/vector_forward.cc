#include "custom/vector_foward.hh"


VectorForward::VectorForward(const std::string &name,
  MinorCPU &cpu_,
  MinorCPUParams &params,
  Latch<ForwardInstData>::Input out_) : 
        
  Named(name),
  cpu(cpu_),
  out(out_) {
    
    
}
