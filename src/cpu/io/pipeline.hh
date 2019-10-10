//-----------------------------------------------------------------------------
// pipeline.hh
//-----------------------------------------------------------------------------
// Layout of pipeline in io model
//

#ifndef __CPU_IO_PIPELINE_HH__
#define __CPU_IO_PIPELINE_HH__

//#include <memory>

#include "params/IOCPU.hh"

class Stage;
class IOCPU;

/**
 * Keep track of how many pipe stages we have for modularity bonuses
 * This is also the order of the pipeline stages
*/ 
typedef enum StageIdx {
  FetchIdx = 0,
  VectorIdx,
  DecodeIdx,
  RenameIdx,
  IEWIdx,
  CommitIdx,
  NumStages
} StageIdx;

class Pipeline {
  
  public:
    // return a 
    static std::array<std::shared_ptr<Stage>, (int)StageIdx::NumStages> create(IOCPU *_cpu_p, IOCPUParams* params);
    
    // TODO function that looks up appropriate buffer size?
  
};

#endif
