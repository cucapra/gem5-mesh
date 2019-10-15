//-----------------------------------------------------------------------------
// pipeline.hh
//-----------------------------------------------------------------------------
// Layout of pipeline in io model
//

#ifndef __CPU_IO_PIPELINE_HH__
#define __CPU_IO_PIPELINE_HH__

#include <vector>
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
    static unsigned Len;
    static std::vector<StageIdx> Order;
  
  public:
    // return a 
    static std::vector<std::shared_ptr<Stage>> create(IOCPU *_cpu_p, IOCPUParams* params);
   
    static int lookupPos(StageIdx currStage);
    static StageIdx getNextStageIdx(StageIdx currStage);
    static StageIdx getPrevStageIdx(StageIdx currStage);
    static bool hasNextStage(StageIdx currStage);
    static bool hasPrevStage(StageIdx currStage);
    static bool stageCmp(StageIdx a, StageIdx b);
  
    // TODO function that looks up appropriate buffer size?
  
};

#endif
