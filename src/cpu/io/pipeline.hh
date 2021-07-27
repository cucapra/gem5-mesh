//-----------------------------------------------------------------------------
// pipeline.hh
//-----------------------------------------------------------------------------
// Layout of pipeline in io model
//
// Authors: Philip Bedoukian

#ifndef __CPU_IO_PIPELINE_HH__
#define __CPU_IO_PIPELINE_HH__

#include <memory>
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
  EarlyVectorIdx,
  LateVectorIdx,
  DecodeIdx,
  RenameIdx,
  IEWIdx,
  CommitIdx,
  NumStages
} StageIdx;

class Pipeline {
  private:
    std::vector<StageIdx> _order;
    std::vector<std::shared_ptr<Stage>> _stages;
    
  public:
    // return a 
    //std::vector<std::shared_ptr<Stage>> create(IOCPU *_cpu_p, IOCPUParams* params);
    Pipeline(IOCPU *_cpu_p, IOCPUParams* params); 
 
    int lookupPos(StageIdx currStage);
    StageIdx getNextStageIdx(StageIdx currStage);
    StageIdx getPrevStageIdx(StageIdx currStage);
    bool hasNextStage(StageIdx currStage);
    bool hasPrevStage(StageIdx currStage);
    bool stageCmp(StageIdx a, StageIdx b);
    bool isStageSeq(StageIdx stage);
    void setPrevStageUnemployed(StageIdx stage, bool val);
 
    /** extract specific stages from a list based on order */
    template<typename T>
    T* extractStagePtr(StageIdx stage) {
      int idx = lookupPos(stage); 
      if (idx >= 0)      
        return std::dynamic_pointer_cast<T>(_stages[idx]).get();
      else
        return nullptr;
    }

    unsigned getLen() const { return _order.size(); }
    std::vector<StageIdx> getOrder() const { return _order; }
    std::vector<std::shared_ptr<Stage>> getStages() const { return _stages; }
    std::shared_ptr<Stage>& operator [](int i) { return _stages[i]; }


    // TODO function that looks up appropriate buffer size?
    int getBufSize(IOCPUParams* params, StageIdx stage);
    int getOutBufSize(IOCPUParams* params, StageIdx stage);
  
};

#endif
