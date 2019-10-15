#include "cpu/io/pipeline.hh"
#include "cpu/io/stage.hh"
#include "cpu/io/cpu.hh"

unsigned Pipeline::Len = (int)StageIdx::NumStages;

std::vector<StageIdx> Pipeline::Order;

std::vector<std::shared_ptr<Stage>>
Pipeline::create(IOCPU *_cpu_p, IOCPUParams* params) {
   
  // figure out order of the stages
  Order.push_back(FetchIdx);
  
  if (params->includeVector)
    Order.push_back(VectorIdx);

  Order.push_back(DecodeIdx);
  Order.push_back(RenameIdx);
  Order.push_back(IEWIdx);
  Order.push_back(CommitIdx);


  Len = Order.size(); // prob dont need this 
  auto stages = std::vector<std::shared_ptr<Stage>>();
  
  // create stages in order given above
  for (int i = 0; i < Len; i++) {
    StageIdx stageIdx = Order[i];
    switch (stageIdx) {
      
      // core stages
      case FetchIdx:
        stages.push_back(std::make_shared<Fetch>(_cpu_p, params));
        break;
      case DecodeIdx:
        stages.push_back(std::make_shared<Decode>(_cpu_p, params));
        break;
      case RenameIdx:
        stages.push_back(std::make_shared<Rename>(_cpu_p, params));
        break;
      case IEWIdx:
        stages.push_back(std::make_shared<IEW>(_cpu_p, params));
        break;
      case CommitIdx:
        stages.push_back(std::make_shared<Commit>(_cpu_p, params));
        break;
      
      // new stages
      case VectorIdx:
        stages.push_back(std::make_shared<Vector>(_cpu_p, params));
        break;
        
      // 
      default:
        break;
    }
  }
  
  return stages;
}

int
Pipeline::lookupPos(StageIdx currStage) {
  int idx = -1;
  for (int i = 0; i < Pipeline::Len; i++) {
    if (Order[i] == currStage) {
      idx = i;
    }
  }
  
  assert(idx >= 0);
  
  return idx;
}

// should cache this in stage.hh
StageIdx
Pipeline::getNextStageIdx(StageIdx currStage) {
  // find stage in the order array and add 1
  int idx = lookupPos(currStage);
  
  assert(idx < Pipeline::Len - 1);
  return Order[idx + 1];
}

StageIdx
Pipeline::getPrevStageIdx(StageIdx currStage) {
  // find stage in the order array and add 1
  int idx = lookupPos(currStage);
  
  assert(idx >= 1 && idx < Pipeline::Len);
  return Order[idx - 1];
}

bool
Pipeline::hasNextStage(StageIdx currStage) {
  int idx = lookupPos(currStage);
  
  return (idx < Pipeline::Len - 1);
}

bool
Pipeline::hasPrevStage(StageIdx currStage) {
  int idx = lookupPos(currStage);
  
  return (idx >= 1); 
}

bool
Pipeline::stageCmp(StageIdx a, StageIdx b) {
  int idxA = lookupPos(a);
  int idxB = lookupPos(b);
  
  if (idxA >= idxB) { // ge
    return true;
  }
  else { // lt
    return false;
  }
}


