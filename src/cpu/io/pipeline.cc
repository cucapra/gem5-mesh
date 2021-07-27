// Authors: Philip Bedoukian

#include "cpu/io/pipeline.hh"
#include "cpu/io/stage.hh"
#include "cpu/io/cpu.hh"

Pipeline::Pipeline(IOCPU *_cpu_p, IOCPUParams* params) {
 
  // figure out order of the stages
  _order.push_back(FetchIdx);
  
  if (params->includeVector)
    _order.push_back(EarlyVectorIdx);

  _order.push_back(DecodeIdx);
  _order.push_back(RenameIdx);
  _order.push_back(IEWIdx);
  
  _order.push_back(CommitIdx);
  
  if (params->includeVector)
    _order.push_back(LateVectorIdx);
  
  // create stages in order given above
  for (int i = 0; i < _order.size(); i++) {
    StageIdx stageIdx = _order[i];
    switch (stageIdx) {
      
      // core stages
      case FetchIdx:
        _stages.push_back(std::make_shared<Fetch>(_cpu_p, params, 
            getBufSize(params, FetchIdx), getOutBufSize(params, FetchIdx)));
        break;
      case DecodeIdx:
        _stages.push_back(std::make_shared<Decode>(_cpu_p, params, 
            getBufSize(params, DecodeIdx), getOutBufSize(params, DecodeIdx)));
        break;
      case RenameIdx:
        _stages.push_back(std::make_shared<Rename>(_cpu_p, params, 
            getBufSize(params, RenameIdx), getOutBufSize(params, RenameIdx)));
        break;
      case IEWIdx:
        _stages.push_back(std::make_shared<IEW>(_cpu_p, params,
            getBufSize(params, IEWIdx), getOutBufSize(params, IEWIdx)));
        break;
      case CommitIdx:
        _stages.push_back(std::make_shared<Commit>(_cpu_p, params, 
            getBufSize(params, CommitIdx), getOutBufSize(params, CommitIdx)));
        break;
      
      // new stages
      case EarlyVectorIdx:
        _stages.push_back(std::make_shared<Vector>(_cpu_p, params, 
            getBufSize(params, EarlyVectorIdx), getOutBufSize(params, EarlyVectorIdx), 
            StageIdx::EarlyVectorIdx, false, true));
        break;
        
      case LateVectorIdx:
        _stages.push_back(std::make_shared<Vector>(_cpu_p, params, 
            getBufSize(params, LateVectorIdx), getOutBufSize(params, LateVectorIdx), 
            StageIdx::LateVectorIdx, true, false));
        break;
        
      // 
      default:
        break;
    }
  }
  
  //return stages;
}

int
Pipeline::getBufSize(IOCPUParams* params, StageIdx stage) {
  if (!hasPrevStage(stage)) return 0;
  
  switch (stage) {
      case FetchIdx:
        return 0;
      case DecodeIdx:
        return params->decodeBufferSize;
      case RenameIdx:
        return params->renameBufferSize;
      case IEWIdx:
        return params->iewBufferSize;
      case CommitIdx:
        return params->commitBufferSize;
      case EarlyVectorIdx:
        return params->vectorBufferSize;
      case LateVectorIdx:
        return params->vectorBufferSize;
      default:
        return params->decodeBufferSize;
  }
}

int
Pipeline::getOutBufSize(IOCPUParams* params, StageIdx stage) {
  if (!hasNextStage(stage)) return 0;
  
  StageIdx nextStage = getNextStageIdx(stage);
  return getBufSize(params, nextStage);
}

int
Pipeline::lookupPos(StageIdx currStage) {
  int idx = -1;
  for (int i = 0; i < _order.size(); i++) {
    if (_order[i] == currStage) {
      idx = i;
    }
  }
  
  //assert(idx >= 0);
  
  return idx;
}

// should cache this in stage.hh
StageIdx
Pipeline::getNextStageIdx(StageIdx currStage) {
  // find stage in the order array and add 1
  int idx = lookupPos(currStage);
  
  //assert(idx < Pipeline::Len - 1);
  if (idx >= 0 && idx < _order.size() - 1)
    return _order[idx + 1];
  else
    // just return random thing
    return StageIdx::NumStages;
}

StageIdx
Pipeline::getPrevStageIdx(StageIdx currStage) {
  // find stage in the order array and add 1
  int idx = lookupPos(currStage);
  
  //assert(idx >= 1 && idx < _order.size());
  if (idx >= 1 && idx < _order.size())
    return _order[idx - 1];
  else
    return StageIdx::NumStages; // just return something
}

bool
Pipeline::hasNextStage(StageIdx currStage) {
  int idx = lookupPos(currStage);
  
  return (idx < _order.size() - 1);
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

bool
Pipeline::isStageSeq(StageIdx stage) {
  int idx = lookupPos(stage);
  if (idx >= 0)
    return _stages[idx]->isSequential();
  else
    return true;
}

void
Pipeline::setPrevStageUnemployed(StageIdx stage, bool val) {
  int idx = lookupPos(stage);
  if (idx >= 1) {
    _stages[idx - 1]->setUnemployed(val);
  }
}

