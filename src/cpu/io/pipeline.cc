#include "cpu/io/pipeline.hh"
#include "cpu/io/stage.hh"
#include "cpu/io/cpu.hh"

std::array<std::shared_ptr<Stage>, (int)StageIdx::NumStages>
Pipeline::create(IOCPU *_cpu_p, IOCPUParams* params) {
  
  auto stages = std::array<std::shared_ptr<Stage>, (int)StageIdx::NumStages>();
  
  for (int i = 0; i < (int)StageIdx::NumStages; i++) {
    StageIdx stageIdx = (StageIdx)i;
    switch (stageIdx) {
      case FetchIdx:
        stages[i] = std::make_shared<Fetch>(_cpu_p, params);
        break;
      case DecodeIdx:
        stages[i] = std::make_shared<Decode>(_cpu_p, params);
        break;
      case RenameIdx:
        stages[i] = std::make_shared<Rename>(_cpu_p, params);
        break;
      case IEWIdx:
        stages[i] = std::make_shared<IEW>(_cpu_p, params);
        break;
      case CommitIdx:
        stages[i] = std::make_shared<Commit>(_cpu_p, params);
        break;
      default:
        break;
    }
  }
  
  return stages;
}

