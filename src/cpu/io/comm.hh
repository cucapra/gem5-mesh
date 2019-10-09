//-----------------------------------------------------------------------------
// comm.hh
//-----------------------------------------------------------------------------
// Inter-stage communicate data structures
//
// Author: Tuan Ta
// Date  : 19/08/18

#ifndef __CPU_IO_COMM_HH__
#define __CPU_IO_COMM_HH__

#include <list>

#include "arch/types.hh"
#include "cpu/io/dyn_inst.hh"


/**
 * Keep track of how many pipe stages we have for modularity bonuses
*/ 
typedef enum StageIdx {
  FetchIdx = 0,
  //VectorIdx
  DecodeIdx,
  RenameIdx,
  IEWIdx,
  CommitIdx,
  NumStages
} StageIdx;

class IODynInst;

/**
 * Instruction communication (forward communication btw stages)
 */
struct InstComm {
  std::list<IODynInstPtr> inst_buffer[(int)StageIdx::NumStages];
  
  // preserve helpers to core stage buffers 
  // Fetch -> Decode
  std::list<IODynInstPtr> &to_decode_insts() {
    return inst_buffer[(int)StageIdx::DecodeIdx];
  }

  // Decode -> Rename
  std::list<IODynInstPtr> &to_rename_insts() {
    return inst_buffer[(int)StageIdx::RenameIdx];
  }

  // Rename -> IEW
  std::list<IODynInstPtr> &to_iew_insts() {
    return inst_buffer[(int)StageIdx::IEWIdx];
  }

  // IEW -> Commit
  std::list<IODynInstPtr> &to_commit_insts() {
    return inst_buffer[(int)StageIdx::CommitIdx];
  }
  
  std::list<IODynInstPtr> &from_prev_stage(StageIdx stage) {
    return inst_buffer[(int)stage];
  }
  
  std::list<IODynInstPtr> &to_next_stage(StageIdx stage) {
    int nextStageIdx = (int)stage + 1;
    assert(nextStageIdx < (int)StageIdx::NumStages);
    return inst_buffer[nextStageIdx];
  }
  
  
};

/**
 * Communication exchanging credits between stages
 */
struct CreditComm {
  CreditComm()
  { 
      for (int i = 0; i < (int)StageIdx::NumStages; i++) {
        stage_credits[i] = 0;
      }
  }

  size_t stage_credits[(int)StageIdx::NumStages];
  
  // keep helpers to access the core stages
  size_t &from_decode() {
    return stage_credits[(int)StageIdx::DecodeIdx];
  }
  
  size_t &from_rename() {
    return stage_credits[(int)StageIdx::RenameIdx];
  }
  
  size_t &from_iew() {
    return stage_credits[(int)StageIdx::IEWIdx];
  }
  
  size_t &from_commit() {
    return stage_credits[(int)StageIdx::CommitIdx];
  }
  
  // modular accessors
  size_t &to_prev_stage(StageIdx stage) {
    return stage_credits[(int)stage];
  }
  
  size_t &from_next_stage(StageIdx stage) {
    int nextStageIdx = (int)stage + 1;
    assert(nextStageIdx < (int)StageIdx::NumStages);
    return stage_credits[nextStageIdx];
  }
  
};

/**
 * Squash signal communication
 * Any of three stages Decode, IEW and Commit can initiate a squash signal
 */
struct SquashComm {
  /** Squash initiated by Decode (listened by Fetch) */
  struct DecodeSquash {
    DecodeSquash() : mispred_inst(nullptr), squash(false) { }

    TheISA::PCState next_pc;    // PC to fetch next after this squash
    IODynInstPtr mispred_inst;  // branch instruction that is mispredicted
    bool branch_taken;          // was the branch taken?
    bool squash;                // True if this squash signal is high
  };

  /** Squash initiated by IEW (listened by Fetch, Decode, Rename, Commit) */
  struct IEWSquash {
    IEWSquash() : mispred_inst(nullptr), squash(false) { }

    TheISA::PCState next_pc;    // PC to fetch next after this squash
    IODynInstPtr mispred_inst;  // branch instruction that is mispredicted
    bool branch_taken;          // was the branch taken?
    bool squash;                // True if this squash signal is high
  };

  /** Squash initiated by Commit (listened by Fetch, Decode, Rename, IEW) */
  struct CommitSquash {
    CommitSquash()
        : fault_inst(nullptr),
          squash(false),
          is_trap_pending(false)
    { }

    TheISA::PCState next_pc;      // PC to fetch next after this squash
    IODynInstPtr fault_inst;      // fault instruction triggerring this squash
    bool squash;                  // True if this squash signal is high
    bool is_trap_pending;         // True if there's an in-flight trap
  };

  DecodeSquash decode_squash;
  IEWSquash    iew_squash;
  CommitSquash commit_squash;
};

/**
 * Other pipeline information (e.g., seq_num of retired instructions) going
 * backward in the pipeline. This information is used by Fetch to update its
 * branch predictor and used by Rename to update its history buffer and free
 * phys regs.
 */
struct InfoComm {
  struct CommitInfo {
    CommitInfo() : committed_inst(nullptr) { }

    IODynInstPtr committed_inst;
  };

  CommitInfo commit_info;
};

#endif // __CPU_IO_COMM_HH__
