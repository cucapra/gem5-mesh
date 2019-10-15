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
#include "cpu/io/pipeline.hh"

class IODynInst;

/**
 * Instruction communication (forward communication btw stages)
 */
struct InstComm {
  std::list<IODynInstPtr> inst_buffer[(int)StageIdx::NumStages];
  
  // preserve helpers to core stage buffers 
  // Fetch -> Decode
  /*std::list<IODynInstPtr> &to_decode_insts() {
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
  }*/
  
  std::list<IODynInstPtr> &from_prev_stage(StageIdx curStageIdx) {
    return inst_buffer[(int)curStageIdx];
  }
  
  // should prob accepted cached for better perf
  /*std::list<IODynInstPtr> &to_next_stage(StageIdx stage) {
    int nextStageIdx = (int)stage + 1;
    assert(nextStageIdx < (int)StageIdx::NumStages);
    return inst_buffer[nextStageIdx];
  }*/
  std::list<IODynInstPtr> &to_next_stage(StageIdx nextStageIdx) {
    //int nextStageIdx = (int)stage + 1;
    //assert(nextStageIdx < (int)StageIdx::NumStages);
    return inst_buffer[(int)nextStageIdx];
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

  int stage_credits[(int)StageIdx::NumStages];
  
  /*// keep helpers to access the core stages
  int &from_decode() {
    return stage_credits[(int)StageIdx::DecodeIdx];
  }
  
  int &from_rename() {
    return stage_credits[(int)StageIdx::RenameIdx];
  }
  
  int &from_iew() {
    return stage_credits[(int)StageIdx::IEWIdx];
  }
  
  int &from_commit() {
    return stage_credits[(int)StageIdx::CommitIdx];
  }*/
  
  // modular accessors
  int &to_prev_stage(StageIdx curStageIdx) {
    return stage_credits[(int)curStageIdx];
  }
  
  /*int &from_next_stage(StageIdx stage) {
    int nextStageIdx = (int)stage + 1;
    assert(nextStageIdx < (int)StageIdx::NumStages);
    return stage_credits[nextStageIdx];
  }*/
  
 int &from_next_stage(StageIdx nextStageIdx) {
    //int nextStageIdx = (int)stage + 1;
    //assert(nextStageIdx < (int)StageIdx::NumStages);
    return stage_credits[(int)nextStageIdx];
  }



};

/**
 * Squash signal communication
 * Any of three stages Decode, IEW and Commit can initiate a squash signal
 */
struct SquashComm {
  // common to all squash 
  struct BaseSquash {
    BaseSquash(StageIdx _stage_idx) : stage_idx(_stage_idx), trig_inst(nullptr), squash(false) { }
    BaseSquash() : stage_idx(StageIdx::NumStages), trig_inst(nullptr), squash(false) { }
    
    StageIdx stage_idx;
    TheISA::PCState next_pc;    // PC to fetch next after this squash
    IODynInstPtr trig_inst;     // Inst triggering the squash
    bool squash;                // True if this squash signal is high
  };
  
  /** Squash initiated by Decode (listened by Fetch) */
  struct DecodeSquash : public BaseSquash {
    DecodeSquash() : BaseSquash(StageIdx::DecodeIdx) { }

    bool branch_taken;          // was the branch taken?
  };

  /** Squash initiated by IEW (listened by Fetch, Decode, Rename, Commit) */
  struct IEWSquash : public BaseSquash {
    IEWSquash() : BaseSquash(StageIdx::IEWIdx) { }

    bool branch_taken;          // was the branch taken?
  };

  /** Squash initiated by Commit (listened by Fetch, Decode, Rename, IEW) */
  struct CommitSquash : public BaseSquash {
    CommitSquash()
        : BaseSquash(StageIdx::CommitIdx), 
          is_trap_pending(false)
    { }
    
    bool is_trap_pending;         // True if there's an in-flight trap
  };
  
  std::unordered_map<int, BaseSquash> squash_signals;

  SquashComm() {
    squash_signals[(int)StageIdx::DecodeIdx] = DecodeSquash();
    squash_signals[(int)StageIdx::IEWIdx] = IEWSquash();
    squash_signals[(int)StageIdx::CommitIdx] = CommitSquash();
  }

  DecodeSquash *decode_squash() { return (DecodeSquash*)&(squash_signals[(int)StageIdx::DecodeIdx]); }
  IEWSquash    *iew_squash() { return (IEWSquash*)&(squash_signals[(int)StageIdx::IEWIdx]); }
  CommitSquash *commit_squash() { return (CommitSquash*)&(squash_signals[(int)StageIdx::CommitIdx]); }
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
