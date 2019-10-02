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

class IODynInst;

/**
 * Instruction communication (forward communication btw stages)
 */
struct InstComm {
  // Fetch -> Decode
  std::list<IODynInstPtr> to_decode_insts;

  // Decode -> Rename
  std::list<IODynInstPtr> to_rename_insts;

  // Rename -> IEW
  std::list<IODynInstPtr> to_iew_insts;

  // IEW -> Commit
  std::list<IODynInstPtr> to_commit_insts;
};

/**
 * Communication exchanging credits between stages
 */
struct CreditComm {
  CreditComm()
      : from_decode(0),
        from_rename(0),
        from_iew(0),
        from_commit(0)
  { }

  size_t from_decode;
  size_t from_rename;
  size_t from_iew;
  size_t from_commit;
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
