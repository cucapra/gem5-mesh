//=============================================================================
// commit.hh
//=============================================================================
// Commit stage in IO CPU
//
// Author: Tuan Ta
// Date:   19/08/26

#ifndef __CPU_IO_COMMIT_HH__
#define __CPU_IO_COMMIT_HH__

#include <iomanip>
#include <queue>
#include <sstream>
#include <vector>

#include "cpu/inst_seq.hh"
#include "cpu/io/comm.hh"
#include "cpu/io/dyn_inst.hh"
#include "cpu/io/rob.hh"
#include "cpu/o3/rename_map.hh"
#include "cpu/timebuf.hh"
#include "params/IOCPU.hh"
#include "cpu/io/stage.hh"

class Commit : public Stage
{
  public:
    Commit(IOCPU* _cpu_p, IOCPUParams* params);
    ~Commit() = default;

    /** Init (this is called after all CPU structures are created) */
    void init() override;

    /** Return name of this stage object */
    std::string name() const override;

    /** Register stats */
    void regStats() override;

    /** Main tick function */
    void tick() override;

    /** Wake up this stage */
    void wakeup() override;

    /** Suspend this stage */
    void suspend() override;

    /** Line trace */
    void linetrace(std::stringstream& ss) override;

    /**
     * Get info about instruction being processed/committed.
     * TODO: need to revisit this. Not sure why we need those functions
     * These functions are needed by thread context
     */
    Addr instAddr(ThreadID tid) { return m_commit_pc[tid].instAddr(); }
    Addr nextInstAddr(ThreadID tid) { return m_commit_pc[tid].nextInstAddr(); }
    Addr microPC(ThreadID tid) { return m_commit_pc[tid].microPC(); }
    TheISA::PCState pcState(ThreadID tid) { return m_commit_pc[tid]; }
    void pcState(const TheISA::PCState& val, ThreadID tid)
    { m_commit_pc[tid] = val; }

  private:
    enum CommitStatus {
      InitSquash,
      Squashed,
      Busy,
      NumStatus
    };

  private:
    /** Do commit */
    void doCommit();

    /** Instruction conditions that should initiate a squash */
    bool shouldFault(IODynInstPtr inst);
    
    /** Initiate a squash signal */
    void initiateSquash(IODynInstPtr faulty_inst);

    /** Squash all instructions younger than the given squash instruction */
    void doSquash(SquashComm::BaseSquash &squashInfo, StageIdx initiator) override;

    /** Commit head instruction from the given thread */
    void commitHead(ThreadID tid);

  private:
    /** Number of threads */
    size_t m_num_threads;

    /** Max number of instructions that can be committed in a cycle */
    size_t m_commit_width;

    /** ROBs */
    std::vector<ROB*> m_robs;

    /** Commit rename map. This holds all arch -> phys reg mappings until the
     * last commited instruction. This is different from the rename map in
     * Rename stage which can hold reg mappings of instructions not yet
     * committed */
    std::vector<UnifiedRenameMap*> m_commit_rename_maps;

    /** The commit PC state of each thread. This referrs to the instruction
     * being processed/committed */
    std::vector<TheISA::PCState> m_commit_pc;

    /** Stage's status (for line trace) */
    std::bitset<CommitStatus::NumStatus> m_stage_status;

    /** List of insts processed in the current cycle */
    std::vector<IODynInstPtr> m_committed_insts;
};

#endif // __CPU_IO_COMMIT_HH__
