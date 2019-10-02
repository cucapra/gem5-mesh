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

class IOCPU;

class Commit
{
  public:
    Commit(IOCPU* _cpu_p, IOCPUParams* params);
    ~Commit() = default;

    /** Init (this is called after all CPU structures are created) */
    void init();

    /** Return name of this stage object */
    std::string name() const;

    /** Register stats */
    void regStats();

    /** Set incoming/outgoing communication wires */
    void setCommBuffers(TimeBuffer<InstComm>& inst_buffer,
                        TimeBuffer<CreditComm>& credit_buffer,
                        TimeBuffer<SquashComm>& squash_buffer,
                        TimeBuffer<InfoComm>& info_buffer);

    /** Main tick function */
    void tick();

    /** Wake up this stage */
    void wakeup();

    /** Suspend this stage */
    void suspend();

    /** Line trace */
    void linetrace(std::stringstream& ss);

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

    /** Put all instructions to be processed this cycle into m_insts queue */
    void queueInsts();

    /** Check squash signal. Return true if we're squashing */
    bool checkSquash();

    /** Initiate a squash signal */
    void initiateSquash(IODynInstPtr faulty_inst);

    /** Squash all instructions younger than the given squash instruction */
    void doSquash(IODynInstPtr squash_inst);

    /** Commit head instruction from the given thread */
    void commitHead(ThreadID tid);

  private:
    /** Pointer to the main CPU */
    IOCPU* m_cpu_p;

    /** Number of threads */
    size_t m_num_threads;

    /** True if the stage is active */
    bool m_is_active;

    /** Max number of instructions that can be committed in a cycle */
    size_t m_commit_width;

    /** Queue of all instructions coming from writeback this cycle. This models
     * an N-entry bypass queue */
    std::queue<IODynInstPtr> m_insts;

    /** Max input queue's size */
    const size_t m_input_queue_size;

    /**
     * Time buffer interface
     */
    TimeBuffer<InstComm>::wire m_incoming_inst_wire;      // from IEW
    TimeBuffer<CreditComm>::wire m_outgoing_credit_wire;  // to IEW
    TimeBuffer<SquashComm>::wire m_outgoing_squash_wire;
    TimeBuffer<SquashComm>::wire m_incoming_squash_wire;
    TimeBuffer<InfoComm>::wire m_outgoing_info_wire;      // to Fetch/Rename

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
