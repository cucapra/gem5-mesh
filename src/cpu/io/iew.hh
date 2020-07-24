//=============================================================================
// iew.hh
//=============================================================================
// Combination of 3 stages: Issue, Execute and Writeback
//
//    issue stage => multiple exec pipes/units => writeback stage
//
// - Issue:
//    + Check dependencies
//    + Issue instruction to an execution pipe/unit
// - Exec pipes/units
//    + Take and execute issued instructions in one or more cycles
// - Writeback
//    + Take executed instructions
//    + Update register file
//    + Initiate squash signal (mispredicted branch target)
//
// Author: Tuan Ta
// Date  : 19/08/21

#ifndef __CPU_IO_IEW_HH__
#define __CPU_IO_IEW_HH__

#include <iomanip>
#include <queue>
#include <sstream>
#include <vector>

#include "cpu/io/comm.hh"
#include "cpu/io/dyn_inst.hh"
#include "cpu/io/mem_unit.hh"
#include "cpu/io/pipelined_exec_unit.hh"
#include "cpu/io/rob.hh"
#include "cpu/io/unpipelined_exec_unit.hh"
#include "cpu/o3/scoreboard.hh"
#include "cpu/timebuf.hh"
#include "params/IOCPU.hh"
#include "cpu/io/stage.hh"

/**
 * IEW stage: Issue, Execute, Writeback stages
 */
class IEW : public Stage
{
  public:
    IEW(IOCPU* _cpu_p, IOCPUParams* params, size_t in_size, size_t out_size);
    ~IEW();

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

    /** Get pointer to memory unit */
    MemUnit* getMemUnitPtr();

    /** Line trace */
    void linetrace(std::stringstream& ss) override;

    /** Accessor to predicate */
    bool getPred() const;
    void setPred(bool val);

  private:
    enum IEWStatus {
      // Issue stage
      IssueSquashed,
      IssueInitStall,
      IssueBusy,
      // Writeback stage
      WBSquashed,
      WBStalled,
      WBInitSquash,
      WBBusy,
      NumStatus
    };

  private:
    /** Do issue */
    void doIssue();

    /** Do execute */
    void doExecute();

    /** Do writeback */
    void doWriteback();

    /** Do squash */
    void doSquash(SquashComm::BaseSquash &squashInfo, StageIdx initiator) override;

    /** Initiate a squash signal */
    void initiateSquash(const IODynInstPtr mispred_inst);

    /** Place the given instruction into the buffer to the next stage */
    void sendInstToNextStage(IODynInstPtr inst) override;

  private:
    /** Number of threads */
    size_t m_num_threads;

    /** Issue width */
    size_t m_issue_width;

    /** Writeback width */
    size_t m_wb_width;

    /** Vector of execution units */
    std::vector<ExecUnit*> m_exec_units;

    /** List of exec units that are traced */
    std::vector<ExecUnit*> m_traced_exec_units;

    /** Map of Op_Class and index to the execution unit vector */
    std::unordered_map<OpClass, size_t> m_op_to_unit_map;

    /** Names of all execution units */
    std::vector<std::string> m_exec_unit_names;

    /** Pointer to the memory unit */
    MemUnit* m_mem_unit_p;

    /** Index of the next exec unit to be selected to write back (used in
     * round-robin selection in Writeback stage) */
    size_t m_next_wb_exec_unit_idx;
    
    /** Since we can't squash the pipeline and too expensive to atomically update
     * We need to update the PC of an instruction before it enters the ALUs
     * We'll do this by keeping a register in the EXE stage, read to update inst pc
     * Write it at the end of the int ALU (1 cycle) so ready for the next instruction
     * Important that int ALU is only 1 cycle for this to work
     * */
    std::vector<TheISA::PCState> m_trace_pcs;

    /** ROBs */
    std::vector<ROB*> m_robs;

    /** Global scoreboard (for all threads) */
    Scoreboard* m_scoreboard_p;

    /** Predicate flag. If set and vec mode then do not perform effect of instruction */
    bool m_pred_flag;

#ifdef DEBUG
    /** Stage's status (for line trace) */
    std::bitset<IEWStatus::NumStatus> m_stage_status;

    /** List of instructions processed in the current cycle */
    std::vector<IODynInstPtr> m_issued_insts;
    std::vector<IODynInstPtr> m_wb_insts;
#endif

  private:
    Stats::Scalar m_dep_stalls;
    Stats::Scalar m_mem_barrier_stalls;
    Stats::Scalar m_commit_buf_stalls;
    Stats::Scalar m_exe_unit_busy_stalls;
    Stats::Vector2d m_stall_rob_head_insts;
    Stats::Vector2d iew_dep_insts;
    Stats::Vector2d iew_dep_on;
    Stats::Scalar m_frame_start_tokens;
    Stats::Scalar m_frame_start_remem;

    Stats::Vector2d executed_insts;
};

#endif // CPU_IO_IEW_HH
