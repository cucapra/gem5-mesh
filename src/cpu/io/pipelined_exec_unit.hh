//=============================================================================
// pipelined_exec_unit.hh
//=============================================================================
// Pipelined execution unit
//
// Author: Tuan Ta
// Date:   19/09/10

#ifndef __CPU_IO_PIPELINED_EXEC_UNIT_HH__
#define __CPU_IO_PIPELINED_EXEC_UNIT_HH__

#include "cpu/io/exec_unit.hh"

class PipelinedExecUnit : public ExecUnit
{
  public:
    PipelinedExecUnit(const char* _iew_name, const char* _name,
                      size_t num_stages);
    ~PipelinedExecUnit() override = default;

    /** Return name of this execution unit */
    const std::string name() const override;

    /** Set pointer to scoreboard (used when an exec unit wants to bypass
     * values early before writeback stage) */
    void setScoreboardPtr(Scoreboard* scoreboard_p) override;

    /** Insert an instruction into the unit (from Issue stage). This assumes
     * the unit is able to take in new instruction */
    void insert(IODynInstPtr inst) override;

    /** Remove the next completed instruction from the unit */
    IODynInstPtr removeCompletedInst() override;

    /** Return true if the unit has any completed instruction waiting to be
     * written back */
    bool hasInstsToWriteBack() const override;

    /** Return true if the unit is full and can't take any further inst */
    bool isBusy() const override;

    /** Tick by advancing this unit */
    void tick() override;

    /** Squash all instructions younger than the squash instruction */
    void doSquash(IODynInstPtr squash_inst) override;

    /** Linetrace */
    void linetrace(std::stringstream& ss) override;

  private:
    enum Status {
      Squashed,   // Squashing
      Busy,       // Executed one inst
      Stalled,    // Stalled by WB stage
      NumStatus
    };

  private:
    /** Name of IEW controlling this unit */
    const std::string m_iew_name;

    /** Name of this unit */
    const std::string m_name;

    /** True if do early bypass */
    bool m_early_bypass;

    /** Pointer to CPU's scoreboard (for early bypass) */
    Scoreboard* m_scoreboard_p;

    /** Number of pipeline stages */
    const size_t m_num_stages;

    /** Incoming instruction (nullptr if no incoming inst) */
    IODynInstPtr m_incoming_inst;

    /** A vector of instructions to model an N-stage pipeline */
    std::vector<IODynInstPtr> m_pipeline;

    /** Number of in-flight operations */
    size_t m_num_inflight_ops;

#ifdef DEBUG
    /** Unit status */
    std::bitset<PipelinedExecUnit::NumStatus> m_status;

    /** List of insts that are taken into this unit in the current cycle */
    std::vector<IODynInstPtr> m_executed_insts;
#endif
};

#endif // __CPU_IO_PIPELINED_EXEC_UNIT_HH__
