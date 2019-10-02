//=============================================================================
// unpipelined_exec_unit.hh
//=============================================================================
// Un-Pipelined execution unit
//
// Author: Tuan Ta
// Date:   19/09/10

#ifndef __CPU_IO_UNPIPELINED_EXEC_UNIT_HH__
#define __CPU_IO_UNPIPELINED_EXEC_UNIT_HH__

#include "cpu/io/exec_unit.hh"

class UnpipelinedExecUnit : public ExecUnit
{
  public:
    UnpipelinedExecUnit(const char* _iew_name, const char* _name,
                        size_t _latency);
    ~UnpipelinedExecUnit() override = default;

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

    /** Name of the unit */
    const std::string m_name;

    /** True if do early bypass */
    bool m_early_bypass;

    /** Pointer to CPU's scoreboard (for early bypass) */
    Scoreboard* m_scoreboard_p;

    /** Latency to complete an instruction */
    Cycles m_latency;

    /** Current in-flight instruction */
    IODynInstPtr m_inst;

    /** Current cycle count for the current in-flight instruction */
    Cycles m_cycle_count;

#ifdef DEBUG
    /** Unit status */
    std::bitset<UnpipelinedExecUnit::NumStatus> m_status;

    /** Executed inst */
    IODynInstPtr m_executed_inst;
#endif
};

#endif // CPU_IO_UNPIPELINED_EXEC_UNIT_HH
