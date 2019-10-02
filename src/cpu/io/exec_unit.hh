//=============================================================================
// exec_unit.hh
//=============================================================================
// Abstract execution unit
//
// Author: Tuan Ta
// Date:   19/09/10

#ifndef __CPU_IO_EXEC_UNIT_HH__
#define __CPU_IO_EXEC_UNIT_HH__

#include <iomanip>
#include <sstream>

#include "cpu/io/dyn_inst.hh"
#include "cpu/o3/scoreboard.hh"

class ExecUnit
{
  public:
    /** Destructor */
    virtual ~ExecUnit() = default;

    /** Return name of this execution unit */
    virtual const std::string name() const = 0;

    /** Set pointer to scoreboard (used when an exec unit wants to bypass
     * values early before writeback stage) */
    virtual void setScoreboardPtr(Scoreboard* scoreboard_p) = 0;

    /** Insert an instruction into the unit (from Issue stage). This assumes
     * the unit is able to take in new instruction */
    virtual void insert(IODynInstPtr inst) = 0;

    /** Remove the next completed instruction from the unit */
    virtual IODynInstPtr removeCompletedInst() = 0;

    /** Return true if the unit has any completed instruction waiting to be
     * written back */
    virtual bool hasInstsToWriteBack() const = 0;

    /** Return true if the unit is full and can't take any further inst */
    virtual bool isBusy() const = 0;

    /** Tick by advancing this unit */
    virtual void tick() = 0;

    /** Squash all instructions younger than the squash instruction */
    virtual void doSquash(IODynInstPtr squash_inst) = 0;

    /** Linetrace */
    virtual void linetrace(std::stringstream& ss) = 0;
};

#endif // __CPU_IO_EXEC_UNIT_HH__
