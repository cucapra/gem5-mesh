//-----------------------------------------------------------------------------
// rename.hh
//-----------------------------------------------------------------------------
// Rename stage for IO CPU
//
// Author: Tuan Ta
// Date  : 19/08/22

#ifndef __CPU_IO_RENAME_HH__
#define __CPU_IO_RENAME_HH__

#include <iomanip>
#include <queue>
#include <sstream>

#include "cpu/io/comm.hh"
#include "cpu/io/dyn_inst.hh"
#include "cpu/o3/free_list.hh"
#include "cpu/o3/rename_map.hh"
#include "cpu/timebuf.hh"
#include "params/IOCPU.hh"
#include "cpu/io/stage.hh"

class Rename : public Stage
{
  public:
    Rename(IOCPU* _cpu_p, IOCPUParams* params);
    ~Rename() = default;

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

  private:
    enum RenameStatus {
      Squashed,
      InitStall,
      Stalled,
      Busy,
      NumStatus
    };

    struct RenameHistory {
      RenameHistory(InstSeqNum _instSeqNum,
                    const RegId& _archReg,
                    PhysRegIdPtr _newPhysReg,
                    PhysRegIdPtr _prevPhysReg)
          : instSeqNum(_instSeqNum), archReg(_archReg),
            newPhysReg(_newPhysReg), prevPhysReg(_prevPhysReg)
      { }

      /** The sequence number of the instruction that renamed. */
      InstSeqNum instSeqNum;
      /** The architectural register index that was renamed. */
      RegId archReg;
      /** The new physical register that the arch. register is renamed to. */
      PhysRegIdPtr newPhysReg;
      /** The old physical register that the arch. register was renamed to.
       */
      PhysRegIdPtr prevPhysReg;
    };

  private:
    /** Do rename */
    void doRename();

    /** Rename src regs */
    void renameSrcRegs(const IODynInstPtr& inst, ThreadID tid);

    /** Rename dest regs */
    void renameDestRegs(const IODynInstPtr& inst, ThreadID tid);

    /** Check squash signal. Return true if we're being squashed */
    bool checkSquash();

    /** Read info from subsequent stages */
    void readInfo();

    /** Squash all instructions younger than the given squash instruction */
    void doSquash(IODynInstPtr squash_inst);

    /** Place the given instruction into the buffer to the next stage */
    void sendInstToNextStage(IODynInstPtr inst) override;

  private:
    /** Number of threads */
    size_t m_num_threads;

    /** Max number of instructions that can be renamed in 1 cycle */
    size_t m_rename_width;

    /** Per-thread rename maps */
    std::vector<UnifiedRenameMap*> m_rename_maps;

    /** Global free register list */
    UnifiedFreeList* m_free_list_p;

    /** Rename history */
    std::vector<std::list<RenameHistory>> m_history_buffers;

#ifdef DEBUG
    /** Stage's status (for line trace) */
    std::bitset<RenameStatus::NumStatus> m_stage_status;

    /** List of instructions processed in the current cycle (for line trace) */
    std::vector<IODynInstPtr> m_renamed_insts;
#endif
};

#endif
