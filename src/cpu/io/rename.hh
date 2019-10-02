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

class IOCPU;

class Rename
{
  public:
    Rename(IOCPU* _cpu_p, IOCPUParams* params);
    ~Rename() = default;

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

    /** Put all instructions to be processed this cycle into m_insts queue */
    void queueInsts();

    /** Check squash signal. Return true if we're being squashed */
    bool checkSquash();

    /** Read credit signal */
    void readCredits();

    /** Read info from subsequent stages */
    void readInfo();

    /** Squash all instructions younger than the given squash instruction */
    void doSquash(IODynInstPtr squash_inst);

    /** Place the given instruction into the buffer to the next stage */
    void sendInstToNextStage(IODynInstPtr inst);

  private:
    /** Pointer to the main CPU */
    IOCPU* m_cpu_p;

    /** Number of threads */
    size_t m_num_threads;

    /** Is this stage active? */
    bool m_is_active;

    /** N-entry incoming instruction queue */
    std::queue<IODynInstPtr> m_insts;

    /** Max input queue's size */
    const size_t m_input_queue_size;

    /** Max number of instructions that can be renamed in 1 cycle */
    size_t m_rename_width;

    /** Max number of credits. This is equal to the size of input buffer in the
     * next stage */
    const size_t m_max_num_credits;

    /** Number of credits for forward communication */
    size_t m_num_credits;

    /**
     * Time buffer interface
     */
    TimeBuffer<InstComm>::wire m_outgoing_inst_wire;     // to IEW
    TimeBuffer<InstComm>::wire m_incoming_inst_wire;     // from Decode

    TimeBuffer<CreditComm>::wire m_outgoing_credit_wire;   // to Decode
    TimeBuffer<CreditComm>::wire m_incoming_credit_wire;   // from IEW

    TimeBuffer<SquashComm>::wire m_incoming_squash_wire; // from IEW/Commit

    TimeBuffer<InfoComm>::wire m_incoming_info_wire;     // from Commit

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
