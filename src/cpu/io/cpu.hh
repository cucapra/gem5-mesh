//-----------------------------------------------------------------------------
// cpu.hh
//-----------------------------------------------------------------------------
// In-order CPU model
//
// Author: Tuan Ta
// Date:  19/08/18

#ifndef __CPU_IO_CPU_HH__
#define __CPU_IO_CPU_HH__

#include <bitset>

#include "arch/generic/types.hh"
#include "arch/types.hh"
#include "base/statistics.hh"
#include "config/the_isa.hh"
#include "cpu/base.hh"
#include "cpu/io/comm.hh"
#include "cpu/io/commit.hh"
#include "cpu/io/decode.hh"
#include "cpu/io/dyn_inst.hh"
#include "cpu/io/fetch.hh"
#include "cpu/io/iew.hh"
#include "cpu/io/rename.hh"
#include "cpu/o3/free_list.hh"    // borrowed from O3
#include "cpu/o3/regfile.hh"      // borrowed from O3
#include "cpu/o3/rename_map.hh"   // borrowed from O3
#include "cpu/o3/scoreboard.hh"   // borrowed from O3
#include "cpu/timebuf.hh"
#include "params/IOCPU.hh"
#include "sim/process.hh"

class IOCPU : public BaseCPU
{
  public:
    using VecElem = TheISA::VecElem;
    using VecRegContainer = TheISA::VecRegContainer;
    using VecPredRegContainer = TheISA::VecPredRegContainer;

    /**
     * Enum types
     */
    enum Status {
      Running,      // at least one thread is up and running
      Idle,         // no thread is running
      SwitchedOut
    };

    /** Constructor */
    IOCPU(IOCPUParams* params);

    /** Destructor */
    ~IOCPU();

    /** Init sim object */
    void init() override;

    /** Start up this CPU */
    void startup() override;

    /** Wake up a given thread */
    void wakeup(ThreadID tid) override;

    /** Register stats */
    void regStats() override;

    /** Switch out from this CPU */
    void switchOut() override;

    /** Take over simulation from the given CPU */
    void takeOverFrom(BaseCPU* old_cpu) override;

    /** Activate the given thread context (e.g., a clone syscall is called) */
    void activateContext(ThreadID tid) override;

    /** Suspend the given thread context. Temporarily put a thread out of
     * schedule list (e.g., due to a futex wait syscall) */
    void suspendContext(ThreadID tid) override;

    /** Halt the given thread context. A thread exitted, and its context is no
     * longer valid (e.g., due to exit syscall) */
    void haltContext(ThreadID tid) override;

    /** Reset all micro-architectural states of the given thread as the thread
     * is being halted */
    void resetStates(ThreadID tid);

    /** Serialize thread */
    void serializeThread(CheckpointOut& cp, ThreadID tid) const override;

    /** Unserialize thread */
    void unserializeThread(CheckpointIn& cp, ThreadID tid) override;

    /** Wake up this CPU from Idle state */
    void wakeup();

    /** Suspend this CPU. Put it in Idle state */
    void suspend();

    /** Get reference to icache port */
    MasterPort& getInstPort() override;

    /** Get reference to dcache port */
    MasterPort& getDataPort() override;

    /** Tick */
    void tick();

    /** Sets the commit PC state of a specific thread. */
    void pcState(const TheISA::PCState& newPCState, ThreadID tid);

    /** Reads the commit PC state of a specific thread. */
    TheISA::PCState pcState(ThreadID tid);

    /** Trap to handle given fault */
    void trap(const Fault& fault, ThreadID tid, const StaticInstPtr& inst);

    /** Get decoder of a thread */
    TheISA::Decoder* getDecoderPtr(ThreadID tid);

    /** Get top-level system pointer */
    System* getSystemPtr();

    /** Get ISA for the given thread */
    TheISA::ISA* getISAPtr(ThreadID tid);

    /** Reads the commit PC of a specific thread. */
    Addr instAddr(ThreadID tid);

    /** Reads the commit micro PC of a specific thread. */
    MicroPC microPC(ThreadID tid);

    /** Reads the next PC of a specific thread. */
    Addr nextInstAddr(ThreadID tid);

    /** Return pointer to a thread context */
    ThreadContext* tcBase(ThreadID tid);

    /** Return and increment the seq num */
    InstSeqNum getAndIncrementInstSeq();

    /** Return the total number of dynamic instructions */
    Counter totalInsts() const;

    /** Return the total number of dynamic operations */
    Counter totalOps() const;

    /** Push a data memory request into LD/ST queue */
    Fault pushMemReq(IODynInst* inst, bool is_load, uint8_t* data,
                     unsigned int size, Addr addr, Request::Flags flags,
                     uint64_t* res, AtomicOpFunctor* amo_op = nullptr);

    /** Get cache line's size */
    size_t getCacheLineSize() const;

    /**
     * Get pointers to pipeline data structures
     */
    PhysRegFile* getPhysRegFilePtr();
    ROB* getROBPtr(ThreadID tid);
    Scoreboard* getScoreboardPtr();
    UnifiedFreeList* getFreeListPtr();
    UnifiedRenameMap* getRenameMapPtr(ThreadID tid);
    UnifiedRenameMap* getCommitRenameMapPtr(ThreadID tid);

    /**
     * Read/write to register file
     */
    RegVal readIntReg(PhysRegIdPtr phys_reg);
    void setIntReg(PhysRegIdPtr phys_reg, RegVal val);
    RegVal readArchIntReg(int reg_idx, ThreadID tid);
    void setArchIntReg(int reg_idx, RegVal val, ThreadID tid);

    RegVal readFloatReg(PhysRegIdPtr phys_reg);
    void setFloatReg(PhysRegIdPtr phys_reg, RegVal val);
    RegVal readArchFloatReg(int reg_idx, ThreadID tid);
    void setArchFloatReg(int reg_idx, RegVal val, ThreadID tid);

    const VecRegContainer& readVecReg(PhysRegIdPtr reg_idx) const;
    const VecRegContainer& readArchVecReg(int reg_idx, ThreadID tid) const;

    const VecElem& readVecElem(PhysRegIdPtr reg_idx) const;
    const VecElem& readArchVecElem(const RegIndex& reg_idx,
                                   const ElemIndex& ldx, ThreadID tid) const;

    const VecPredRegContainer& readVecPredReg(PhysRegIdPtr reg_idx) const;
    const VecPredRegContainer& readArchVecPredReg(int reg_idx,
                                                  ThreadID tid) const;

    VecRegContainer& getWritableVecReg(PhysRegIdPtr reg_idx);
    VecRegContainer& getWritableArchVecReg(int reg_idx, ThreadID tid);

    VecPredRegContainer& getWritableVecPredReg(PhysRegIdPtr reg_idx);
    VecPredRegContainer& getWritableArchVecPredReg(int reg_idx, ThreadID tid);

    template<typename VecElem, int LaneIdx>
    VecLaneT<VecElem, true> readVecLane(PhysRegIdPtr phys_reg) const;
    template<typename VecElem>
    VecLaneT<VecElem, true> readVecLane(PhysRegIdPtr phys_reg) const;
    template<typename VecElem>
    VecLaneT<VecElem, true> readArchVecLane(int reg_idx, int lId,
                                            ThreadID tid) const;

    void setVecReg(PhysRegIdPtr reg_idx, const VecRegContainer& val);
    void setArchVecReg(int reg_idx, const VecRegContainer& val, ThreadID tid);

    void setVecElem(PhysRegIdPtr reg_idx, const VecElem& val);
    void setArchVecElem(const RegIndex& reg_idx, const ElemIndex& ldx,
                        const VecElem& val, ThreadID tid);

    void setVecPredReg(PhysRegIdPtr reg_idx, const VecPredRegContainer& val);
    void setArchVecPredReg(int reg_idx, const VecPredRegContainer& val,
                           ThreadID tid);

    template<typename LD>
    void setVecLane(PhysRegIdPtr phys_reg, const LD& val);
    template<typename LD>
    void setArchVecLane(int reg_idx, int lId, ThreadID tid, const LD& val);

    RegVal readMiscRegNoEffect(int misc_reg, ThreadID tid) const;
    RegVal readMiscReg(int misc_reg, ThreadID tid);
    void setMiscRegNoEffect(int misc_reg, RegVal val, ThreadID tid);
    void setMiscReg(int misc_reg, RegVal val, ThreadID tid);

    RegVal readCCReg(PhysRegIdPtr phys_reg);
    void setCCReg(PhysRegIdPtr phys_reg, RegVal val);

    RegVal readArchCCReg(int reg_idx, ThreadID tid);
    void setArchCCReg(int reg_idx, RegVal val, ThreadID tid);

    /**
     * Stats-related functions
     */

    /** Increment the number of committed insts */
    void incrNumCommittedInsts(ThreadID tid);

  private:
    /**
     * Icache port
     */
    class IcachePort : public MasterPort
    {
      public:
        IcachePort(Fetch* _fetch_p, IOCPU* _cpu_p, int _num_cache_ports);
        ~IcachePort() = default;

        /** Send timing request */
        virtual bool sendTimingReq(PacketPtr pkt);

        /** Receive timing response */
        virtual bool recvTimingResp(PacketPtr pkt);

        /** Receive retry request */
        virtual void recvReqRetry();

        /** Reset the number of used ports. This will call CPU to retry if
         * necessary */
        void reset();

      public:
        Fetch* fetch_p;
        const int num_cache_ports;
        int num_used_cache_ports;
        bool need_retry;
    };

    /**
     * Dcache port
     */
    class DcachePort : public MasterPort
    {
      public:
        DcachePort(MemUnit* _mem_unit_p, IOCPU* _cpu_p, int _num_cache_ports);
        ~DcachePort() = default;

        /** Send timing request */
        virtual bool sendTimingReq(PacketPtr pkt);

        /** Receive timing response */
        virtual bool recvTimingResp(PacketPtr pkt);

        /** Receive retry request */
        virtual void recvReqRetry();

        /** Reset the number of used ports. This will call CPU to retry if
         * necessary */
        void reset();

      public:
        MemUnit* mem_unit_p;
        const int num_cache_ports;
        int num_used_cache_ports;
        bool need_retry;
    };

    /** Schedule and un-schedule tick event */
    void scheduleTickEvent(Cycles delay);
    void unscheduleTickEvent();

    /** Print line trace */
    void linetrace();

  public:
    /** Overall status of this CPU */
    Status status;

    /** Pointers to TLBs */
    BaseTLB *itb;
    BaseTLB *dtb;

  private:
    /** Pointer to the central system */
    System* m_system_p;

    /** Number of threads */
    size_t m_num_threads;

    /** Main tick event */
    EventFunctionWrapper m_tick_event;

    /**
     * Pipeline stages
     */
    Fetch  m_fetch;
    Decode m_decode;
    Rename m_rename;
    IEW    m_iew;     // Issue, Execute, Writeback
    Commit m_commit;

    /**
     * Inter-stage communication buffers
     *    m_inst_buffer: store instructions passing through the pipeline
     *    m_credit_buffer: store credit signals
     *    m_squash_buffer: store squash signals
     *    m_info_buffer: store information (e.g., committed pc) traveling
     *                   backward
     */
    TimeBuffer<InstComm>   m_inst_buffer;
    TimeBuffer<CreditComm> m_credit_buffer;
    TimeBuffer<SquashComm> m_squash_buffer;
    TimeBuffer<InfoComm>   m_info_buffer;

    /** Single global physical register file */
    PhysRegFile m_reg_file;

    /** Per-thread ROB */
    std::vector<ROB> m_robs;

    /** Single global scoreboard */
    Scoreboard m_scoreboard;

    /** Single global free register list */
    UnifiedFreeList m_free_list;

    /** Per-thread rename maps */
    std::vector<UnifiedRenameMap> m_rename_maps;

    /** Per-thread rename maps for committed instructions */
    std::vector<UnifiedRenameMap> m_commit_rename_maps;

    /** Icache port */
    IcachePort m_icache_port;

    /** Dcache port */
    DcachePort m_dcache_port;

    /** IDs of all active threads */
    std::list<ThreadID> m_active_thread_ids;

    /** List of all ISA instances (one per thread) */
    std::vector<TheISA::ISA*> m_isa_list;

    /** This CPU's sequence number counter */
    InstSeqNum m_global_seq_num;

    /**
     * Stats variables
     */

    /** Last active cycle */
    Cycles m_last_active_cycle;

    /** Stat for total number of cycles the CPU spends descheduled. */
    Stats::Scalar m_idle_cycles;

    /** Stat for the number of committed instructions per thread. */
    Stats::Vector m_committed_insts;

    /** Stat for the CPI per thread. */
    Stats::Formula m_cpi;

    /** Stat for the total CPI. */
    Stats::Formula m_total_cpi;

    /** Stat for the IPC per thread. */
    Stats::Formula m_ipc;

    /** Stat for the total IPC. */
    Stats::Formula m_total_ipc;

    /** number of integer register file accesses */
    Stats::Scalar m_int_regfile_reads;
    Stats::Scalar m_int_regfile_writes;

    /** number of float register file accesses */
    Stats::Scalar m_fp_regfile_reads;
    Stats::Scalar m_fp_regfile_writes;

    /** number of misc */
    Stats::Scalar m_misc_regfile_reads;
    Stats::Scalar m_misc_regfile_writes;
};

template<typename VecElem, int LaneIdx>
VecLaneT<VecElem, true>
IOCPU::readVecLane(PhysRegIdPtr phys_reg) const
{
  return m_reg_file.readVecLane<VecElem, LaneIdx>(phys_reg);
}

template<typename VecElem>
VecLaneT<VecElem, true>
IOCPU::readVecLane(PhysRegIdPtr phys_reg) const
{
  return m_reg_file.readVecLane<VecElem>(phys_reg);
}

template<typename VecElem>
VecLaneT<VecElem, true>
IOCPU::readArchVecLane(int reg_idx, int lId, ThreadID tid) const
{
  PhysRegIdPtr phys_reg = m_commit_rename_maps[tid].
                                        lookup(RegId(VecRegClass, reg_idx));
  return readVecLane<VecElem>(phys_reg);
}

template<typename LD>
void
IOCPU::setVecLane(PhysRegIdPtr phys_reg, const LD& val)
{
  m_reg_file.setVecLane(phys_reg, val);
}

template<typename LD>
void
IOCPU::setArchVecLane(int reg_idx, int lId, ThreadID tid, const LD& val)
{
  PhysRegIdPtr phys_reg = m_commit_rename_maps[tid].
                                  lookup(RegId(VecRegClass, reg_idx));
  setVecLane(phys_reg, val);
}

#endif // CPU_IO_CPU_HH
