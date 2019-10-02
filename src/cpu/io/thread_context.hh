//=============================================================================
// thread_context.hh
//=============================================================================
// Thread context for IO CPU model
//
// Author: Tuan Ta
// Date:   19/08/28

#ifndef __CPU_IO_THREAD_CONTEXT_HH__
#define __CPU_IO_THREAD_CONTEXT_HH__

#include "arch/decoder.hh"
#include "arch/generic/traits.hh"
#include "arch/kernel_stats.hh"
#include "arch/registers.hh"
#include "arch/utility.hh"
#include "config/the_isa.hh"
#include "cpu/quiesce_event.hh"
#include "cpu/thread_context.hh"
#include "cpu/thread_state.hh"
#include "debug/IOCPU.hh"

class IOThreadContext : public ThreadContext
{
  public:
    IOThreadContext(IOCPU* _cpu_p, ThreadID _tid, Process* _process)
        : m_cpu_p(_cpu_p),
          m_thread_p(new ThreadState(_cpu_p, _tid, _process))
    {
      assert(m_cpu_p && m_thread_p);
    }

    ~IOThreadContext()
    {
      delete m_thread_p;
    }

    /** Get pointer to IO CPU */
    BaseCPU* getCpuPtr() override
    {
      return m_cpu_p;
    }

    /** Get CPU ID */
    int cpuId() const override
    {
      return m_cpu_p->cpuId();
    }

    /** Get socket ID */
    uint32_t socketId() const override
    {
      return m_cpu_p->socketId();
    }

    /** Get thread ID */
    int threadId() const override
    {
      return m_thread_p->threadId();
    }

    /** Set thread ID */
    void setThreadId(int id) override
    {
      m_thread_p->setThreadId(id);
    }

    /** Get context ID */
    int contextId() const override
    {
      return m_thread_p->contextId();
    }

    /** Set context ID */
    void setContextId(int id) override
    {
      m_thread_p->setContextId(id);
    }

    /** Get pointer to I-TLB */
    BaseTLB *getITBPtr() override
    {
      return m_cpu_p->itb;
    }

    /** Get pointer to D-TLB */
    BaseTLB *getDTBPtr() override
    {
      return m_cpu_p->dtb;
    }

    /** Get pointer to checker CPU */
    CheckerCPU *getCheckerCpuPtr() override
    {
      warn("Checker CPU is not supported in IO CPU\n");
      return nullptr;
    }

    /** Get pointer to Decoder */
    TheISA::Decoder *getDecoderPtr() override
    {
      return m_cpu_p->getDecoderPtr(m_thread_p->threadId());
    }

    /** Get pointer to top-level system */
    System *getSystemPtr() override
    {
      return m_cpu_p->getSystemPtr();
    }

    /** Get kernel stats */
    TheISA::Kernel::Statistics *getKernelStats() override
    {
      warn("Kernel stats are not supported in IO CPU\n");
      return nullptr;
    }

    /** Get physical proxy port */
    PortProxy &getPhysProxy() override
    {
      return m_thread_p->getPhysProxy();
    }

    /** Get virtual proxy port */
    FSTranslatingPortProxy &getVirtProxy() override
    {
      return m_thread_p->getVirtProxy();
    }

    /**
     * Initialise the physical and virtual port proxies and tie them to
     * the data port of the CPU.
     *
     * tc ThreadContext for the virtual-to-physical translation
     */
    void initMemProxies(ThreadContext *tc) override
    {
      m_thread_p->initMemProxies(tc);
    }

    /** Get syscall-emulation memory proxy */
    SETranslatingPortProxy &getMemProxy() override
    {
      return m_thread_p->getMemProxy();
    }

    /** Get pointer to the corresponding process */
    Process *getProcessPtr() override
    {
      return m_thread_p->getProcessPtr();
    }

    /** Set pointer to the corresponding process*/
    void setProcessPtr(Process *p) override
    {
      m_thread_p->setProcessPtr(p);
    }

    /** Get thread's status */
    Status status() const override
    {
      return m_thread_p->status();
    }

    /** Set thread's status */
    void setStatus(Status new_status) override
    {
      m_thread_p->setStatus(new_status);
    }

    /** Activate this thread context */
    void activate() override
    {
      assert(status() == Suspended || status() == Halted);
      m_cpu_p->activateContext(m_thread_p->threadId());
      setStatus(Active);
    }

    /** Suspend this thread context */
    void suspend() override
    {
      assert(status() == Active);
      m_cpu_p->suspendContext(m_thread_p->threadId());
      setStatus(Suspended);
    }

    /** Halt this thread context */
    void halt() override
    {
      // a context can be halted when it's suspended (e.g., at the end of
      // simulation), so we don't assert its active status here.
      m_cpu_p->haltContext(m_thread_p->threadId());
      setStatus(Halted);
    }

    /** Dump func profile */
    void dumpFuncProfile() override
    {
      panic("dumpFuncProfile is not supported in IO CPU\n");
    }

    /** Take over from an old thread context */
    void takeOverFrom(ThreadContext *old_context) override
    {
      ::takeOverFrom(*this, *old_context);
      TheISA::Decoder *newDecoder = getDecoderPtr();
      TheISA::Decoder *oldDecoder = old_context->getDecoderPtr();
      newDecoder->takeOverFrom(oldDecoder);

      m_thread_p->kernelStats = old_context->getKernelStats();
      m_thread_p->funcExeInst = old_context->readFuncExeInst();
    }

    /** Register stats */
    void regStats(const std::string &name) override
    {
      assert(!FullSystem);
    }

    /** Get pointer to a quiesce event */
    EndQuiesceEvent *getQuiesceEvent() override
    {
      return m_thread_p->quiesceEvent;
    }

    /** Last tick this thread was activated */
    Tick readLastActivate() override
    {
      return m_thread_p->lastActivate;
    }

    /** Last tick this thread was suspended */
    Tick readLastSuspend() override
    {
      return m_thread_p->lastSuspend;
    }

    /** */
    void profileClear() override
    {
      m_thread_p->profileClear();
    }

    /** */
    void profileSample() override
    {
      m_thread_p->profileSample();
    }

    /** */
    void copyArchRegs(ThreadContext *tc) override
    {
      // copy registers
      TheISA::copyRegs(tc, this);

      if (!FullSystem)
        m_thread_p->funcExeInst = tc->readFuncExeInst();
    }

    /** */
    void clearArchRegs() override
    {
      m_cpu_p->getISAPtr(m_thread_p->threadId())->clear();
    }

    /** */
    RegVal readIntReg(int reg_idx) override
    {
      return readIntRegFlat(flattenRegId(RegId(IntRegClass, reg_idx)).index());
    }

    /** */
    RegVal readFloatReg(int reg_idx) override
    {
      return readFloatRegFlat(
                    flattenRegId(RegId(FloatRegClass, reg_idx)).index());
    }

    /** */
    const VecRegContainer& readVecReg(const RegId& reg) const override
    {
      return readVecRegFlat(flattenRegId(reg).index());
    }

    /** */
    VecRegContainer& getWritableVecReg(const RegId& reg) override
    {
      return getWritableVecRegFlat(flattenRegId(reg).index());
    }

    /**
     * Vector Register Lane Interfaces.
     */

    /** Reads source vector 8bit operand. */
    ConstVecLane8 readVec8BitLaneReg(const RegId& reg) const override
    {
      return readVecLaneFlat<uint8_t>(flattenRegId(reg).index(),
                                      reg.elemIndex());
    }

    /** Reads source vector 16bit operand. */
    ConstVecLane16 readVec16BitLaneReg(const RegId& reg) const override
    {
      return readVecLaneFlat<uint16_t>(flattenRegId(reg).index(),
                                       reg.elemIndex());
    }

    /** Reads source vector 32bit operand. */
    ConstVecLane32 readVec32BitLaneReg(const RegId& reg) const override
    {
      return readVecLaneFlat<uint32_t>(flattenRegId(reg).index(),
                                       reg.elemIndex());
    }

    /** Reads source vector 64bit operand. */
    ConstVecLane64 readVec64BitLaneReg(const RegId& reg) const override
    {
      return readVecLaneFlat<uint64_t>(flattenRegId(reg).index(),
                                       reg.elemIndex());
    }

    /** Write a lane of the destination vector register. */
    void setVecLane(const RegId& reg,
                    const LaneData<LaneSize::Byte>& val) override
    {
      return setVecLaneFlat(flattenRegId(reg).index(), reg.elemIndex(), val);
    }

    void setVecLane(const RegId& reg,
                    const LaneData<LaneSize::TwoByte>& val) override
    {
      return setVecLaneFlat(flattenRegId(reg).index(), reg.elemIndex(), val);
    }

    void setVecLane(const RegId& reg,
            const LaneData<LaneSize::FourByte>& val) override
    {
      return setVecLaneFlat(flattenRegId(reg).index(), reg.elemIndex(), val);
    }

    void setVecLane(const RegId& reg,
            const LaneData<LaneSize::EightByte>& val) override
    {
      return setVecLaneFlat(flattenRegId(reg).index(), reg.elemIndex(), val);
    }

    const VecElem& readVecElem(const RegId& reg) const override
    {
      return readVecElemFlat(flattenRegId(reg).index(), reg.elemIndex());
    }

    const VecPredRegContainer& readVecPredReg(const RegId& reg) const override
    {
      return readVecPredRegFlat(flattenRegId(reg).index());
    }

    VecPredRegContainer& getWritableVecPredReg(const RegId& reg) override
    {
      return getWritableVecPredRegFlat(flattenRegId(reg).index());
    }

    RegVal readCCReg(int reg_idx) override
    {
      return readCCRegFlat(flattenRegId(RegId(CCRegClass, reg_idx)).index());
    }

    void setIntReg(int reg_idx, RegVal val) override
    {
      setIntRegFlat(flattenRegId(RegId(IntRegClass, reg_idx)).index(), val);
    }

    void setFloatReg(int reg_idx, RegVal val) override
    {
      setFloatRegFlat(flattenRegId(RegId(FloatRegClass, reg_idx)).index(),
                      val);
    }

    void setVecReg(const RegId& reg, const VecRegContainer& val) override
    {
      setVecRegFlat(flattenRegId(reg).index(), val);
    }

    void setVecElem(const RegId& reg, const VecElem& val) override
    {
      setVecElemFlat(flattenRegId(reg).index(), reg.elemIndex(), val);
    }

    void setVecPredReg(const RegId& reg,
                       const VecPredRegContainer& val) override
    {
      setVecPredRegFlat(flattenRegId(reg).index(), val);
    }

    void setCCReg(int reg_idx, RegVal val) override
    {
      setCCRegFlat(flattenRegId(RegId(CCRegClass, reg_idx)).index(), val);
    }

    TheISA::PCState pcState() override
    {
      return m_cpu_p->pcState(m_thread_p->threadId());
    }

    void pcState(const TheISA::PCState &val) override
    {
      m_cpu_p->pcState(val, m_thread_p->threadId());
    }

    void pcStateNoRecord(const TheISA::PCState &val) override
    {
      m_cpu_p->pcState(val, m_thread_p->threadId());
    }

    Addr instAddr() override
    {
      return m_cpu_p->instAddr(m_thread_p->threadId());
    }

    Addr nextInstAddr() override
    {
      return m_cpu_p->nextInstAddr(m_thread_p->threadId());
    }

    MicroPC microPC() override
    {
      return m_cpu_p->microPC(m_thread_p->threadId());
    }

    RegVal readMiscRegNoEffect(int misc_reg) const override
    {
      return m_cpu_p->readMiscRegNoEffect(misc_reg, m_thread_p->threadId());
    }

    RegVal readMiscReg(int misc_reg) override
    {
      return m_cpu_p->readMiscReg(misc_reg, m_thread_p->threadId());
    }

    void setMiscRegNoEffect(int misc_reg, RegVal val) override
    {
      m_cpu_p->setMiscRegNoEffect(misc_reg, val, m_thread_p->threadId());
    }

    void setMiscReg(int misc_reg, RegVal val) override
    {
      m_cpu_p->setMiscReg(misc_reg, val, m_thread_p->threadId());
    }

    RegId flattenRegId(const RegId& regId) const override
    {
      return m_cpu_p->getISAPtr(m_thread_p->threadId())->flattenRegId(regId);
    }

    // Also not necessarily the best location for these two. Hopefully will go
    // away once we decide upon where st cond failures goes.
    unsigned readStCondFailures() override
    {
      return m_thread_p->storeCondFailures;
    }

    void setStCondFailures(unsigned sc_failures) override
    {
      m_thread_p->storeCondFailures = sc_failures;
    }

    // Same with st cond failures.
    Counter readFuncExeInst() override
    {
      return m_thread_p->funcExeInst;
    }

    void syscall(int64_t callnum, Fault *fault) override
    {
      (m_thread_p->getProcessPtr())->syscall(callnum, this, fault);
    }

    /**
     * Flat register interfaces
     *
     * Some architectures have different registers visible in
     * different modes. Such architectures "flatten" a register (see
     * flattenRegId()) to map it into the
     * gem5 register file. This interface provides a flat interface to
     * the underlying register file, which allows for example
     * serialization code to access all registers.
     */

    RegVal readIntRegFlat(int idx) override
    {
      return m_cpu_p->readArchIntReg(idx, m_thread_p->threadId());
    }

    void setIntRegFlat(int idx, RegVal val) override
    {
      m_cpu_p->setArchIntReg(idx, val, m_thread_p->threadId());
    }

    RegVal readFloatRegFlat(int idx) override
    {
      return m_cpu_p->readArchFloatReg(idx, m_thread_p->threadId());
    }

    void setFloatRegFlat(int idx, RegVal val) override
    {
      m_cpu_p->setArchFloatReg(idx, val, m_thread_p->threadId());
    }

    const VecRegContainer& readVecRegFlat(int idx) const override
    {
      return m_cpu_p->readArchVecReg(idx, m_thread_p->threadId());
    }

    VecRegContainer& getWritableVecRegFlat(int idx) override
    {
      return m_cpu_p->getWritableArchVecReg(idx, m_thread_p->threadId());
    }

    void setVecRegFlat(int idx, const VecRegContainer& val) override
    {
      m_cpu_p->setArchVecReg(idx, val, m_thread_p->threadId());
    }

    const VecElem& readVecElemFlat(const RegIndex& idx,
                                   const ElemIndex& elemIdx) const override
    {
      return m_cpu_p->readArchVecElem(idx, elemIdx, m_thread_p->threadId());
    }

    void setVecElemFlat(const RegIndex& idx, const ElemIndex& elemIdx,
                        const VecElem& val) override
    {
      m_cpu_p->setArchVecElem(idx, elemIdx, val, m_thread_p->threadId());
    }

    const VecPredRegContainer& readVecPredRegFlat(int idx) const override
    {
      return m_cpu_p->readArchVecPredReg(idx, m_thread_p->threadId());
    }

    VecPredRegContainer& getWritableVecPredRegFlat(int idx) override
    {
      return m_cpu_p->getWritableArchVecPredReg(idx, m_thread_p->threadId());
    }

    void setVecPredRegFlat(int idx, const VecPredRegContainer& val) override
    {
      m_cpu_p->setArchVecPredReg(idx, val, m_thread_p->threadId());
    }

    RegVal readCCRegFlat(int idx) override
    {
      return m_cpu_p->readArchCCReg(idx, m_thread_p->threadId());
    }

    void setCCRegFlat(int idx, RegVal val) override
    {
      m_cpu_p->setArchCCReg(idx, val, m_thread_p->threadId());
    }

    template <typename VecElem>
    VecLaneT<VecElem, true> readVecLaneFlat(int idx, int lId) const
    {
      return m_cpu_p->template readArchVecLane<VecElem>(idx,
                                                  lId, m_thread_p->threadId());
    }

    template <typename LD>
    void setVecLaneFlat(int idx, int lId, const LD& val)
    {
      m_cpu_p->template setArchVecLane(idx, lId, m_thread_p->threadId(), val);
    }

  private:
    /** Pointer to CPU */
    IOCPU* m_cpu_p;

    /** Pointer to thread state */
    ThreadState* m_thread_p;
};

#endif // __CPU_IO_THREAD_CONTEXT_HH__
