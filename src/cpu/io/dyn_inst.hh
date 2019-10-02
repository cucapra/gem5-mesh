//-----------------------------------------------------------------------------
// dyn_inst.hh
//-----------------------------------------------------------------------------
// Dynamic instruction for IO CPU
//
// This contains all information about dynamic execution context of an
// instruction. This stays from when an instruction is fetched into the
// pipeline until the instruction retires from the pipeline.
//
// Author: Tuan Ta
// Date:   19/08/18

#ifndef __CPU_IO_DYN_INST_HH__
#define __CPU_IO_DYN_INST_HH__

#include <array>

#include "arch/isa_traits.hh"
#include "config/the_isa.hh"
#include "cpu/exec_context.hh"
#include "cpu/inst_seq.hh"
#include "cpu/phys_reg_id.hh"
#include "cpu/reg_class.hh"
#include "cpu/thread_state.hh"

class IOCPU;

class IODynInst : public ExecContext
{
  public:
    IODynInst(const StaticInstPtr& static_inst,
              TheISA::PCState pc,
              InstSeqNum seq_num,
              ThreadID tid,
              IOCPU* cpu_p);

    ~IODynInst();

    /** Execute this instruction */
    Fault execute();

    /** Initiate a memory access */
    Fault initiateAcc();

    /** Complete an access */
    Fault completeAcc(PacketPtr pkt);

  /**
   * Functions returning status of instruction
   */
  public:
    /** Checking instruction fault */
    bool isFault() const;

    /**
     * Checking instruction types
     */
    bool isNop()            const { return static_inst_p->isNop(); }
    bool isMemRef()         const { return static_inst_p->isMemRef(); }
    bool isLoad()           const { return static_inst_p->isLoad(); }
    bool isStore()          const { return static_inst_p->isStore(); }
    bool isAtomic()         const { return static_inst_p->isAtomic(); }
    bool isStoreConditional() const
    { return static_inst_p->isStoreConditional(); }
//    bool isLLSC()           const { return memReqFlags & Request::LLSC; }
    bool isInteger()        const { return static_inst_p->isInteger(); }
    bool isFloating()       const { return static_inst_p->isFloating(); }
    bool isVector()         const { return static_inst_p->isVector(); }
    bool isControl()        const { return static_inst_p->isControl(); }
    bool isCall()           const { return static_inst_p->isCall(); }
    bool isReturn()         const { return static_inst_p->isReturn(); }
    bool isDirectCtrl()     const { return static_inst_p->isDirectCtrl(); }
    bool isIndirectCtrl()   const { return static_inst_p->isIndirectCtrl(); }
    bool isCondCtrl()       const { return static_inst_p->isCondCtrl(); }
    bool isUncondCtrl()     const { return static_inst_p->isUncondCtrl(); }
    bool isSerializing()    const { return static_inst_p->isSerializing(); }
//    bool isSerializeBefore() const
//    { return static_inst_p->isSerializeBefore() || status[SerializeBefore]; }
//    bool isSerializeAfter() const
//    { return static_inst_p->isSerializeAfter() || status[SerializeAfter]; }
    bool isSquashAfter()    const { return static_inst_p->isSquashAfter(); }
    bool isMemBarrier()     const { return static_inst_p->isMemBarrier(); }
    bool isWriteBarrier()   const { return static_inst_p->isWriteBarrier(); }
    bool isNonSpeculative() const { return static_inst_p->isNonSpeculative(); }
    bool isQuiesce()        const { return static_inst_p->isQuiesce(); }
    bool isIprAccess()      const { return static_inst_p->isIprAccess(); }
    bool isUnverifiable()   const { return static_inst_p->isUnverifiable(); }
    bool isSyscall()        const { return static_inst_p->isSyscall(); }
    bool isMacroop()        const { return static_inst_p->isMacroop(); }
    bool isMicroop()        const { return static_inst_p->isMicroop(); }
    bool isLastMicroop()    const { return static_inst_p->isLastMicroop(); }
    bool isFirstMicroop()   const { return static_inst_p->isFirstMicroop(); }
    bool isMicroBranch()    const { return static_inst_p->isMicroBranch(); }
    bool isRoCC()           const { return static_inst_p->isRoCC(); }

    /**
     * Checking instruction status
     */
    bool canIssue()         const { return m_status[CanIssue]; }
    bool isIssued()         const { return m_status[Issued]; }
    bool canIssueToMem()    const { return m_status[CanIssueToMem]; }
    bool isIssuedToMem()    const { return m_status[IssuedToMem]; }
    bool isExecuted()       const { return m_status[Executed]; }
    bool needToTrapFault()  const { return m_status[NeedToTrapFault]; }
    bool canCommit()        const { return m_status[CanCommit]; }
    bool isCommitted()      const { return m_status[Committed]; }
    bool isSquashed()       const { return m_status[Squashed]; }

    /**
     * Setting instruction status
     */
    void setCanIssue()        { m_status.set(CanIssue); }
    void setIssued()          { m_status.set(Issued); }
    void setCanIssueToMem()   { m_status.set(CanIssueToMem); }
    void setIssuedToMem()     { m_status.set(IssuedToMem); }
    void setExecuted()        { m_status.set(Executed); }
    void setNeedToTrapFault() { m_status.set(NeedToTrapFault); }
    void setCanCommit()       { m_status.set(CanCommit); }
    void setCommitted()       { m_status.set(Committed); }
    void setSquashed()        { m_status.set(Squashed); }

    /**
     * Checking instruction flags
     */
    bool isNotAnInst() const { return m_inst_flags[NotAnInst]; }
    bool isEffAddrValid() const
    { return m_inst_flags[EffAddrValid]; }
    bool isPredTaken() const { return m_inst_flags[PredTaken]; }

    /**
     * Setting instruction flags
     */
    void setNotAnInst() { m_inst_flags.set(NotAnInst); }
    void setEffAddrValid() { m_inst_flags.set(EffAddrValid); }
    void setPredTaken() { m_inst_flags.set(PredTaken); }

    /**
     * Instruction's register-specific information
     */
    int8_t numSrcRegs() const { return static_inst_p->numSrcRegs(); }
    int8_t numDestRegs() const { return static_inst_p->numDestRegs(); }
    int8_t numFPDestRegs() const { return static_inst_p->numFPDestRegs(); }
    int8_t numIntDestRegs() const { return static_inst_p->numIntDestRegs(); }
    int8_t numCCDestRegs() const { return static_inst_p->numCCDestRegs(); }
    int8_t numVecDestRegs() const { return static_inst_p->numVecDestRegs(); }
    int8_t numVecElemDestRegs() const
    { return static_inst_p->numVecElemDestRegs(); }
    int8_t numVecPredDestRegs() const
    { return static_inst_p->numVecPredDestRegs(); }

    const RegId& destRegIdx(int i) const
    { return static_inst_p->destRegIdx(i); }
    const RegId& srcRegIdx(int i) const
    { return static_inst_p->srcRegIdx(i); }

    /** set predicted target PC */
    void setPredTarg(const TheISA::PCState& _pred_pc);

    /** read predicted target PC */
    const TheISA::PCState& readPredTarg();

    /** Return the branch target of this instruction */
    TheISA::PCState branchTarget();

    /** Returns whether the instruction is mispredicted */
    bool isMispredicted();

    /**
     * Renamed regs
     */
    PhysRegIdPtr renamedDestRegIdx(int idx) const;
    PhysRegIdPtr renamedSrcRegIdx(int idx) const;
    void renameDestReg(int idx,
                       PhysRegIdPtr renamed_dest,
                       PhysRegIdPtr prev_rename);
    void renameSrcReg(int idx, PhysRegIdPtr renamed_src);

    /**
     * Flattened regs
     */
    const RegId& flattenedDestRegIdx(int idx) const;
    void flattenDestReg(int idx, const RegId& flattened_dest);

    /** Return previous renamed dest reg idx */
    PhysRegIdPtr prevDestRegIdx(int idx) const;

    /** Print */
    std::string toString(bool full = false);

  /**
   * Inheritted functions from ExecContext
   */
  public:
    /** Reads an integer register. */
    RegVal readIntRegOperand(const StaticInst *si, int idx) override;

    /** Sets an integer register to a value. */
    void setIntRegOperand(const StaticInst *si, int idx, RegVal val) override;

    /** Reads a floating point register in its binary format, instead
     * of by value. */
    RegVal readFloatRegOperandBits(const StaticInst *si, int idx) override;

    /** Sets the bits of a floating point register of single width
     * to a binary value. */
    void setFloatRegOperandBits(const StaticInst *si, int idx,
                                RegVal val) override;

    /** Reads source vector register operand. */
    const VecRegContainer& readVecRegOperand(const StaticInst *si,
                                             int idx) const override;

    /** Gets destination vector register operand for modification. */
    VecRegContainer& getWritableVecRegOperand(const StaticInst *si,
                                              int idx) override;

    /** Sets a destination vector register operand to a value. */
    void setVecRegOperand(const StaticInst *si, int idx,
                          const VecRegContainer& val) override;

    /** Reads source vector 8bit operand. */
    ConstVecLane8 readVec8BitLaneOperand(const StaticInst *si,
                                         int idx) const override;

    /** Reads source vector 16bit operand. */
    ConstVecLane16 readVec16BitLaneOperand(const StaticInst *si,
                                           int idx) const override;

    /** Reads source vector 32bit operand. */
    ConstVecLane32 readVec32BitLaneOperand(const StaticInst *si,
                                           int idx) const override;

    /** Reads source vector 64bit operand. */
    ConstVecLane64 readVec64BitLaneOperand(const StaticInst *si,
                                           int idx) const override;

    template <typename LD>
    void setVecLaneOperandT(const StaticInst* si, int idx, const LD& val);

    void setVecLaneOperand(const StaticInst *si, int idx,
                           const LaneData<LaneSize::Byte>& val) override;

    void setVecLaneOperand(const StaticInst *si, int idx,
                           const LaneData<LaneSize::TwoByte>& val) override;

    void setVecLaneOperand(const StaticInst *si, int idx,
                           const LaneData<LaneSize::FourByte>& val) override;

    void setVecLaneOperand(const StaticInst *si, int idx,
                           const LaneData<LaneSize::EightByte>& val) override;

    /** Reads an element of a vector register. */
    VecElem readVecElemOperand(const StaticInst *si, int idx) const override;

    /** Sets a vector register to a value. */
    void setVecElemOperand(const StaticInst *si, int idx,
                           const VecElem val) override;

    /** Reads source predicate register operand. */
    const VecPredRegContainer& readVecPredRegOperand(const StaticInst *si,
                                                     int idx) const override;

    /** Gets destination predicate register operand for modification. */
    VecPredRegContainer& getWritableVecPredRegOperand(const StaticInst *si,
                                                      int idx) override;

    /** Sets a destination predicate register operand to a value. */
    void setVecPredRegOperand(const StaticInst *si, int idx,
                              const VecPredRegContainer& val) override;

    /**
     * Condition Code Registers
     */

    RegVal readCCRegOperand(const StaticInst *si, int idx) override;

    void setCCRegOperand(const StaticInst *si, int idx, RegVal val) override;

    /**
     * @name Misc Register Interfaces
     */
    RegVal readMiscRegOperand(const StaticInst *si, int idx) override;

    void setMiscRegOperand(const StaticInst *si,
                           int idx, RegVal val) override;

    /**
     * Reads a miscellaneous register, handling any architectural
     * side effects due to reading that register.
     */
    RegVal readMiscReg(int misc_reg) override;

    /**
     * Sets a miscellaneous register, handling any architectural
     * side effects due to writing that register.
     */
    void setMiscReg(int misc_reg, RegVal val) override;

    /**
     * Handle necessary operations associated with a given misc register
     */
    void handleMiscRegOp(int misc_reg, RegVal old_val,
                         const RegVal &val) override;

    /** Called at the commit stage to update misc regs */
    void updateMiscRegs();

    /**
     * @name PC Control
     */
    TheISA::PCState pcState() const override;

    void pcState(const TheISA::PCState &val) override;

    /**
     * Perform an atomic memory read operation.  Must be overridden
     * for exec contexts that support atomic memory mode.  Not pure
     * virtual since exec contexts that only support timing memory
     * mode need not override (though in that case this function
     * should never be called).
     */
    Fault readMem(Addr addr, uint8_t *data, unsigned int size,
                  Request::Flags flags) override;

    /**
     * Initiate a timing memory read operation.  Must be overridden
     * for exec contexts that support timing memory mode.  Not pure
     * virtual since exec contexts that only support atomic memory
     * mode need not override (though in that case this function
     * should never be called).
     */
    Fault initiateMemRead(Addr addr, unsigned int size,
                          Request::Flags flags) override;

    /**
     * For atomic-mode contexts, perform an atomic memory write operation.
     * For timing-mode contexts, initiate a timing memory write operation.
     */
    Fault writeMem(uint8_t *data, unsigned int size, Addr addr,
                   Request::Flags flags, uint64_t *res) override;

    /**
     * For atomic-mode contexts, perform an atomic AMO (a.k.a., Atomic
     * Read-Modify-Write Memory Operation)
     */
    Fault amoMem(Addr addr, uint8_t *data, unsigned int size,
                 Request::Flags flags, AtomicOpFunctor *amo_op) override;

    /**
     * For timing-mode contexts, initiate an atomic AMO (atomic
     * read-modify-write memory operation)
     */
    Fault initiateMemAMO(Addr addr, unsigned int size,
                         Request::Flags flags,
                         AtomicOpFunctor *amo_op) override;

    /**
     * Sets the number of consecutive store conditional failures.
     */
    void setStCondFailures(unsigned int sc_failures) override;

    /**
     * Returns the number of consecutive store conditional failures.
     */
    unsigned int readStCondFailures() const override;

    /**
     * Executes a syscall specified by the callnum.
     */
    void syscall(int64_t callnum, Fault *fault) override;

    /** Returns a pointer to the ThreadContext. */
    ThreadContext *tcBase() override;

    /**
     * Somewhat Alpha-specific function that handles returning from an
     * error or interrupt.
     */
    Fault hwrei() override
    { panic("IO CPU does not support Alpha-specific hwrei()\n"); }

    /**
     * Check for special simulator handling of specific PAL calls.  If
     * return value is false, actual PAL call will be suppressed.
     */
    bool simPalCheck(int palFunc) override
    { panic("IO CPU does not support Alpha-specific simPalCheck()\n"); }

    /**
     * @name ARM-Specific Interfaces
     */

    bool readPredicate() const override
    { panic("IO CPU does not support ARM-specific readPredicate()\n"); }

    void setPredicate(bool val) override
    { panic("IO CPU does not support ARM-specific setPredicate()\n"); }

    /**
     * @name X86-Specific Interfaces
     */

    /**
     * Invalidate a page in the DTLB <i>and</i> ITLB.
     */
    void demapPage(Addr vaddr, uint64_t asn) override
    { panic("IO CPU does not support X86-specific demapPage()\n"); }

    void armMonitor(Addr address) override
    { panic("IO CPU does not support X86-specific armMonitor()\n"); }

    bool mwait(PacketPtr pkt) override
    { panic("IO CPU does not support X86-specific mwait()\n"); }

    void mwaitAtomic(ThreadContext *tc) override
    { panic("IO CPU does not support X86-specific mwaitAtomic()\n"); }

    AddressMonitor *getAddrMonitor() override
    { panic("IO CPU does not support X86-specific getAddrMonitor()\n"); }

  /**
   * Public member variables
   */
  public:
    /** Sequence number */
    InstSeqNum seq_num;

    /** Pointer to the static inst */
    StaticInstPtr static_inst_p;

    /** PC state for this instruction */
    TheISA::PCState pc;

    /** Thread ID of this instruction */
    ThreadID thread_id;

    /** Fault */
    Fault fault;

    /** Predicted to be taken */
    bool predicted_taken;

    /** Pointer to memory request (for memory instructions only) */
    RequestPtr mem_req_p;

    /** Data to memory (owned and deleted by memory packet) */
    PacketDataPtr mem_data_p;

  private:
    /**
     * Instruction status
     */
    enum Status {
      CanIssue,
      Issued,
      CanIssueToMem,    // inst can issue request to memory
      IssuedToMem,      // inst has already issued request to memory
      Executed,         // inst completes its execution
      NeedToTrapFault,  // inst is ready to handle fault
      CanCommit,
      Committed,
      Squashed,
      NumStatus
    };

    /**
     * Instruction flags
     */
    enum Flags {
      NotAnInst,
      EffAddrValid,
      PredTaken,
      MaxFlags
    };

    /** Pointer to IO CPU */
    IOCPU* m_cpu_p;

    /** status of instruction */
    std::bitset<NumStatus> m_status;

    /** flag of instruction */
    std::bitset<MaxFlags> m_inst_flags;

    /** bitset showing which src regs are ready */
    std::bitset<TheISA::MaxInstSrcRegs> m_ready_src_reg_idx;

    /** flattened register index of dest regs */
    std::array<RegId, TheISA::MaxInstDestRegs> m_flat_dest_reg_idx;

    /** physical reg idx of dest regs */
    std::array<PhysRegIdPtr, TheISA::MaxInstDestRegs> m_dest_reg_idx;

    /** physical reg idx of src regs */
    std::array<PhysRegIdPtr, TheISA::MaxInstSrcRegs> m_src_reg_idx;

    /** physical reg idx of previous producers of the arch dest regs */
    std::array<PhysRegIdPtr, TheISA::MaxInstDestRegs> m_prev_dest_reg_idx;

    /** values to be written to dest misc regs */
    std::array<RegVal, TheISA::MaxMiscDestRegs> m_dest_misc_reg_val;

    /** indices of dest misc regs. They're needed to defer write accesses to
     * misc regs until commit stage, when the inst is no longer speculative. */
    std::array<short, TheISA::MaxMiscDestRegs> m_dest_misc_reg_idx;

    /** Number of dest misc regs */
    uint8_t m_num_dest_misc_regs;

    /** Predicted target PC (branch prediction) */
    TheISA::PCState m_pred_pc;

    /** A string representation of disassmbled inst */
    std::string m_inst_str;
};

typedef std::shared_ptr<IODynInst> IODynInstPtr;

#endif // CPU_IO_DYN_INST_HH
