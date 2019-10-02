//=============================================================================
// mem_unit.hh
//=============================================================================
// Memory unit: Handle memory instructions: load, store, atomic,
// load-linked, store-conditional, and memory barriers.
//
// Author: Tuan Ta
// Date:   19/09/10

#ifndef __CPU_IO_MEM_UNIT_HH__
#define __CPU_IO_MEM_UNIT_HH__

#include <bitset>
#include <queue>
#include <unordered_map>

#include "cpu/io/exec_unit.hh"
#include "mem/request.hh"
#include "params/IOCPU.hh"

/**
 * This models a pipelined memory unit consisting of three stages
 *    - Address calculation   (S0)
 *    - Address translation   (S1)
 *    - Memory request issue  (S2)
 *
 * s0_inst -> [ S0 ] -> s1_inst -> [ S1 ] -> s2_inst -> [ S2 ]
 *
 * Since this unit is designed for a simple processor pipeline (i.e., in-order
 * CPU), it should be kept simple.
 *
 *  - Instructions in load and store queue are drained in the order they're
 *    inserted into the queues. This models FIFO queues or circular buffers in
 *    HW to avoid expensive associative lookups.
 *
 *  - Loads and stores are issued in order. There can be multiple loads and
 *    stores in flight at the same time (i.e., depending on the size of load
 *    and store queues)
 *
 *  - A load following a store to the same address must be stalled until the
 *    store is executed (i.e., being issued to memory). This avoid adding logic
 *    to passing value between the two instructions.
 */

class MemUnit : public ExecUnit
{
  public:
    /**
     * Translation class for memory unit
     */
    class MemTranslation : public BaseTLB::Translation
    {
      public:
        MemTranslation(MemUnit* _mem_unit_p) : m_mem_unit_p(_mem_unit_p) { }
        void markDelayed() { }
        void finish(const Fault& fault, const RequestPtr& req,
                    ThreadContext* tc, BaseTLB::Mode mode)
        {
          m_mem_unit_p->finishTranslation(fault, req);
          delete this;
        }

      private:
        MemUnit* m_mem_unit_p;
    };

    struct SenderState : public Packet::SenderState
    {
      SenderState(IODynInstPtr _inst) : inst(_inst) { }
      IODynInstPtr inst;
    };

  public:
    MemUnit(const char* _iew_name, const char* _name, IOCPU* _cpu_p,
            IOCPUParams* params);
    ~MemUnit() override = default;

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

    /** Process a response packet from cache */
    void processCacheCompletion(PacketPtr pkt);

    /** Complete a retry request from cache */
    void completeRetryReq();

    /** Push a request into Load/Store queue */
    Fault pushMemReq(IODynInst* inst, bool is_load, uint8_t* data,
                     unsigned int size, Addr addr, Request::Flags flags,
                     uint64_t* res, AtomicOpFunctor* amo_op);

    /** Complete addr translation */
    void finishTranslation(const Fault& fault, RequestPtr req);

    /** Check if store buffer is empty */
    bool isStoreBufferEmpty(ThreadID tid) const;

  private:
    enum Status {
      Squashed,
      S0_Stalled,       // addr calc stage is stalled
      S1_Stalled,       // addr translation stage is stalled
      S2_Stalled,       // inst issue stage is stalled
      S0_Busy,
      S1_Busy,
      S2_Busy,
      NumStatus
    };

  private:
    /** Caclulate addr for m_s0_inst (if any) */
    void doAddrCalc();

    /** Translate virtual addr of m_s1_inst (if any) */
    void doTranslation();

    /** Issue (if any) */
    void doMemIssue();

    /** Check whether a load depends on an older store in SQ. Return true if
     * there is any dependency */
    bool checkLdStDependency(IODynInstPtr ld_inst);

    /** Check if LQ has inst to write back */
    bool isLQReadyToWB() const;

    /** Check if SQ has inst to write back */
    bool isSQReadyToWB() const;

  private:
    /** Name of IEW controlling this unit */
    const std::string m_iew_name;

    /** Name of this unit */
    const std::string m_name;

    /** Pointer to main CPU (for addr translation) */
    IOCPU* m_cpu_p;

    /** Pointer to CPU's scoreboard (for early bypass) */
    Scoreboard* m_scoreboard_p;

    /** Size of load queue */
    const size_t m_num_lq_entries;

    /** Size of store queue */
    const size_t m_num_sq_entries;

    /** Number of dcache ports */
    const size_t m_num_dcache_ports;

    /** Load queue: list of outstanding load requests (i.e., already issued to
     * cache and waiting for their memory responses */
    std::list<IODynInstPtr> m_ld_queue;

    /** Store & AMO queue: list of outstanding store and AMO requests (i.e.,
     * for both store and AMO, waiting for Commit to mark them ready to issue
     * to memory, and for AMO only, this also waits for memory responses) */
    std::list<IODynInstPtr> m_st_queue;

    /** ST-LDs dependency map. A map between a store instruction's sequence
     * number to all load instructions dependeing on it */
    std::unordered_map<InstSeqNum, std::vector<IODynInstPtr>> m_st_ld_map;

    /** Instruction whose address is being calculated
     * (i.e., input of S0 stage) */
    IODynInstPtr m_s0_inst;

    /** Instruction whose virtual address is being translated
     * (i.e., input of S1 stage) */
    IODynInstPtr m_s1_inst;

#ifdef DEBUG
    /** Unit's status */
    std::bitset<Status::NumStatus> m_status;

    /** Recored insts (for linetrace) */
    IODynInstPtr m_addr_calculated_inst;
    IODynInstPtr m_translated_inst;
    std::vector<IODynInstPtr> m_issued_insts;
#endif
};

#endif //  __CPU_IO_MEM_UNIT_HH__
