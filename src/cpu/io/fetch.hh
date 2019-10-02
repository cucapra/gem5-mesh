//-----------------------------------------------------------------------------
// fetch.hh
//-----------------------------------------------------------------------------
// Fetch stage in IO CPU
//
// Author: Tuan Ta
// Date  : 19/08/18

#ifndef __CPU_IO_FETCH_HH__
#define __CPU_IO_FETCH_HH__

#include <iomanip>
#include <sstream>
#include <vector>

#include "arch/decoder.hh"
#include "cpu/io/comm.hh"
#include "cpu/io/dyn_inst.hh"
#include "cpu/pred/bpred_unit.hh"
#include "cpu/timebuf.hh"
#include "params/IOCPU.hh"

class IOCPU;

class Fetch
{
  public:
    /**
     * Translation class for instruction fetch
     */
    class FetchTranslation : public BaseTLB::Translation
    {
      public:
        FetchTranslation(Fetch* _fetch_p);

        void markDelayed();
        void finish(const Fault& fault, const RequestPtr& req,
                    ThreadContext* tc, BaseTLB::Mode mode);
      private:
        Fetch* m_fetch_p;
    };

  private:
    /**
     * Event to delay delivery of a fetch translation result in case of a fault
     * and the nop to carry the fault can't be generated immediately
     */
    class FinishTranslationEvent : public Event
    {
      public:
        FinishTranslationEvent(Fetch* _fetch_p);

        void setFault(Fault _fault);
        void setReq(const RequestPtr& _req);
        void process();
        const char* description() const;

      private:
        Fetch* m_fetch_p;
        Fault m_fault;
        RequestPtr m_req_p;
    };

    enum FetchStatus {
      Squashed,
      Stalled,
      Busy,
      NumStatus
    };

  public:
    /**
     * Per-thread status
     */
    enum ThreadStatus {
      Inactive,     // the thread is not activated
      Active,       // the thread is active and ready to fetch next instrutions
      TrapPending   // waiting for Commit to process a pending trap/fault
    };

    Fetch(IOCPU* _cpu_p, IOCPUParams *params);
    ~Fetch();

    /** Init (this is called after all CPU structures are created) */
    void init();

    /** Return name of this stage object */
    std::string name() const;

    /** Register statistics */
    void regStats();

    /** Set incoming/outgoing communication wires */
    void setCommBuffers(TimeBuffer<InstComm>& inst_buffer,
                        TimeBuffer<CreditComm>& credit_buffer,
                        TimeBuffer<SquashComm>& squash_buffer,
                        TimeBuffer<InfoComm>& info_buffer);

    /** Get decoder of a thread */
    TheISA::Decoder* getDecoderPtr(ThreadID tid);

    /** Process response packet from icache */
    void processCacheCompletion(PacketPtr pkt);

    /** Complete retry request from icache */
    void completeRetryReq();

    /** Main tick function */
    void tick();

    /** Wake up this stage */
    void wakeup();

    /** Suspend this stage */
    void suspend();

    /** Activate the given thread */
    void activateThread(ThreadID tid);

    /** Deactivate the given thread */
    void deactivateThread(ThreadID tid);

    /** Reset all microarchitectural states belonging to this thread */
    void resetStates(ThreadID tid);

    /** Set new PC for a given thread */
    void pcState(const TheISA::PCState& new_pc, ThreadID tid);

    /** Line trace */
    void linetrace(std::stringstream& ss);

  private:
    /** Check squash signal. Return true if Fetch is being squashed  */
    bool checkSquash();

    /** Read credit signal & update my credits */
    void readCredits();

    /** Read info on committed instruction (used to update branch predictor)*/
    void readInfo();

    /** Do instruction fetch */
    void doFetch(ThreadID tid);

    /** Do squash */
    void doSquash(const TheISA::PCState& new_pc,
                  const IODynInstPtr squash_inst);

    /** Send a request to I-cache to fetch a cache line*/
    bool fetchCacheLine(Addr vaddr, ThreadID tid, Addr pc);

    /** Finish address translation */
    void finishTranslation(const Fault& fault, const RequestPtr& mem_req);

    /** Return cache-line-aligned address of the given address */
    Addr getCacheLineAlignedAddr(Addr addr) const;

    /** Look up next PC. Return true if next_pc is a taken branch */
    bool lookupAndUpdateNextPC(const IODynInstPtr& inst,
                               TheISA::PCState& next_pc);

    /** Return the next thread to fetch */
    ThreadID getNextThread();

    /** Place the given instruction into the buffer to the next stage */
    void sendInstToNextStage(IODynInstPtr inst);

    /** Return the number of active threads */
    size_t getNumActiveThreads() const;

  private:
    /** Pointer to the main CPU */
    IOCPU* m_cpu_p;

    /** Number of threads */
    size_t m_num_threads;

    /** Is this stage active */
    bool m_is_active;

    /** Max number of credits. This is equal to the size of input buffer in the
     * next stage */
    const size_t m_max_num_credits;

    /** Number of credits for forward communication */
    size_t m_num_credits;

    /** Size of L0 fetch buffer */
    size_t m_fetch_buffer_size;

    /** Fetch buffers */
    std::vector<uint8_t*> m_fetch_buffers;

    /** Base virtual address of data in fetch buffers */
    std::vector<Addr> m_fetch_buffer_vaddrs;

    /** Valid bit vector for all fetch buffers */
    std::vector<bool> m_fetch_buffer_valid;

    /** Vector of per-thread current PCs */
    std::vector<TheISA::PCState> m_pcs;

    /** Offset to part of the current PC being fetched */
    std::vector<size_t> m_fetch_offsets;

    /** Vector of per-thread current macro-op*/
    std::vector<StaticInstPtr> m_macro_insts;

    /** Vector of per-thread outstanding requests going to I-cache */
    std::vector<RequestPtr> m_mem_reqs;

    /** Packet to be re-sent to I-cache */
    PacketPtr m_retry_pkt;

    /** Per-thread Fetch status vector */
    std::vector<ThreadStatus> m_thread_status;

    /** Branch predictor */
    BPredUnit* m_branch_pred_p;

    /** Last fetching thread */
    ThreadID m_last_thread;

    /** Decoders */
    std::vector<TheISA::Decoder*> m_decoders;

    /**
     * Time buffer wires from/to other stages
     */
    TimeBuffer<InstComm>::wire    m_outgoing_inst_wire;
    TimeBuffer<CreditComm>::wire  m_incoming_credit_wire;
    TimeBuffer<SquashComm>::wire  m_incoming_squash_wire;
    TimeBuffer<InfoComm>::wire    m_incoming_info_wire;

#ifdef DEBUG
    /** Stage's status (for line trace) */
    std::bitset<FetchStatus::NumStatus> m_stage_status;

    /** List of instructions processed in the current cycle (for line trace) */
    std::vector<IODynInstPtr> m_fetched_insts;
#endif
};

#endif // CPU_IO_FETCH_HH
