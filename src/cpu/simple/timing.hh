/*
 * Copyright (c) 2012-2013,2015 ARM Limited
 * All rights reserved
 *
 * The license below extends only to copyright in the software and shall
 * not be construed as granting a license to any other intellectual
 * property including but not limited to intellectual property relating
 * to a hardware implementation of the functionality of the software
 * licensed hereunder.  You may use the software subject to the license
 * terms below provided that you ensure that this notice is replicated
 * unmodified and in its entirety in all distributions of the software,
 * modified or unmodified, in source code or in binary form.
 *
 * Copyright (c) 2002-2005 The Regents of The University of Michigan
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met: redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer;
 * redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution;
 * neither the name of the copyright holders nor the names of its
 * contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * Authors: Steve Reinhardt
 */

#ifndef __CPU_SIMPLE_TIMING_HH__
#define __CPU_SIMPLE_TIMING_HH__

#include "cpu/simple/base.hh"
#include "cpu/simple/exec_context.hh"
#include "cpu/translation.hh"
#include "params/TimingSimpleCPU.hh"

class TimingSimpleCPU : public BaseSimpleCPU
{
  public:

    TimingSimpleCPU(TimingSimpleCPUParams * params);
    virtual ~TimingSimpleCPU();

    void init() override;

  private:

    /*
     * If an access needs to be broken into fragments, currently at most two,
     * the the following two classes are used as the sender state of the
     * packets so the CPU can keep track of everything. In the main packet
     * sender state, there's an array with a spot for each fragment. If a
     * fragment has already been accepted by the CPU, aka isn't waiting for
     * a retry, it's pointer is NULL. After each fragment has successfully
     * been processed, the "outstanding" counter is decremented. Once the
     * count is zero, the entire larger access is complete.
     */
    class SplitMainSenderState : public Packet::SenderState
    {
      public:
        int outstanding;
        PacketPtr fragments[2];

        int
        getPendingFragment()
        {
            if (fragments[0]) {
                return 0;
            } else if (fragments[1]) {
                return 1;
            } else {
                return -1;
            }
        }
    };

    class SplitFragmentSenderState : public Packet::SenderState
    {
      public:
        SplitFragmentSenderState(PacketPtr _bigPkt, int _index) :
            bigPkt(_bigPkt), index(_index)
        {}
        PacketPtr bigPkt;
        int index;

        void
        clearFromParent()
        {
            SplitMainSenderState * main_send_state =
                dynamic_cast<SplitMainSenderState *>(bigPkt->senderState);
            main_send_state->fragments[index] = NULL;
        }
    };

    class FetchTranslation : public BaseTLB::Translation
    {
      protected:
        TimingSimpleCPU *cpu;

      public:
        FetchTranslation(TimingSimpleCPU *_cpu)
            : cpu(_cpu)
        {}

        void
        markDelayed()
        {
            assert(cpu->_status == BaseSimpleCPU::Running);
            cpu->_status = ITBWaitResponse;
        }

        void
        finish(const Fault &fault, const RequestPtr &req, ThreadContext *tc,
               BaseTLB::Mode mode)
        {
            cpu->sendFetch(fault, req, tc);
        }
    };
    FetchTranslation fetchTranslation;

    void threadSnoop(PacketPtr pkt, ThreadID sender);
    void sendData(const RequestPtr &req,
                  uint8_t *data, uint64_t *res, bool read);
    void sendSplitData(const RequestPtr &req1, const RequestPtr &req2,
                       const RequestPtr &req,
                       uint8_t *data, bool read);

    void translationFault(const Fault &fault);

    PacketPtr buildPacket(const RequestPtr &req, bool read);
    void buildSplitPacket(PacketPtr &pkt1, PacketPtr &pkt2,
            const RequestPtr &req1, const RequestPtr &req2,
            const RequestPtr &req,
            uint8_t *data, bool read);

    bool handleReadPacket(PacketPtr pkt);
    // This function always implicitly uses dcache_pkt.
    bool handleWritePacket();

    /**
     * A TimingCPUPort overrides the default behaviour of the
     * recvTiming and recvRetry and implements events for the
     * scheduling of handling of incoming packets in the following
     * cycle.
     */
    class TimingCPUPort : public MasterPort
    {
      public:

        TimingCPUPort(const std::string& _name, TimingSimpleCPU* _cpu)
            : MasterPort(_name, _cpu), cpu(_cpu),
              retryRespEvent([this]{ sendRetryResp(); }, name())
        { }

      protected:

        TimingSimpleCPU* cpu;

        struct TickEvent : public Event
        {
            PacketPtr pkt;
            TimingSimpleCPU *cpu;

            TickEvent(TimingSimpleCPU *_cpu) : pkt(NULL), cpu(_cpu) {}
            const char *description() const { return "Timing CPU tick"; }
            void schedule(PacketPtr _pkt, Tick t);
        };

        EventFunctionWrapper retryRespEvent;
    };

    class IcachePort : public TimingCPUPort
    {
      public:

        IcachePort(TimingSimpleCPU *_cpu)
            : TimingCPUPort(_cpu->name() + ".icache_port", _cpu),
              tickEvent(_cpu)
        { }

      protected:

        virtual bool recvTimingResp(PacketPtr pkt);

        virtual void recvReqRetry();

        struct ITickEvent : public TickEvent
        {

            ITickEvent(TimingSimpleCPU *_cpu)
                : TickEvent(_cpu) {}
            void process();
            const char *description() const { return "Timing CPU icache tick"; }
        };

        ITickEvent tickEvent;

    };

    class DcachePort : public TimingCPUPort
    {
      public:

        DcachePort(TimingSimpleCPU *_cpu)
            : TimingCPUPort(_cpu->name() + ".dcache_port", _cpu),
              tickEvent(_cpu)
        {
           cacheBlockMask = ~(cpu->cacheLineSize() - 1);
        }

        Addr cacheBlockMask;
      protected:

        /** Snoop a coherence request, we need to check if this causes
         * a wakeup event on a cpu that is monitoring an address
         */
        virtual void recvTimingSnoopReq(PacketPtr pkt);
        virtual void recvFunctionalSnoop(PacketPtr pkt);

        virtual bool recvTimingResp(PacketPtr pkt);

        virtual void recvReqRetry();

        virtual bool isSnooping() const {
            return true;
        }

        struct DTickEvent : public TickEvent
        {
            DTickEvent(TimingSimpleCPU *_cpu)
                : TickEvent(_cpu) {}
            void process();
            const char *description() const { return "Timing CPU dcache tick"; }
        };

        DTickEvent tickEvent;

    };

    void updateCycleCounts();

    IcachePort icachePort;
    DcachePort dcachePort;

    PacketPtr ifetch_pkt;
    PacketPtr dcache_pkt;

    Cycles previousCycle;
    
    // pbb implement mesh ports
    // port that goes out over the mesh
    class MeshOutPort : public TimingCPUPort {
      public:
        MeshOutPort(TimingSimpleCPU *_cpu)
            : TimingCPUPort(_cpu->name() + ".mesh_out_port", _cpu),
              tickEvent(_cpu)
        { }

      protected:

        // important to implement/override these from MasterPort class
        virtual bool recvTimingResp(PacketPtr pkt);
        virtual void recvReqRetry();

        // the event to put on the event queue when a resp is received
        struct MOTickEvent : public TickEvent
        {

            MOTickEvent(TimingSimpleCPU *_cpu)
                : TickEvent(_cpu) {}
            void process();
            const char *description() const { return "Timing CPU to mesh tick"; }
        };

        MOTickEvent tickEvent;
    };
    
    // similar purpose as TimingCPUPort (derived from master port)
    class TimingCPUSlavePort : public SlavePort {
      public:

        TimingCPUSlavePort(const std::string& _name, TimingSimpleCPU* _cpu)
            : SlavePort(_name, _cpu), cpu(_cpu)
             //, retryRespEvent([this]{ sendRetryResp(); }, name())
        { }

      protected:

        // cpu reference, so we can do things in it on response
        TimingSimpleCPU* cpu;

        // base for delayed events like processing a request
        struct TickEvent : public Event
        {
            PacketPtr pkt;
            TimingSimpleCPU *cpu;

            TickEvent(TimingSimpleCPU *_cpu) : pkt(NULL), cpu(_cpu) {}
            const char *description() const { return "Timing CPU slave tick"; }
            void schedule(PacketPtr _pkt, Tick t);
        };

        // potentially want to schedule a retry event in the case that
        // two things arrive? in the same cycle ??
        // not sure if would also sched retry in the slave?
        //EventFunctionWrapper retryRespEvent;
    };
    
    class MeshInPort : public TimingCPUSlavePort {
      public:
        MeshInPort(TimingSimpleCPU *_cpu)
            : TimingCPUSlavePort(_cpu->name() + ".mesh_in_port", _cpu),
              tickEvent(_cpu)
        { }

        virtual AddrRangeList getAddrRanges() const;

      protected:

        // important to implement/override these from slave port
        virtual bool recvTimingReq(PacketPtr pkt);
        virtual void recvRespRetry();
        virtual Tick recvAtomic(PacketPtr pkt) { panic("recvAtomic unimpl"); };
        virtual void recvFunctional(PacketPtr pkt);

        // the event to put on the event queue when a resp is received
        struct MITickEvent : public TickEvent
        {

            MITickEvent(TimingSimpleCPU *_cpu)
                : TickEvent(_cpu) {}
            void process();
            const char *description() const { return "Mesh to timing CPU tick"; }
        };

        MITickEvent tickEvent;
    };
    
    // define the ports we're going to use for to access the mesh net
    MeshOutPort meshOutPort;
    MeshInPort meshInPort;

  protected:
    // pbb override function that maps ports declared and connected in 
    // the python config to c port objects
    Port &getPort(const std::string &if_name,
                  PortID idx=InvalidPortID) override;

     /** Return a reference to the data port. */
    MasterPort &getDataPort() override { return dcachePort; }

    /** Return a reference to the instruction port. */
    MasterPort &getInstPort() override { return icachePort; }

  public:

    DrainState drain() override;
    void drainResume() override;

    void switchOut() override;
    void takeOverFrom(BaseCPU *oldCPU) override;

    void verifyMemoryMode() const override;

    void activateContext(ThreadID thread_num) override;
    void suspendContext(ThreadID thread_num) override;

    Fault initiateMemRead(Addr addr, unsigned size,
                          Request::Flags flags) override;

    Fault writeMem(uint8_t *data, unsigned size,
                   Addr addr, Request::Flags flags, uint64_t *res) override;

    Fault initiateMemAMO(Addr addr, unsigned size, Request::Flags flags,
                         AtomicOpFunctor *amo_op) override;

    void fetch();
    void sendFetch(const Fault &fault,
                   const RequestPtr &req, ThreadContext *tc);
    void completeIfetch(PacketPtr );
    void completeDataAccess(PacketPtr pkt);
    void advanceInst(const Fault &fault);

    /** This function is used by the page table walker to determine if it could
     * translate the a pending request or if the underlying request has been
     * squashed. This always returns false for the simple timing CPU as it never
     * executes any instructions speculatively.
     * @ return Is the current instruction squashed?
     */
    bool isSquashed() const { return false; }

    /**
     * Print state of address in memory system via PrintReq (for
     * debugging).
     */
    void printAddr(Addr a);

    /**
     * Finish a DTB translation.
     * @param state The DTB translation state.
     */
    void finishTranslation(WholeTranslationState *state);

  private:

    EventFunctionWrapper fetchEvent;

    struct IprEvent : Event {
        Packet *pkt;
        TimingSimpleCPU *cpu;
        IprEvent(Packet *_pkt, TimingSimpleCPU *_cpu, Tick t);
        virtual void process();
        virtual const char *description() const;
    };

    /**
     * Check if a system is in a drained state.
     *
     * We need to drain if:
     * <ul>
     * <li>We are in the middle of a microcode sequence as some CPUs
     *     (e.g., HW accelerated CPUs) can't be started in the middle
     *     of a gem5 microcode sequence.
     *
     * <li>Stay at PC is true.
     *
     * <li>A fetch event is scheduled. Normally this would never be the
     *     case with microPC() == 0, but right after a context is
     *     activated it can happen.
     * </ul>
     */
    bool isDrained() {
        SimpleExecContext& t_info = *threadInfo[curThread];
        SimpleThread* thread = t_info.thread;

        return thread->microPC() == 0 && !t_info.stayAtPC &&
               !fetchEvent.scheduled();
    }

    /**
     * Try to complete a drain request.
     *
     * @returns true if the CPU is drained, false otherwise.
     */
    bool tryCompleteDrain();
};

#endif // __CPU_SIMPLE_TIMING_HH__
