//-----------------------------------------------------------------------------
// cpu.cc
//-----------------------------------------------------------------------------
//
// Author: Tuan Ta
// Date  : 19/08/19

#include "cpu/io/cpu.hh"

#include <iomanip>
#include <sstream>

#include "cpu/io/mem_unit.hh"
#include "cpu/io/thread_context.hh"
#include "debug/IOCPU.hh"
#include "debug/LineTrace.hh"

//-----------------------------------------------------------------------------
// IOCPU::IcachePort
//-----------------------------------------------------------------------------

IOCPU::IcachePort::IcachePort(Fetch* _fetch_p, IOCPU* _cpu_p,
                              int _num_cache_ports)
    : MasterPort(_cpu_p->name() + ".icache_port", _cpu_p),
      fetch_p(_fetch_p),
      num_cache_ports(_num_cache_ports),
      num_used_cache_ports(0),
      need_retry(false)
{ }

bool
IOCPU::IcachePort::sendTimingReq(PacketPtr pkt)
{
  assert(num_used_cache_ports <= num_cache_ports);

  if (num_used_cache_ports == num_cache_ports) {
    DPRINTF(IOCPU, "Running out of icache ports\n");
    need_retry = true;
    return false;
  }

  if (MasterPort::sendTimingReq(pkt)) {
    num_used_cache_ports++;
    return true;
  }

  return false;
}

bool
IOCPU::IcachePort::recvTimingResp(PacketPtr pkt)
{
  assert(pkt);
  DPRINTF(IOCPU, "icache port received a response packet: %s\n", pkt->print());
  fetch_p->processCacheCompletion(pkt);
  // CPU should be always ready to receive response packets, so always return
  // true here.
  return true;
}

void
IOCPU::IcachePort::recvReqRetry()
{
  DPRINTF(IOCPU, "icache port received a retry request\n");
  fetch_p->completeRetryReq();
}

void
IOCPU::IcachePort::reset()
{
  num_used_cache_ports = 0;
  if (need_retry) {
    need_retry = false;
    fetch_p->completeRetryReq();
  }
}

//-----------------------------------------------------------------------------
// IOCPU::DcachePort
//-----------------------------------------------------------------------------

IOCPU::DcachePort::DcachePort(MemUnit* _mem_unit_p, IOCPU* _cpu_p,
                              int _num_cache_ports)
    : MasterPort(_cpu_p->name() + ".dcache_port", _cpu_p),
      mem_unit_p(_mem_unit_p),
      num_cache_ports(_num_cache_ports),
      num_used_cache_ports(0),
      need_retry(false)
{ }

bool
IOCPU::DcachePort::sendTimingReq(PacketPtr pkt)
{
  assert(num_used_cache_ports <= num_cache_ports);

  if (num_used_cache_ports == num_cache_ports) {
    DPRINTF(IOCPU, "Running out of dcache ports\n");
    need_retry = true;
    return false;
  }

  if (MasterPort::sendTimingReq(pkt)) {
    num_used_cache_ports++;
    return true;
  }

  return false;
}

bool
IOCPU::DcachePort::recvTimingResp(PacketPtr pkt)
{
  assert(pkt);
  DPRINTF(IOCPU, "dcache port received a response packet: %s\n", pkt->print());
  mem_unit_p->processCacheCompletion(pkt);
  // CPU should be always ready to receive response packets, so always return
  // true here.
  return true;
}

void
IOCPU::DcachePort::recvReqRetry()
{
  DPRINTF(IOCPU, "dcache port received a retry request\n");
  mem_unit_p->completeRetryReq();
}

void
IOCPU::DcachePort::reset()
{
  num_used_cache_ports = 0;
  if (need_retry) {
    need_retry = false;
    mem_unit_p->completeRetryReq();
  }
}

//-----------------------------------------------------------------------------
// IOCPU
//-----------------------------------------------------------------------------

IOCPU::IOCPU(IOCPUParams* params)
    : BaseCPU(params),
      status(Idle),
      itb(params->itb),
      dtb(params->dtb),
      m_system_p(params->system),
      m_num_threads(params->numThreads),
      m_tick_event([this]{ tick(); }, "IO_CPU tick",
                   false, Event::CPU_Tick_Pri),
      m_fetch(this, params),
      m_decode(this, params),
      m_rename(this, params),
      m_iew(this, params),
      m_commit(this, params),
      // TODO need to figure out the delays for those buffers. For now, assume
      // it can hold message for one next cycle and one past cycle.
      m_inst_buffer(1, 1),
      m_credit_buffer(1, 1),
      m_squash_buffer(1, 1),
      m_info_buffer(1, 1),
      m_reg_file(params->numPhysIntRegs,
                 params->numPhysFloatRegs,
                 params->numPhysVecRegs,
                 params->numPhysVecPredRegs,
                 params->numPhysCCRegs,
                 Enums::VecRegRenameMode::Full),
      m_scoreboard(name() + ".scoreboard", m_reg_file.totalNumPhysRegs()),
      m_free_list(name() + ".freelist", &m_reg_file),
      m_icache_port(&m_fetch, this, params->numIcachePorts),
      m_dcache_port(m_iew.getMemUnitPtr(), this, params->numDcachePorts),
      m_active_thread_ids(),
      m_isa_list(),
      m_global_seq_num(1),
      m_last_active_cycle(0)
{
  // IOCPU does not support FullSystem mode yet
  assert(!FullSystem);

  // Set up communication wires for all stages
  m_fetch.setCommBuffers(m_inst_buffer, m_credit_buffer,
                         m_squash_buffer, m_info_buffer);
  m_decode.setCommBuffers(m_inst_buffer, m_credit_buffer,
                          m_squash_buffer, m_info_buffer);
  m_rename.setCommBuffers(m_inst_buffer, m_credit_buffer,
                          m_squash_buffer, m_info_buffer);
  m_iew.setCommBuffers(m_inst_buffer, m_credit_buffer,
                       m_squash_buffer, m_info_buffer);
  m_commit.setCommBuffers(m_inst_buffer, m_credit_buffer,
                          m_squash_buffer, m_info_buffer);

  // IOCPU does not support simulating multiple workloads
  assert(params->workload.size() == 1);

  // create new thread contexts
  for (ThreadID tid = 0; tid < m_num_threads; ++tid) {
    ThreadContext* tc = new IOThreadContext(this, tid, params->workload[0]);
    threadContexts.push_back(tc);
  }

  // Create ROBs
  for (ThreadID tid = 0; tid < m_num_threads; ++tid) {
    m_robs.emplace_back(params);
  }

  // Set up rename maps
  m_rename_maps.resize(m_num_threads);
  m_commit_rename_maps.resize(m_num_threads);
  m_isa_list.resize(m_num_threads);

  for (ThreadID tid = 0; tid < m_num_threads; ++tid) {
    m_isa_list[tid] = params->isa[tid];

    // Only Alpha has an FP zero register, so for other ISAs we
    // use an invalid FP register index to avoid special treatment
    // of any valid FP reg.
    RegIndex invalidFPReg = TheISA::NumFloatRegs + 1;
    RegIndex fpZeroReg = (THE_ISA == ALPHA_ISA) ?
                                  TheISA::ZeroReg : invalidFPReg;

    m_commit_rename_maps[tid].init(&m_reg_file,
                                   TheISA::ZeroReg,
                                   fpZeroReg,
                                   &m_free_list,
                                   Enums::VecRegRenameMode::Full);

    m_rename_maps[tid].init(&m_reg_file,
                            TheISA::ZeroReg,
                            fpZeroReg,
                            &m_free_list,
                            Enums::VecRegRenameMode::Full);
  }

  // Initialize rename maps for all thread contexts. Basically this maps each
  // arch reg (from each thread) to a specific phys reg.
  PhysRegIdPtr zero_int_phys_reg = nullptr;
  PhysRegIdPtr zero_float_phys_reg = nullptr;

  for (ThreadID tid = 0; tid < m_num_threads; tid++) {
    for (RegIndex ridx = 0; ridx < TheISA::NumIntRegs; ++ridx) {
      // check if ridx is ZeroReg and a physical register has already
      // been assigned to the zero register. If so, try to use the
      // existing mapping.
      PhysRegIdPtr phys_reg = nullptr;
      if (ridx == TheISA::ZeroReg && zero_int_phys_reg)
        phys_reg = zero_int_phys_reg;
      else
        phys_reg = m_free_list.getIntReg();

      m_rename_maps[tid].setEntry(RegId(IntRegClass, ridx), phys_reg);
      m_commit_rename_maps[tid].setEntry(RegId(IntRegClass, ridx), phys_reg);

      if (ridx == TheISA::ZeroReg && !zero_int_phys_reg)
        zero_int_phys_reg = phys_reg;
    }

    for (RegIndex ridx = 0; ridx < TheISA::NumFloatRegs; ++ridx) {
      // check if ridx is ZeroReg and a physical register has already
      // been assigned to the zero register. If so, try to use the
      // existing mapping.
      PhysRegIdPtr phys_reg = nullptr;
      if (THE_ISA == ALPHA_ISA && ridx == TheISA::ZeroReg &&
          zero_float_phys_reg)
        phys_reg = zero_float_phys_reg;
      else
        phys_reg = m_free_list.getFloatReg();

      m_rename_maps[tid].setEntry(RegId(FloatRegClass, ridx), phys_reg);
      m_commit_rename_maps[tid].setEntry(RegId(FloatRegClass, ridx), phys_reg);

      if (THE_ISA == ALPHA_ISA && ridx == TheISA::ZeroReg &&
          !zero_float_phys_reg) {
        zero_float_phys_reg = phys_reg;
      }
    }

    /* Here we need two 'interfaces' the 'whole register' and the 'register
     * element'. At any point only one of them will be active.
     *
     * @tuan: I assume the vecMode is Full. If vector-element mode is ever
     * needed in the future, please refer to O3CPU to see how it should be
     * implemented */

    /* Initialize the full-vector interface */
    for (RegIndex ridx = 0; ridx < TheISA::NumVecRegs; ++ridx) {
      RegId rid = RegId(VecRegClass, ridx);
      PhysRegIdPtr phys_reg = m_free_list.getVecReg();
      m_rename_maps[tid].setEntry(rid, phys_reg);
      m_commit_rename_maps[tid].setEntry(rid, phys_reg);
    }

    for (RegIndex ridx = 0; ridx < TheISA::NumVecPredRegs; ++ridx) {
      PhysRegIdPtr phys_reg = m_free_list.getVecPredReg();
      m_rename_maps[tid].setEntry(RegId(VecPredRegClass, ridx), phys_reg);
      m_commit_rename_maps[tid].setEntry(
              RegId(VecPredRegClass, ridx), phys_reg);
    }

#if THE_ISA == X86_ISA
    for (RegIndex ridx = 0; ridx < TheISA::NumCCRegs; ++ridx) {
      PhysRegIdPtr phys_reg = m_free_list.getCCReg();
      m_rename_maps[tid].setEntry(RegId(CCRegClass, ridx), phys_reg);
      m_commit_rename_maps[tid].setEntry(RegId(CCRegClass, ridx), phys_reg);
    }
#endif
  }
}

IOCPU::~IOCPU()
{ }

void
IOCPU::init()
{
  BaseCPU::init();
  for (ThreadID tid = 0; tid < m_num_threads; ++tid)
    threadContexts[tid]->initMemProxies(threadContexts[tid]);

  m_fetch.init();
  m_decode.init();
  m_rename.init();
  m_iew.init();
  m_commit.init();
}

void
IOCPU::startup()
{
  BaseCPU::startup();

  // start up ISA instances
  for (ThreadID tid = 0; tid < m_num_threads; ++tid)
    m_isa_list[tid]->startup(threadContexts[tid]);
}

void
IOCPU::wakeup(ThreadID tid)
{

}

void
IOCPU::switchOut()
{
  // TODO: currently draining IO pipeline is not supported, so calling this
  // function is essentially no-op.
  warn("switchOut not implemented\n");
}

void
IOCPU::takeOverFrom(BaseCPU* old_cpu)
{
  // reset all stages
  for (ThreadID tid = 0; tid < m_num_threads; ++tid)
    resetStates(tid);

  // reset status
  status = Idle;

  // transfer states over from old thread contexts to current thread contexts
  BaseCPU::takeOverFrom(old_cpu);

  assert(!m_tick_event.scheduled());

  for (ThreadContext* tc_p : threadContexts)
    if (tc_p->status() == ThreadContext::Active)
      activateContext(tc_p->threadId());
}

void
IOCPU::activateContext(ThreadID tid)
{
  DPRINTF(IOCPU, "[tid:%d] Activating thread context\n", tid);

  // sanity check
  assert(!switchedOut());
  if (status == Idle)
    assert(m_active_thread_ids.empty());
  else
    assert(!m_active_thread_ids.empty());

  // assert that the thread is not active yet
  assert(std::find(m_active_thread_ids.begin(), m_active_thread_ids.end(),
                   tid) == m_active_thread_ids.end());

  // push the new thread into the active list
  m_active_thread_ids.push_back(tid);

  // if this is the first thread running after the CPU is idle, we need to wake
  // up the pipeline.
  if (m_active_thread_ids.size() == 1)
    wakeup();

  // tell fetch stage to put this thread into its scheduling list
  m_fetch.activateThread(tid);

  // call activateContext in the base class
  BaseCPU::activateContext(tid);
}

void
IOCPU::suspendContext(ThreadID tid)
{
  DPRINTF(IOCPU, "[tid:%d] Suspending thread context\n", tid);

  // sanity check
  assert(!switchedOut());
  assert(status != Idle);

  // remove the thread from the active list
  auto it = std::find(m_active_thread_ids.begin(), m_active_thread_ids.end(),
                      tid);
  assert(it != m_active_thread_ids.end());

  // erase it
  m_active_thread_ids.erase(it);

  // tell fetch stage to put this thread out of its scheduling list
  m_fetch.deactivateThread(tid);

  // if this is was the last thread, unschedule the tick event
  if (m_active_thread_ids.empty())
    suspend();

  // call suspendContext in the base class
  BaseCPU::suspendContext(tid);
}

void
IOCPU::haltContext(ThreadID tid)
{
  // Halting a thread context is similar to suspending a context except that
  // all microarchitectural states of the thread context are removed from the
  // pipeline since this thread will never be waken up.

  DPRINTF(IOCPU, "[tid:%d] Halting thread context\n", tid);

  // sanity check
  // A thread context may be halted while the CPU hosting it stays idle (e.g.,
  // all threads mapped to the CPU are being suspended). This happens typically
  // at the end of a simulation. Therefore, we can't assert its status here.
  assert(!switchedOut());

  // remove the thread from the active list
  auto it = std::find(m_active_thread_ids.begin(), m_active_thread_ids.end(),
                      tid);

  // erase it
  if (it != m_active_thread_ids.end()) {
    m_active_thread_ids.erase(it);

    // tell fetch stage to put this thread out of its scheduling list
    m_fetch.deactivateThread(tid);
  }

  // if this is was the last thread, unschedule the tick event
  if (m_active_thread_ids.empty())
    suspend();

  // reset states
  resetStates(tid);

  // call haltContext in the base class
  BaseCPU::haltContext(tid);
}

void
IOCPU::resetStates(ThreadID tid)
{
  // TODO: need to reset states related to the thread in all data structures of
  // the pipeline
  m_fetch.resetStates(tid);
}

void
IOCPU::serializeThread(CheckpointOut& cp, ThreadID tid) const
{
  panic("IOCPU does not support serializeThread yet\n");
}

void
IOCPU::unserializeThread(CheckpointIn& cp, ThreadID tid)
{
  panic("IOCPU does not support unserializeThread yet\n");
}

void
IOCPU::wakeup()
{
  DPRINTF(IOCPU, "Waking up\n");
  assert(status == Idle && !m_tick_event.scheduled());
  status = Running;

  // need to wakeup all stages
  m_fetch.wakeup();
  m_decode.wakeup();
  m_rename.wakeup();
  m_iew.wakeup();
  m_commit.wakeup();

  // schedule a tick event in the next clock edge
  schedule(m_tick_event, clockEdge());

  // update stats
  m_idle_cycles += (curCycle() - m_last_active_cycle);
  numCycles += (curCycle() - m_last_active_cycle);
}

void
IOCPU::suspend()
{
  DPRINTF(IOCPU, "Suspending\n");

  if (status != Idle) {
    status = Idle;
    // need to suspend all stages
    m_fetch.suspend();
    m_decode.suspend();
    m_rename.suspend();
    m_iew.suspend();
    m_commit.suspend();

    // update stats
    m_last_active_cycle = curCycle();
  }

  // unschedule tick event
  unscheduleTickEvent();
}

MasterPort&
IOCPU::getInstPort()
{
  return m_icache_port;
}

MasterPort&
IOCPU::getDataPort()
{
  return m_dcache_port;
}

void
IOCPU::tick()
{
  assert(!switchedOut());

  // free resources in cache port in the beginning of the cycle
  m_icache_port.reset();
  m_dcache_port.reset();

  // tick each stage in the forward order
  m_fetch.tick();
  m_decode.tick();
  m_rename.tick();
  m_iew.tick();
  m_commit.tick();

  // advance communication buffers so that signals are propagated at the end of
  // this cycle and will be seen in beginning of the next cycle.
  m_inst_buffer.advance();
  m_credit_buffer.advance();
  m_squash_buffer.advance();
  m_info_buffer.advance();

  // schedule next tick
  if (status == Running && !m_tick_event.scheduled()) {
    schedule(m_tick_event, clockEdge(Cycles(1)));
  }

  // update stats
  numCycles++;

#ifdef DEBUG
  linetrace();
#endif
}

void
IOCPU::pcState(const TheISA::PCState& newPCState, ThreadID tid)
{
  m_fetch.pcState(newPCState, tid);
  // TODO: what pc state is tracked in commit stage? do we need it?
  m_commit.pcState(newPCState, tid);
}

TheISA::PCState
IOCPU::pcState(ThreadID tid)
{
  return m_commit.pcState(tid);
}

void
IOCPU::trap(const Fault& fault, ThreadID tid, const StaticInstPtr& inst)
{
  DPRINTF(IOCPU, "Invoking trap for fault %s\n", fault->name());
  fault->invoke(threadContexts[tid], inst);
}

TheISA::Decoder*
IOCPU::getDecoderPtr(ThreadID tid)
{
  return m_fetch.getDecoderPtr(tid);
}

System*
IOCPU::getSystemPtr()
{
  return m_system_p;
}

TheISA::ISA*
IOCPU::getISAPtr(ThreadID tid)
{
  return m_isa_list[tid];
}

Addr
IOCPU::instAddr(ThreadID tid)
{
  return m_commit.instAddr(tid);
}

MicroPC
IOCPU::microPC(ThreadID tid)
{
  return m_commit.microPC(tid);
}

Addr
IOCPU::nextInstAddr(ThreadID tid)
{
  return m_commit.nextInstAddr(tid);
}

ThreadContext*
IOCPU::tcBase(ThreadID tid)
{
  return threadContexts[tid];
}

InstSeqNum
IOCPU::getAndIncrementInstSeq()
{
  return m_global_seq_num++;
}

void
IOCPU::scheduleTickEvent(Cycles delay)
{
  if (!m_tick_event.scheduled())
    schedule(m_tick_event, clockEdge(delay));
}

void
IOCPU::unscheduleTickEvent()
{
  if (m_tick_event.scheduled())
    deschedule(m_tick_event);
}

Counter
IOCPU::totalInsts() const
{
  return m_committed_insts.total();
}

Counter
IOCPU::totalOps() const
{
  return totalInsts();
}

Fault
IOCPU::pushMemReq(IODynInst* inst, bool is_load, uint8_t* data,
                  unsigned int size, Addr addr, Request::Flags flags,
                  uint64_t* res, AtomicOpFunctor* amo_op)
{
  return m_iew.getMemUnitPtr()->pushMemReq(inst, is_load, data, size, addr,
                                           flags, res, amo_op);
}

size_t
IOCPU::getCacheLineSize() const
{
  return m_system_p->cacheLineSize();
}

//-----------------------------------------------------------------------------
// Accessors for pipeline data structures
//-----------------------------------------------------------------------------

PhysRegFile*
IOCPU::getPhysRegFilePtr()
{
  return &m_reg_file;
}

ROB*
IOCPU::getROBPtr(ThreadID tid)
{
  return &m_robs[tid];
}

Scoreboard*
IOCPU::getScoreboardPtr()
{
  return &m_scoreboard;
}

UnifiedFreeList*
IOCPU::getFreeListPtr()
{
  return &m_free_list;
}

UnifiedRenameMap*
IOCPU::getRenameMapPtr(ThreadID tid)
{
  return &m_rename_maps[tid];
}

UnifiedRenameMap*
IOCPU::getCommitRenameMapPtr(ThreadID tid)
{
  return &m_commit_rename_maps[tid];
}

//-----------------------------------------------------------------------------
// Functions managing register file
//-----------------------------------------------------------------------------

RegVal
IOCPU::readIntReg(PhysRegIdPtr phys_reg)
{
  return m_reg_file.readIntReg(phys_reg);
}

void
IOCPU::setIntReg(PhysRegIdPtr phys_reg, RegVal val)
{
  m_reg_file.setIntReg(phys_reg, val);
}

RegVal
IOCPU::readArchIntReg(int reg_idx, ThreadID tid)
{
  PhysRegIdPtr phys_reg = m_commit_rename_maps[tid].
                                          lookup(RegId(IntRegClass, reg_idx));
  return m_reg_file.readIntReg(phys_reg);
}

void
IOCPU::setArchIntReg(int reg_idx, RegVal val, ThreadID tid)
{
  PhysRegIdPtr phys_reg = m_commit_rename_maps[tid].
                                          lookup(RegId(IntRegClass, reg_idx));
  m_reg_file.setIntReg(phys_reg, val);
}

RegVal
IOCPU::readFloatReg(PhysRegIdPtr phys_reg)
{
  return m_reg_file.readFloatReg(phys_reg);
}

void
IOCPU::setFloatReg(PhysRegIdPtr phys_reg, RegVal val)
{
  m_reg_file.setFloatReg(phys_reg, val);
}

RegVal
IOCPU::readArchFloatReg(int reg_idx, ThreadID tid)
{
  PhysRegIdPtr phys_reg = m_commit_rename_maps[tid].
                                        lookup(RegId(FloatRegClass, reg_idx));
  return m_reg_file.readIntReg(phys_reg);
}

void
IOCPU::setArchFloatReg(int reg_idx, RegVal val, ThreadID tid)
{
  PhysRegIdPtr phys_reg = m_commit_rename_maps[tid].
                                        lookup(RegId(FloatRegClass, reg_idx));
  m_reg_file.setFloatReg(phys_reg, val);
}

auto
IOCPU::readVecReg(PhysRegIdPtr reg_idx) const -> const VecRegContainer&
{
  return m_reg_file.readVecReg(reg_idx);
}

auto
IOCPU::readArchVecReg(int reg_idx, ThreadID tid) const
                                                -> const VecRegContainer&
{
  PhysRegIdPtr phys_reg = m_commit_rename_maps[tid].
                                  lookup(RegId(VecRegClass, reg_idx));
  return readVecReg(phys_reg);
}

auto
IOCPU::readVecElem(PhysRegIdPtr reg_idx) const -> const VecElem&
{
  return m_reg_file.readVecElem(reg_idx);
}

auto
IOCPU::readArchVecElem(const RegIndex& reg_idx, const ElemIndex& ldx,
                       ThreadID tid) const -> const VecElem&
{
  PhysRegIdPtr phys_reg = m_commit_rename_maps[tid].
                                    lookup(RegId(VecElemClass, reg_idx, ldx));
  return readVecElem(phys_reg);
}

auto
IOCPU::readVecPredReg(PhysRegIdPtr reg_idx) const -> const VecPredRegContainer&
{
  return m_reg_file.readVecPredReg(reg_idx);
}

auto
IOCPU::readArchVecPredReg(int reg_idx, ThreadID tid) const
                                                  -> const VecPredRegContainer&
{
  PhysRegIdPtr phys_reg = m_commit_rename_maps[tid].
                                  lookup(RegId(VecPredRegClass, reg_idx));
  return readVecPredReg(phys_reg);
}

auto
IOCPU::getWritableVecReg(PhysRegIdPtr reg_idx) -> VecRegContainer&
{
  return m_reg_file.getWritableVecReg(reg_idx);
}

auto
IOCPU::getWritableArchVecReg(int reg_idx, ThreadID tid) -> VecRegContainer&
{
  PhysRegIdPtr phys_reg = m_commit_rename_maps[tid].
                                  lookup(RegId(VecRegClass, reg_idx));
  return getWritableVecReg(phys_reg);
}

auto
IOCPU::getWritableVecPredReg(PhysRegIdPtr reg_idx) -> VecPredRegContainer&
{
  return m_reg_file.getWritableVecPredReg(reg_idx);
}

auto
IOCPU::getWritableArchVecPredReg(int reg_idx, ThreadID tid)
                                                    -> VecPredRegContainer&
{
  PhysRegIdPtr phys_reg = m_commit_rename_maps[tid].
                                  lookup(RegId(VecPredRegClass, reg_idx));
  return getWritableVecPredReg(phys_reg);
}

void
IOCPU::setVecReg(PhysRegIdPtr reg_idx, const VecRegContainer& val)
{
  m_reg_file.setVecReg(reg_idx, val);
}

void
IOCPU::setArchVecReg(int reg_idx, const VecRegContainer& val, ThreadID tid)
{
  PhysRegIdPtr phys_reg = m_commit_rename_maps[tid].
                                  lookup(RegId(VecRegClass, reg_idx));
  setVecReg(phys_reg, val);
}

void
IOCPU::setVecElem(PhysRegIdPtr reg_idx, const VecElem& val)
{
  m_reg_file.setVecElem(reg_idx, val);
}

void
IOCPU::setArchVecElem(const RegIndex& reg_idx, const ElemIndex& ldx,
                      const VecElem& val, ThreadID tid)
{
    PhysRegIdPtr phys_reg = m_commit_rename_maps[tid].
                                    lookup(RegId(VecElemClass, reg_idx, ldx));
    setVecElem(phys_reg, val);
}

void
IOCPU::setVecPredReg(PhysRegIdPtr reg_idx, const VecPredRegContainer& val)
{
  m_reg_file.setVecPredReg(reg_idx, val);
}

void
IOCPU::setArchVecPredReg(int reg_idx, const VecPredRegContainer& val,
                         ThreadID tid)
{
    PhysRegIdPtr phys_reg = m_commit_rename_maps[tid].
                                lookup(RegId(VecPredRegClass, reg_idx));
    setVecPredReg(phys_reg, val);
}

RegVal
IOCPU::readMiscRegNoEffect(int misc_reg, ThreadID tid) const
{
  return m_isa_list[tid]->readMiscRegNoEffect(misc_reg);
}

RegVal
IOCPU::readMiscReg(int misc_reg, ThreadID tid)
{
  return m_isa_list[tid]->readMiscReg(misc_reg, tcBase(tid));
}

void
IOCPU::setMiscRegNoEffect(int misc_reg, RegVal val, ThreadID tid)
{
  m_isa_list[tid]->setMiscRegNoEffect(misc_reg, val);
}

void
IOCPU::setMiscReg(int misc_reg, RegVal val, ThreadID tid)
{
  m_isa_list[tid]->setMiscReg(misc_reg, val, tcBase(tid));
}

RegVal
IOCPU::readCCReg(PhysRegIdPtr phys_reg)
{
  return m_reg_file.readCCReg(phys_reg);
}

void
IOCPU::setCCReg(PhysRegIdPtr phys_reg, RegVal val)
{
  m_reg_file.setCCReg(phys_reg, val);
}

RegVal
IOCPU::readArchCCReg(int reg_idx, ThreadID tid)
{
  PhysRegIdPtr phys_reg = m_commit_rename_maps[tid].
                                  lookup(RegId(CCRegClass, reg_idx));
  return m_reg_file.readCCReg(phys_reg);
}

void
IOCPU::setArchCCReg(int reg_idx, RegVal val, ThreadID tid)
{
  PhysRegIdPtr phys_reg = m_commit_rename_maps[tid].
                                  lookup(RegId(CCRegClass, reg_idx));
  m_reg_file.setCCReg(phys_reg, val);
}

void
IOCPU::incrNumCommittedInsts(ThreadID tid)
{
  m_committed_insts[tid]++;
}

void
IOCPU::regStats()
{
  BaseCPU::regStats();

  m_idle_cycles
      .name(name() + ".num_idle_cycles")
      .desc("Total number of cycles in which CPU is idle")
      .prereq(m_idle_cycles);

  m_committed_insts
      .init(m_num_threads)
      .name(name() + ".num_committed_insts")
      .desc("Number of Instructions Simulated")
      .flags(Stats::total);

  m_cpi
      .name(name() + ".cpi")
      .desc("CPI: Cycles Per Instruction")
      .precision(6);
  m_cpi = numCycles / m_committed_insts;

  m_total_cpi
      .name(name() + ".cpi_total")
      .desc("CPI: Total CPI of All Threads")
      .precision(6);
  m_total_cpi = numCycles / sum(m_committed_insts);

  m_ipc
      .name(name() + ".ipc")
      .desc("IPC: Instructions Per Cycle")
      .precision(6);
  m_ipc = m_committed_insts / numCycles;

  m_total_ipc
      .name(name() + ".ipc_total")
      .desc("IPC: Total IPC of All Threads")
      .precision(6);
  m_total_ipc =  sum(m_committed_insts) / numCycles;

  m_int_regfile_reads
      .name(name() + ".int_regfile_reads")
      .desc("number of integer regfile reads")
      .prereq(m_int_regfile_reads);

  m_int_regfile_writes
      .name(name() + ".int_regfile_writes")
      .desc("number of integer regfile writes")
      .prereq(m_int_regfile_writes);

  m_fp_regfile_reads
      .name(name() + ".fp_regfile_reads")
      .desc("number of floating regfile reads")
      .prereq(m_fp_regfile_reads);

  m_fp_regfile_writes
      .name(name() + ".fp_regfile_writes")
      .desc("number of floating regfile writes")
      .prereq(m_fp_regfile_writes);

  m_misc_regfile_reads
      .name(name() + ".misc_regfile_reads")
      .desc("number of misc regfile reads")
      .prereq(m_misc_regfile_reads);

  m_misc_regfile_writes
      .name(name() + ".misc_regfile_writes")
      .desc("number of misc regfile writes")
      .prereq(m_misc_regfile_writes);

//  m_fetch.regStats();
//  m_decode.regStats();
//  m_rename.regStats();
//  m_iew.regStats();
//  m_commit.regStats();
//  m_rob.regStats();
}

//-----------------------------------------------------------------------------
// Line trace utilities
//-----------------------------------------------------------------------------

void
IOCPU::linetrace()
{
#ifdef DEBUG
  std::stringstream ss;
  ss << std::setw(10) << curTick() / clockPeriod();

  m_fetch.linetrace(ss);
  m_decode.linetrace(ss);
  m_rename.linetrace(ss);
  m_iew.linetrace(ss);
  m_commit.linetrace(ss);

  DPRINTF(LineTrace, "%s\n", ss.str());
#endif
}

//-----------------------------------------------------------------------------
// IOCPUParams::create()
//-----------------------------------------------------------------------------

IOCPU*
IOCPUParams::create()
{
  return new IOCPU(this);
}
