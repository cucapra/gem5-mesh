//=============================================================================
// commit.cc
//=============================================================================
//
// Author: Tuan Ta
// Date:   19/08/26

#include "cpu/io/commit.hh"

#include "arch/utility.hh"
#include "cpu/io/cpu.hh"
#include "debug/Commit.hh"

Commit::Commit(IOCPU* _cpu_p, IOCPUParams* params)
    : Stage(_cpu_p, params->commitBufferSize, 0, StageIdx::CommitIdx, true),
      m_num_threads(params->numThreads),
      m_commit_width(params->commitWidth),
      m_commit_pc(m_num_threads, TheISA::PCState(0))
{
  assert(m_commit_width > 0);
}

void
Commit::init()
{
  m_robs.resize(m_num_threads);
  m_commit_rename_maps.resize(m_num_threads);
  for (size_t tid = 0; tid < m_num_threads; ++tid) {
    m_robs[tid] = m_cpu_p->getROBPtr(tid);
    m_commit_rename_maps[tid] = m_cpu_p->getCommitRenameMapPtr(tid);
  }
}

std::string
Commit::name() const
{
  return m_cpu_p->name() + ".commit";
}

void
Commit::regStats()
{

}

void
Commit::wakeup()
{
  Stage::wakeup();
}

void
Commit::suspend()
{
  Stage::suspend();
}

void
Commit::tick()
{
  // interact with credit and inst buffers
  Stage::tick();

  // check squash
  bool is_squashed = checkSquash();

  // if there is no squash from other stages, we do commit
  if (!is_squashed) {
    doCommit();
  }
#ifdef DEBUG
  else {
    m_stage_status.set(CommitStatus::Squashed);
  }
#endif
}

void
Commit::doCommit()
{
  // Try to mark as many instructions coming from IEW as possible as ready to
  // commit.
  while (!m_insts.empty()) {
    IODynInstPtr inst = m_insts.front();
    ThreadID tid = inst->thread_id;

    // assert inst exists in ROB
    assert(m_robs[tid]->hasInst(inst));
    assert(!inst->isCommitted() && !inst->canCommit());

    // If this inst does not have any fault, mark it as "CanCommit".
    // Otherwise, mark it as "NeedToTrapFault"
    if (inst->fault == NoFault)
      inst->setCanCommit();
    else
      inst->setNeedToTrapFault();

    // pop the inst from the incoming buffer and give credits to the previous
    // stage
    consumeInst();
  }

  // Try to commit as many ready-to-commit instructions from the top of ROB
  // TODO for now, assume only thread 0 is active
  ThreadID commit_tid = 0;
  IODynInstPtr head_inst = nullptr;
  size_t num_committed_insts = 0;

  while (num_committed_insts < m_commit_width &&
         !m_robs[commit_tid]->isEmpty()) {
    // get head inst from ROB
    head_inst = m_robs[commit_tid]->getHead();
    if (head_inst->canCommit()) {
      commitHead(commit_tid);
      num_committed_insts++;
#ifdef DEBUG
      // record
      m_stage_status.set(CommitStatus::Busy);
      m_committed_insts.push_back(head_inst);
#endif

      // if this requires squash after commit, then do it here. 
      // don't wait for 1 cycle like trap case below
      if (head_inst->isSquashAfter()) {
        initiateSquash(head_inst);
      }

    } else {
      // The head instruction is not ready to commit yet, check we need to trap
      // any fault for this inst If so, initiatiate a squash in this cycle.
      if (head_inst->needToTrapFault()) {
        assert(head_inst->fault != NoFault);
        // In this cycle, we simply initiate a squash. After the squash
        // completes in the future cycles, we can handle the trap magically. We
        // need to do in this strict order because the trap handler may destroy
        // the current thread context (e.g., doing exit syscall) before we can
        // finish the squash.
        initiateSquash(head_inst);
#ifdef DEBUG
        // record
        m_stage_status.set(CommitStatus::InitSquash);
#endif
        DPRINTF(Commit, "Initiated a squash for %s due to fault\n",
                        head_inst->toString());

        // mark this instruction as ready-to-commit
        head_inst->setCanCommit();
      }
      // check if the head inst is a store. If so, mark it as "CanIssueToMem"
      // so that it can be executed and safely go out to memory
      else if ((head_inst->isStore() || head_inst->isAtomic() ||
           head_inst->isStoreConditional()) &&
          !head_inst->canIssueToMem()) {
        DPRINTF(Commit, "Mark %s as CanIssueToMem\n", head_inst->toString());
        head_inst->setCanIssueToMem();
      }
      // stop commiting further instructions
      break;
    }
  }
}

void
Commit::commitHead(ThreadID tid)
{
  assert(!m_robs[tid]->isEmpty() && m_robs[tid]->isHeadReady());

  IODynInstPtr inst = m_robs[tid]->getHead();
  m_commit_pc[tid] = inst->pcState();

  if (inst->isSquashed()) {
    DPRINTF(Commit, "[tid:%d] Retiring squashed instruction\n", tid);
    m_robs[tid]->commitHead();

    // update last committed instruction
    m_outgoing_info_wire->commit_info.committed_inst = inst;

    return;
  } else if (inst->fault != NoFault) {
    // Handle fault (exception or syscall) in this cycle and initiate a
    // squash for the next cycle to squash all instructions younger than the
    // faulty instruction.
    DPRINTF(Commit, "[tid:%d] Handling trap for [sn:%d]\n",
                    tid, inst->seq_num);

    // Execute the trap. This is done magically without any timing delay.
    m_cpu_p->trap(inst->fault, tid, inst->isNotAnInst() ?
                  StaticInst::nullStaticInstPtr : inst->static_inst_p);
  }

  // TODO: need to re-visit this. Not sure why we need to keep track of pc
  // being processed/committed.
  TheISA::advancePC(m_commit_pc[tid], inst->static_inst_p);

  // update the commit rename map
  for (int i = 0; i < inst->numDestRegs(); ++i) {
    m_commit_rename_maps[tid]->setEntry(inst->flattenedDestRegIdx(i),
                                        inst->renamedDestRegIdx(i));
  }

  // commit the instruction
  m_robs[tid]->commitHead();

  // update misc regs
  inst->updateMiscRegs();

  // update last committed instruction
  m_outgoing_info_wire->commit_info.committed_inst = inst;

  // update cpu stats
  m_cpu_p->incrNumCommittedInsts(tid);
}

void
Commit::doSquash(SquashComm::BaseSquash &squashInfo, StageIdx initiator)
{
  IODynInstPtr squash_inst = squashInfo.trig_inst;
  
  if (initiator == StageIdx::CommitIdx)
    DPRINTF(Decode, "Squash from Commit: squash inst [tid:%d] [sn:%d]\n",
                    squash_inst->thread_id, squash_inst->seq_num);
  else if (initiator == StageIdx::IEWIdx)
    DPRINTF(Decode, "Squash from IEW: squash inst [tid:%d] [sn:%d]\n",
                    squash_inst->thread_id, squash_inst->seq_num);
  
  ThreadID tid = squash_inst->thread_id;

  // walk through all insts in the m_insts queue and remove all instructions
  // belonging to thread tid
  size_t qsize = m_insts.size();
  size_t count = 0;
  IODynInstPtr inst = nullptr;
  while (count < qsize) {
    inst = m_insts.front();
    m_insts.pop();
    if (inst->thread_id != tid || inst->seq_num <= squash_inst->seq_num) {
      m_insts.push(inst);
    } else {
      DPRINTF(Commit, "Squashing %s\n", inst->toString());
      // update the number of credits to previous stage
      outputCredit()++;
    }
    count++;
  }

  // Squash all instructions younger than squash_inst in ROB
  m_robs[tid]->squash(squash_inst);
}

void
Commit::initiateSquash(IODynInstPtr faulty_inst)
{
  DPRINTF(Commit, "Commit is initiating a squash due to a fault. "
                  "Squashing instruction %s\n", faulty_inst->toString());

  // Figure out the next PC after a fault. The next PC is the PC right after
  // the faulty instruction.
  TheISA::PCState next_pc = faulty_inst->pc;
  TheISA::advancePC(next_pc, faulty_inst->static_inst_p);

  m_outgoing_squash_wire->commit_squash()->squash = true;
  m_outgoing_squash_wire->commit_squash()->next_pc = next_pc;
  m_outgoing_squash_wire->commit_squash()->trig_inst = faulty_inst;
  m_outgoing_squash_wire->commit_squash()->is_trap_pending = true;
  
  // not actually a fault, just need to squash from commit
  if (faulty_inst->isSquashAfter())
    m_outgoing_squash_wire->commit_squash()->is_trap_pending = false;
  
}

void
Commit::linetrace(std::stringstream& ss)
{
#ifdef DEBUG
  std::string s = " [C] ";
  if (m_stage_status[CommitStatus::Squashed]) {
    s += "x";
  } else if (m_stage_status[CommitStatus::Busy]) {
    assert(!m_committed_insts.empty());
    for (auto inst : m_committed_insts)
      if (!inst->isSquashed())  s += inst->toString(true);
  } else if (m_stage_status[CommitStatus::InitSquash]) {
    s += "^x";
  }

  ss << std::setw(55) << std::left << s;

  // reset linetrace record
  m_stage_status.reset();
  m_committed_insts.clear();
#endif
}
