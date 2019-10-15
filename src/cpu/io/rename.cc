//-----------------------------------------------------------------------------
// rename.cc
//-----------------------------------------------------------------------------
//
// Author: Tuan Ta
// Date  : 19/08/22

#include "cpu/io/rename.hh"

#include "cpu/io/cpu.hh"
#include "debug/Rename.hh"

Rename::Rename(IOCPU* _cpu_p, IOCPUParams* params)
    : Stage(_cpu_p, params->renameBufferSize, params->iewBufferSize, StageIdx::RenameIdx, true),
      m_num_threads(params->numThreads),
      m_rename_width(1),
      m_free_list_p(nullptr),
      m_history_buffers(m_num_threads)
{ }

void
Rename::init()
{
  m_free_list_p = m_cpu_p->getFreeListPtr();
  m_rename_maps.resize(m_num_threads);
  for (size_t tid = 0; tid < m_num_threads; ++tid)
    m_rename_maps[tid] = m_cpu_p->getRenameMapPtr(tid);
}

std::string
Rename::name() const
{
  return m_cpu_p->name() + ".rename";
}

void
Rename::regStats()
{

}

void
Rename::wakeup()
{
  Stage::wakeup();
}

void
Rename::suspend()
{
  Stage::suspend();
}

void
Rename::tick()
{
  
  // interact with credit and inst buffers
  Stage::tick();

  // check squash
  bool is_squashed = checkSquash();

  // read info from the subsequent stage(s)
  readInfo();

  // do actual rename if we're not squashing and we have enough credits to the
  // next stage
  if (!is_squashed && m_num_credits > 0) {
    doRename();
  }
#ifdef DEBUG
  else if (is_squashed) {
    m_stage_status.set(RenameStatus::Squashed);
  } else if (m_num_credits == 0) {
    m_stage_status.set(RenameStatus::Stalled);
  }
#endif
}

void
Rename::readInfo()
{
  // get the last retired/committed instruction from Commit (if any). We need
  // to remove the instruction and all instructions younger than it from the
  // history buffer and free corresponding phys regs.
  if (m_incoming_info_wire->commit_info.committed_inst != nullptr) {
    IODynInstPtr last_committed_inst =
                         m_incoming_info_wire->commit_info.committed_inst;
    ThreadID tid = last_committed_inst->thread_id;

    DPRINTF(Rename, "[tid:%d] Removing all committed insts younger and"
                    " including %s\n", tid, last_committed_inst->toString());

    auto hb_it = m_history_buffers[tid].end();
    hb_it--;

    // Commit all the renames up until (and including) the committed sequence
    // number. Some or even all of the committed instructions may not have
    // rename histories if they did not have destination registers that were
    // renamed.
    while (!m_history_buffers[tid].empty() &&
           hb_it != m_history_buffers[tid].end() &&
           hb_it->instSeqNum <= last_committed_inst->seq_num) {
      // Don't free special phys regs like misc and zero regs, which
      // can be recognized because the new mapping is the same as
      // the old one.
      // Free previously mapped phys reg since it's no longer holding
      // up-to-date value
      if (hb_it->newPhysReg != hb_it->prevPhysReg)
        m_free_list_p->addReg(hb_it->prevPhysReg);

      // remove the instruction from history buffer
      m_history_buffers[tid].erase(hb_it--);
    }
  }
}

void
Rename::doRename()
{
  // try to rename as many incoming instructions as possible unless we run out
  // of credits to the next stage
  while (!m_insts.empty() && m_num_credits > 0) {
    IODynInstPtr inst = m_insts.front();
    ThreadID tid = inst->thread_id;

    // check if we can rename this instruction
    if (!m_rename_maps[tid]->canRename(inst->numIntDestRegs(),
                                       inst->numFPDestRegs(),
                                       inst->numVecDestRegs(),
                                       inst->numVecElemDestRegs(),
                                       inst->numVecPredDestRegs(),
                                       inst->numCCDestRegs())) {
      DPRINTF(Rename, "Blocking due to the lack of free registers\n");

#ifdef DEBUG
      // record status
      m_stage_status.set(RenameStatus::InitStall);
#endif
      break;
    }

    // if we can rename, do it now
    renameSrcRegs(inst, inst->thread_id);
    renameDestRegs(inst, inst->thread_id);

    // send the already renamed inst to IEW
    sendInstToNextStage(inst);

    // remove the inst from the queue and update the number of credits to the
    // previous stage
    consumeInst();
  }
}

void
Rename::renameSrcRegs(const IODynInstPtr& inst, ThreadID tid)
{
  ThreadContext* tc = inst->tcBase();
  UnifiedRenameMap* map = m_rename_maps[tid];
  size_t num_src_regs = inst->numSrcRegs();

  // look for physical registers for all source operands
  for (int src_idx = 0; src_idx < num_src_regs; ++src_idx) {
    const RegId& src_reg = inst->srcRegIdx(src_idx);
    PhysRegIdPtr renamed_reg = map->lookup(tc->flattenRegId(src_reg));
    inst->renameSrcReg(src_idx, renamed_reg);

    DPRINTF(Rename, "[tid:%d] [sn:%d] Look up arch reg %i (%s). "
                    "Get phys reg %i (%s)\n",
                    tid, inst->seq_num,
                    src_reg.index(),
                    src_reg.className(),
                    renamed_reg->index(),
                    renamed_reg->className());
  }
}

void
Rename::renameDestRegs(const IODynInstPtr& inst, ThreadID tid)
{
  ThreadContext* tc = inst->tcBase();
  UnifiedRenameMap* map = m_rename_maps[tid];
  size_t num_dest_regs = inst->numDestRegs();

  // rename dest registers
  for (int dest_idx = 0; dest_idx < num_dest_regs; ++dest_idx) {
    const RegId& dest_reg = inst->destRegIdx(dest_idx);
    RegId flat_dest_regid = tc->flattenRegId(dest_reg);

    // rename
    UnifiedRenameMap::RenameInfo rename_result = map->rename(flat_dest_regid);
    inst->flattenDestReg(dest_idx, flat_dest_regid);

    DPRINTF(Rename, "[tid:%d] [sn:%d] Rename arch reg %i (%s) "
                    "to phys reg %i (%s). Previous phys reg %i (%s)\n",
                    tid, inst->seq_num,
                    dest_reg.index(),
                    dest_reg.className(),
                    rename_result.first->index(),
                    rename_result.first->className(),
                    rename_result.second->index(),
                    rename_result.second->className());

    // Record this rename in the history buffer
    RenameHistory hb_entry(inst->seq_num, flat_dest_regid,
                           rename_result.first, rename_result.second);
    m_history_buffers[tid].push_front(hb_entry);

    DPRINTF(Rename, "[tid:%d] Added inst [sn:%d] to history buffer "
                    "(size = %d)\n",
                    tid, inst->seq_num, m_history_buffers[tid].size());

    // Tel the instruction to rename the appropriate dest register to the new
    // physical register (rename_result.first), and record the previous
    // physical register that the same logical register was renamed to
    // (rename_result.second).
    inst->renameDestReg(dest_idx, rename_result.first, rename_result.second);
  }
}

void
Rename::doSquash(SquashComm::BaseSquash &squashInfo, StageIdx initiator)
{
  IODynInstPtr squash_inst = squashInfo.trig_inst;
  
  if (initiator == StageIdx::CommitIdx)
    DPRINTF(Rename, "Squash from Commit: squash inst [tid:%d] [sn:%d]\n",
                    squash_inst->thread_id, squash_inst->seq_num);
  else if (initiator == StageIdx::IEWIdx)
    DPRINTF(Rename, "Squash from IEW: squash inst [tid:%d] [sn:%d]\n",
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
    if (inst->thread_id != tid) {
      m_insts.push(inst);
    } else {
      DPRINTF(Rename, "Squashing %s\n", inst);
      assert(inst->seq_num > squash_inst->seq_num);
      // update the number of credits to the previous stage
      outputCredit()++;
    }
    count++;
  }

  // Walk through the history buffer of thread tid, revert reg mappings that
  // correspond to instructions younger than the squash_inst, and free related
  // phys regs.
  auto hb_it = m_history_buffers[tid].begin();

  while (!m_history_buffers[tid].empty() &&
         hb_it->instSeqNum > squash_inst->seq_num) {
    assert(hb_it != m_history_buffers[tid].end());

    // Undo the rename mapping only if it was really a change. Special regs
    // that are not really renamed (like misc regs and zero reg) can be
    // recognized because the new mapping is the same as the old one. We don't
    // want to put these on the free list.
    if (hb_it->newPhysReg != hb_it->prevPhysReg) {
      // Tell the rename map to set the arch reg to the previous phys reg that
      // it was renamed to.
      m_rename_maps[tid]->setEntry(hb_it->archReg, hb_it->prevPhysReg);

      // Put the renamed physical register back on the free list
      m_free_list_p->addReg(hb_it->newPhysReg);
    }

    DPRINTF(Rename, "Removing [sn:%d] from history buffer\n",
                    hb_it->instSeqNum);

    // Erase the entry in history buffer
    m_history_buffers[tid].erase(hb_it++);
  }
}

void
Rename::sendInstToNextStage(IODynInstPtr inst)
{
  // actually send inst
  Stage::sendInstToNextStage(inst);

#ifdef DEBUG
  // record for linetrace
  m_stage_status.set(RenameStatus::Busy);
  m_renamed_insts.push_back(inst);
#endif
}

void
Rename::linetrace(std::stringstream& ss)
{
#ifdef DEBUG
  std::string s = " [R] ";
  if (m_stage_status[RenameStatus::Squashed]) {
    s += "x";
  } else if (m_stage_status[RenameStatus::Stalled]) {
    s += "#";
  } else if (m_stage_status[RenameStatus::InitStall]) {
    s += "^#";
  } else if (m_stage_status[RenameStatus::Busy]) {
    assert(!m_renamed_insts.empty());
    for (auto inst : m_renamed_insts)
      s += inst->toString() + " ";
  }
  ss << std::setw(30) << std::left << s;

  // reset status (for linetrace)
  m_stage_status.reset();
  m_renamed_insts.clear();
#endif
}
