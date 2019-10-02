//-----------------------------------------------------------------------------
// iew.cc
//-----------------------------------------------------------------------------
//
// Author: Tuan Ta
// Date  : 19/08/21

#include "cpu/io/iew.hh"

#include "arch/utility.hh"
#include "cpu/io/cpu.hh"
#include "debug/IEW.hh"

//-----------------------------------------------------------------------------
// IEW
//-----------------------------------------------------------------------------

IEW::IEW(IOCPU* _cpu_p, IOCPUParams* params)
    : m_cpu_p(_cpu_p),
      m_num_threads(params->numThreads),
      m_is_active(false),
      m_input_queue_size(params->iewBufferSize),
      m_issue_width(params->issueWidth),
      m_wb_width(params->writebackWidth),
      m_max_num_credits(params->commitBufferSize),
      m_num_credits(m_max_num_credits),
      m_scoreboard_p(nullptr)
{
  // create Int ALU exec unit
  size_t idx = 0;
  m_exec_units.push_back(new PipelinedExecUnit(this->name().c_str(), "IntALU",
                                               params->intAluOpLatency));
  m_op_to_unit_map[Enums::IntAlu]     = idx;
  m_op_to_unit_map[Enums::No_OpClass] = idx;
  m_traced_exec_units.push_back(m_exec_units.back());

  // create Int Mul exec unit
  idx++;
  m_exec_units.push_back(new PipelinedExecUnit(this->name().c_str(), "IntMul",
                                               params->intMulOpLatency));
  m_op_to_unit_map[Enums::IntMult] = idx;

  // create Div exec unit
  idx++;
  m_exec_units.push_back(new UnpipelinedExecUnit(this->name().c_str(), "Div",
                                                 params->divOpLatency));
  m_op_to_unit_map[Enums::IntDiv]    = idx;
  m_op_to_unit_map[Enums::FloatDiv]  = idx;
  m_op_to_unit_map[Enums::FloatSqrt] = idx;

  // create FP ALU exec unit
  idx++;
  m_exec_units.push_back(new PipelinedExecUnit(this->name().c_str(), "FpALU",
                                               params->fpAluOpLatency));
  m_op_to_unit_map[Enums::FloatAdd] = idx;
  m_op_to_unit_map[Enums::FloatCmp] = idx;
  m_op_to_unit_map[Enums::FloatCvt] = idx;

  // create FP Mul exec unit
  idx++;
  m_exec_units.push_back(new PipelinedExecUnit(this->name().c_str(), "FpMult",
                                               params->fpMulOpLatency));
  m_op_to_unit_map[Enums::FloatMult]    = idx;
  m_op_to_unit_map[Enums::FloatMultAcc] = idx;
  m_op_to_unit_map[Enums::FloatMisc]    = idx;

  // create memory unit
  idx++;
  m_exec_units.push_back(new MemUnit(this->name().c_str(), "Mem", m_cpu_p,
                                     params));
  m_op_to_unit_map[Enums::MemRead]       = idx;
  m_op_to_unit_map[Enums::MemWrite]      = idx;
  m_op_to_unit_map[Enums::FloatMemRead]  = idx;
  m_op_to_unit_map[Enums::FloatMemWrite] = idx;
  m_traced_exec_units.push_back(m_exec_units.back());

  // init other exec_unit fields
  m_mem_unit_p = dynamic_cast<MemUnit*>(m_exec_units.back());
  m_next_wb_exec_unit_idx = 0;
}

IEW::~IEW()
{
  for (auto& u : m_exec_units)
    delete u;
}

void
IEW::init()
{
  m_scoreboard_p = m_cpu_p->getScoreboardPtr();
  m_robs.resize(m_num_threads);
  for (size_t tid = 0; tid < m_num_threads; ++tid)
    m_robs[tid] = m_cpu_p->getROBPtr(tid);

  // set scoreboard pointers for all exec units
  for (auto& u : m_exec_units)
    u->setScoreboardPtr(m_scoreboard_p);
}

std::string
IEW::name() const
{
  return m_cpu_p->name() + ".iew";
}

void
IEW::regStats()
{

}

void
IEW::setCommBuffers(TimeBuffer<InstComm>& inst_buffer,
                    TimeBuffer<CreditComm>& credit_buffer,
                    TimeBuffer<SquashComm>& squash_buffer,
                    TimeBuffer<InfoComm>& info_buffer)
{
  m_outgoing_inst_wire = inst_buffer.getWire(0);
  m_incoming_inst_wire = inst_buffer.getWire(-1);

  m_outgoing_credit_wire = credit_buffer.getWire(0);
  m_incoming_credit_wire = credit_buffer.getWire(-1);

  m_outgoing_squash_wire = squash_buffer.getWire(0);
  m_incoming_squash_wire = squash_buffer.getWire(-1);
}

void
IEW::wakeup()
{
  assert(!m_is_active);
  m_is_active = true;
}

void
IEW::suspend()
{
  assert(m_is_active);
  m_is_active = false;
}

MemUnit*
IEW::getMemUnitPtr()
{
  return m_mem_unit_p;
}

void
IEW::tick()
{
  // sanity check
  assert(m_is_active);

  // put all instructions from Rename in m_insts queue
  queueInsts();

  // check squash
  bool is_squashed = checkSquash();

  // read credits from Commit
  readCredits();

  // If we're not squashing this cycle, do writeback, execute and issue in a
  // reversed order to model the pipeline.
  if (!is_squashed) {
    doWriteback();
    doExecute();
    doIssue();
  }
#ifdef DEBUG
  else {
    m_stage_status.set(IEWStatus::IssueSquashed);
    m_stage_status.set(IEWStatus::WBSquashed);
  }
#endif
}

void
IEW::doWriteback()
{
  if (m_num_credits == 0) {
#ifdef DEBUG
    m_stage_status.set(IEWStatus::WBStalled);
#endif
    return;
  }

  // number of instructions writing back successfully this cycle
  size_t num_wb_insts = 0;

  // walk through all units starting from the m_next_wb_exec_unit_idx unit and
  // try to write back as many instructions as possible until we run out of wb
  // bandwidth and credits to Commit stage
  size_t i = 0;
  ExecUnit* next_unit_p = nullptr;
  while (i < m_exec_units.size() &&
         num_wb_insts < m_wb_width &&
         m_num_credits > 0) {
    // get the next unit
    next_unit_p = m_exec_units[m_next_wb_exec_unit_idx];

    // check if the unit has instruction(s) to write back
    if (next_unit_p->hasInstsToWriteBack()) {
      IODynInstPtr inst = next_unit_p->removeCompletedInst();

      // if the instruction is not squashed, process it this cycle. Otherwise,
      // just skip it
      if (!inst->isSquashed()) {
        assert(inst->isExecuted());

        // check if this is a mispredicted instruction. If so, init a squash
        if (inst->isMispredicted()) {
          DPRINTF(IEW, "Branch misprediction: "
                       "[sn:%d] predicted target PC: %s\n",
                       inst->seq_num, inst->readPredTarg());
#ifdef DEBUG
          // record
          m_stage_status.set(IEWStatus::WBInitSquash);
#endif
          // initiate a squash signal
          initiateSquash(inst);
        }

        // make sure all dest regs are marked as ready by exec units
        for (int i = 0; i < inst->numDestRegs(); ++i)
          assert(m_scoreboard_p->getReg(inst->renamedDestRegIdx(i)));

        // send instruction to Commit
        sendInstToNextStage(inst);

        // increment the number of wb insts this cycle
        num_wb_insts++;
      }
    }

    // move to the next unit
    i++;
    m_next_wb_exec_unit_idx =
                      (m_next_wb_exec_unit_idx + 1) % m_exec_units.size();
  }
}

void
IEW::doExecute()
{
  // Tick all execute pipes
  for (auto exec_unit_p : m_exec_units)
    exec_unit_p->tick();
}

void
IEW::doIssue()
{
  size_t num_issued_insts = 0;

  // try to issue all incoming instructions unless we run out of issue
  // bandwidth
  while (!m_insts.empty() && num_issued_insts < m_issue_width) {
    IODynInstPtr inst = m_insts.front();
    ThreadID tid = inst->thread_id;
    OpClass op_class = inst->static_inst_p->opClass();

    // Check this instruction's dependencies are cleared
    for (int i = 0; i < inst->numSrcRegs(); ++i) {
      if (!m_scoreboard_p->getReg(inst->renamedSrcRegIdx(i))) {
        DPRINTF(IEW, "[sn:%d] Can't issue due to src reg %i %s not ready\n",
                      inst->seq_num,
                      inst->renamedSrcRegIdx(i)->index(),
                      inst->renamedSrcRegIdx(i)->className());
#ifdef DEBUG
        // record
        m_stage_status.set(IEWStatus::IssueInitStall);
#endif
        return;
      }
    }

    // If this is a memory barrier, we need to check if all previous memory
    // instructions have retired. If not, we must stall
    if (inst->isMemBarrier() && m_robs[tid]->getMemInstCount() > 0) {
      DPRINTF(IEW, "[sn:%d] Can't issue mem barrier due to pending younger "
                   "memory instructions\n", inst->seq_num);
#ifdef DEBUG
      // record
      m_stage_status.set(IEWStatus::IssueInitStall);
#endif
      return;
    }

    // Check if ROB is full
    if (m_robs[tid]->isFull()) {
      DPRINTF(IEW, "[tid:%d] ROB is full. Can't issue [sn:%d]\n",
                    tid, inst->seq_num);
#ifdef DEBUG
      // record
      m_stage_status.set(IEWStatus::IssueInitStall);
#endif
      return;
    }

    // make sure we have a unit that can execute this instruction
    if (m_op_to_unit_map.count(op_class) != 1)
      panic("No exec unit to execute %s\n", Enums::OpClassStrings[op_class]);

    size_t exec_unit_idx = m_op_to_unit_map[op_class];
    ExecUnit* exec_unit_p = m_exec_units[exec_unit_idx];

    // Check if exec pipe is able to take this instruction this cycle
    if (exec_unit_p->isBusy()) {
      DPRINTF(IEW, "Exec unit %s is busy. Can't issue [sn:%d]\n",
                    exec_unit_p->name(), inst->seq_num);
#ifdef DEBUG
      // record
      m_stage_status.set(IEWStatus::IssueInitStall);
#endif
      return;
    }

    // issue the instruction now
    exec_unit_p->insert(inst);

    // Mark its dest reg not ready
    for (int i = 0; i < inst->numDestRegs(); ++i) {
      m_scoreboard_p->unsetReg(inst->renamedDestRegIdx(i));
    }

    // Add the instruction to ROB
    m_robs[tid]->push(inst);

    // remove the inst from the queue
    m_insts.pop();
    num_issued_insts++;
    m_outgoing_credit_wire->from_iew++;

#ifdef DEBUG
    // record issued inst
    m_stage_status.set(IEWStatus::IssueBusy);
    m_issued_insts.push_back(inst);
#endif
  }
}

void
IEW::queueInsts()
{
  // move all new coming instructions into m_insts
  for (auto inst : m_incoming_inst_wire->to_iew_insts)
    m_insts.push(inst);
  assert(m_insts.size() <= m_input_queue_size);
}

bool
IEW::checkSquash()
{
  // check all possible squash signals coming from subsequent stages. It's
  // important to do this in a reversed order since earlier stages may squash
  // younger instructions.

  // check squash signals coming from Commit
  if (m_incoming_squash_wire->commit_squash.squash) {
    IODynInstPtr fault_inst = m_incoming_squash_wire->commit_squash.fault_inst;
    assert(fault_inst);
    DPRINTF(IEW, "Squash from Commit: squash inst [tid:%d] [sn:%d]\n",
                 fault_inst->thread_id, fault_inst->seq_num);
    doSquash(fault_inst);
    return true;
  }

  // check squash coming from IEW (due to branch misprediction)
  if (m_incoming_squash_wire->iew_squash.squash) {
    IODynInstPtr mispred_inst = m_incoming_squash_wire->
                                              iew_squash.mispred_inst;
    assert(mispred_inst);
    DPRINTF(IEW, "Squash from IEW: squash inst [tid:%d] [sn:%d]\n",
                  mispred_inst->thread_id, mispred_inst->seq_num);
    doSquash(mispred_inst);
    return true;
  }

  return false;
}

void
IEW::doSquash(IODynInstPtr squash_inst)
{
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
      DPRINTF(IEW, "Squashing %s\n", inst);
      assert(inst->seq_num > squash_inst->seq_num);
      // update the number of credits to previous stage
      m_outgoing_credit_wire->from_iew++;
    }
    count++;
  }

  // tell all exec units to squash as well
  for (auto exec_unit_p : m_exec_units)
    exec_unit_p->doSquash(squash_inst);
}

void
IEW::readCredits()
{
  // read and update my number of credits to Commit stage
  m_num_credits += m_incoming_credit_wire->from_commit;
  assert(m_num_credits <= m_max_num_credits);
}

void
IEW::initiateSquash(const IODynInstPtr mispred_inst)
{
  // get the correct branch target. Since this instruction has not committed
  // yet, we can't get the correct branch target from register file. However,
  // after a branch instruction is executed, its NPC field in PCState is
  // updated to the correct target PC. Therefore, we just need to advance the
  // current inst to get the correct target PC for the branch. See more details
  // on how a branch inst is executed in its execute().
  TheISA::PCState target_pc = mispred_inst->pc;
  TheISA::advancePC(target_pc, mispred_inst->static_inst_p);

  DPRINTF(IEW, "[tid:%d]: IEW is initiating a squash due to incorrect "
               "branch prediction. Squashing instruction [sn:%d]."
               "Redirecting to pc %s\n",
                mispred_inst->thread_id, mispred_inst->seq_num,
                target_pc);

  m_outgoing_squash_wire->iew_squash.squash = true;
  m_outgoing_squash_wire->iew_squash.mispred_inst = mispred_inst;
  m_outgoing_squash_wire->iew_squash.next_pc = target_pc;
  m_outgoing_squash_wire->iew_squash.branch_taken =
                                                  mispred_inst->pc.branching();
}

void
IEW::sendInstToNextStage(IODynInstPtr inst)
{
  // sanity check: make sure we have enough credit before we sent the inst
  assert(m_num_credits > 0);
  // Place inst into the buffer
  m_outgoing_inst_wire->to_commit_insts.push_back(inst);
  // consume one credit
  m_num_credits--;

#ifdef DEBUG
  // record
  m_stage_status.set(IEWStatus::WBBusy);
  m_wb_insts.push_back(inst);
#endif
}

void
IEW::linetrace(std::stringstream& ss)
{
#ifdef DEBUG
  // Issue stage
  std::string s = " [I] ";
  if (m_stage_status[IEWStatus::IssueSquashed]) {
    s += "x";
  } else if (m_stage_status[IEWStatus::IssueInitStall]) {
    s += "^#";
  } else if (m_stage_status[IEWStatus::IssueBusy]) {
    assert(!m_issued_insts.empty());
    for (auto inst : m_issued_insts)
      s += inst->toString() + " ";
  }
  ss << std::setw(30) << std::left << s;

  // Execute stage
  for (auto unit_p : m_traced_exec_units)
    unit_p->linetrace(ss);

  // Writeback stage
  s = " [W] ";
  if (m_stage_status[IEWStatus::WBSquashed]) {
    s += "x";
  } else if (m_stage_status[IEWStatus::WBStalled]) {
    s += "#";
  } else if (m_stage_status[IEWStatus::WBBusy]) {
    assert(!m_wb_insts.empty());
    for (auto inst : m_wb_insts)
      s += inst->toString() + " ";
    // a written-back inst may init a squash signal, so record it as well
    if (m_stage_status[IEWStatus::WBInitSquash]) {
      s += "^x";
    }
  }
  ss << std::setw(30) << std::left << s;

  // reset linetrace record
  m_stage_status.reset();
  m_issued_insts.clear();
  m_wb_insts.clear();
#endif
}
