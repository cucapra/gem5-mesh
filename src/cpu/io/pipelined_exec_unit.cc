//=============================================================================
// pipelined_exec_unit.cc
//=============================================================================
//
// Author: Tuan Ta
// Date:   19/09/10

#include "cpu/io/pipelined_exec_unit.hh"

#include "debug/IEW.hh"

PipelinedExecUnit::PipelinedExecUnit(const char* _iew_name, const char* _name,
                                     size_t num_stages)
    : m_iew_name(_iew_name),
      m_name(_name),
      m_num_stages(num_stages),
      m_incoming_inst(nullptr),
      m_pipeline(m_num_stages, nullptr),
      m_num_inflight_ops(0)
{ }

const std::string
PipelinedExecUnit::name() const
{
  return m_iew_name + "." + m_name;
}

void
PipelinedExecUnit::setScoreboardPtr(Scoreboard* scoreboard_p)
{
  m_scoreboard_p = scoreboard_p;
}

void
PipelinedExecUnit::insert(IODynInstPtr inst)
{
  assert(!isBusy());
  m_incoming_inst = inst;
  m_num_inflight_ops++;
}

IODynInstPtr
PipelinedExecUnit::removeCompletedInst()
{
  assert(hasInstsToWriteBack());
  IODynInstPtr inst = m_pipeline.back();
  assert(inst->isExecuted());
  m_pipeline.back() = nullptr;
  m_num_inflight_ops--;
  return inst;
}

bool
PipelinedExecUnit::hasInstsToWriteBack() const
{
  return m_pipeline.back() != nullptr;
}

bool
PipelinedExecUnit::isBusy() const
{
  return m_incoming_inst != nullptr;
}

void
PipelinedExecUnit::tick()
{
  assert(m_num_inflight_ops <= m_num_stages + 1);

  if (m_num_inflight_ops == 0)
    return; // unit is empty

  if (m_pipeline.back() != nullptr) {
#ifdef DEBUG
    m_status.set(Status::Stalled);
#endif
    return; // unit is full, can't advance it
  }

  // shift all instruction toward the end by one slot
  for (int i = m_num_stages - 2; i >= 0; --i)
    m_pipeline[i + 1] = m_pipeline[i];

  // functionally execute the incoming inst
  if (m_incoming_inst && m_incoming_inst->fault == NoFault) {
    m_incoming_inst->execute();
    m_incoming_inst->setExecuted();

#ifdef DEBUG
    // record incoming inst for linetrace
    m_executed_insts.push_back(m_incoming_inst);
    m_status.set(Status::Busy);
#endif
  }

  // place the incoming inst into the first slot
  m_pipeline[0] = m_incoming_inst;
  m_incoming_inst = nullptr;

  // early bypass: mark dest reg of the last inst in the pipeline ready
  if (m_pipeline.back() != nullptr) {
    assert(m_pipeline.back()->isExecuted());
    for (int i = 0; i < m_pipeline.back()->numDestRegs(); ++i) {
      DPRINTF(IEW, "[sn:%d] Setting dest reg %i (%s) ready\n",
                    m_pipeline.back()->seq_num,
                    m_pipeline.back()->renamedDestRegIdx(i)->index(),
                    m_pipeline.back()->renamedDestRegIdx(i)->className());
      m_scoreboard_p->setReg(m_pipeline.back()->renamedDestRegIdx(i));
    }
  }
}

void
PipelinedExecUnit::doSquash(IODynInstPtr squash_inst)
{
  for (auto& inst : m_pipeline) {
    if (inst != nullptr &&
        inst->thread_id == squash_inst->thread_id &&
        inst->seq_num > squash_inst->seq_num) {
      DPRINTF(IEW, "Execute: squashing %s\n", inst->toString());
      inst->setSquashed();
      inst = nullptr;
      m_num_inflight_ops--;
    }
  }

  if (m_incoming_inst &&
      m_incoming_inst->thread_id == squash_inst->thread_id &&
      m_incoming_inst->seq_num > squash_inst->seq_num) {
    DPRINTF(IEW, "Execute: squashing %s\n", m_incoming_inst->toString());
    m_incoming_inst->setSquashed();
    m_incoming_inst = nullptr;
    m_num_inflight_ops--;
  }

#ifdef DEBUG
  m_status.set(Status::Squashed);
#endif
}

void
PipelinedExecUnit::linetrace(std::stringstream& ss)
{
#ifdef DEBUG
  std::string s = " [X_" + m_name + "] ";
  if (m_status[Status::Squashed]) {
    s += "x";
  } else if (m_status[Status::Stalled]) {
    s += "#";
  } else if (m_status[Status::Busy]) {
    assert(!m_executed_insts.empty());
    for (auto inst : m_executed_insts)
      s += inst->toString() + " ";
  }
  ss << std::setw(35) << std::left << s;

  // reset
  m_executed_insts.clear();
  m_status.reset();
#endif
}
