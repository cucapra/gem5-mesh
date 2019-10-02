//=============================================================================
// pipelined_exec_unit.cc
//=============================================================================
//
// Author: Tuan Ta
// Date:   19/09/19

#include "cpu/io/unpipelined_exec_unit.hh"

#include "debug/IEW.hh"

UnpipelinedExecUnit::UnpipelinedExecUnit(const char* _iew_name,
                                         const char* _name, size_t _latency)
    : m_iew_name(_iew_name),
      m_name(_name),
      m_latency(_latency),
      m_inst(nullptr),
      m_cycle_count(0)
{ }

const std::string
UnpipelinedExecUnit::name() const
{
  return m_iew_name + "." + m_name;
}

void
UnpipelinedExecUnit::setScoreboardPtr(Scoreboard* scoreboard_p)
{
  m_scoreboard_p = scoreboard_p;
}

void
UnpipelinedExecUnit::insert(IODynInstPtr inst)
{
  assert(!isBusy());
  m_inst = inst;
  m_cycle_count = Cycles(0);

  // functionally execute the incoming inst
  if (m_inst && m_inst->fault == NoFault) {
    m_inst->execute();
    m_inst->setExecuted();
  }
}

IODynInstPtr
UnpipelinedExecUnit::removeCompletedInst()
{
  assert(hasInstsToWriteBack());
  IODynInstPtr inst = m_inst;
  m_inst = nullptr;
  m_cycle_count = Cycles(0);
  return inst;
}

bool
UnpipelinedExecUnit::hasInstsToWriteBack() const
{
  return m_inst != nullptr && m_cycle_count == m_latency;
}

bool
UnpipelinedExecUnit::isBusy() const
{
  return m_inst != nullptr;
}

void
UnpipelinedExecUnit::tick()
{
  if (m_inst != nullptr && m_cycle_count < m_latency) {
    ++m_cycle_count;

    // early bypass: mark destination reg ready
    if (m_cycle_count == m_latency) {
      assert(m_inst->isExecuted());
      for (int i = 0; i < m_inst->numDestRegs(); ++i) {
        DPRINTF(IEW, "[sn:%d] Setting dest reg %i (%s) ready\n",
                      m_inst->seq_num,
                      m_inst->renamedDestRegIdx(i)->index(),
                      m_inst->renamedDestRegIdx(i)->className());
        m_scoreboard_p->setReg(m_inst->renamedDestRegIdx(i));
      }
    }
#ifdef DEBUG
    m_status.set(Status::Busy);
    m_executed_inst = m_inst;
  } else if (m_inst != nullptr && m_cycle_count == m_latency) {
    m_status.set(Status::Stalled);
#endif
  }

}

void
UnpipelinedExecUnit::doSquash(IODynInstPtr squash_inst)
{
  if (m_inst != nullptr &&
      m_inst->thread_id == squash_inst->thread_id &&
      m_inst->seq_num > squash_inst->seq_num) {
    DPRINTF(IEW, "Execute: squashing %s\n", m_inst->toString());
    m_inst->setSquashed();
    m_inst = nullptr;
    m_cycle_count = Cycles(0);
  }
#ifdef DEBUG
  m_status.set(Status::Squashed);
#endif
}

void
UnpipelinedExecUnit::linetrace(std::stringstream& ss)
{
#ifdef DEBUG
  std::string s = "[X_" + m_name + "] ";
  if (m_status[Status::Squashed]) {
    s += "x";
  } else if (m_status[Status::Stalled]) {
    s += "#";
  } else if (m_status[Status::Busy]) {
    assert(m_executed_inst != nullptr);
    s += m_executed_inst->toString();
  }
  ss << std::setw(35) << std::left << s;

  // reset
  m_executed_inst = nullptr;
  m_status.reset();
#endif
}
