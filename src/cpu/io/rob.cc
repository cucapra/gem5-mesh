//=============================================================================
// rob.cc
//=============================================================================
//
// Author: Tuan Ta
// Date:   19/08/28

#include "cpu/io/rob.hh"

#include "debug/ROB.hh"

ROB::ROB(IOCPUParams* params)
    : m_size(params->numROBEntries)
{
  assert(m_size > 0);
}

bool
ROB::isEmpty() const
{
  return m_inst_list.empty();
}

bool
ROB::isFull() const
{
  return m_inst_list.size() == m_size;
}

bool
ROB::isHeadReady() const
{
  if (isEmpty())
    return false;
  return m_inst_list.front()->canCommit();
}

void
ROB::push(IODynInstPtr inst)
{
  assert(!isFull());
  DPRINTF(ROB, "ROB: Adding %s\n", inst->toString());
  m_inst_list.push_back(inst);
}

void
ROB::commitHead()
{
  assert(!isEmpty() && isHeadReady());
  DPRINTF(ROB, "ROB: Committing %s\n", m_inst_list.front()->toString());
  m_inst_list.front()->setCommitted();
  m_inst_list.pop_front();
}

void
ROB::squash(IODynInstPtr squash_inst)
{
  DPRINTF(ROB, "ROB: Squashing instructions younger than [sn:%d]\n",
                squash_inst->seq_num);

  while (!isEmpty() &&
         m_inst_list.back()->seq_num > squash_inst->seq_num) {
    DPRINTF(ROB, "ROB: Squashing %s\n", m_inst_list.back()->toString());
    m_inst_list.back()->setSquashed();
    m_inst_list.pop_back();
  }
}

IODynInstPtr
ROB::getHead()
{
  assert(!isEmpty());
  return m_inst_list.front();
}

bool
ROB::hasInst(IODynInstPtr inst) const
{
  return std::find(m_inst_list.begin(),
                   m_inst_list.end(), inst) != m_inst_list.end();
}

size_t
ROB::getMemInstCount() const
{
  return std::count_if(m_inst_list.begin(), m_inst_list.end(),
                    [](const IODynInstPtr& inst) { return inst->isMemRef(); });
}
