// Authors: Tuan Ta
//          Philip Bedoukian

#include "cpu/io/stage.hh"
#include "cpu/io/cpu.hh"


#include "debug/Decode.hh"
#include "debug/Rename.hh"
#include "debug/Mesh.hh"
#include "debug/IEW.hh"
#include "debug/Commit.hh"

Stage::Stage(IOCPU* _cpu_p, size_t inputBufSize, size_t outputBufSize,
    StageIdx stageIdx, bool isSequential)
    : m_cpu_p(_cpu_p),
      m_is_active(false),
      m_input_queue_size(inputBufSize),
      m_max_num_credits(outputBufSize),
      m_num_credits(m_max_num_credits),
      m_stage_idx(stageIdx),
      m_next_stage_inst_idx(StageIdx::NumStages), // init with temp value
      m_next_stage_credit_idx(StageIdx::NumStages),
      m_is_sequential(isSequential),
      m_is_unemployed(false)
{ }

void
Stage::setCommBuffers(TimeBuffer<InstComm>& inst_buffer,
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
  
  m_outgoing_info_wire = info_buffer.getWire(0);
  m_incoming_info_wire   = info_buffer.getWire(-1);

  // TODO not great that this is here
  // find where the next stages are to send insts and get credits from
  // these connections are easy to do in hardware, but to make this CL sim
  // more modular have to jump through some hoops
  auto& pipeline = m_cpu_p->getPipeline();
  m_next_stage_inst_idx = pipeline.getNextStageIdx(m_stage_idx);
  
  // if there is a 'combinational stage' (fake) next then we need to get pay attention
  // to the credits buffer of the next sequential stage
  m_next_stage_credit_idx = m_next_stage_inst_idx;
  while (pipeline.hasNextStage(m_next_stage_credit_idx) && 
      !pipeline.isStageSeq(m_next_stage_credit_idx)) {
    m_next_stage_credit_idx = pipeline.getNextStageIdx(m_next_stage_credit_idx);
  }

  // if there's no seq stage at the end need to 

  // TODO for more stages?
  // TODO unused
  //m_any_seq_stage_after = !hasNextStage() || pipeline.isStageSeq(m_next_stage_credit_idx);
 //DPRINTF(Mesh, "any seq after %d %d\n", m_next_stage_credit_idx, m_any_seq_stage_after);
  // lookup stages that can squash this one based on squash wire
  // check all possible squash signals coming from subsequent stages. It's
  // important to do this in a reversed order since earlier stages may squash
  // younger instructions.
  
  // every stage needs to check IEW for branch mispredicts (even stages after, i.e. commit)
  //auto& pipeline = m_cpu_p->getPipeline();
  for (int i = pipeline.getLen() - 1; i >= 0; i--) {
    StageIdx stage = pipeline.getOrder()[i];

    // check if allowed to squash instructions in this stage
    if ( pipeline.stageCmp(stage, m_stage_idx) || stage == StageIdx::IEWIdx || stage == StageIdx::CommitIdx ) {
      
      // check if the stage even has a squash signal
      if (m_incoming_squash_wire->squash_signals.count((int)stage) > 0) {    
        
        m_squashing_stages.push_back(stage);
      
      }
    }    
  }

}

bool
Stage::hasNextStage() {
  //return ((int)m_stage_idx + 1 < Pipeline::Len);
  return m_cpu_p->getPipeline().hasNextStage(m_stage_idx);
}

bool
Stage::hasPrevStage() {
  //return ((int)m_stage_idx - 1 >= 0);
  return m_cpu_p->getPipeline().hasPrevStage(m_stage_idx);
}
    
std::list<std::shared_ptr<IODynInst>>&
Stage::inputInst() {
  // if the stage takes one cycle (don't know why it would) then get from
  // incoming buffer as other stages do
  if (m_is_sequential) {
    return m_incoming_inst_wire->from_prev_stage(m_stage_idx);
  }
  // if vector stage takes zero cycles, then we should pull from outgoing
  // and not wait for it to switch to incoming on the next cycle
  else {
    return m_outgoing_inst_wire->from_prev_stage(m_stage_idx);
  }
}

std::list<std::shared_ptr<IODynInst>>&
Stage::outputInst() {
  return m_outgoing_inst_wire->to_next_stage(m_next_stage_inst_idx);
}
    
int&
Stage::inputCredit() {
  return m_incoming_credit_wire->from_next_stage(m_next_stage_credit_idx);
}

// TODO this might be problematic i.e. fetch happens before vector, but wont know about credits until vec?
// but combo can just give a credit every cycle to keep going
int&
Stage::outputCredit() {
  return m_outgoing_credit_wire->to_prev_stage(m_stage_idx);
}

void
Stage::queueInsts() {
  if (!hasPrevStage()) return;
  
  for (auto inst : inputInst())
    m_insts.push(inst);
  
  assert(m_insts.size() <= m_input_queue_size);
}

void
Stage::sendInstToNextStage(IODynInstPtr inst) {
  if (!hasNextStage()) return;
 
  // sanity check: make sure we have enough credit before we sent the inst
  //if (m_any_seq_stage_after)
  assert(m_num_credits > 0);
  // Place inst into the buffer
  outputInst().push_back(inst);
  // consume one credit
  m_num_credits--;
}

void
Stage::consumeInst() {
  if (!hasPrevStage()) return;
  
  // Remove the inst from the queue and increment the credit to the previous
  // stage.
  m_insts.pop();
  outputCredit()++;
}

// TODO should put in tick
void
Stage::readCredits() {
  if (!hasNextStage()) return;
  
  // read and update my number of credits
  m_num_credits += inputCredit();
  assert(m_num_credits <= m_max_num_credits);
  assert(m_num_credits >= 0);
}

void
Stage::tick() {
  // sanity check
  // if after commit then don't worry about the check.
  // this HACK is needed to handle pthread launch which turns cpu on,off,on
  // TODO either make that launch process better or need this function to
  // stop child stages from running (which this kind of does?)
  if (!hasNextStage() && !m_is_active) return;
  assert(m_is_active);

  // put all coming instructions to process in m_insts queue
  queueInsts();

  // read credits from the next stage
  readCredits();
  
  // GEM5-specific hack
  // if we are unemployed then we can't get any credits (effectively a fake stall)
  // note we would not need this in hardware
  if (m_is_unemployed) { 
    m_num_credits = 0;
  }
  
  /*// check if stage can proceed
  if (!checkSquash() && !checkStall()) {
    forwardTick();
  }*/
}

void
Stage::wakeup()
{
  assert(!m_is_active);
  m_is_active = true;
}

void
Stage::suspend()
{
  assert(m_is_active);
  m_is_active = false;
}

bool
Stage::checkStall() {
  return (m_num_credits == 0 && hasNextStage());
}

// TODO 
bool
Stage::checkSquash() {
  // check all possible squash signals coming from subsequent stages.
  for (int i = 0; i < m_squashing_stages.size(); i++) {
    StageIdx squashing_stage = m_squashing_stages[i];
    
    // check if there is actually a squash
    auto &bs = m_incoming_squash_wire->squash_signals[(int)squashing_stage];
    if (bs.squash) {
      IODynInstPtr trig_inst = bs.trig_inst;
      assert(trig_inst);
          
      doSquash(bs, (StageIdx)squashing_stage);
          
      return true;
    }
        
  }
    
  // no squash
  return false;
  
}

/*
void
Stage::doSquash(SquashComm::BaseSquash &squashInfo, StageIdx initiator) {
  
  IODynInstPtr squash_inst = squashInfo.trig_inst;
  ThreadID tid = squash_inst->thread_id;

  // walk through all insts in the m_insts queue and remove all instructions
  // belonging to thread tid
  size_t qsize = m_insts.size();
  size_t count = 0;
  IODynInstPtr inst = nullptr;
  while (count < qsize) {
    inst = m_insts.front();
    m_insts.pop();
    bool canSquash = inst->decAndCheckSquash();
    if (inst->thread_id != tid || !canSquash) {
      m_insts.push(inst);
    } else {
      if (m_stage_idx != CommitIdx)
        assert(inst->seq_num > squash_inst->seq_num);
      
      // debug out statements (TODO might want lambda function here?)
      switch(m_stage_idx) {
        case FetchIdx:
          break;
        case DecodeIdx:
          DPRINTF(Decode, "Squashing %s\n", inst->toString());
          break;
        case VectorIdx:
          DPRINTF(Mesh, "Squashing %s\n", inst);
          break;
        case RenameIdx:
          DPRINTF(Rename, "Squashing %s\n", inst);
          break;
        case IEWIdx:
          DPRINTF(IEW, "Squashing %s\n", inst);
          break;
        case CommitIdx:
          DPRINTF(Commit, "Squashing %s\n", inst->toString());
          break;
        default:
          break;
      }
      
      // increment the number of credits to the previous stage
      outputCredit()++;
    }
    count++;
  }
}
*/
