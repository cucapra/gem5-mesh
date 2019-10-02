//-----------------------------------------------------------------------------
// decode.cc
//-----------------------------------------------------------------------------
//
// Author: Tuan Ta
// Date:   19/08/20

#include "cpu/io/decode.hh"

#include "cpu/io/cpu.hh"
#include "debug/Decode.hh"

Decode::Decode(IOCPU* _cpu_p, IOCPUParams* params)
    : m_cpu_p(_cpu_p),
      m_is_active(false),
      m_input_queue_size(params->decodeBufferSize),
      m_decode_width(1),
      m_max_num_credits(params->renameBufferSize),
      m_num_credits(m_max_num_credits)
{ }

std::string
Decode::name() const
{
  return m_cpu_p->name() + ".decode";
}

void
Decode::init()
{

}

void
Decode::regStats()
{

}

void
Decode::setCommBuffers(TimeBuffer<InstComm>& inst_buffer,
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
Decode::wakeup()
{
  assert(!m_is_active);
  m_is_active = true;
}

void
Decode::suspend()
{
  assert(m_is_active);
  m_is_active = false;
}

void
Decode::tick()
{
  // sanity check
  assert(m_is_active);

  // put all coming instructions to process in m_insts queue
  queueInsts();

  // check squash
  bool is_squashed = checkSquash();

  // read credits from the next stage
  readCredits();

  // do actual decode
  if (!is_squashed && m_num_credits > 0) {
    doDecode();
  }
#ifdef DEBUG
  else if (is_squashed) {
    m_stage_status.set(DecodeStatus::Squashed);
  } else if (m_num_credits == 0) {
    m_stage_status.set(DecodeStatus::Stalled);
  }
#endif
}

void
Decode::queueInsts()
{
  // push new coming instructions to m_insts
  for (auto inst : m_incoming_inst_wire->to_decode_insts)
    m_insts.push(inst);
  assert(m_insts.size() <= m_input_queue_size);
}

bool
Decode::checkSquash()
{
  // check all possible squash signals coming from subsequent stages. It's
  // important to do this in a reversed order since earlier stages may squash
  // younger instructions.

  // check squash coming from Commit
  if (m_incoming_squash_wire->commit_squash.squash) {
    IODynInstPtr fault_inst = m_incoming_squash_wire->
                                            commit_squash.fault_inst;
    assert(fault_inst);
    DPRINTF(Decode, "Squash from Commit: squash inst [tid:%d] [sn:%d]\n",
                    fault_inst->thread_id, fault_inst->seq_num);

    doSquash(fault_inst);
    return true;
  }

  // check squash coming from IEW (due to branch misprediction)
  if (m_incoming_squash_wire->iew_squash.squash) {
    IODynInstPtr mispred_inst = m_incoming_squash_wire->
                                                  iew_squash.mispred_inst;
    assert(mispred_inst);
    DPRINTF(Decode, "Squash from IEW: squash inst [tid:%d] [sn:%d]\n",
                    mispred_inst->thread_id, mispred_inst->seq_num);
    doSquash(mispred_inst);
    return true;
  }

  // check squash coming from Decode stage (last cycle) (due to branch
  // misprediction). We handle the squash initiated in the last cycle in the
  // current cycle.
  if (m_incoming_squash_wire->decode_squash.squash) {
    IODynInstPtr mispred_inst =
                    m_incoming_squash_wire->decode_squash.mispred_inst;
    assert(mispred_inst);
    DPRINTF(Decode, "Squash from Decode: squash inst [tid:%d] [sn:%d]\n",
                    mispred_inst->thread_id, mispred_inst->seq_num);
    doSquash(mispred_inst);
    return true;
  }

  return false;
}

void
Decode::readCredits()
{
  // read and update my number of credits
  m_num_credits += m_incoming_credit_wire->from_rename;
  assert(m_num_credits <= m_max_num_credits);
}

void
Decode::doDecode()
{
  // try to decode as many incoming instructions as possible unless we run out
  // of credits to the next stage
  while (!m_insts.empty() && m_num_credits > 0) {
    IODynInstPtr inst = m_insts.front();
    ThreadID tid = inst->thread_id;
    DPRINTF(Decode, "[tid:%d] Decoding inst [sn:%lli] with PC %s\n",
                    tid, inst->seq_num, inst->pc);

    // send out this inst
    sendInstToNextStage(inst);

    // Remove the inst from the queue and increment the credit to the previous
    // stage.
    m_insts.pop();
    m_outgoing_credit_wire->from_decode++;

    // Check for branch misprediction. Technically, at this point (i.e., after
    // the instruction is fully decoded), we know for sure that the instruction
    // is a branch or not. However, in this model, we assume that the Fetch
    // stage can figure out if an instruction is a branch before it's fully
    // decoded. With this assumption, we don't need to check if the instruction
    // is mispredicted as a branch instruction in Decode stage.
    //
    // Branch target can be mispredicted. Here we check if the predicted target
    // and the actual target match. If they don't, we need to initiate a squash
    // to squash all instructions after this instruction since they are in a
    // wrong execution branch.
    if (inst->isDirectCtrl() && (inst->isUncondCtrl() || inst->isPredTaken()))
    {
      if (inst->branchTarget() != inst->readPredTarg()) {
        // set the right branch target for this instruction
        inst->setPredTarg(inst->branchTarget());
        // initiate a squash signal so that Fetch can fetch from the right
        // stream.
        initiateSquash(inst);
        // stop decoding further instructions since we just initiated a squash
        // signal
        break;
      }
    }
  }
}

void
Decode::doSquash(IODynInstPtr squash_inst)
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
      DPRINTF(Decode, "Squashing %s\n", inst->toString());
      assert(inst->seq_num > squash_inst->seq_num);
      // increment the number of credits to the previous stage
      m_outgoing_credit_wire->from_decode++;
    }
    count++;
  }
}

void
Decode::initiateSquash(const IODynInstPtr& mispred_inst)
{
  DPRINTF(Decode, "[tid:%d]: Decode is initiating a squash due to incorrect \
                  branch prediction. Squashing instruction [sn:%d]\n",
                  mispred_inst->thread_id, mispred_inst->seq_num);

  // tell Fetch to squash from a certain instruction due to branch
  // misprediction
  m_outgoing_squash_wire->decode_squash.squash = true;
  m_outgoing_squash_wire->decode_squash.mispred_inst = mispred_inst;
  m_outgoing_squash_wire->decode_squash.next_pc = mispred_inst->branchTarget();
  m_outgoing_squash_wire->decode_squash.branch_taken =
                                                  mispred_inst->pc.branching();

#ifdef DEBUG
  // record for linetrace
  m_stage_status.set(DecodeStatus::InitSquash);
#endif
}

void
Decode::sendInstToNextStage(IODynInstPtr inst)
{
  // sanity check: make sure we have enough credit before we sent the inst
  assert(m_num_credits > 0);
  // Place inst into the buffer
  m_outgoing_inst_wire->to_rename_insts.push_back(inst);
  // consume one credit
  m_num_credits--;

#ifdef DEBUG
  // record for linetrace
  m_stage_status.set(DecodeStatus::Busy);
  m_decoded_insts.push_back(inst);
#endif
}

void
Decode::linetrace(std::stringstream& ss)
{
#ifdef DEBUG
  std::string s = " [D] ";
  if (m_stage_status[DecodeStatus::Squashed]) {
    s += "x";
  } else if (m_stage_status[DecodeStatus::Stalled]) {
    s += "#";
  } else if (m_stage_status[DecodeStatus::Busy]) {
    assert(!m_decoded_insts.empty());
    for (auto inst : m_decoded_insts)
      s += inst->toString() + " ";
    // a decoded inst may init a squash signal, so record it as well
    if (m_stage_status[DecodeStatus::InitSquash]) {
      s += "^x";
    }
  }
  ss << std::setw(30) << std::left << s;

  // reset status (for line trace)
  m_stage_status.reset();
  m_decoded_insts.clear();
#endif
}
