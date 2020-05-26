//-----------------------------------------------------------------------------
// decode.cc
//-----------------------------------------------------------------------------
//
// Author: Tuan Ta
// Date:   19/08/20

#include "cpu/io/decode.hh"

#include "cpu/io/cpu.hh"
#include "debug/Decode.hh"

Decode::Decode(IOCPU* _cpu_p, IOCPUParams* params, size_t in_size, size_t out_size)
    : Stage(_cpu_p, in_size, out_size, StageIdx::DecodeIdx, true),
    m_decode_width(1)
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
Decode::wakeup()
{
  Stage::wakeup();
}

void
Decode::suspend()
{
  Stage::suspend();
}

void
Decode::tick()
{
  // interact with credit and inst buffers
  Stage::tick();

  // check squash
  bool is_squashed = checkSquash();

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
Decode::doDecode()
{
  // try to decode as many incoming instructions as possible unless we run out
  // of credits to the next stage
  while (!m_insts.empty() && m_num_credits > 0) {
    IODynInstPtr inst = m_insts.front();

    #ifndef NDEBUG
    ThreadID tid = inst->thread_id;
    DPRINTF(Decode, "[tid:%d] Decoding inst [sn:%lli] with PC %s\n",
                    tid, inst->seq_num, inst->pc);
    #endif

    // send out this inst
    sendInstToNextStage(inst);

    // Remove the inst from the queue and increment the credit to the previous
    // stage.
    consumeInst();

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
    // This is NOT checking indirect jumps b/c haven't read regfile yet
    if (inst->isDirectCtrl() && (inst->isUncondCtrl() || inst->isPredTaken()))
    {
      if (inst->branchTarget() != inst->readPredTarg()) {
        // set the right branch target for this instruction
        inst->setPredTarg(inst->branchTarget());
        
        // check if this is a trace and we were expecting this branch target (i.e. from a jal)
        // TODO not really sure what to do here, I think can just always pass true if trace
        //if (inst->checkTrace(inst->master_taken, inst->branchTarget())) {
        if (inst->from_trace) {
          continue;
        }
        
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
Decode::doSquash(SquashComm::BaseSquash &squashInfo, StageIdx initiator)
{
  IODynInstPtr squash_inst = squashInfo.trig_inst;
  
  if (initiator == StageIdx::CommitIdx)
    DPRINTF(Decode, "Squash from Commit: squash inst [tid:%d] [sn:%d]\n",
                    squash_inst->thread_id, squash_inst->seq_num);
  else if (initiator == StageIdx::IEWIdx)
    DPRINTF(Decode, "Squash from IEW: squash inst [tid:%d] [sn:%d]\n",
                    squash_inst->thread_id, squash_inst->seq_num);
  else if (initiator == StageIdx::DecodeIdx)
    DPRINTF(Decode, "Squash from Decode: squash inst [tid:%d] [sn:%d]\n",
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
      DPRINTF(Decode, "Squashing %s\n", inst->toString());
      assert(inst->seq_num > squash_inst->seq_num);
      // increment the number of credits to the previous stage
      outputCredit()++;
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
  m_outgoing_squash_wire->decode_squash()->squash = true;
  m_outgoing_squash_wire->decode_squash()->trig_inst = mispred_inst;
  m_outgoing_squash_wire->decode_squash()->next_pc = mispred_inst->branchTarget();
  m_outgoing_squash_wire->decode_squash()->branch_taken =
                                                  mispred_inst->pc.branching();

#ifdef DEBUG
  // record for linetrace
  m_stage_status.set(DecodeStatus::InitSquash);
#endif
}

void
Decode::sendInstToNextStage(IODynInstPtr inst)
{
  // actual send inst to next stage
  Stage::sendInstToNextStage(inst);

  // store the clock edge at which the instruction is pushed to the next stage
  //inst->decode_cycles = m_cpu_p->curCycle();
  inst->master_info[2] = m_cpu_p->curCycle();

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
