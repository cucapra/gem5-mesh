//-----------------------------------------------------------------------------
// dyn_inst.cc
//-----------------------------------------------------------------------------
//
// Author: Tuan Ta
// Date  : 19/08/19

#include "cpu/io/dyn_inst.hh"

#include <sstream>

#include "arch/utility.hh"
#include "cpu/io/cpu.hh"

IODynInst::IODynInst(const StaticInstPtr& static_inst,
                     TheISA::PCState _pc,
                     InstSeqNum seq_num,
                     ThreadID tid,
                     IOCPU* cpu_p)
    : seq_num(seq_num),
      static_inst_p(static_inst),
      pc(_pc),
      thread_id(tid),
      fault(NoFault),
      predicted_taken(false),
      mem_req_p(nullptr),
      mem_data_p(nullptr),
      m_cpu_p(cpu_p),
      m_num_dest_misc_regs(0),
      m_pred_pc(0),
      m_inst_str(static_inst->disassemble(pc.pc()))
{ }

IODynInst::~IODynInst()
{
  if (mem_data_p)
    delete[] mem_data_p;
}

Fault
IODynInst::execute()
{
  this->fault = static_inst_p->execute(this, nullptr);
  return this->fault;
}

Fault
IODynInst::initiateAcc()
{
  return static_inst_p->initiateAcc(this, nullptr);
}

Fault
IODynInst::completeAcc(PacketPtr pkt)
{
  return static_inst_p->completeAcc(pkt, this, nullptr);
}

bool
IODynInst::isFault() const
{
  return fault != NoFault;
}

void
IODynInst::setPredTarg(const TheISA::PCState& _pred_pc)
{
  m_pred_pc = _pred_pc;
}

const TheISA::PCState&
IODynInst::readPredTarg()
{
  return m_pred_pc;
}

TheISA::PCState
IODynInst::branchTarget()
{
  TheISA::PCState target_pc;
  if (!static_inst_p->hasBranchTarget(pc, tcBase(), target_pc))
    panic("%s does not have branch target\n", this->toString(true));
  return target_pc;
}

bool
IODynInst::isMispredicted()
{
  TheISA::PCState temp_pc = pc;
  TheISA::advancePC(temp_pc, static_inst_p);
  return !(temp_pc == m_pred_pc);
}

PhysRegIdPtr
IODynInst::renamedDestRegIdx(int idx) const
{
  assert(idx < TheISA::MaxInstDestRegs);
  return m_dest_reg_idx[idx];
}

PhysRegIdPtr
IODynInst::renamedSrcRegIdx(int idx) const
{
  assert(idx < TheISA::MaxInstSrcRegs);
  return m_src_reg_idx[idx];
}

void
IODynInst::renameDestReg(int idx, PhysRegIdPtr renamed_dest,
                         PhysRegIdPtr prev_rename)
{
  m_dest_reg_idx[idx] = renamed_dest;
  m_prev_dest_reg_idx[idx] = prev_rename;
}

void
IODynInst::renameSrcReg(int idx, PhysRegIdPtr renamed_src)
{
  m_src_reg_idx[idx] = renamed_src;
}

const RegId&
IODynInst::flattenedDestRegIdx(int idx) const
{
  return m_flat_dest_reg_idx[idx];
}

void
IODynInst::flattenDestReg(int idx, const RegId& flattened_dest)
{
  m_flat_dest_reg_idx[idx] = flattened_dest;
}

PhysRegIdPtr
IODynInst::prevDestRegIdx(int idx) const
{
  return m_prev_dest_reg_idx[idx];
}

std::string
IODynInst::toString(bool full)
{
  std::stringstream ss;
  ss << "[sn:" << seq_num;

  if (full) {
    ss << "/" << "tid=" << std::dec << thread_id;
    ss << "/0x" << std::hex << pc.pc();
    ss << "/" << m_inst_str << "]";
  } else {
    ss << "/" << static_inst_p->getName();
    ss << "]";
  }

  return ss.str();
}

//-----------------------------------------------------------------------------
// Implementation of functions inheritted from ExecContext
//-----------------------------------------------------------------------------

RegVal
IODynInst::readIntRegOperand(const StaticInst *si, int idx)
{
  return m_cpu_p->readIntReg(m_src_reg_idx[idx]);
}

void
IODynInst::setIntRegOperand(const StaticInst *si, int idx, RegVal val)
{
  m_cpu_p->setIntReg(m_dest_reg_idx[idx], val);
}

RegVal
IODynInst::readFloatRegOperandBits(const StaticInst *si, int idx)
{
  return m_cpu_p->readFloatReg(m_src_reg_idx[idx]);
}

void
IODynInst::setFloatRegOperandBits(const StaticInst *si, int idx, RegVal val)
{
  m_cpu_p->setFloatReg(m_dest_reg_idx[idx], val);
}

auto
IODynInst::readVecRegOperand(const StaticInst *si, int idx) const
                                                -> const VecRegContainer&
{
  return m_cpu_p->readVecReg(m_src_reg_idx[idx]);
}

auto
IODynInst::getWritableVecRegOperand(const StaticInst *si, int idx)
                                                      -> VecRegContainer&
{
  return m_cpu_p->getWritableVecReg(m_dest_reg_idx[idx]);
}

void
IODynInst::setVecRegOperand(const StaticInst *si, int idx,
                            const VecRegContainer& val)
{
  m_cpu_p->setVecReg(m_dest_reg_idx[idx], val);
}

ConstVecLane8
IODynInst::readVec8BitLaneOperand(const StaticInst *si, int idx) const
{
  return m_cpu_p->template readVecLane<uint8_t>(m_src_reg_idx[idx]);
}

ConstVecLane16
IODynInst::readVec16BitLaneOperand(const StaticInst *si, int idx) const
{
  return m_cpu_p->template readVecLane<uint16_t>(m_src_reg_idx[idx]);
}

ConstVecLane32
IODynInst::readVec32BitLaneOperand(const StaticInst *si, int idx) const
{
  return m_cpu_p->template readVecLane<uint32_t>(m_src_reg_idx[idx]);
}

ConstVecLane64
IODynInst::readVec64BitLaneOperand(const StaticInst *si, int idx) const
{
  return m_cpu_p->template readVecLane<uint64_t>(m_src_reg_idx[idx]);
}

template <typename LD>
void
IODynInst::setVecLaneOperandT(const StaticInst* si, int idx, const LD& val)
{
  m_cpu_p->template setVecLane(m_dest_reg_idx[idx], val);
}

void
IODynInst::setVecLaneOperand(const StaticInst *si, int idx,
                             const LaneData<LaneSize::Byte>& val)
{
  setVecLaneOperandT(si, idx, val);
}

void
IODynInst::setVecLaneOperand(const StaticInst *si, int idx,
                             const LaneData<LaneSize::TwoByte>& val)
{
  setVecLaneOperandT(si, idx, val);
}

void
IODynInst::setVecLaneOperand(const StaticInst *si, int idx,
                             const LaneData<LaneSize::FourByte>& val)
{
  setVecLaneOperandT(si, idx, val);
}

void
IODynInst::setVecLaneOperand(const StaticInst *si, int idx,
                             const LaneData<LaneSize::EightByte>& val)
{
  setVecLaneOperandT(si, idx, val);
}

auto
IODynInst::readVecElemOperand(const StaticInst *si, int idx) const -> VecElem
{
  return m_cpu_p->readVecElem(m_src_reg_idx[idx]);
}

void
IODynInst::setVecElemOperand(const StaticInst *si, int idx, const VecElem val)
{
  m_cpu_p->setVecElem(m_dest_reg_idx[idx], val);
}

auto
IODynInst::readVecPredRegOperand(const StaticInst *si, int idx) const
                                              -> const VecPredRegContainer&
{
  return m_cpu_p->readVecPredReg(m_src_reg_idx[idx]);
}

auto
IODynInst::getWritableVecPredRegOperand(const StaticInst *si, int idx)
                                              -> VecPredRegContainer&
{
  return m_cpu_p->getWritableVecPredReg(m_dest_reg_idx[idx]);
}

void
IODynInst::setVecPredRegOperand(const StaticInst *si, int idx,
                                const VecPredRegContainer& val)
{
  m_cpu_p->setVecPredReg(m_dest_reg_idx[idx], val);
}

RegVal
IODynInst::readCCRegOperand(const StaticInst *si, int idx)
{
  return m_cpu_p->readCCReg(m_src_reg_idx[idx]);
}

void
IODynInst::setCCRegOperand(const StaticInst *si, int idx, RegVal val)
{
  m_cpu_p->setCCReg(m_dest_reg_idx[idx], val);
}

RegVal
IODynInst::readMiscRegOperand(const StaticInst *si, int idx)
{
  const RegId& reg = si->srcRegIdx(idx);
  assert(reg.isMiscReg());
  return m_cpu_p->readMiscReg(reg.index(), thread_id);
}

void
IODynInst::setMiscRegOperand(const StaticInst *si, int idx, RegVal val)
{
  const RegId& reg = si->destRegIdx(idx);
  assert(reg.isMiscReg());
  setMiscReg(reg.index(), val);
}

RegVal
IODynInst::readMiscReg(int misc_reg)
{
  return m_cpu_p->readMiscReg(misc_reg, thread_id);
}

void
IODynInst::setMiscReg(int misc_reg, RegVal val)
{
  /** Writes to misc. registers are recorded and deferred until the
   * commit stage, when updateMiscRegs() is called. First, check if
   * the misc reg has been written before and update its value to be
   * committed instead of making a new entry. If not, make a new
   * entry and record the write.
   */
  for (int idx = 0; idx < m_num_dest_misc_regs; idx++) {
      if (m_dest_misc_reg_idx[idx] == misc_reg) {
          m_dest_misc_reg_val[idx] = val;
          return;
      }
  }

  assert(m_num_dest_misc_regs < TheISA::MaxMiscDestRegs);
  m_dest_misc_reg_idx[m_num_dest_misc_regs] = misc_reg;
  m_dest_misc_reg_val[m_num_dest_misc_regs] = val;
  m_num_dest_misc_regs++;
}

void
IODynInst::handleMiscRegOp(int misc_reg, RegVal old_val, const RegVal &val)
{
  ExecContext::handleMiscRegOp(misc_reg, old_val, val);
}

void
IODynInst::updateMiscRegs()
{
  for (int i = 0; i < m_num_dest_misc_regs; i++) {
    m_cpu_p->setMiscReg(m_dest_misc_reg_idx[i],
                        m_dest_misc_reg_val[i],
                        thread_id);
  }
}

TheISA::PCState
IODynInst::pcState() const
{
  return pc;
}

void
IODynInst::pcState(const TheISA::PCState &val)
{
  pc = val;
}

Fault
IODynInst::readMem(Addr addr, uint8_t *data, unsigned int size,
                   Request::Flags flags)
{
  panic("IO CPU does not support atomic read\n");
  return NoFault;
}

Fault
IODynInst::initiateMemRead(Addr addr, unsigned int size,
                           Request::Flags flags)
{
  return m_cpu_p->pushMemReq(this, true, nullptr, size, addr, flags,
                             nullptr, nullptr);
}

Fault
IODynInst::writeMem(uint8_t *data, unsigned int size, Addr addr,
                    Request::Flags flags, uint64_t *res)
{
  return m_cpu_p->pushMemReq(this, false, data, size, addr, flags,
                             res, nullptr);
}

Fault
IODynInst::amoMem(Addr addr, uint8_t *data, unsigned int size,
                  Request::Flags flags, AtomicOpFunctor *amo_op)
{
  panic("IO CPU does not support atomic AMO\n");
  return NoFault;
}

Fault
IODynInst::initiateMemAMO(Addr addr, unsigned int size,
                          Request::Flags flags, AtomicOpFunctor *amo_op)
{
  return m_cpu_p->pushMemReq(this, false, nullptr, size, addr, flags,
                             nullptr, amo_op);
}

void
IODynInst::setStCondFailures(unsigned int sc_failures)
{
  m_cpu_p->tcBase(thread_id)->setStCondFailures(sc_failures);
}

unsigned int
IODynInst::readStCondFailures() const
{
  return m_cpu_p->tcBase(thread_id)->readStCondFailures();
}

void
IODynInst::syscall(int64_t callnum, Fault *fault)
{
  if (FullSystem)
    panic("Syscall emulation isn't available in FS mode.\n");
  tcBase()->syscall(callnum, fault);
}

ThreadContext*
IODynInst::tcBase()
{
  return m_cpu_p->tcBase(thread_id);
}
