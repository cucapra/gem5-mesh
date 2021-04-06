/*
 * Authors: Khalid Al-Hawaj
 */

#ifndef __ARCH_RISCV_INST_VECTOR_HH__
#define __ARCH_RISCV_INST_VECTOR_HH__

#include <string>

#include "arch/riscv/insts/static_inst.hh"
#include "cpu/exec_context.hh"
#include "cpu/static_inst.hh"

namespace RiscvISA
{

class VOp : public RiscvStaticInst
{
  protected:
    /* Instruction fields */
    bool xd;
    bool xs1;
    bool xs2;

    uint64_t rd;
    uint64_t rs1;
    uint64_t rs2;

    uint64_t funct;
    uint64_t opcode;

    uint64_t csr;
    uint64_t vmop;

    // immediate
    int64_t imm;

  protected:
    VOp(const char *mnem, ExtMachInst _machInst, OpClass __opClass)
       : RiscvStaticInst(mnem, _machInst, __opClass),
         xd(false), xs1(false), xs2(false),
         rd(0), rs1(0), rs2(0),
         funct(0), opcode(0),
         csr(0), vmop(0),
         imm(0)
    {}
};

}

#endif // __ARCH_RISCV_INST_VECTOR_HH__
