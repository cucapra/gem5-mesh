#ifndef __BIND_DEFS_H__
#define __BIND_DEFS_H__

// include spec from gem5
#include "../../src/custom/bind_spec.hh"

// 20 bit / 5 hex
#define ALL_NORM  0x00000




// if you want to include a comma in a macro, need to indirectly do like
// so, otherwise the pre-processor will assume its a delimiter for the
// macro args
#define COMMA ,

// https://forums.sifive.com/t/confusion-regarding-freedom-e-sdk-inline-asm/383
// # is stringify, 'reg' must be explictliy written out
// 'val' must be defined at compile time
// in c this means it MUST BE a define or value
// in c++ it can be define, value, or const int
#define WRITE_CSR(reg, val) \
  asm volatile ("csrrwi x0, " #reg ", %[x]\n\t" :: [x] "i" (val))

// 0x400 is the csr specified in gem5 in src/arch/riscv/register.hh
#define WRITE_MESH_CSR(val) \
  WRITE_CSR(0x400, val)

#define BIND_EXE(val) \
  asm volatile (".insn u 0x6b, x0, %[x]\n\t" :: [x] "i" (val))

// to ensure that the compiler doesn't place unwanted instructions
// within the binds we enforce with a single asm volatile
#define BINDED_SECTION(sbind, ebind, code, wr, rd)  \
  asm volatile (                                    \
    ".insn u 0x6b, x0, %[bind0]\n\t"                 \
    code                                            \
    ".insn u 0x6b, x0, %[bind1]\n\t"                 \
    : wr                                            \
    : [bind0] "i" (sbind), [bind1] "i" (ebind) rd        \
  )

#endif
