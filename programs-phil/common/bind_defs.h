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

#define BIND_FET(val) \
  asm volatile (".insn u 0x77, x0, %[x]\n\t" :: [x] "i" (val))

// to ensure that the compiler doesn't place unwanted instructions
// within the binds we enforce with a single asm volatile
#define BINDED_EXE_SECTION(sbind, ebind, code, wr, rd)  \
  asm volatile (                                        \
    ".insn u 0x6b, x0, %[bind0]\n\t"                    \
    code                                                \
    ".insn u 0x6b, x0, %[bind1]\n\t"                    \
    : wr                                                \
    : [bind0] "i" (sbind), [bind1] "i" (ebind) rd       \
  )
  
// fetch goes back to normal after specified number of executes
// so no need to unbind
// should exe also do this? no neccessary but will reduce instruction count
/*#define BINDED_FET_SECTION(sbind, timer, code, wr, rd)      \
  asm volatile (                                            \
    ".insn u 0x77, x0, %[bind0]\n\t"                        \
    code                                                    \
    : wr                                                    \
    : [bind0] "i" (sbind | (timer << FET_COUNT_SHAMT)) rd  \
  )
*/
  
// bind certain config
// run vectorized code
// devec
// label magic
// https://stackoverflow.com/questions/1777990/is-it-possible-to-store-the-address-of-a-label-in-a-variable-and-use-goto-to-jum
#define BINDED_FET_SECTION(sbind, id, code, wr, rd)         \
  asm volatile (                                            \
    ".insn u 0x77, x0, %[bind0]\n\t"                        \
    code                                                    \
    ".insn u 0x7b, x0, devec_label" #id "\n\t"              \
    "devec_label" #id ":\n\t"                               \
    : wr                                                    \
    : [bind0] "i" (sbind) rd                                \
  )                                                 
  
  
// bind both exe and fetch
#define BINDED_TIMED_SECTION(ebind0, ebind1, fbind0, timer, code, wr, rd)  \
  asm volatile (                                            \
    ".insn u 0x6b, x0, %[ebind0]\n\t"                       \
    ".insn u 0x77, x0, %[fbind0]\n\t"                       \
    code                                                    \
    ".insn u 0x6b, x0, %[ebind1]\n\t"                       \
    : wr                                                    \
    : [ebind0] "i" (ebind0), [ebind1] "i" (ebind1),         \
      [fbind0] "i" (fbind0 | (timer << FET_COUNT_SHAMT)) rd \
  )

#define BINDED_SECTION(ebind0, ebind1, fbind0, fbind1, code, wr, rd)  \
  asm volatile (                                            \
    ".insn u 0x6b, x0, %[eb0]\n\t"                          \
    ".insn u 0x77, x0, %[fb0]\n\t"                          \
    code                                                    \
    ".insn u 0x77, x0, %[fb1]\n\t"                          \
    ".insn u 0x6b, x0, %[eb1]\n\t"                          \
    : wr                                                    \
    : [eb0] "i" (ebind0), [eb1] "i" (ebind1),               \
      [fb0] "i" (fbind0), [fb1] "i" (fbind1) rd             \
  )

#endif
