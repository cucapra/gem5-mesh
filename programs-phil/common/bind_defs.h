#ifndef __BIND_DEFS_H__
#define __BIND_DEFS_H__

// include spec from gem5
#include "../../src/custom/bind_spec.hh"

#if !defined(__x86_64__) && !defined(__i386__)

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
  
// 0x401 is MISCREG_FET
#define VECTOR_EPOCH(val) \
  asm volatile (".insn i 0x77, 0, x0, %[x], 0x401\n\t" :: [x] "r" (val))

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
  
// 1) bind certain config
// 
// after a config the pipeline needs to be flushed
// we can achieve this easily in software with delay slots
// I think fetch2 and decode need to be flushed so put two slots
// Minor is a 7? (f1 -> f2 -> d -> i -> x -> m -> w) stage pipeline
// writeback, looks like need 5 delay slots!!!
//
// 2) figure out jump point (don't have to do this when resync though)
// this instruction can be put into one of the delay slots since doesnt need vec
// actually instructions can be put in delay slots and still be functional
// just won't be vectorized
//
// 3) run vectorized code
// 
// 4) devec
#define BINDED_FET_SECTION(sbind, ebind, id, code, wr, rd)  \
  asm volatile (                                            \
    ".insn u 0x77, x0, %[sbind0]\n\t"                       \
    ".insn uj 0x0b, x28, label" #id "\n\t"                  \
    "nop\n\t"                                               \
    "nop\n\t"                                               \
    "nop\n\t"                                               \
    "nop\n\t"                                               \
    "nop\n\t"                                               \
    "nop\n\t"                                               \
    "nop\n\t"                                               \
    code                                                    \
    ".insn u 0x7b, x28, %[ebind0]\n\t"                      \
    "nop\n\t"                                               \
    "nop\n\t"                                               \
    "nop\n\t"                                               \
    "nop\n\t"                                               \
    "nop\n\t"                                               \
    "nop\n\t"                                               \
    "nop\n\t"                                               \
    "nop\n\t"                                               \
    "label" #id ":\n\t"                                     \
    : wr                                                    \
    : [sbind0] "i" (sbind), [ebind0] "i" (ebind) rd         \
  )                                                 
 
// when using -O2/3 getting error using labels (already defined)
// normal solution is to use block unique name with %=
// BUT  can't use %= to generate unique labels because across diff blocks
// use 'goto' qualifier to jump outside of block?
#define BINDED_FET_SOURCE(sbind, ebind, code)               \
  asm volatile (                                            \
    ".insn u 0x77, x0, %[sbind0]\n\t"                       \
    ::[sbind0] "i" (sbind));                                \
  code                                                      \
  asm volatile (                                            \
    ".insn u 0x77, x0, %[ebind0]\n\t"                       \
    ::[ebind0] "i" (ebind));                                
  
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
  
static inline void stats_on()
{
#if !defined(__x86_64__) && !defined(__i386__)
  int on = 1;
 __asm__ volatile ("csrw 0x7C1, %0;"
                    :
                    : "r" (on)
                    :);
#endif
}

static inline void stats_off()
{
#if !defined(__x86_64__) && !defined(__i386__)
  int off = 10; // can't use 0, but anything other than 1
 __asm__ volatile ("csrw 0x7C1, %0;"
                    :
                    : "r" (off)
                    :);
#endif
}


#endif
