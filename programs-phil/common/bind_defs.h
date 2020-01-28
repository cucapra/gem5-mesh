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

// revec instruction with unique hash id
#define REVEC(hash)                                                           \
  asm volatile (".insn u 0x7b, x0, %[id]\n\t" :: [id] "i" (hash))
  
// remem instruction with unique hash id (mem barrier instead of control barrier)
#define REMEM(hash)                                                           \
  asm volatile ("nop\n\t"::)

// do a csr read on register containing the number of open regions
#define READ_OPEN_REGIONS(ret) \
  asm volatile ("csrr %[rdest], <csrreg>\n\t" : [rdest] "=r" (ret) :)

static int __readOpenRegions() {
  int ret;
  READ_OPEN_REGIONS(ret);
  return ret;
}
  
  // revec instruction with unique hash id
/*#define REVEC(hash)                                                           \
  asm volatile ("" ::: "memory");                                             \
  asm volatile (".insn u 0x7b, x0, %[id]\n\t" :: [id] "i" (hash) : "memory"); \
  asm volatile ("" ::: "memory");
*/
// 0x0f << 2 & 0x3 = 0x3f
// actually use, unused sw funct3
// if don't do this then compiler thinks its a 64bit instructions which
// messes up gem5
#define VPREFETCH(spadAddr, memAddr, offset) \
  asm volatile (".insn sb 0x23, 0x4, %[spad], %[off](%[mem])\n\t" :: \
    [spad] "r" (spadAddr), [mem] "r" (memAddr), [off] "i" (offset))
    
#define LWSPEC(dest, spadAddr, offset)                    \
  asm volatile (                                          \
    ".insn s 0x03, 0x7, %[destreg], %[off](%[mem])\n\t"   \
    : [destreg] "=r" (dest)                               \
    : [mem] "r" (spadAddr), [off] "i" (offset))         
    
/*#define LWSPEC_RESET(val, spadAddr, offset)             \
  asm volatile (                                        \
    ".insn s 0x07, 0x6, %[val], %[off](%[mem])\n\t"     \
    : [val] "=r" (val)                                  \
    : [mem] "r" (spadAddr), [off] "i" (offset))
*/

#define STORE_NOACK(data, memAddr, offset) \
  asm volatile (".insn sb 0x23, 0x5, %[dataReg], %[off](%[mem])\n\t" :: \
    [dataReg] "r" (data), [mem] "r" (memAddr), [off] "i" (offset))     

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

int getVecMask(int origin_x, int origin_y, int tid_x, int tid_y, int dim_x, int dim_y) {
  int mask = ALL_NORM;
  
  #ifndef _VEC
  return mask;
  #else
  
  // upper left corner is the master
  if (tid_x == 0 && tid_y == 0) {
    mask = FET_O_INST_DOWN_SEND | FET_O_INST_RIGHT_SEND;
  }
  
  // right edge does not send to anyone
  else if (tid_x == dim_x - 1) {
    mask = FET_I_INST_LEFT;
  }
  
  // bottom left corner just sends to the right
  else if (tid_x == 0 && tid_y == dim_y - 1) {
    mask = FET_I_INST_UP | FET_O_INST_RIGHT_SEND;
  }
  
  // the left edge (besides corners) sends down and to the right
  else if (tid_x == 0) {
    mask = FET_I_INST_UP | FET_O_INST_DOWN_SEND | FET_O_INST_RIGHT_SEND;
  }
  
  // otherwise we're just forwarding to the right in the middle area
  else {
    mask = FET_I_INST_LEFT | FET_O_INST_RIGHT_SEND;
  }
  
  // specify the vlen
  int vlenX = dim_x;
  int vlenY = dim_y;
  mask |= (origin_x << FET_XORIGIN_SHAMT) | (origin_y << FET_YORIGIN_SHAMT) | (vlenX << FET_XLEN_SHAMT) | (vlenY << FET_YLEN_SHAMT);

  // specify each core is an execute core
  mask |= (0 << FET_DAE_SHAMT);

  return mask;
  #endif
}

int getDAEMask(int origin_x, int origin_y, int tid_x, int tid_y, int dim_x, int dim_y) {
  int mask = (1 << FET_DAE_SHAMT) | 
            (origin_x << FET_XORIGIN_SHAMT) | 
            (origin_y << FET_YORIGIN_SHAMT) | 
            (dim_x << FET_XLEN_SHAMT) | 
            (dim_y << FET_YLEN_SHAMT);
  return mask;
}

#endif
