#ifndef __BIND_DEFS_H__
#define __BIND_DEFS_H__

// include spec from gem5
#include "../../src/custom/bind_spec.hh"

// #if !defined(__x86_64__) && !defined(__i386__)

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

// #define BIND_EXE(val) \
//   asm volatile (".insn u 0x6b, x0, %[x]\n\t" :: [x] "i" (val))

// #define BIND_FET(val) \
//   asm volatile (".insn u 0x77, x0, %[x]\n\t" :: [x] "i" (val))
  
// 0x401 is MISCREG_FET
#define VECTOR_EPOCH(val) \
  asm volatile (".insn i 0x77, 0, x0, %[x], 0x401\n\t" :: [x] "r" (val))

// revec instruction with unique hash id
#define REVEC(hash)                                                           \
  asm volatile (".insn u 0x7b, x0, %[id]\n\t" :: [id] "i" (hash))
  
// remem instruction with unique hash id (mem barrier instead of control barrier)
#define REMEM(hash)                                                           \
  asm volatile (".insn u 0x0b, x0, %[id]\n\t":: [id] "i" (hash))

#define ISSUE_VINST(label, inst_cnt)                                          \
  asm volatile goto (".insn uj 0x6b, x0, %l[" #label "]\n\t"                   \
    :                                                                         \
    :                                                                         \
    :                                                                         \
    : label                                                                   \
  )

#define PREFETCH_EPOCH(val) \
  asm volatile ("csrw 0x402, %[x]\n\t" :: [x] "r" (val))
  
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

#define STORE_NOACK(data, memAddr, offset) \
  asm volatile (".insn sb 0x23, 0x5, %[dataReg], %[off](%[mem])\n\t" :: \
    [dataReg] "r" (data), [mem] "r" (memAddr), [off] "i" (offset))     

  
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

// mask that guarentees a linear chain with no fanout
// implements a snake pattern
// -> -> -> v
// v <- <- <-
// -> -> -> v
// 0 <- <- <-
int getSerializedMask(int origin_x, int origin_y, int tid_x, int tid_y, int dim_x, int dim_y) {
  int mask = ALL_NORM;
  
  #ifndef _VEC
  return mask;
  #else
  
  // each row alternates between different behavior
  if (tid_y % 2 == 0) {
    // if first column either recv from above or not at all
    if (tid_x == 0) {
      if (tid_y == 0) {
        mask |= ALL_NORM;
      }
      else {
        mask |= FET_I_INST_UP;
      }
    }
    // otherwise recv from the left
    else {
      mask |= FET_I_INST_LEFT;
    }

    // send to the right if not at edge
    if (tid_x < dim_x - 1) {
      mask |= FET_O_INST_RIGHT_SEND;
    }
    // if at the edge send down
    else {
      mask |= FET_O_INST_DOWN_SEND;
    }
  }
  else {
    // input either above if at the right edge or from the right
    if (tid_x == dim_x - 1) {
      mask |= FET_I_INST_UP;
    }
    else {
      mask |= FET_I_INST_RIGHT;
    }

    // output either to the left or down if at left edge
    if (tid_x == 0) {
      mask |= FET_O_INST_DOWN_SEND;
    }
    else {
      mask |= FET_O_INST_LEFT_SEND;
    }
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
  