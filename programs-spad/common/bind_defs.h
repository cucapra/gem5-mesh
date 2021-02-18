#ifndef __BIND_DEFS_H__
#define __BIND_DEFS_H__

// include spec from gem5
#include "../../src/custom/bind_spec.hh"
#include "group_templates.h"

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
  asm volatile (".insn i 0x77, 0, x0, %[x], 0x401\n\t" :: [x] "r" (val) : "memory")

// revec instruction with unique hash id
#define REVEC(hash)                                                           \
  asm volatile (".insn u 0x7b, x0, %[id]\n\t" :: [id] "i" (hash))
  
#define END_FRAME()                                                             \
  asm volatile (".insn i 0x1b, 0x2, x0, x0, 0\n\t"::: "memory")

// remem instruction with unique hash id (mem barrier instead of control barrier)
#define REMEM(count)                                                              \
  END_FRAME()

// #define REMEM(count)                                                           \
//   asm volatile (".insn i 0x1b, 0x2, x0, %[src0], 0\n\t":: [src0] "r" (count) : "memory")

#define START_FRAME()                                                         \
  asm volatile (".insn i 0x1b, 0x3, x0, x0, 0\n\t"::: "memory")

#define FRAME_START(count)                                                     \
  START_FRAME()

  // asm volatile (".insn i 0x1b, 0x3, x0, %[src0], 0\n\t":: [src0] "r" (count) : "memory")

#define ISSUE_VINST(label)                                                    \
  asm volatile goto (".insn uj 0x6b, x0, %l[" #label "]\n\t"                  \
    :                                                                         \
    :                                                                         \
    :                                                                         \
    : label                                                                   \
  )

#define DEVEC(devec_id)                                                       \
  devec_id:                                                                   \
  asm volatile goto (".insn uj 0x2b, x0, %l[" #devec_id "]\n\t"               \
    :                                                                         \
    :                                                                         \
    :                                                                         \
    : devec_id                                                                \
  )

#define PREFETCH_EPOCH(val) \
  asm volatile ("csrw 0x402, %[x]\n\t" :: [x] "r" (val) : "memory")
  
#define BROADCAST(dest_reg, val, imm) \
  asm volatile (".insn i 0x1b, 0x6, " #dest_reg ", %[src_reg], %[imm_val]\n\t" \
    :: [src_reg] "r" (val), [imm_val] "i" (imm)                                \
  )

// allow following instructions to proceed if registers equal
#define PRED_EQ(reg0, reg1) \
  asm volatile (".insn r 0x33, 0x7, 0x5, x0, %[rs1], %[rs2]\n\t" \
  :: [rs1] "r" (reg0), [rs2] "r" (reg1) : "memory")

// allow following instructions to proceed if registers not equal
#define PRED_NEQ(reg0, reg1) \
  asm volatile (".insn r 0x33, 0x7, 0x6, x0, %[rs1], %[rs2]\n\t" \
  :: [rs1] "r" (reg0), [rs2] "r" (reg1) : "memory")

#define TERMINATE_BLOCK() \
  asm volatile(".insn i 0x1b, 0x7, x0, x0, 0\n\t")

// Programmer must ensure if's go the same way
// and CANNOT use beqz instructions (due to vissue-asm.py predication block handling)
#define CONVERGENT_IF(cond) \
  if (cond)

#define CONVERGENT_ELIF(cond) \
  else if (cond)

#define CONVERGENT_ELSE \
  else

// config defs for prefetch. horizontal or vertical prefetching
// #define HORIZONTAL 0
// #define VERTICAL   1

#define TO_ONE_CORE 0
#define TO_ALL_CORES 1

// set mask and do barrier because need to wait for all cores to be on same page about frame size
// CSR writes in gem5 require the data to have changed for some reason, so need to switch to 0 before updating
#define SET_PREFETCH_MASK(num_frames, frame_size, barrier_ptr) \
  PREFETCH_EPOCH((num_frames << PREFETCH_NUM_REGION_SHAMT) | (frame_size << PREFETCH_REGION_SIZE_SHAMT)); \
  pthread_barrier_wait(barrier_ptr)

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
// #define VPREFETCH(spadAddr, memAddr, group_start, group_end) \
//   asm volatile (".insn sb 0x23, 0x4, %[spad], %[off](%[mem])\n\t" :: \
//     [spad] "r" (spadAddr), [mem] "r" (memAddr), [off] "i" ((group_start << 6) | (group_end - group_start)))

#define LWSPEC(dest, spadAddr, offset)                    \
  asm volatile (                                          \
    ".insn s 0x03, 0x7, %[destreg], %[off](%[mem])\n\t"   \
    : [destreg] "=r" (dest)                               \
    : [mem] "r" (spadAddr), [off] "i" (offset))         

#if __GNUC__ >= 10
#define STORE_NOACK(data, memAddr, offset) \
  asm volatile (".insn s 0x23, 0x5, %[dataReg], %[off](%[mem])\n\t" :: \
    [dataReg] "r" (data), [mem] "r" (memAddr), [off] "i" (offset))     

#define VPREFETCH_L(spadOffset, memAddr, coreOffset, count, config)   \
  asm volatile (".insn s 0x23, 0x6, %[spad], %[off](%[mem])\n\t" ::   \
    [spad] "r" ((coreOffset << 12) | spadOffset),                     \
    [mem] "r" (memAddr),                                              \
    [off] "i" ((count << 2) | config))

#define VPREFETCH_R(spadOffset, memAddr, coreOffset, count, config)   \
  asm volatile (".insn s 0x23, 0x7, %[spad], %[off](%[mem])\n\t" ::   \
    [spad] "r" ((coreOffset << 12) | spadOffset),                     \
    [mem] "r" (memAddr),                                              \
    [off] "i" ((count << 2) | config))
#else
#define STORE_NOACK(data, memAddr, offset) \
  asm volatile (".insn sb 0x23, 0x5, %[dataReg], %[off](%[mem])\n\t" :: \
    [dataReg] "r" (data), [mem] "r" (memAddr), [off] "i" (offset))     

#define VPREFETCH_L(spadOffset, memAddr, coreOffset, count, config)   \
  asm volatile (".insn sb 0x23, 0x6, %[spad], %[off](%[mem])\n\t" ::   \
    [spad] "r" ((coreOffset << 12) | spadOffset),                     \
    [mem] "r" (memAddr),                                              \
    [off] "i" ((count << 2) | config))

#define VPREFETCH_R(spadOffset, memAddr, coreOffset, count, config)   \
  asm volatile (".insn sb 0x23, 0x7, %[spad], %[off](%[mem])\n\t" ::   \
    [spad] "r" ((coreOffset << 12) | spadOffset),                     \
    [mem] "r" (memAddr),                                              \
    [off] "i" ((count << 2) | config))
#endif

#define VPREFETCH_LR(sp, memIdx, core, len, style)  \
  VPREFETCH_L(sp, memIdx, core, len, style);        \
  VPREFETCH_R(sp, memIdx, core, len, style)
  
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

typedef struct Vector2_t {
  int x;
  int y;
  // int touched;
  int o;
} Vector2_t;

static int isCoordEqual(Vector2_t a, Vector2_t b) {
  return (a.x == b.x && a.y == b.y);
}

static Vector2_t addVec2(Vector2_t a, Vector2_t b) {
  Vector2_t sum = { .x = a.x + b.x, .y = a.y + b.y };
  return sum;
}

// point sample x,y
// topleft box coord x,y
// dimx > dim to right
// dimy v dim under (but maps to positively increasing)
static int pointIntersectsBox(Vector2_t pt, Vector2_t boxOrig, Vector2_t boxDim) {
  return ((pt.x >= boxOrig.x && pt.x < boxOrig.x + boxDim.x) &&
          (pt.y >= boxOrig.y && pt.y < boxOrig.y + boxDim.y));
}

static int getInputFromOutput(int outputDir) {
  if (outputDir == FET_O_INST_UP_SEND) return FET_I_INST_DOWN;
  if (outputDir == FET_O_INST_DOWN_SEND) return FET_I_INST_UP;
  if (outputDir == FET_O_INST_LEFT_SEND) return FET_I_INST_RIGHT;
  if (outputDir == FET_O_INST_RIGHT_SEND) return FET_I_INST_LEFT;
  else {
    return -1;
  }
}

// vector orientation of group with specific sending pattern
static int getSIMDMaskHoriz(Vector2_t master, Vector2_t origin, Vector2_t tid, Vector2_t dim, Vector2_t virtVectorSrc, Vector2_t vectorSrc) {
  int mask = ALL_NORM;
  // for the rest of the cores, you can determine sending pattern based on location 
  // of this core relative to vector src
  // if +/-y you recv from that respective direction
  // if you are even you recv +/-x
  int yDiff = tid.y - virtVectorSrc.y;
  int xDiff = tid.x - virtVectorSrc.x;
  // printf("vec src (%d,%d) tid (%d,%d) diffs (%d,%d)\n", virtVectorSrc.x, virtVectorSrc.y, tid.x, tid.y, xDiff, yDiff);
  // recv from above and send below unless you are at the bottom
  if (yDiff > 0) {
    mask |= FET_I_INST_UP;
    // printf("in up\n");
    if (tid.y != dim.y - 1) {
      // printf("out down\n");
      mask |= FET_O_INST_DOWN_SEND;
    }
  }
  // recv from below and send above unless you are at the top
  else if (yDiff < 0) {
    mask |= FET_I_INST_DOWN;
    // printf("in down\n");
    if (tid.y != 0) {
      // printf("out up\n");
      mask |= FET_O_INST_UP_SEND;
    }
  }
  // if you are equal then need to look at x direction
  else {
    // recv from left and send to right unless at right edge
    if (xDiff > 0) {
      if (tid.x != 0) {
        mask |= FET_I_INST_LEFT;
        // printf("in left\n");
      }
      if (tid.x != dim.x - 1) {
        // printf("out right\n");
        mask |= FET_O_INST_RIGHT_SEND;
      }
    }
    // recv from right and send to the left unless at left edge
    else if (xDiff < 0) {
      if (tid.x != dim.x - 1) {
        mask |= FET_I_INST_RIGHT;
        // printf("in right\n");
      }
      if (tid.x != 0) {
        // printf("out left\n");
        mask |= FET_O_INST_LEFT_SEND;
      }
    }
    // figure out what to do if you are at the vecSrc
    else {
      // if cores to the right, need to send to the right
      if (vectorSrc.x < origin.x + dim.x - 1) {
        // printf("out right\n");
        mask |= FET_O_INST_RIGHT_SEND;
      }
      // if cores to the left, need to send to the left
      if (vectorSrc.x > origin.x) {
        // printf("out left\n");
        mask |= FET_O_INST_LEFT_SEND;
      }
    }

    // need row at equal height to send up/down
    // if vectorSrc is above bottom of group need to send down
    if (vectorSrc.y < origin.y + dim.y - 1) {
      // printf("out down\n");
      mask |= FET_O_INST_DOWN_SEND;
    }
    // if vectorSrc is below top of group then need to send up
    if (vectorSrc.y > origin.y) {
      // printf("out up\n");
      mask |= FET_O_INST_UP_SEND;
    }
  }

  return mask;
  
}

// configuration for vector-simd group, takes up size dim+1, so be careful about planning
// master x,y --> where the master is
// origin x,y --> where the top-left core is for the trailing cores
// tid    x,y --> thread id within the group, don't care for master
// dim    x,y --> dimension of the trailing core group
// is_master  --> whether this core is the master or not
static int getSIMDMask(core_config_info_t *cinfo) {
  // unpack struct
  int master_x  = cinfo->master.x;
  int master_y  = cinfo->master.y;
  int origin_x  = cinfo->orig.x;
  int origin_y  = cinfo->orig.y;
  int tid_x     = cinfo->vtid.x;
  int tid_y     = cinfo->vtid.y;
  int dim_x     = cinfo->vdim.x;
  int dim_y     = cinfo->vdim.y;
  int is_master = cinfo->is_scalar;
  
  // TODO does not handle case where master is above or below due to nesting order?????????

  // pack x,y into coord struct
  Vector2_t master = { .x = master_x, .y = master_y };
  Vector2_t origin = { .x = origin_x, .y = origin_y };
  Vector2_t tid    = { .x = tid_x,    .y = tid_y    };
  Vector2_t dim    = { .x = dim_x,    .y = dim_y    };

  // initialize to no vector mask
  int mask = ALL_NORM;

  // output directions
  Vector2_t directions[4] = { {.x =  1, .y =  0, .o = FET_O_INST_RIGHT_SEND  }, 
                              {.x =  0, .y =  1, .o = FET_O_INST_DOWN_SEND   },
                              {.x = -1, .y =  0, .o = FET_O_INST_LEFT_SEND   },
                              {.x =  0, .y = -1, .o = FET_O_INST_UP_SEND     },
                            };

  // core in vector adjacent to the master core
  Vector2_t vectorSrc;

  // direction master should send
  int masterSendDir = 0;

  // find closest tile in vector group, master will send to that one
  // do this by trying each cardinal diection and seeing if intersect the vector box
  for (int i = 0; i < 4; i++) {
    Vector2_t loc = addVec2(master, directions[i]);
    if (pointIntersectsBox(loc, origin, dim)) {
      vectorSrc = loc;
      masterSendDir = directions[i].o;
    }
  }

  // the master sends to vector src and vectorSrc recvs from master
  if (is_master) {
    // if (masterSendDir == FET_O_INST_UP_SEND) printf("out up\n");
    // if (masterSendDir == FET_O_INST_DOWN_SEND) printf("out down\n");
    // if (masterSendDir == FET_O_INST_LEFT_SEND) printf("out left\n");
    // if (masterSendDir == FET_O_INST_RIGHT_SEND) printf("out right\n");
    mask |= masterSendDir;
  }
  else {
    // make sure vectorSrc is virtualized within the group
    Vector2_t virtVecSrc = { .x = vectorSrc.x - origin.x, .y = vectorSrc.y - origin.y };
    if (isCoordEqual(virtVecSrc, tid)) {
      // if (getInputFromOutput(masterSendDir) == FET_I_INST_UP) printf("in up\n");
      // if (getInputFromOutput(masterSendDir) == FET_I_INST_DOWN) printf("in down\n");
      // if (getInputFromOutput(masterSendDir) == FET_I_INST_LEFT) printf("in left\n");
      // if (getInputFromOutput(masterSendDir) == FET_I_INST_RIGHT) printf("in right\n");
      mask |= getInputFromOutput(masterSendDir);
    }

    // send directions for the non-master vector group
    mask |= getSIMDMaskHoriz(master, origin, tid, dim, virtVecSrc, vectorSrc);
  }

  // specify the vlen
  int vlenX = dim_x;
  int vlenY = dim_y;
  mask |= (origin_x << FET_XORIGIN_SHAMT) | (origin_y << FET_YORIGIN_SHAMT) | (vlenX << FET_XLEN_SHAMT) | (vlenY << FET_YLEN_SHAMT);

  // specify each core is an execute core
  mask |= (is_master << FET_DAE_SHAMT);

  return mask;
}

// mask used for debugging prefetching. no instruction forwarding just works for prefetching
// NOTE DANGEROUS because no syncronization between scalar and vector cores.
// Avoid overfilling the scratchpad because may overwrite incorretly
static int getDebugMask(core_config_info_t *cinfo) {
  // unpack
  int origin_x  = cinfo->orig.x;
  int origin_y  = cinfo->orig.y;
  int dim_x     = cinfo->vdim.x;
  int dim_y     = cinfo->vdim.y;
  int is_master = cinfo->is_scalar;

  return (origin_x << FET_XORIGIN_SHAMT) | (origin_y << FET_YORIGIN_SHAMT) | (dim_x << FET_XLEN_SHAMT) | (dim_y << FET_YLEN_SHAMT) | (is_master << FET_DAE_SHAMT);
}

#endif
  
