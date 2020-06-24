#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

#include "pthread_launch.h"
#include "vvadd.h"
#include "spad.h"
#include "../../common/bind_defs.h"
#include "vvadd_kernel.h"

// one of these should be defined to dictate config
// #define NO_VEC 1
#define VEC_4_SIMD 1
// #define VEC_4_SIMD_VERTICAL 1
// #define VEC_4_SIMD_SPATIAL_UNROLLED 1

// in current system cacheline size is 16 so doesn't make sense to go beyond this for now
// #define VEC_16_SIMD 1
// #define VEC_16_SIMD_VERTICAL 1
// #define VEC_16_SIMD_SPATIAL_UNROLLED 1

// can also input orthogonal #ifdefs
// NO_VEC/USE_VEC, VEC_4_SIMD, VEC_16_SIMD
// SPATIAL, VERTICAL, SPATIAL_UNROLL
// PREFETCH_LEN

// vvadd_execute config directives
#if !defined(NO_VEC)
#define USE_VEC 1
#endif
#if defined(VEC_4_SIMD_VERTICAL) || defined(VEC_16_SIMD_VERTICAL)
#define VERTICAL_LOADS 1
#endif
#if defined(VEC_4_SIMD_SPATIAL_UNROLLED) || defined(VEC_16_SIMD_SPATIAL_UNROLLED)
#define SPATIAL_UNROLL 1
#endif

// vector grouping directives
#if defined(VEC_4_SIMD) || defined(VEC_4_SIMD_BCAST) || defined(VEC_4_SIMD_VERTICAL) || defined(VEC_4_SIMD_SPATIAL_UNROLLED)
#define VECTOR_LEN 4
#endif
#if defined(VEC_16_SIMD) || defined(VEC_16_SIMD_VERTICAL) || defined(VEC_16_SPATIAL_UNROLLED)
#define VECTOR_LEN 16
#endif

// prefetch sizings
#if defined(VERTICAL_LOADS) || defined(SPATIAL_UNROLL)
// load 16 words (whole cacheline at a time)
#define LOAD_LEN 16
#define REGION_SIZE LOAD_LEN * 2
#define NUM_REGIONS 16
#elif defined(USE_VEC)
#define REGION_SIZE 2
#define NUM_REGIONS 256
#endif

// define prefetch len externally
#ifdef PF
#define PREFETCH_LEN PF
// default size is the vlen
#else
#define PREFETCH_LEN VECTOR_LEN
#endif

// https://stackoverflow.com/questions/3407012/c-rounding-up-to-the-nearest-multiple-of-a-number
int roundUp(int numToRound, int multiple) {
  if (multiple == 0) {
    return numToRound;
  }

  int remainder = abs(numToRound) % multiple;
  if (remainder == 0) {
    return numToRound;
  }

  if (numToRound < 0) {
    return -(abs(numToRound) - remainder);
  }
  else {
    return numToRound + multiple - remainder;
  }
}

inline int min(int a, int b) {
  if (a > b) {
    return b;
  }
  else {
    return a;
  }
}

// // NOTE optimize("-fno-inline") prevents return block from being at the end, which is kind of needed for the scheme
// // ACTUALLY any second label causes a problem???
// #ifdef USE_VEC
// void __attribute__((optimize("-fno-reorder-blocks"), optimize("-fno-align-labels")))
// vvadd_execute(DTYPE *a, DTYPE *b, DTYPE *c, int start, int end, int ptid, int vtid, int dim, int mask, int is_master) {

//   volatile int ohjeez = 1;
//   if (ohjeez) {

//   // enter vector epoch within function, b/c vector-simd can't have control flow
//   VECTOR_EPOCH(mask); 

//   // should be a constant from static analysis of dim
//   int pRatio = VECTOR_LEN / PREFETCH_LEN;

//   #if defined(VERTICAL_LOADS) || defined(SPATIAL_UNROLL)
//   int numInitFetch = LOAD_LEN;
//   #else
//   int numInitFetch = 16;
//   #endif

//   // do a bunch of prefetching in the beginning to get ahead
//   int totalIter = (end - start) / dim;
//   int beginIter = min(numInitFetch, totalIter);

//   #ifdef VERTICAL_LOADS
//   for (int core = 0; core < dim; core++) {
//     VPREFETCH_L(0       , a + start + LOAD_LEN * core, core, LOAD_LEN, 1);
//     VPREFETCH_L(LOAD_LEN, b + start + LOAD_LEN * core, core, LOAD_LEN, 1);
//   }
//   #else
//   for (int i = 0; i < beginIter; i++) {
//     for (int p = 0; p < pRatio; p++) { // NOTE unrolled b/c can statically determine pRatio is const
//       VPREFETCH_L(i * 2 + 0, a + start + (i * dim + p * PREFETCH_LEN), p * PREFETCH_LEN, PREFETCH_LEN, 0);
//       VPREFETCH_L(i * 2 + 1, b + start + (i * dim + p * PREFETCH_LEN), p * PREFETCH_LEN, PREFETCH_LEN, 0);
//     }
//   }
//   #endif

//   // issue header instructions
//   ISSUE_VINST(fable0);

//   int localIter = beginIter * 2;

//   #ifdef VERTICAL_LOADS
//   for (int i = beginIter; i < totalIter; i+=LOAD_LEN) {
//     for (int core = 0; core < dim; core++) {
//       VPREFETCH_L(localIter + 0       , a + start + i * dim + LOAD_LEN * core, core, LOAD_LEN, 1);
//       VPREFETCH_L(localIter + LOAD_LEN, b + start + i * dim + LOAD_LEN * core, core, LOAD_LEN, 1);
//     }

//     ISSUE_VINST(fable1);
//     localIter+=REGION_SIZE;
//     if (localIter == (NUM_REGIONS * REGION_SIZE)) localIter = 0;
//   }
//   #elif defined(SPATIAL_UNROLL)
//   for (int i = beginIter; i < totalIter; i+=LOAD_LEN) {
//     for (int j = 0; j < LOAD_LEN; j++) {
//       for (int p = 0; p < pRatio; p++) {
//         VPREFETCH_L(localIter + j * 2 + 0, a + start + ((i + j) * dim + p * PREFETCH_LEN), p * PREFETCH_LEN, PREFETCH_LEN, 0);
//         VPREFETCH_L(localIter + j * 2 + 1, b + start + ((i + j) * dim + p * PREFETCH_LEN), p * PREFETCH_LEN, PREFETCH_LEN, 0);
//       }
//     }

//     ISSUE_VINST(fable1);
//     localIter+=REGION_SIZE;
//     if (localIter == (NUM_REGIONS * REGION_SIZE)) localIter = 0;
//   }
//   #else
//   for (int i = beginIter; i < totalIter; i++) {

//     // prefetch for future iterations
//     for (int p = 0; p < pRatio; p++) {
//       VPREFETCH_L(localIter + 0, a + start + (i * dim + p * PREFETCH_LEN), p * PREFETCH_LEN, PREFETCH_LEN, 0);
//       VPREFETCH_L(localIter + 1, b + start + (i * dim + p * PREFETCH_LEN), p * PREFETCH_LEN, PREFETCH_LEN, 0);
//     }

//     // issue fable1
//     ISSUE_VINST(fable1);

//     localIter+=REGION_SIZE;
//     if (localIter == (NUM_REGIONS * REGION_SIZE)) {
//       localIter = 0;
//     }
//   }
//   #endif

//   // issue the rest
//   #if defined(VERTICAL_LOADS) || defined(SPATIAL_UNROLL)
//   for (int i = totalIter - beginIter; i < totalIter; i+=LOAD_LEN) {
//     ISSUE_VINST(fable1);
//   }
//   #else
//   for (int i = totalIter - beginIter; i < totalIter; i++) {
//     ISSUE_VINST(fable1);
//   }
//   #endif

//   // devec with unique tag
//   DEVEC(devec_0);

//   // we are doing lazy store acks, so use this to make sure all stores have commited to memory
//   asm volatile("fence\n\t");

//   return;
//   }

//   // vector engine code

//   // declarations
//   DTYPE a_, b_, c_;
//   int64_t iter; // avoids sext.w instruction when doing broadcast // TODO maybe should be doing rv32
//   DTYPE *cPtr;
//   int *spadAddr;

//   // entry block
//   // NOTE need to do own loop-invariant code hoisting?
//   fable0:
//     iter = 0;
//     spadAddr = (int*)getSpAddr(ptid, 0);
//     #ifdef VERTICAL_LOADS
//     cPtr = c + start + vtid * LOAD_LEN;
//     #else
//     cPtr = c + start + vtid;
//     #endif
  
//   // loop body block
//   fable1:
//     // unrolled version when doing vertical loads
//     #ifdef VERTICAL_LOADS
//     FRAME_START(REGION_SIZE);

//     // load values from scratchpad
//     #pragma GCC unroll(16)
//     for (int i = 0; i < LOAD_LEN; i++) {
//       a_ = spadAddr[iter + i + 0];
//       b_ = spadAddr[iter + i + LOAD_LEN];

//       // compute and store
//       c_ = a_ + b_;
//       STORE_NOACK(c_, cPtr + i, 0);
//     }

//     REMEM(REGION_SIZE);

//     cPtr += LOAD_LEN * dim;
//     iter = (iter + REGION_SIZE) % (NUM_REGIONS * REGION_SIZE);
//     #elif defined(SPATIAL_UNROLL)
//     FRAME_START(REGION_SIZE);

//     // load values from scratchpad
//     #pragma GCC unroll(16)
//     for (int i = 0; i < LOAD_LEN; i++) {
//       a_ = spadAddr[iter + i * 2 + 0];
//       b_ = spadAddr[iter + i * 2 + 1];

//       // compute and store
//       c_ = a_ + b_;
//       STORE_NOACK(c_, cPtr + i * dim, 0);
//     }

//     REMEM(REGION_SIZE);

//     cPtr += LOAD_LEN * dim;
//     iter = (iter + REGION_SIZE) % (NUM_REGIONS * REGION_SIZE);
//     #else
//     #ifdef SIMD_BCAST
//     // try to get compiler to use register that will recv broadcasted values
//     // can make compiler pass
//     asm volatile(
//       "add %[var], t0, x0\n\t"
//       : [var] "=r" (iter)
//     );
//     #endif

//     FRAME_START(REGION_SIZE);

//     // load values from scratchpad
//     a_ = spadAddr[iter + 0];
//     b_ = spadAddr[iter + 1];

//     // remem as soon as possible, so don't stall loads for next iterations
//     // currently need to stall for remem b/c need to issue LWSPEC with a stable remem cnt
//     REMEM(REGION_SIZE);

//     // compute and store
//     c_ = a_ + b_;
//     STORE_NOACK(c_, cPtr, 0);
//     cPtr += dim;

//     iter = (iter + REGION_SIZE) % (NUM_REGIONS * REGION_SIZE);
//     #endif

//     // need this jump to create loop carry dependencies
//     // an assembly pass will remove this instruction
//     asm volatile goto("j %l[fable1]\n\t"::::fable1);

//   return;
// }
// #else
void __attribute__((optimize("-freorder-blocks-algorithm=simple"), optimize("-fno-inline"))) 
vvadd(DTYPE *a, DTYPE *b, DTYPE *c, int start, int end, 
    int ptid, int vtid, int dim, int unroll_len, int is_da, int origin) {
  
  for (int i = start + vtid; i < end; i+=unroll_len*dim) {
      DTYPE a_, b_, c_;
      a_ = a[i];
      b_ = b[i];
      // add and then store
      c_ = a_ + b_;
      STORE_NOACK(c_, c + i, 0);
  }
}

void __attribute__((optimize("-freorder-blocks-algorithm=simple"))) kernel(
    DTYPE *a, DTYPE *b, DTYPE *c, int n,
    int tid_x, int tid_y, int dim_x, int dim_y) {
  
  // start recording all stats (all cores)
  if (tid_x == 0 && tid_y == 0) {
    stats_on();
  }

  // linearize tid and dim
  int tid = tid_x + tid_y * dim_x;
  int dim = dim_x * dim_y;

  // split into physical and virtual tids + dim
  int ptid_x = tid_x;
  int ptid_y = tid_y;
  int ptid   = tid;
  int pdim_x = dim_x;
  int pdim_y = dim_y;
  int pdim   = dim;
  int vtid_x = 0;
  int vtid_y = 0;
  int vtid   = 0;
  int vdim_x = 0;
  int vdim_y = 0;
  int vdim   = 0;
  int start  = 0;
  int end    = 0;
  int orig_x = 0;
  int orig_y = 0;
  int is_da  = 0;
  int master_x = 0;
  int master_y = 0;
  int unique_id = 0;
  int total_groups = 0;
  int used = 0;

  // group construction
  #ifdef VECTOR_LEN

  #if VECTOR_LEN==4
  template_info_t tinfo = init_template_4x4_2x2();
  // template_info_t tinfo = init_template_debug();
  #elif VECTOR_LEN==16
  template_info_t tinfo = init_template_8x8_4x4();
  #endif
  core_config_info_t cinfo = vector_group_template(ptid_x, ptid_y, pdim_x, pdim_y, &tinfo);

  vtid = cinfo.vtid;
  vtid_x = cinfo.vtid_x;
  vtid_y = cinfo.vtid_y;
  vdim_x = cinfo.vdim_x;
  vdim_y = cinfo.vdim_y;
  orig_x = cinfo.orig_x;
  orig_y = cinfo.orig_y;
  is_da  = cinfo.is_scalar;
  master_x = cinfo.master_x;
  master_y = cinfo.master_y;
  unique_id = cinfo.unique_id;
  total_groups = cinfo.total_groups;
  used = cinfo.used;
  vdim = VECTOR_LEN;

  if (used) {
    int alignment = 16 * vdim;
    start = roundUp((unique_id + 0) * n / total_groups, alignment);
    end   = roundUp((unique_id + 1) * n / total_groups, alignment);
  }

  #elif !defined(USE_VEC)

  vdim_x = 1;
  vdim_y = 1;
  vdim   = vdim_x * vdim_y;
  vtid_x = 0;
  vtid_y = 0;
  vtid   = 0;
  start  = ( ( ptid + 0 ) * n ) / pdim;
  end    = ( ( ptid + 1 ) * n ) / pdim;


  #endif

  // linearize some fields
  int orig = orig_x + orig_y * dim_x;

  // construct special mask for dae example
  #ifdef USE_VEC
  int mask = getSIMDMask(master_x, master_y, orig_x, orig_y, vtid_x, vtid_y, vdim_x, vdim_y, is_da);
  #endif

  // printf("ptid %d(%d,%d) vtid %d(%d,%d) dim %d(%d,%d) %d->%d\n", ptid, ptid_x, ptid_y, vtid, vtid_x, vtid_y, vdim, vdim_x, vdim_y, start, end); 

  #ifdef NUM_REGIONS
  SET_PREFETCH_MASK(NUM_REGIONS, REGION_SIZE, &start_barrier);
  #endif

  // only let certain tids continue
  #if defined(USE_VEC)
  // if (ptid != 0 && ptid != 1 && ptid != 2 && ptid != 5 && ptid != 6) return; 
  // if (ptid == 3) return;
  if (used == 0) return;
  #endif

  // run the actual kernel with the configuration
  #ifdef UNROLL
  int unroll_len = REGION_SIZE / 2;
  #else
  int unroll_len = 1;
  #endif

  // save the stack pointer to top of spad and change the stack pointer to point into the scratchpad
  // reset after the kernel is done
  // do before the function call so the arg stack frame is on the spad
  // store the the current spAddr to restore later 
  unsigned long long *spTop = getSpTop(ptid);
  spTop -= 30;

  unsigned long long stackLoc;
  unsigned long long temp;
  #pragma GCC unroll(30)
  for(int i=0;i<30;i++){
    asm volatile("ld t0, %[id](sp)\n\t"
                "sd t0, %[id](%[spad])\n\t"
                : "=r"(temp)
                : [id] "i"(i*8), [spad] "r"(spTop));
  }
  asm volatile (// save the stack ptr
      "addi %[dest], sp, 0\n\t"
      // overwrite stack ptr
      "addi sp, %[spad], 0\n\t"
      : [ dest ] "=r"(stackLoc)
      : [ spad ] "r"(spTop));

  // configure
  #ifdef USE_VEC
  tril_vvadd(mask, a, b, c, start, end, ptid, vtid, dim, is_da);
  #else
  vvadd(a, b, c, start, end, ptid, vtid, vdim, unroll_len, is_da, orig);
  #endif

  // restore stack pointer
  asm volatile (
    "addi sp, %[stackTop], 0\n\t" :: [stackTop] "r" (stackLoc)
  );

}


// helper functions
Kern_Args *construct_args(DTYPE *a, DTYPE *b, DTYPE *c, int size,
  int tid_x, int tid_y, int dim_x, int dim_y) {

  Kern_Args *args = (Kern_Args*)malloc(sizeof(Kern_Args));
  
  args->a = a;
  args->b = b;
  args->c = c;
  args->size = size;
  args->tid_x = tid_x;
  args->tid_y = tid_y;
  args->dim_x = dim_x;
  args->dim_y = dim_y;
  
  return args;
      
}

void *pthread_kernel(void *args) {
  // guarentee one thread goes to each core, by preventing any threads
  // from finishing early
  pthread_barrier_wait(&start_barrier);
  
  // call the spmd kernel
  Kern_Args *a = (Kern_Args*)args;
  
  kernel(a->a, a->b, a->c, a->size, 
      a->tid_x, a->tid_y, a->dim_x, a->dim_y);
      
  pthread_barrier_wait(&start_barrier);

  if (a->tid_x == 0 && a->tid_y == 0) {
    stats_off();
  }

  // BUG: note this printf fails if have the VECTOR_EPOCH(0), but mayber just timing thing
  // printf("ptid (%d,%d)\n", a->tid_x, a->tid_y);

  return NULL;
}
