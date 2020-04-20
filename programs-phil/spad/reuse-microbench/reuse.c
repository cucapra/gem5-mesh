#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

#include "pthread_launch.h"
#include "reuse.h"
#include "spad.h"
#include "../../common/bind_defs.h"

/*
  Microbenchmark remote loads compared to normal loads and prefetching
*/

// one of these should be defined to dictate config
// #define NO_VEC_L2 1
#define NO_VEC_RL 1
// #define VEC_PF 1
// #define VEC_L2 1
// #define VEC_RL 1

// vector and memory settings
#if defined(VEC_PF) || defined(VEC_L2) || defined(VEC_RL)
#define USE_VEC 1
#endif

#if defined(VEC_PF)
#define USE_PREFETCH 1
#elif defined(VEC_L2) || defined(NO_VEC_L2)
#define USE_DIRECT_L2 1
#elif defined(VEC_RL) || defined(NO_VEC_RL)
#define USE_REMOTE_LOADS 1
#endif

// prefetch sizings
#if defined(USE_PREFETCH)
#define REGION_SIZE FILTER_DIM * FILTER_DIM
#define NUM_REGIONS 64
#define POST_REGION_WORD NUM_REGIONS * REGION_SIZE
#endif

// remote load settings
#if defined(USE_REMOTE_LOADS)
#define NEIGHBOR_DIST 1
#endif

// warmup the cache if going to be accessing it in the kernel
#define WARMUP_LLC 1

inline int min(int a, int b) {
  if (a > b) {
    return b;
  }
  else {
    return a;
  }
}

// having this function not inlined messing up hacky host/vec seperation
// maybe not prefetch all the way, so fill in rest or hardware frame
// this kinda sux
#ifdef REGION_SIZE
inline void completeHardwareFrame(int spadIdx, int *someData) {
  int remainingEntries = REGION_SIZE - (spadIdx % REGION_SIZE);
  for (int i = 0; i < remainingEntries; i++) {
    VPREFETCH_L(spadIdx, someData, 0, 4);
    spadIdx++;
  }
}
#endif

void __attribute__((optimize("-fno-reorder-blocks"))) 
    microbench(DTYPE *a, DTYPE *b, int start, int end, int vtid, int ptid, int vdim, int pdim) {
    
  // turn on a 'vector' config so that we can get targetted debug flags
  VECTOR_EPOCH((1 << FET_XLEN_SHAMT) |
            (1 << FET_YLEN_SHAMT));

  DTYPE *thisSpad = (DTYPE*)getSpAddr(ptid, 0);
  int nextTid = 0;
  if (vtid == 0 || vtid == 2) nextTid = ptid + 1;
  else if (vtid == 1) nextTid = ptid + 3;
  else if (vtid == 3) nextTid = ptid - 5; // wrap
  DTYPE *nextSpad = (DTYPE*)getSpAddr(nextTid, 0);

  // actually do stores at the end
  int v0 = 0;
  int v1 = 0;
  int v2 = 0;
  int v3 = 0;
  int v4 = 0;
  int v5 = 0;
  int v6 = 0;
  int v7 = 0;

  // start recording all stats (all cores)
  asm volatile("":::"memory");
  if (ptid == 1) {
    stats_on();
  }

  #ifndef USE_VEC

  #ifdef USE_DIRECT_L2
  v0 = a[0];
  v1 = a[1];
  v2 = a[2];
  v3 = a[3];
  v4 = a[4];
  v5 = a[5];
  v6 = a[6];
  v7 = a[7];
  #endif

  #ifdef USE_REMOTE_LOADS
  v0 = nextSpad[0];
  v1 = nextSpad[1];
  v2 = nextSpad[2];
  v3 = nextSpad[3];
  v4 = nextSpad[4];
  v5 = nextSpad[5];
  v6 = nextSpad[6];
  v7 = nextSpad[7];
  #endif

  #endif


  // start recording all stats (all cores)
  if (ptid == 1) {
    stats_off();
  }
  // memory barrier so we don't do stores during the kernel
  asm volatile ("":::"memory");
  b[0] = v0;
  b[1] = v1;
  b[2] = v2;
  b[3] = v3;
  b[4] = v4;
  b[5] = v5;
  b[6] = v6;
  b[7] = v7;

  VECTOR_EPOCH(0);
  
}

void __attribute__((optimize("-freorder-blocks-algorithm=simple"))) kernel(
    DTYPE *a, DTYPE *b, int n, int tid_x, int tid_y, int dim_x, int dim_y) {
  
  // // start recording all stats (all cores)
  // if (tid_x == 0 && tid_y == 0) {
  //   stats_on();
  // }

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

  #if defined(VEC_SIZE_4_SIMD)
    // virtual group dimension
  vdim_x = 2;
  vdim_y = 2;

  // group 1 top left (master = 0)
  if (ptid == 1) vtid = 0;
  if (ptid == 2) vtid = 1;
  if (ptid == 5) vtid = 2;
  if (ptid == 6) vtid = 3;
  if (ptid == 0) is_da = 1;
  if (ptid == 0 || ptid == 1 || ptid == 2 || ptid == 5 || ptid == 6) {
    start = 0;
    end = (float)effRows / 3.0f;
    orig_x = 1;
    orig_y = 0;
    master_x = 0;
    master_y = 0;
  }

  // group 2 bot left (master == 4)
  if (ptid == 8) vtid = 0;
  if (ptid == 9) vtid = 1;
  if (ptid == 12) vtid = 2;
  if (ptid == 13) vtid = 3;
  if (ptid == 4) is_da = 1;
  if (ptid == 4 || ptid == 8 || ptid == 9 || ptid == 12 || ptid == 13) {
    start = (float)effRows / 3.0f;
    end = (float)(2 * effRows) / 3.0f;
    orig_x = 0;
    orig_y = 2;
    master_x = 0;
    master_y = 1;
  }

  // group 3 bottom right (master == 7)
  if (ptid == 10)  vtid = 0;
  if (ptid == 11) vtid = 1;
  if (ptid == 14) vtid = 2;
  if (ptid == 15) vtid = 3;
  if (ptid == 7) is_da = 1;
  if (ptid == 7 || ptid == 10 || ptid == 11 || ptid == 14 || ptid == 15) {
    start = (float)(2 * effRows) / 3.0f;
    end = effRows;
    orig_x = 2;
    orig_y = 2;
    master_x = 3;
    master_y = 1;
  }

  vtid_x = vtid % vdim_x;
  vtid_y = vtid / vdim_y;

  #elif !defined(USE_VEC)

  if (ptid == 1) vtid = 0;
  if (ptid == 2) vtid = 1;
  if (ptid == 5) vtid = 2;
  if (ptid == 6) vtid = 3;

  vdim_x = 2;
  vdim_y = 2;
  start  = vtid * ( n / pdim );
  end    = ( vtid + 1 ) * ( n / pdim );

  #endif

  vtid_x = vtid % vdim_x;
  vtid_y = vtid / vdim_y;
  // linearize some fields
  vdim = vdim_x * vdim_y;
  int orig = orig_x + orig_y * dim_x;

  // setup vector masks
  #ifdef USE_VEC
  // volatile so dont reorder this function call
  int mask = getSIMDMask(master_x, master_y, orig_x, orig_y, vtid_x, vtid_y, vdim_x, vdim_y, is_da);
  #endif

  // setup frames if going to prefetch
  #ifdef USE_PREFETCH
  int prefetchMask = (NUM_REGIONS << PREFETCH_NUM_REGION_SHAMT) | (REGION_SIZE << PREFETCH_REGION_SIZE_SHAMT);
  PREFETCH_EPOCH(prefetchMask);

  // make sure all cores have done this before begin kernel section --> do thread barrier for now
  // TODO hoping for a cleaner way to do this
  pthread_barrier_wait(&start_barrier);
  #endif

  // only let certain tids continue
  #if defined(USE_VEC)
  if (ptid != 0 && ptid != 1 && ptid != 2 && ptid != 5 && ptid != 6) return;
  if (ptid == 3) return;
  #else
  if (ptid != 1 && ptid != 2 && ptid != 5 && ptid != 6) return;
  #endif

  // warmup the cache to ignore dram latency
  #ifdef WARMUP_LLC
  volatile int dummy = 0;
  if (ptid == 0) {
    for (int i = 0; i < n; i++) {
      dummy += a[i];
      dummy += b[i];
    }
  }
  #endif

  // preload data in scratchpads
  // everyone loads the whole array, and then later on decide who to fetch from
  // might as well have all cases do this to be slightly more consistent
  // #ifdef USE_REMOTE_LOADS
  DTYPE *spadAddr = (DTYPE*)getSpAddr(ptid, 0);
  for (int i = 0; i < n; i++) {
    spadAddr[i] = a[i];
  }
  // #endif

  // only have one do the test
  // if (ptid != 1 && ptid != 2) return;

  // save the stack pointer to top of spad and change the stack pointer to point into the scratchpad
  // reset after the kernel is done
  // do before the function call so the arg stack frame is on the spad
  // store the the current spAddr to restore later 
  unsigned long long *spTop = getSpTop(ptid);
  // guess the remaining of the part of the frame that might be needed??
  spTop -= 6;

  unsigned long long stackLoc;
  asm volatile (
    // copy part of the stack onto the scratchpad in case there are any loads to scratchpad right before
    // function call
    "ld t0, 0(sp)\n\t"
    "sd t0, 0(%[spad])\n\t"
    "ld t0, 8(sp)\n\t"
    "sd t0, 8(%[spad])\n\t"
    "ld t0, 16(sp)\n\t"
    "sd t0, 16(%[spad])\n\t"
    "ld t0, 24(sp)\n\t"
    "sd t0, 24(%[spad])\n\t"
    "ld t0, 32(sp)\n\t"
    "sd t0, 32(%[spad])\n\t"
    "ld t0, 40(sp)\n\t"
    "sd t0, 40(%[spad])\n\t"
    // save the stack ptr
    "addi %[dest], sp, 0\n\t" 
    // overwrite stack ptr
    "addi sp, %[spad], 0\n\t"
    : [dest] "=r" (stackLoc)
    : [spad] "r" (spTop)
  );

  microbench(a, b, start, end, vtid, ptid, vdim, dim);

  // restore stack pointer
  asm volatile (
    "addi sp, %[stackTop], 0\n\t" :: [stackTop] "r" (stackLoc)
  );

}


// helper functions
Kern_Args *construct_args(DTYPE *a, DTYPE *b, int n,
  int tid_x, int tid_y, int dim_x, int dim_y) {

  Kern_Args *args = (Kern_Args*)malloc(sizeof(Kern_Args));
  
  args->a = a;
  args->b = b;
  args->n = n;
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
  
  kernel(a->a, a->b, a->n, 
      a->tid_x, a->tid_y, a->dim_x, a->dim_y);
      
  pthread_barrier_wait(&start_barrier);

  // if (a->tid_x == 0 && a->tid_y == 0) {
  //   stats_off();
  // }

  // BUG: note this printf fails if have the VECTOR_EPOCH(0), but mayber just timing thing
  // printf("ptid (%d,%d)\n", a->tid_x, a->tid_y);

  return NULL;
}
