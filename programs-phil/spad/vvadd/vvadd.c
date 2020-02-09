#include <stdlib.h>
#include <stdio.h>

#include "pthread_launch.h"
#include "vvadd.h"
#include "../../common/bind_defs.h"

#define SYNC_ADDR 1000

// one of these should be defined to dictate config
// #define NO_VEC 1
// #define VEC_16 1
// #define VEC_16_UNROLL 1
// #define VEC_4 1
// #define VEC_4_UNROLL 1
// #define VEC_4_DA 1
// #define VEC_16_UNROLL_SERIAL 1
// #define VEC_4_DA_SMALL_FRAME 1
// #define NO_VEC_DA 1
#define NO_VEC_W_VLOAD 1

// vvadd_execute config directives
#if defined(NO_VEC)
#define USE_NORMAL_LOAD 1
#endif
#if defined(VEC_16) || defined(VEC_16_UNROLL) || defined(VEC_4) || defined(VEC_4_UNROLL) \
  || defined(VEC_4_DA) || defined(VEC_16_UNROLL_SERIAL) || defined(VEC_4_DA_SMALL_FRAME)
#define USE_VEC 1
#endif
#if defined(VEC_16_UNROLL) || defined(VEC_4_UNROLL) || defined(VEC_4_DA) || defined(VEC_16_UNROLL_SERIAL) \
  || defined(VEC_4_DA_SMALL_FRAME) || defined(NO_VEC_DA)
#define UNROLL 1
#endif
#if defined(VEC_4_DA) || defined(VEC_4_DA_SMALL_FRAME) || defined(NO_VEC_DA)
#define USE_DA 1
#endif
#if !defined(UNROLL) && !defined(USE_NORMAL_LOAD)
#define WEIRD_PREFETCH 1
#endif
#if defined(VEC_16_UNROLL_SERIAL)
#define SERIAL_MASK 1
#endif
#if !defined(USE_VEC) && defined(NO_VEC_W_VLOAD)
#define FORCE_VEC_LOAD 1
#endif

// vector grouping directives
#if defined(VEC_16) || defined(VEC_16_UNROLL) || defined(VEC_16_UNROLL_SERIAL)
#define VEC_SIZE_16 1
#endif
#if defined(VEC_4) || defined(VEC_4_UNROLL)
#define VEC_SIZE_4 1
#endif
#if defined(VEC_4_DA) || defined(VEC_4_DA_SMALL_FRAME) || defined(NO_VEC_DA)
#define VEC_SIZE_4_DA 1
#endif

// prefetch sizings
#if defined(VEC_4_DA) || defined(NO_VEC_DA) || defined(VEC_16_UNROLL) || defined(VEC_4_UNROLL)
#define REGION_SIZE 32
#define NUM_REGIONS 16
#elif defined(VEC_4_DA_SMALL_FRAME) || defined(WEIRD_PREFETCH)
#define REGION_SIZE 2
#define NUM_REGIONS 256
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

void __attribute__((optimize("-freorder-blocks-algorithm=simple"))) 
vvadd_execute(float *a, float *b, float *c, int start, int end, int ptid, int vtid, int dim, int unroll_len) {
  int *spAddr = (int*)getSpAddr(ptid, 0);
  
  #ifdef UNROLL
  int numRegions = NUM_REGIONS;
  int regionSize = REGION_SIZE;
  int memEpoch = 0;
  #endif

  #ifdef WEIRD_PREFETCH // b/c you can't have single frame scratchpad?? need to make circular
  int spadRegion = 0;
  #endif

  for (int i = start + vtid; i < end; i+=unroll_len*dim) {

    #ifdef UNROLL // unroll and recv a bunch of loads into spad at once

    // region of spad memory we can use
    int *spAddrRegion = spAddr + (memEpoch % numRegions) * regionSize;

    #ifndef USE_DA // master is the one responsible for prefetch the bunch of loads
    for (int j = 0; j < unroll_len; j++) {
      VPREFETCH(spAddrRegion + j * 2    , a + i + j * dim, 0);
      VPREFETCH(spAddrRegion + j * 2 + 1, b + i + j * dim, 0);
    }
    // TODO might want to remem more frequently, so can start accessing some of the
    // data earlier
    #endif

    for (int j = 0; j < unroll_len; j++) {

      int* spAddrA = spAddrRegion + j * 2;
      int* spAddrB = spAddrRegion + j * 2 + 1;

      float a_, b_;
      LWSPEC(a_, spAddrA, 0);
      LWSPEC(b_, spAddrB, 0);

      float c_ = a_ + b_;
      STORE_NOACK(c_, c + i + j * dim, 0);
    }
    
    // increment mem epoch to know which region to fetch mem from
    memEpoch++;

    // inform DA we are done with region, so it can start to prefetch for that region
    spAddr[SYNC_ADDR] = memEpoch;

    // try to revec at the end of loop iteration
    // REVEC(0);
    // also up the memory epoch internally
    REMEM(0);
    #else // don't use prefetch unrolling
    float a_, b_, c_;
    #ifdef USE_NORMAL_LOAD // load using standard lw
    a_ = a[i];
    b_ = b[i];
    #else // load using master prefetch
    // drawback of this approach is that it doesn't work for single region prefetch zones
    // so need to add extra increment logic
    int *spAddrA = spAddr + spadRegion*2 + 0;
    int *spAddrB = spAddr + spadRegion*2 + 1;
    #ifdef FORCE_VEC_LOAD // force core with vtid 0 to do a master load even tho no systolic forwarding
    if (vtid == 0) {
    #endif
    VPREFETCH(spAddrA, a + i, 0);
    VPREFETCH(spAddrB, b + i, 0);
    #ifdef FORCE_VEC_LOAD
    }
    #endif
    LWSPEC(a_, spAddrA, 0);
    LWSPEC(b_, spAddrB, 0);
    spadRegion = (spadRegion + 1) % NUM_REGIONS;
    REMEM(0);
    #endif
    // add and then store
    c_ = a_ + b_;
    STORE_NOACK(c_, c + i, 0);
    #endif
  }
}

void __attribute__((optimize("-freorder-blocks-algorithm=simple"))) 
vvadd_access(float *a, float *b, float *c, int start, int end, int ptid, int vtid, int dim, int unroll_len, int spadCheckIdx) {
  #ifdef USE_DA
  int *spAddr = (int*)getSpAddr(ptid, 0);

  int numRegions = NUM_REGIONS;
  int regionSize = REGION_SIZE;

  // variable to control rate of sending
  int memEpoch = 0;
  volatile int loadedEpoch = 0;

  for (int i = start; i < end; i+=unroll_len*dim) {
    // check how many regions are available for prefetch by doing a remote load
    // to master cores scratchpad to get stored epoch number there
    // THIS BECOMES THE BOTTLENECK FOR SMALL FRAMES
    while(memEpoch >= loadedEpoch + numRegions) {
      loadedEpoch = ((int*)getSpAddr(spadCheckIdx, 0))[SYNC_ADDR];
    }

    // region of spad memory we can use
    int *spAddrRegion = spAddr + (memEpoch % numRegions) * regionSize;

    for (int j = 0; j < unroll_len; j++) {
      VPREFETCH(spAddrRegion + j * 2    , a + i + j * dim, 0);
      VPREFETCH(spAddrRegion + j * 2 + 1, b + i + j * dim, 0);
    }
    memEpoch++;

    // up memory epoch in the core
    REMEM(0);

  }
  #endif
}

void __attribute__((optimize("-freorder-blocks-algorithm=simple"), optimize("-fno-inline"))) 
vvadd(float *a, float *b, float *c, int start, int end, 
    int ptid, int vtid, int dim, int unroll_len, int is_da, int origin) {
  if (is_da) {
    vvadd_access(a, b, c, start, end, ptid, vtid, dim, unroll_len, origin);
  }
  else {
    vvadd_execute(a, b, c, start, end, ptid, vtid, dim, unroll_len);
  }
}

void __attribute__((optimize("-freorder-blocks-algorithm=simple"))) kernel(
    float *a, float *b, float *c, int n,
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

  // group construction
  #ifdef VEC_SIZE_4
  // virtual group dimension
  vdim_x = 2;
  vdim_y = 2;
  
  // tid in that group
  vtid_x = ptid_x % vdim_x;
  vtid_y = ptid_y % vdim_y;
  vtid   = vtid_x + vtid_y * vdim_x;

  // TODO figure out how to do get group ID
  // origin for vector fetch and chunk of data
  if (ptid_x < 2 && ptid_y < 2) {
    orig_x = 0;
    orig_y = 0;
    start = 0;
    end = n / 4;
  }
  else if (ptid_x < 4 && ptid_y < 2) {
    orig_x = 2;
    orig_y = 0;
    start = n / 4;
    end = n / 2;
  }
  else if (ptid_x < 2 && ptid_y < 4) {
    orig_x = 0;
    orig_y = 2;
    start = n / 2;
    end = 3 * n / 4;
  }
  else if (ptid_x < 4 && ptid_y < 4) {
    orig_x = 2;
    orig_y = 2;
    start = 3 * n / 4;
    end = n;
  } 

  // not decoupled access core
  is_da = 0;

  #elif defined(VEC_SIZE_16)
  // virtual group dimension
  vdim_x = 4;
  vdim_y = 4;
  
  // tid in that group
  vtid_x = ptid_x % vdim_x;
  vtid_y = ptid_y % vdim_y;
  vtid   = vtid_x + vtid_y * vdim_x;

  orig_x = 0;
  orig_y = 0;
  start = 0;
  end = n;

  // not decoupled access core
  is_da = 0;

  #elif defined(VEC_SIZE_4_DA)
  // virtual group dimension
  vdim_x = 2;
  vdim_y = 2;

  int alignment = 16 * vdim_x * vdim_y;

  // group 1 top left (da == 8)
  if (ptid == 0) vtid = 0;
  if (ptid == 1) vtid = 1;
  if (ptid == 4) vtid = 2;
  if (ptid == 5) vtid = 3;
  if (ptid == 8) is_da = 1;
  if (ptid == 0 || ptid == 1 || ptid == 4 || ptid == 5 || ptid == 8) {
    start = 0;
    end = roundUp(n / 3, alignment); // make sure aligned to cacheline 
    orig_x = 0;
    orig_y = 0;
  }

  // group 2 top right (da == 11)
  if (ptid == 2) vtid = 0;
  if (ptid == 3) vtid = 1;
  if (ptid == 6) vtid = 2;
  if (ptid == 7) vtid = 3;
  if (ptid == 11) is_da = 1;
  if (ptid == 2 || ptid == 3 || ptid == 6 || ptid == 7 || ptid == 11) {
    start = roundUp(n / 3, alignment);
    end = roundUp(2 * n / 3, alignment);
    orig_x = 2;
    orig_y = 0;
  }

  // group 3 bottom (da == 15)
  if (ptid == 9)  vtid = 0;
  if (ptid == 10) vtid = 1;
  if (ptid == 13) vtid = 2;
  if (ptid == 14) vtid = 3;
  if (ptid == 15) is_da = 1;
  if (ptid == 9 || ptid == 10 || ptid == 13 || ptid == 14 || ptid == 15) {
    start = roundUp(2 * n / 3, alignment);
    end = n;
    orig_x = 1;
    orig_y = 2;
  }

  // ptid/core = 12 doesn't do anything in this config

  vtid_x = vtid % vdim_x;
  vtid_y = vtid / vdim_y;

  #elif !defined(USE_VEC)

  vdim_x = 1;
  vdim_y = 1;
  vtid_x = 0;
  vtid_y = 0;
  vtid   = 0;
  start  = ptid * ( n / pdim );
  end    = ( ptid + 1 ) * ( n / pdim );

  #endif

  // linearize some fields
  vdim = vdim_x * vdim_y;
  int orig = orig_x + orig_y * dim_x;

  // construct special mask for dae example
  int mask = 0;
  if (is_da) {
    mask = getDAEMask(orig_x, orig_y, vtid_x, vtid_y, vdim_x, vdim_y);
  }
  else {
    #ifdef USE_VEC
    #ifdef SERIAL_MASK
    mask = getSerializedMask(orig_x, orig_y, vtid_x, vtid_y, vdim_x, vdim_y);
    #else
    mask = getVecMask(orig_x, orig_y, vtid_x, vtid_y, vdim_x, vdim_y);
    #endif
    #endif
  }
  #ifdef FORCE_VEC_LOAD
  mask = (orig_x << FET_XORIGIN_SHAMT) | (orig_y << FET_YORIGIN_SHAMT) | 
        (pdim_x << FET_XLEN_SHAMT) | (pdim_x << FET_YLEN_SHAMT);
  #endif

  // printf("ptid %d(%d,%d) vtid %d(%d,%d) dim %d(%d,%d) %d->%d\n", ptid, ptid_x, ptid_y, vtid, vtid_x, vtid_y, vdim, vdim_x, vdim_y, start, end); 

  #ifdef NUM_REGIONS
  int prefetchMask = (NUM_REGIONS << PREFETCH_NUM_REGION_SHAMT) | (REGION_SIZE << PREFETCH_REGION_SIZE_SHAMT);
  PREFETCH_EPOCH(prefetchMask);

  // make sure all cores have done this before begin kernel section --> do thread barrier for now
  // TODO hoping for a cleaner way to do this
  pthread_barrier_wait(&start_barrier);
  #endif

  // only let certain tids continue
  #ifdef VEC_SIZE_4_DA
  if (tid == 12) return;
  #endif

  // configure
  VECTOR_EPOCH(mask);

  // run the actual kernel with the configuration
  #ifdef UNROLL
  volatile int unroll_len = REGION_SIZE / 2;
  #else
  volatile int unroll_len = 1;
  #endif
  vvadd(a, b, c, start, end, ptid, vtid, vdim, unroll_len, is_da, orig);

  // deconfigure
  VECTOR_EPOCH(0);
  
}


// helper functions
Kern_Args *construct_args(float *a, float *b, float *c, int size,
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
