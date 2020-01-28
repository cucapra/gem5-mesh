#include <stdlib.h>
#include <stdio.h>

#include "pthread_launch.h"
#include "synth.h"
#include "spad.h"
#include "../../common/bind_defs.h"

#define _USE_VLOAD 1

// ordered by priority
#define DAE 1
#define UNROLL 1
#define SPEC_PREFETCH 1

// if don't have this attribute potentially will duplicate inline assembly due
// to code layout reordering. this happens in -O2+ with -freorder-blocks-algorithm=stc
// this is problematic with revec call which needs to sync pc between multiple traces on a revec
void __attribute__((optimize("-freorder-blocks-algorithm=simple"))) 
synthetic(int *a, int *b, int *c, int *d, int n, int tid, int dim) {
  int *spAddr = getSpAddr(tid, 0);
  
  for (int i = tid; i < n; i+=dim) {
    
    int a_;
    #ifdef _USE_VLOAD
    VPREFETCH(spAddr, a + i, 0);
    LWSPEC(a_, spAddr, 0);
    #else
    a_ = a[i];
    #endif
    
    if (a_ == 0) {
      int b_;
      #ifdef _USE_VLOAD
      VPREFETCH(spAddr + 1, b + i, 0);
      LWSPEC(b_, spAddr + 1, 0);
      #else
      b_ = b[i];
      #endif
      int c_ = b_;
      for (int j = 0; j < 2; j++) {
        c_ *= b_;
      }
      c[i] = c_;
    }
    else {
      int b_;
      #ifdef _USE_VLOAD
      // need memory alignment, so needs to be divisible by vlen now
      // if no alignment would need to send two seperate packets to different
      // cache banks
      VPREFETCH(spAddr + 1, d + i, 0);
      LWSPEC(b_, spAddr + 1,  0);
      #else
      b_ = d[i];
      #endif
      int c_ = b_;
      for (int j = 0; j < 2; j++) {
        c_ *= b_;
      }
      c[i] = c_;
    }
    
    
    // try to revec at the end of loop iteration
    REVEC(0);
  }
}

void __attribute__((optimize("-freorder-blocks-algorithm=simple"))) 
  loop_body(int a, int *b, int *c, int *d, int i, int *spAddr) {

    // TODO can we unroll the loads for these somehow? potentially speculatively?
    // presumably there is a hot path known so should get perf improvement doing so!
    if (a == 0) {
      int b_;
      VPREFETCH(spAddr, b + i, 0);
      LWSPEC(b_, spAddr, 0);

      int c_ = b_;
      for (int j = 0; j < 2; j++) {
        c_ *= b_;
      }
      c[i] = c_;
    }
    else {
      int b_;
      VPREFETCH(spAddr, d + i, 0);
      LWSPEC(b_, spAddr,  0);

      int c_ = b_;
      for (int j = 0; j < 2; j++) {
        c_ *= b_;
      }
      c[i] = c_;
      // STORE_NOACK(c_, c + i, 0);
    }

    // try to revec at the end of loop iteration
    //REVEC(0);
}

// don't want to unroll b/c then need to use the stack, which currently skips spad so will get more
// LLC accesses when do more prefetching
// although doesn't seem to respect hint @ 8
void __attribute__((optimize("-freorder-blocks-algorithm=simple"))) 
synthetic_uthread(int *a, int *b, int *c, int *d, int n, int tid, int dim, int unroll_len) {
  int *spAddr = getSpAddr(tid, 0);

  for (int i = tid; i < n; i+=unroll_len*dim) {
    
    // load everybody up front, cheap temporal multithreading
    // maybe the non-vector version can't handle this increased mem traffic?
    for (int j = 0; j < unroll_len; j++) {
      int idx = i + j * dim;
      VPREFETCH(spAddr + j, a + idx, 0);
    }

    #ifdef SPEC_PREFETCH
    for (int j = 0; j < unroll_len; j++) {
      // speculate on load for the hot path
      // not only assuming threads will do the same thing across space, but
      // also speculating will do the same thing across time
      // Would normal vector prefetching do this?
      int idx = i + j * dim;
      VPREFETCH(spAddr + j + unroll_len, b + idx, 0);
    }
    #endif



    for (int j = 0; j < unroll_len; j++) {
      int idx = i + j * dim;
      int a_;
      LWSPEC(a_, spAddr + j, 0);
      if (a_ == 0) {
        int b_;

        #ifndef SPEC_PREFETCH
        VPREFETCH(spAddr + unroll_len, b + idx, 0);
        LWSPEC(b_, spAddr + unroll_len, 0);
        #else
        LWSPEC(b_, spAddr + j + unroll_len, 0);
        #endif

        int c_ = b_;
        for (int k = 0; k < 2; k++) {
          c_ *= b_;
        }
        c[idx] = c_;
      }
      else { // unused in convergent example
        // int b_;
        // VPREFETCH(spAddr + unroll_len, d + idx, 0);
        // LWSPEC(b_, spAddr + unroll_len,  0);

        // int c_ = b_;
        // for (int k = 0; k < 2; k++) {
        //   c_ *= b_;
        // }
        // c[idx] = c_;
      }
    }

    // TODO problem if revec each time, b/c then the epoch of the shared loads
    // up top will be wrong even though fine ... need to fix this mechanism b/c
    // might want to revec each time
    REVEC(0);
  }
}

#define SYNC_ADDR 1000
#define DA_SPAD 0
#define SP_INTS 512
#define NUM_REGIONS 16
#define REGION_SIZE 32


void __attribute__((optimize("-freorder-blocks-algorithm=simple"))) 
synthetic_dae_execute(int *a, int *b, int *c, int *d, int start, int end, int ptid, int vtid, int dim, int unroll_len) {
  int *spAddr = getSpAddr(ptid, 0);
  // int *daeSpad = getSpAddr(DA_SPAD, 0);
  
  int numRegions = NUM_REGIONS;
  int regionSize = REGION_SIZE;
  int memEpoch = 0;

  for (int i = start + vtid; i < end; i+=unroll_len*dim) {
    
    // region of spad memory we can use
    int *spAddrRegion = spAddr + (memEpoch % numRegions) * regionSize;

    for (int j = 0; j < unroll_len; j++) {

      int* spAddrA = spAddrRegion + j * 2;
      int* spAddrB = spAddrRegion + j * 2 + 1;

      int a_;
      // VPREFETCH(spAddrA, a + i + j * dim, 0); 
      LWSPEC(a_, spAddrA, 0);
      
      if (a_ == 0) {
        int b_;
        // VPREFETCH(spAddrB, b + i + j * dim, 0);
        LWSPEC(b_, spAddrB, 0);
        int c_ = b_;
        for (int k = 0; k < 2; k++) {
          c_ *= b_;
        }
        // c[i + j * dim] = c_;
        // spAddr[SYNC_ADDR + 1] = c_;
        STORE_NOACK(c_, c + i + j * dim, 0);
      }
      // else {
      //   int b_;
      //   VPREFETCH(spAddrB, d + i, 0); // need to load when on the off path, although should prob be regular load?
      //   LWSPEC(b_, spAddrB,  0);
      //   int c_ = b_;
      //   for (int k = 0; k < 2; k++) {
      //     c_ *= b_;
      //   }
      //   c[i + j * dim] = c_;
      // }
    }
    
    // increment mem epoch to know which region to fetch mem from
    memEpoch++;

    // TODO also put REMEM here, REVEC should prob be within the inner loop to try to revec each time??
    // inform DA we are done with region, so it can start to prefetch for that region
    // if (tid == 1)
      // daeSpad[SYNC_ADDR] = memEpoch;
    spAddr[SYNC_ADDR] = memEpoch;

    // try to revec at the end of loop iteration
    REVEC(0);
  }
}

void __attribute__((optimize("-freorder-blocks-algorithm=simple"))) 
synthetic_dae_access(int *a, int *b, int *c, int *d, int start, int end, int ptid, int vtid, int dim, int unroll_len, int spadCheckIdx) {
  int *spAddr = getSpAddr(ptid, 0);
  
  int numRegions = NUM_REGIONS;
  int regionSize = REGION_SIZE;
  int memEpoch = 0;

  for (int i = start; i < end; i+=unroll_len*dim) {
    // check how many regions are available for prefetch by doing a remote load
    // to master cores scratchpad to get stored epoch number there
    volatile int loadedEpoch;
    loadedEpoch = ((int*)getSpAddr(spadCheckIdx, 0))[SYNC_ADDR];
    while(memEpoch >= loadedEpoch + numRegions) {
      loadedEpoch = ((int*)getSpAddr(spadCheckIdx, 0))[SYNC_ADDR];
    }
    // printf("do epoch %d\n", memEpoch);

    // region of spad memory we can use
    int *spAddrRegion = spAddr + (memEpoch % numRegions) * regionSize;

    for (int j = 0; j < unroll_len; j++) {
      // printf("start spAddr %#x from addr %#x\n", spAddrRegion + j * 2, a + i + j * dim);
      VPREFETCH(spAddrRegion + j * 2    , a + i + j * dim, 0);
      VPREFETCH(spAddrRegion + j * 2 + 1, b + i + j * dim, 0);
    }
    // printf("complete mem epoch %d\n", memEpoch);
    memEpoch++;

    // up memory epoch in the core
    REVEC(0);

  }
}

// NO-INLINE seems to be needed here... and works??
void __attribute__((optimize("-freorder-blocks-algorithm=simple"), optimize("-fno-inline"))) 
synthetic_dae(int *a, int *b, int *c, int *d, int start, int end, 
    int ptid, int vtid, int dim, int unroll_len, int is_da, int origin) {
  if (is_da) {
    synthetic_dae_access(a, b, c, d, start, end, ptid, vtid, dim, unroll_len, origin);
  }
  else {
    synthetic_dae_execute(a, b, c, d, start, end, ptid, vtid, dim, unroll_len);
  }
}

// TODO you add this __attribute__((optimize("-freorder-blocks-algorithm=simple"))), starts to fail
// 1388841000: system.cpu01.vector: Squash from IEW: squash inst [tid:0] [sn:296]
// 1388841000: system.cpu01.vector: [[INFO]] trace divergence [[sn:296/tid=0/0x10a5a/jal ra, -220]]
// for some reason squashing jal in IEW? expected to be from Decode??
// then if take that out get a problem looking up register, so something is severely wrong...

// actual kernel
void __attribute__((optimize("-freorder-blocks-algorithm=simple"))) kernel(
    int *a, int *b, int *c, int *d, int n,
    int tid_x, int tid_y, int dim_x, int dim_y) {
  
  // start recording all stats (all cores)
  if (tid_x == 0 && tid_y == 0) {
    stats_on();
  }
  
  // linearize tid and dim
  int tid = tid_x + tid_y * dim_x;
  int dim = dim_x * dim_y;

  // only let certain tids continue
  if (tid == 12) return;
  if (tid == 9 || tid == 10 || tid == 13 || tid == 14 || tid == 15) return;

  int vdim_x = 2;
  int vdim_y = 2;
  int vdim = vdim_x * vdim_y;

   // also need to change the tids to reflect position in the group
  int ptid_x = tid_x;
  int ptid_y = tid_y;
  int ptid   = tid;
  int vtid = 0;
  int start = 0;
  int end = 0;
  int origin_x = 0;
  int origin_y = 0;

  // construct 2 groups

  // group 1 top left (da == 8)
  if (ptid == 0) vtid = 0;
  if (ptid == 1) vtid = 1;
  if (ptid == 4) vtid = 2;
  if (ptid == 5) vtid = 3;
  if (ptid == 0 || ptid == 1 || ptid == 4 || ptid == 5 || ptid == 8) {
    start = 0;
    end = n / 2;
    origin_x = 0;
    origin_y = 0;
  }

  // group 2 top right (da == 11)
  if (ptid == 2) vtid = 0;
  if (ptid == 3) vtid = 1;
  if (ptid == 6) vtid = 2;
  if (ptid == 7) vtid = 3;
  if (ptid == 2 || ptid == 3 || ptid == 6 || ptid == 7 || ptid == 11) {
    start = n / 2;
    end = n;
    origin_x = 2;
    origin_y = 0;
  }

  

  // TODO group 3, bot mid

  int origin = origin_x + origin_y * dim_x;

  int vtid_x = vtid % vdim_x;
  int vtid_y = vtid / vdim_y;

  int is_da = 0;
  if (ptid == 8 || ptid == 11) {
    is_da = 1;
  }

  // construct special mask for dae example
  int mask = ALL_NORM;
  if (is_da) {
    mask = getDAEMask(origin_x, origin_y, vtid_x, vtid_y, vdim_x, vdim_y);
  }
  else {
    mask = getVecMask(origin_x, origin_y, vtid_x, vtid_y, vdim_x, vdim_y);
  }
  // printf("ptid %d(%d,%d) vtid %d(%d,%d) dim %d(%d,%d) mask %d\n", ptid, ptid_x, ptid_y, vtid, vtid_x, vtid_y, vdim, vdim_x, vdim_y, mask); 

  VECTOR_EPOCH(mask);

  // run the actual kernel with the configuration
  volatile int unroll_len = 16;
  synthetic_dae(a, b, c, d, start, end, ptid, vtid, vdim, unroll_len, is_da, origin);
  // deconfigure
  // #ifdef _VEC
  VECTOR_EPOCH(ALL_NORM);
  // #endif

  // commit stats (don't have core 0 do this especially when its the Decoupled Access core)
  if (tid_x == 1 && tid_y == 0) {
    stats_off();
  }
  
}


// helper functions
Kern_Args *construct_args(int *a, int *b, int *c, int *d, int n,
  int tid_x, int tid_y, int dim_x, int dim_y) {
      
  Kern_Args *args = (Kern_Args*)malloc(sizeof(Kern_Args));
  
  args->a = a;
  args->b = b;
  args->c = c;
  args->d = d;
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

  kernel(a->a, a->b, a->c, a->d, a->n,
      a->tid_x, a->tid_y, a->dim_x, a->dim_y);

  return NULL;
}
