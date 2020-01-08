#include <stdlib.h>
#include <stdio.h>

#include "pthread_launch.h"
#include "synth.h"
#include "spad.h"
#include "../../common/bind_defs.h"

#define _USE_VLOAD 1

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
    }

    // try to revec at the end of loop iteration
    //REVEC(0);
}

void __attribute__((optimize("-freorder-blocks-algorithm=simple"))) 
synthetic_uthread(int *a, int *b, int *c, int *d, int n, int tid, int dim) {
  int *spAddr = getSpAddr(tid, 0);
  
  for (int i = tid; i < n; i+=4*dim) {
    
    int idx0 = i;
    int idx1 = i + dim;
    int idx2 = i + 2 * dim;
    int idx3 = i + 3 * dim;

    int a_;
    // load everybody up front, cheap temporal multithreading
    // maybe the non-vector version can't handle this increased mem traffic?
    VPREFETCH(spAddr,     a + idx0, 0);
    VPREFETCH(spAddr + 1, a + idx1, 0);
    VPREFETCH(spAddr + 2, a + idx2, 0);
    VPREFETCH(spAddr + 3, a + idx3, 0);

#ifdef SPEC_PREFETCH
    // speculate on load for the hot path
    // not only assuming threads will do the same thing across space, but
    // also speculating will do the same thing across time
    // Would normal vector prefetching do this?
    VPREFETCH(spAddr + 4, b + idx0, 0);
    VPREFETCH(spAddr + 5, b + idx1, 0);
    VPREFETCH(spAddr + 6, b + idx2, 0);
    VPREFETCH(spAddr + 7, b + idx3, 0);
#endif

    // then get data as needed, this procedure is a little more compilcated than unrolling
    // because need to do some load reordering (but maybe that's automatic in compiler after unroll?)
    LWSPEC(a_, spAddr, 0);
    loop_body(a_, b, c, d, idx0, spAddr + 4);
    //REVEC(0);

    LWSPEC(a_, spAddr + 1, 0);
    #ifdef SPEC_PREFETCH
    loop_body(a_, b, c, d, idx1, spAddr + 5);
    #else
    loop_body(a_, b, c, d, idx1, spAddr + 4);
    #endif
    //REVEC(0);

    LWSPEC(a_, spAddr + 2, 0);
    #ifdef SPEC_PREFETCH
    loop_body(a_, b, c, d, idx2, spAddr + 6);
    #else
    loop_body(a_, b, c, d, idx2, spAddr + 4);
    #endif
    //REVEC(0);

    LWSPEC(a_, spAddr + 3, 0);
    #ifdef SPEC_PREFETCH
    loop_body(a_, b, c, d, idx3, spAddr + 7);
    #else
    loop_body(a_, b, c, d, idx3, spAddr + 4);
    #endif

    // TODO problem if revec each time, b/c then the epoch of the shared loads
    // up top will be wrong even though fine ... need to fix this mechanism b/c
    // might want to revec each time
    REVEC(0);
  }
}

// actual kernel
void kernel(
    int *a, int *b, int *c, int *d, int n,
    int tid_x, int tid_y, int dim_x, int dim_y) {
  
  // start recording all stats (all cores)
  if (tid_x == 0 && tid_y == 0) {
    stats_on();
  }
  
  // linearize tid and dim
  int tid = tid_x + tid_y * dim_x;
  int dim = dim_x * dim_y;

  // configure vector is enabled
  #ifdef _VEC
  // get a standard vector mask
  int mask = getVecMask(tid_x, tid_y, dim_x, dim_y);
  
  VECTOR_EPOCH(mask);
  #endif

  // run the actual kernel with the configuration
  #ifdef UNROLL
  synthetic_uthread(a, b, c, d, n, tid, dim);
  #else
  synthetic(a, b, c, d, n, tid, dim);
  #endif

  // deconfigure
  #ifdef _VEC
  VECTOR_EPOCH(ALL_NORM);
  #endif

  // commit stats
  if (tid_x == 0 && tid_y == 0) {
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
