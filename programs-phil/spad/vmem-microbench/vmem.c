#include <stdlib.h>
#include <stdio.h>

#include "pthread_launch.h"
#include "vmem.h"
#include "spad.h"
#include "../../common/bind_defs.h"

// actual kernel
void __attribute__((optimize("-freorder-blocks-algorithm=simple")))
  kernel(
    int *a, int *b, int *c, int *d, int n,
    int tid_x, int tid_y, int dim_x, int dim_y) {
  
  // start recording all stats (all cores)
  if (tid_x == 0 && tid_y == 0) {
    stats_on();
  }
  
  // linearize tid and dim
  int tid = tid_x + tid_y * dim_x;
  int dim = dim_x * dim_y;
  
  // figure out which work this thread should do
  int start = tid * (n / dim);  

  // get end with remainders
  int chunk = (int)(n / dim);
  if (tid_x == dim - 1) {
    chunk += n % dim;
  }
  int end = start + chunk;
  
  //printf("iterations %d->%d\n", start, end);
  
  int mask = getVecMask(tid_x, tid_y, dim_x, dim_y);
  
  // NOTE potential optimization to avoid 64bit pointer store
  // b/c spad addresses are always 32bits in this setup
  int *spAddr = getSpAddr(tid, 0);
  
  int val;
  
  VECTOR_EPOCH(mask);
  
  // divergent before prefetch case, no prefetch in detached path
  if (tid < 2) {
    VPREFETCH(spAddr, a + tid, 0);
    LWSPEC(val, spAddr, 0);
    b[tid] = val;
  }
  else {
    b[tid] = -1;
  }
  
  REVEC(0);
  
  // divergent and does seperate loads
  if (tid == 1) {
    VPREFETCH(spAddr, a + tid, 0);
    LWSPEC(val, spAddr, 0);
    c[tid] += val;
  }
  else {
    VPREFETCH(spAddr, a + tid + dim, 0);
    LWSPEC(val, spAddr, 0);
    c[tid] += val;
  }
  
  REVEC(0);
  
  // TODO divergent because of prefetch case
  
  
  // convergent loop
  for (int i = tid; i < n; i+=dim) {
    VPREFETCH(spAddr, a + i, 0);
    LWSPEC(val, spAddr, 0);
    d[tid] += val;
    
    REVEC(0);
  }
  
  
  VECTOR_EPOCH(ALL_NORM);
  
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
