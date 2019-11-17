#include <stdlib.h>
#include <stdio.h>

#include "pthread_launch.h"
#include "vmem.h"
#include "spad.h"
#include "../../common/bind_defs.h"

// actual kernel
void kernel(
    int *a, int *b, int *c, int n,
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
  
  int mask = ALL_NORM;
  
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
  int vlenX = 2;
  int vlenY = 2;
  mask |= (vlenX << FET_XLEN_SHAMT) | (vlenY << FET_YLEN_SHAMT);
  
  // NOTE potential optimization to avoid 64bit pointer store
  // b/c spad addresses are always 32bits in this setup
  int *spAddr = getSpAddr(tid, 0);
  
  int val;
  
  //VECTOR_EPOCH(mask);
  
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
  
  // divergent because of prefetch case
  
  
  /*REVEC(0);
  
  // divergent prefetch, prefetch
  // worry is that master prefetch may overwrite detached trace prefetch
  // in which case detached trace needs some way to avoid??
  
  
  REVEC(0);*/
  
  // convergent loop
  for (int i = tid; i < n; i+=dim) {
    VPREFETCH(spAddr,     a + i,      0);
    LWSPEC(val, spAddr, 0);
    c[tid] += val;
  }
  
  
  VECTOR_EPOCH(ALL_NORM);
  
  if (tid_x == 0 && tid_y == 0) {
    stats_off();
  }
  
  
  
}


// helper functions
Kern_Args *construct_args(int *a, int *b, int *c, int n,
  int tid_x, int tid_y, int dim_x, int dim_y) {
      
  Kern_Args *args = (Kern_Args*)malloc(sizeof(Kern_Args));
  
  args->a = a;
  args->b = b;
  args->c = c;
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
  
  kernel(a->a, a->b, a->c, a->n,
      a->tid_x, a->tid_y, a->dim_x, a->dim_y);
      
  return NULL;
}
