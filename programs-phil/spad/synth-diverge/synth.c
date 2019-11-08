#include <stdlib.h>
#include <stdio.h>

#include "pthread_launch.h"
#include "synth.h"
#include "spad.h"
#include "../../common/bind_defs.h"

// if don't have this attribute potentially will duplicate inline assembly due
// to code layout reordering. this happens in -O2+ with -freorder-blocks-algorithm=stc
// this is problematic with revec call which needs to sync pc between multiple traces on a revec
void __attribute__((optimize("-freorder-blocks-algorithm=simple"))) 
synthetic(int *a, int *b, int *c, int start, int end) {
  for (int i = start; i < end; i++) {
    
    if (a[i] == 0) {
      int b_ = b[i];
      c[i] = b_ * b_ * b_ * b_;
    }
    else {
      int b_ = b[i];
      c[i] = b_ * b_ * b_;
    }
    
    
    // try to revec at the end of loop iteration
    REVEC(0x0);
  }
}

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
  
  #ifndef _VEC
  synthetic(a, b, c, start, end);
  #else
  
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
  
  VECTOR_EPOCH(mask);
  
  synthetic(a, b, c, start, end);
  
  VECTOR_EPOCH(ALL_NORM);
  
  #endif
  
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
