/*
 * SPMD code
 */ 
 
#include "vec.h"
#include "pthread_launch.h"
#include "bind_defs.h"
#include <stdio.h>

void *vec_pthread(void *args) {
  // unpack args
  vec_args_t *ka = (vec_args_t*)args;
  int tid_x = ka->tid_x;
  int tid_y = ka->tid_y;
  int dim_x = ka->dim_x;
  int dim_y = ka->dim_y;
  
  // guarentee one thread goes to each core, by preventing any threads
  // from finishing early
  pthread_barrier_wait(&start_barrier);
  
  // call the spmd gemm kernel
  vec(tid_x, tid_y, dim_x, dim_y);
}

void vec(int tid_x, int tid_y, int dim_x, int dim_y) {
  
  
  
        
}
