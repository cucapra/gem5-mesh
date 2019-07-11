/*
 * SPMD code
 */ 
 
#include "gemm.h"
#include "pthread_launch.h"
#include <stdio.h>

void *gemm_pthread(void *args) {
  // unpack args
  gemm_args_t *ka = (gemm_args_t*)args;
  int tid_x = ka->tid_x;
  int tid_y = ka->tid_y;
  int dim_x = ka->dim_x;
  int dim_y = ka->dim_y;
  int m     = ka->m;
  int n     = ka->n;
  int t     = ka->t;
  int *a    = ka->a;
  int *b    = ka->b;
  int *c    = ka->c;
  
  // guarentee one thread goes to each core, by preventing any threads
  // from finishing early
  pthread_barrier_wait(&start_barrier);
  
  // call the spmd gemm kernel
  gemm(tid_x, tid_y, dim_x, dim_y, a, b, c, m, n, t);
}

void gemm(int tid_x, int tid_y, int dim_x, int dim_y, int *a,
      int *b, int *c, int m, int n, int t) {
        
  printf("yes\n");
        
}
