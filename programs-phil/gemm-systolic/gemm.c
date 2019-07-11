/*
 * SPMD code
 */ 
 
#include "gemm.h"
#include "pthread_launch.h"
#include "bind_defs.h"
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

// kernel used to load in row of a
void left_kernel(int row, int *a, int t) {
  int op0, op1, rd;
  
  for (int i = 0; i < t; i++) {
    int mem_idx = row * t + i;
    int *vAddr = &(a[mem_idx]);
    
    // TODO unroll and/or bind fetch + counter
    BINDED_SECTION(O_RIGHT_RD, ALL_NORM,
      "ld   %[a0], 0(%[m0])\n\t"
      ,
      [a0] "=r" (rd)
      ,
      COMMA [m0] "r" (vAddr)
    );
  }
}

// kernel used to load in col of b
void top_kernel(int col, int *b, int t) {
  int op0, op1, rd;
  
  for (int i = 0; i < t; i++) {
    int mem_idx = i * t + col;
    int *vAddr = &(b[mem_idx]);
    
    // TODO unroll and/or bind fetch + counter
    BINDED_SECTION(O_DOWN_RD, ALL_NORM,
      "ld   %[a0], 0(%[m0])\n\t"
      ,
      [a0] "=r" (rd)
      ,
      COMMA [m0] "r" (vAddr)
    );
  }
}

void center_kernel(int pos_x, int pos_y, int *c, int n, int t) {
  int op0, op1, rd;
  
  int c_idx = pos_x + pos_y * n;
  
  for (int i = 0; i < t; i++) {
    
    // we don't have the madd instruction (TODO?)
    // so we need to multiply, then add
    BINDED_SECTION(
      I_RS1_LEFT  | I_RS2_UP | 
      O_RIGHT_RS1 | O_DOWN_RS2, 
      ALL_NORM,
      "mul %[a0], %[a], %[b]\n\t"
      ,
      [a0] "=r" (rd)
      ,
      COMMA [a] "r" (op0) COMMA [b] "r" (op1)
    );
    
    // doing add based on stored values
    // potentially can do this work if stalled in the sytolic part?
    // also maybe can do this in batch when all registers full?
    // TODO psums for better caching locality
    c[c_idx] += rd;
  }
}


void gemm(int tid_x, int tid_y, int dim_x, int dim_y, int *a,
      int *b, int *c, int m, int n, int t) {
  
  // if the core is on the edge its responsible for loading
  // left edge gets rows of 'a' matrix (depending on row of grid)
  // top edge gets cols of 'b' matrix (depending on col of grid)
  // everyone else does madd
  
  // or preload b, stream from left, and psum accum top down (1/2 bandwidth?)
  
  // 2x2 matrix --> 3x3 cores
  
  // top row
  if (tid_y == 0) {
    for (int x = 1; x < dim_x; x++) {
      if (tid_x == x) {
        top_kernel(x, b, t);
      }
    }
  }
  
  // left col
  if (tid_x == 0) {
    for (int y = 1; y < dim_y; y++) {
      if (tid_y == y) {
        left_kernel(y, a, t);
      }
    }
  }
  
  // otherwise you are doing accum
  if (tid_y != 0 && tid_x != 0) {
    center_kernel(tid_x - 1, tid_y - 1, c, n, t);
  }
  
        
}
