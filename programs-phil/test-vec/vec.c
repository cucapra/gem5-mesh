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

// some data to use in lw instructions
int mem[4] = { 10, 100, 1000, 10000 };

void vec(int tid_x, int tid_y, int dim_x, int dim_y) {
  
  // TODO want binded section around normal c code!
  // need to figure out how to prevent code reordering around binds
  int op0 = tid_x + 1;
  int op1 = tid_y + 1;
  int rd = 0;
  int *virtualMemAddr = &(mem[tid_x + tid_y * dim_x]);
  
  printf("tid_x %d tid_y %d dim_x %d dim_y %d\n", tid_x, tid_y, dim_x, dim_y);
  
  
  if (tid_x == 0 && tid_y == 0) {
    BINDED_FET_SECTION(
      FET_O_INST_RIGHT_SEND,
      ALL_NORM,
      0,
      
      "add x29, %[a], %[b]\n\t"
      "lw x30, 0(%[m0])\n\t"
      "add %[a0], x29, x30\n\t"
      ,
      [a0] "=r" (rd)
      ,
      COMMA [a] "r" (op0) COMMA [b] "r" (op1) COMMA [m0] "r" (virtualMemAddr)
    );
  }
  else if (tid_x == 1 && tid_y == 0) {
    BINDED_FET_SECTION(
      FET_I_INST_LEFT,
      ALL_NORM,
      1,
      
      "add x29, %[a], %[b]\n\t"
      "lw x30, 0(%[m0])\n\t"
      "add %[a0], x29, x30\n\t"
      ,
      [a0] "=r" (rd)
      ,
      COMMA [a] "r" (op0) COMMA [b] "r" (op1) COMMA [m0] "r" (virtualMemAddr)
    );
  }
  
  
  /*if (tid_x == 0 && tid_y == 0) {
    BINDED_FET_SECTION(
      FET_O_INST_DOWN_SEND | FET_O_INST_RIGHT_SEND,
      ALL_NORM,
      0,
      
      "add x29, %[a], %[b]\n\t"
      "lw x30, 0(%[m0])\n\t"
      "add %[a0], x29, x30\n\t"
      ,
      [a0] "=r" (rd)
      ,
      COMMA [a] "r" (op0) COMMA [b] "r" (op1) COMMA [m0] "r" (virtualMemAddr)
    );
  }
  else if (tid_x == 1 && tid_y == 0) {
    BINDED_FET_SECTION(
      FET_I_INST_LEFT | FET_O_INST_DOWN_SEND,
      ALL_NORM,
      1,
      
      "add x29, %[a], %[b]\n\t"
      "lw x30, 0(%[m0])\n\t"
      "add %[a0], x29, x30\n\t"
      ,
      [a0] "=r" (rd)
      ,
      COMMA [a] "r" (op0) COMMA [b] "r" (op1) COMMA [m0] "r" (virtualMemAddr)
    );
  }
  else if (tid_x == 0 && tid_y == 1) {
    BINDED_FET_SECTION(
      FET_I_INST_UP,
      ALL_NORM,
      2,
      
      "add x29, %[a], %[b]\n\t"
      "lw x30, 0(%[m0])\n\t"
      "add %[a0], x29, x30\n\t"
      ,
      [a0] "=r" (rd)
      ,
      COMMA [a] "r" (op0) COMMA [b] "r" (op1) COMMA [m0] "r" (virtualMemAddr)
    );
  }
  else if (tid_x == 1 && tid_y == 1) {
    BINDED_FET_SECTION(
      FET_I_INST_UP,
      ALL_NORM,
      3,
      
      "add x29, %[a], %[b]\n\t"
      "lw x30, 0(%[m0])\n\t"
      "add %[a0], x29, x30\n\t"
      ,
      [a0] "=r" (rd)
      ,
      COMMA [a] "r" (op0) COMMA [b] "r" (op1) COMMA [m0] "r" (virtualMemAddr)
    );
  }*/
  
  printf("(%d, %d): %d\n", tid_x, tid_y, rd);
  
  
        
}
