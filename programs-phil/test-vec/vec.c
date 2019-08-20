/*
 * SPMD code
 */ 
 
#include "vec.h"
#include "pthread_launch.h"
#include "bind_defs.h"
#include <stdio.h>

// 32 bit instructions -> 4 bytes
#define INST_BYTES 4

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
  
  // TODO want binded section around normal c code!
  // need to figure out how to prevent code reordering around binds
  int op0 = tid_x + 1;
  int op1 = tid_y + 1;
  int rd = 0;
  
  printf("tid_x %d tid_y %d\n", tid_x, tid_y);
  
  // FET_O_INST_DOWN_SEND | FET_O_INST_RIGHT_SEND,
  /*if (tid_x == 0 && tid_y == 0) {
    BINDED_FET_SECTION(
      FET_O_INST_RIGHT_SEND,
      0,
      
      "add %[a0], %[a], %[b]\n\t"
      ,
      [a0] "=r" (rd)
      ,
      COMMA [a] "r" (op0) COMMA [b] "r" (op1)
    );
  }
  // the pc still ticks on these?
  // either replicate instructions or need to not advanceInst()
  
  // I think need to replicate so compiler fills in the supporting code
  // before and after
  //FET_O_INST_DOWN_SEND | FET_I_INST_LEFT,
  else if (tid_x == 1 && tid_y == 0) {
    BINDED_FET_SECTION(
      FET_I_INST_LEFT,
      1,
      
      "add %[a0], %[a], %[b]\n\t"
      ,
      [a0] "=r" (rd)
      ,
      COMMA [a] "r" (op0) COMMA [b] "r" (op1)
    );
  }*/
  
  
  if (tid_x == 0 && tid_y == 0) {
    /*BINDED_FET_SECTION(
      ALL_NORM,
      0,
      
      ,
      ,
    );*/
    int t = 0;
    
    asm volatile (
    // using x1 may create two more instruction in and outside of this loop
    // to save and restore the stack
      ".insn u 0x77, x1, 0x0\n\t"
      
      //"la %[a0], devec_label\n\t"
      //"jalr"
      
      // if you do '.insn uj' it might work. 
      // compiler will do the neccessary offset transformation from absolute label addr
      // but need to do based off the pc at the start of the block, not the 
      // current pc, need to specify src reg.. but the compiler computes the
      // diff based on the addr of this instruction and the address
      
      //".insn uj 0x7b, x1, devec_label\n\t" 
      ".insn u 0x7b, x1, %[offset]\n\t"
      // opcode, funct3, rs1, rs2, target
      //".insn sb 0x7b, 0, x1, x0, %[offset]\n\t"
      
      // works
      // the compiler or riscv format is def doing something to
      // convert jal label to PC + addr
      //"j devec_label\n\t"
      //"jal x0, devec_label\n\t"
      
      //"devec_label:\n\t"
      
      :
      : [offset] "i" (2 * INST_BYTES)
    );
    
    printf("%d\n", t);
    
  }

  /*else if (tid_x == 1 && tid_y == 0) {
    BINDED_FET_SECTION(
      ALL_NORM,
      1,
      
      ,
      ,
    );
  }*/
  
  
  
  /*else if (tid_x == 0 && tid_y == 1) {
    BINDED_SECTION(
      ALL_NORM,
      ALL_NORM,
      
      FET_I_INST_UP,
      ALL_NORM,
      
      "add %[a0], %[a], %[b]\n\t"
      ,
      [a0] "=r" (rd)
      ,
      COMMA [a] "r" (op0) COMMA [b] "r" (op1)
    );
  }
  else if (tid_x == 1 && tid_y == 1) {
    BINDED_SECTION(
      ALL_NORM,
      ALL_NORM,
      
      FET_I_INST_UP,
      ALL_NORM,
      
      "add %[a0], %[a], %[b]\n\t"
      ,
      [a0] "=r" (rd)
      ,
      COMMA [a] "r" (op0) COMMA [b] "r" (op1)
    );
  }*/
  
  printf("(%d, %d): %d\n", tid_x, tid_y, rd);
  
  
        
}
