#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

#include "pthread_launch.h"
#include "pred.h"
#include "spad.h"
#include "../../common/bind_defs.h"

void __attribute__((optimize("-freorder-blocks-algorithm=simple"), optimize("-fno-inline"))) 
kernel(DTYPE *a, DTYPE *b, DTYPE *c, int size,
    int tid_x, int tid_y, int dim_x, int dim_y) {
  
  int answer;

  asm volatile(
    // reset pred register (1)
    ".insn r 0x33, 0x7, 0x5, x0, x0, x0\n\t"
    // initialize values
    "li t0, 123\n\t"
    "li t1, 35\n\t"
    "li t2, 40\n\t"
    "li t3, 40\n\t"
    // deactivate pred (0)
    ".insn r 0x33, 0x7, 0x5, x0, t1, t2\n\t"
    "addi t2, t2, 5\n\t"
    // reactivate (assuming inst didn't run) (1)
    ".insn r 0x33, 0x7, 0x5, x0, t2, t3\n\t"
    "addi t2, t2, -5\n\t"
    // deactivate from dep (0)
    ".insn r 0x33, 0x7, 0x5, x0, t3, t2\n\t"
    "addi t0, t0, -2\n\t"
    "addi t0, t0, -4\n\t"
    // reactivate (1)
    ".insn r 0x33, 0x7, 0x5, x0, t1, t2\n\t"
    "addi t0, t0, 25\n\t"
    "addi t0, t0, 6\n\t"
    "addi %[result], t0, 0\n\t"
    : [result] "=r" (answer)
  );

  if (answer == 154) {
    printf("[[SUCCESS]]\n");
  }
  else {
    printf("[[FAIL]]\n");
  }

}


// helper functions
Kern_Args *construct_args(DTYPE *a, DTYPE *b, DTYPE *c, int size,
  int tid_x, int tid_y, int dim_x, int dim_y) {

  Kern_Args *args = (Kern_Args*)malloc(sizeof(Kern_Args));
  
  args->a = a;
  args->b = b;
  args->c = c;
  args->size = size;
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
  
  kernel(a->a, a->b, a->c, a->size, 
      a->tid_x, a->tid_y, a->dim_x, a->dim_y);
      
  pthread_barrier_wait(&start_barrier);

  if (a->tid_x == 0 && a->tid_y == 0) {
    stats_off();
  }

  // BUG: note this printf fails if have the VECTOR_EPOCH(0), but mayber just timing thing
  // printf("ptid (%d,%d)\n", a->tid_x, a->tid_y);

  return NULL;
}
