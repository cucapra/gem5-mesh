#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

#include "pthread_launch.h"
#include "func.h"
#include "spad.h"
#include "../../common/bind_defs.h"

// #define USE_TERMINATOR 1

void __attribute__((optimize("-fno-inline"))) add(int *c, int *a, int *b, int i) {
  c[i] = a[i] + b[i];
}

void __attribute__((optimize("-fno-reorder-blocks"), optimize("-fno-inline")))
vvadd_kernel(DTYPE *a, DTYPE *b, DTYPE *c, int start, int end, int ptid, int vtid, int dim, int mask, int is_master) {

  // enter vector epoch within function, b/c vector-simd can't have control flow
  VECTOR_EPOCH(mask); 

  // issue header instructions
  ISSUE_VINST(fable0);

  // TODO allow unroll
  #pragma GCC unroll 0
  for (int i = start; i < end; i+=dim) {
    // issue fable1
    ISSUE_VINST(fable1);
  }

  // devec with unique tag
  DEVEC(devec_0);

  // we are doing lazy store acks, so use this to make sure all stores have commited to memory
  asm volatile("fence\n\t");

  return;

  // vector engine code

  // declarations
  int64_t iter; // avoids sext.w instruction when doing broadcast // TODO maybe should be doing rv32
  DTYPE *aPtr, *bPtr, *cPtr;

  // entry block
  // NOTE need to do own loop-invariant code hoisting?
  fable0:
    iter = 0;
    aPtr = a + start + vtid;
    bPtr = b + start + vtid;
    cPtr = c + start + vtid;
    // have script add terminator at the end of the block b/c may do some label adjusting
    // #ifdef USE_TERMINATOR
    // TERMINATE_BLOCK();
    // #endif
  
  // loop body block
  fable1:
    add(cPtr, aPtr, bPtr, iter);
    iter+=dim;
    // #ifdef USE_TERMINATOR
    // TERMINATE_BLOCK();
    // #endif

    // need this jump to create loop carry dependencies
    // an assembly pass will remove this instruction
    asm volatile goto("j %l[fable1]\n\t"::::fable1);

  return;
}

void __attribute__((optimize("-fno-reorder-blocks"))) 
kernel(DTYPE *a, DTYPE *b, DTYPE *c, int size,
    int tid_x, int tid_y, int dim_x, int dim_y) {
  // start recording all stats (all cores)
  if (tid_x == 0 && tid_y == 0) {
    stats_on();
  }

  // linearize tid and dim
  int tid = tid_x + tid_y * dim_x;
  int dim = dim_x * dim_y;

  // split into physical and virtual tids + dim
  int ptid_x = tid_x;
  int ptid_y = tid_y;
  int ptid   = tid;
  int pdim_x = dim_x;
  int pdim_y = dim_y;
  int pdim   = dim;
  int vtid_x = 0;
  int vtid_y = 0;
  int vtid   = 0;
  int vdim_x = 0;
  int vdim_y = 0;
  int vdim   = 0;
  int start  = 0;
  int end    = 0;
  int orig_x = 0;
  int orig_y = 0;
  int is_da  = 0;
  int master_x = 0;
  int master_y = 0;


  // virtual group dimension
  vdim_x = 2;
  vdim_y = 2;
  vdim = vdim_x * vdim_y;

  // group 1 top left (master = 0)
  if (ptid == 1) vtid = 0;
  if (ptid == 2) vtid = 1;
  if (ptid == 5) vtid = 2;
  if (ptid == 6) vtid = 3;
  if (ptid == 0) is_da = 1;
  if (ptid == 0 || ptid == 1 || ptid == 2 || ptid == 5 || ptid == 6) {
    start = 0;
    end = size;
    orig_x = 1;
    orig_y = 0;
    master_x = 0;
    master_y = 0;
  }
  else {
    return;
  }

  vtid_x = vtid % vdim_x;
  vtid_y = vtid / vdim_y;


  int mask = getSIMDMask(master_x, master_y, orig_x, orig_y, vtid_x, vtid_y, vdim_x, vdim_y, is_da);

  // save the stack pointer to top of spad and change the stack pointer to point into the scratchpad
  // reset after the kernel is done
  // do before the function call so the arg stack frame is on the spad
  // store the the current spAddr to restore later 
  unsigned long long *spTop = getSpTop(ptid);
  // guess the remaining of the part of the frame that might be needed??
  spTop -= 4;

  unsigned long long stackLoc;
  asm volatile (
    // copy part of the stack onto the scratchpad in case there are any loads to scratchpad right before
    // function call
    "ld t0, 0(sp)\n\t"
    "sd t0, 0(%[spad])\n\t"
    "ld t0, 8(sp)\n\t"
    "sd t0, 8(%[spad])\n\t"
    "ld t0, 16(sp)\n\t"
    "sd t0, 16(%[spad])\n\t"
    "ld t0, 24(sp)\n\t"
    "sd t0, 24(%[spad])\n\t"
    // save the stack ptr
    "addi %[dest], sp, 0\n\t" 
    // overwrite stack ptr
    "addi sp, %[spad], 0\n\t"
    : [dest] "=r" (stackLoc)
    : [spad] "r" (spTop)
  );

  vvadd_kernel(a, b, c, start, end, ptid, vtid, vdim, mask, is_da);
  
  // restore stack pointer
  asm volatile (
    "addi sp, %[stackTop], 0\n\t" :: [stackTop] "r" (stackLoc)
  );

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
