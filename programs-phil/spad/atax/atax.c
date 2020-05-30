#include <stdlib.h>
#include <stdio.h>

#include "pthread_launch.h"
#include "atax.h"
#include "spad.h"
#include "../../common/bind_defs.h"

#include "atax_kernel.h"



void __attribute__((optimize("-fno-inline")))
atax_manycore()
{

}


void kernel(DTYPE *a, DTYPE *_x, DTYPE *_y, int nx, int ny,
    int tid_x, int tid_y, int dim_x, int dim_y)
{

  // start recording all stats (all cores)
  if (tid_x == 0 && tid_y == 0)
  {
    stats_on();
  }


  // linearize tid and dim
  int tid = tid_x + tid_y * dim_x;
  int dim = dim_x * dim_y;

  // split into physical and virtual tids + dim
  int ptid_x = tid_x;
  int ptid_y = tid_y;
  int ptid = tid;
  int pdim_x = dim_x;
  int pdim_y = dim_y;
  int pdim = dim;
  int vtid_x = 0;
  int vtid_y = 0;
  int vtid = 0;
  int vdim_x = 0;
  int vdim_y = 0;
  int vdim = 0;
  int start = 0;
  int end = 0;
  int orig_x = 0;
  int orig_y = 0;
  int is_da = 0;
  int master_x = 0;
  int master_y = 0;
  int unique_id = 0;
  int total_groups = 0;


  #ifdef VEC_LEN_4
  // vec len 4 currently
 // virtual group dimension
  vdim_x = 2;
  vdim_y = 2;
  vdim = vdim_x * vdim_y;

  int used = vector_group_template_4(ptid_x, ptid_y, pdim_x, pdim_y,
    &vtid, &vtid_x, &vtid_y, &is_da, &orig_x, &orig_y, &master_x, &master_y, &unique_id, &total_groups);

  //divide work here
  
  #elif defined VEC_LEN_16
  vdim_x = 4;
  vdim_y = 4;
  vdim = vdim_x * vdim_y;

  int used = vector_group_template_16(ptid_x, ptid_y, pdim_x, pdim_y,
    &vtid, &vtid_x, &vtid_y, &is_da, &orig_x, &orig_y, &master_x, &master_y, &unique_id, &total_groups);

  //divide work here
  #else
  vdim_x = 1;
  vdim_y = 1;
  vdim   = vdim_x * vdim_y;
  vtid_x = 0;
  vtid_y = 0;
  vtid   = 0;
  orig_x = 0;
  orig_y = 0; //only have 1 group for now
  #endif

// region based mask for scratchpad
#ifdef _VEC
  int prefetchMask = (NUM_REGIONS << PREFETCH_NUM_REGION_SHAMT) | (REGION_SIZE << PREFETCH_REGION_SIZE_SHAMT);
  PREFETCH_EPOCH(prefetchMask);
#endif

//ensure vector config of cores
#ifdef _VEC
  int mask = getSIMDMask(master_x, master_y, orig_x, orig_y, vtid_x, vtid_y, vdim_x, vdim_y, is_da);
#else
  int mask = 0;
#endif


  // save the stack pointer to top of spad and change the stack pointer to point into the scratchpad
  // reset after the kernel is done
  // do before the function call so the arg stack frame is on the spad
  // store the the current spAddr to restore later

  unsigned long long *spTop = getSpTop(ptid);
  // // guess the remaining of the part of the frame (n) that might be needed?? here n = 30
  spTop -= 30;

  unsigned long long stackLoc;
  unsigned long long temp;
  #pragma GCC unroll(30)
  for(int i=0;i<30;i++){
    asm volatile("ld t0, %[id](sp)\n\t"
                "sd t0, %[id](%[spad])\n\t"
                : "=r"(temp)
                : [id] "i"(i*8), [spad] "r"(spTop));
  }
  asm volatile (// save the stack ptr
      "addi %[dest], sp, 0\n\t"
      // overwrite stack ptr
      "addi sp, %[spad], 0\n\t"
      : [ dest ] "=r"(stackLoc)
      : [ spad ] "r"(spTop));


#if defined _VEC
  template_vec(mask);

#else
  template_manycore();
#endif

  // restore stack pointer
  asm volatile(
      "addi sp, %[stackTop], 0\n\t" ::[stackTop] "r"(stackLoc));
}

// helper functions
Kern_Args *construct_args(DTYPE *a, DTYPE *_x, DTYPE *_y, int nx, int ny,
                          int tid_x, int tid_y, int dim_x, int dim_y)
{

  Kern_Args *args = (Kern_Args *)malloc(sizeof(Kern_Args));

  args->a = a;
  args->_x = _x;
  args->_y = _y;
  args->nx = nx;
  args->ny = ny;
  args->tid_x = tid_x;
  args->tid_y = tid_y;
  args->dim_x = dim_x;
  args->dim_y = dim_y;

  return args;
}

void *pthread_kernel(void *args)
{
  // guarentee one thread goes to each core, by preventing any threads
  // from finishing early

  pthread_barrier_wait(&start_barrier);

  // call the spmd kernel
  Kern_Args *a = (Kern_Args *)args;

  kernel(a->a, a->_x, a->_y, a->nx, a->ny,
         a->tid_x, a->tid_y, a->dim_x, a->dim_y);


  if (a->tid_x == 0 && a->tid_y == 0)
  {
    stats_off();
  }

  return NULL;
}
