#include <stdio.h>

#include "pthread_launch.h"
#include "template.h"
#include "spad.h"
#include "bind_defs.h"
#include "token_queue.h"
#include "group_templates.h"
#include "reduction.h"
#include "util.h"

#include "template_kernel.h"


void __attribute__((optimize("-fno-inline")))
template_manycore()
{

}


void kernel(DTYPE *a, int n,
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
  int start = 0;
  int end   = 0;

  #ifdef _VEC
  #if VEC_LEN==4
  template_info_t tinfo = init_template_4x4_2x2();
  #elif VEC_LEN==16
  template_info_t tinfo = init_template_8x8_4x4();
  #endif
  core_config_info_t cinfo = vector_group_template(ptid_x, ptid_y, pdim_x, pdim_y, &tinfo);
  
  if(cinfo.used){
    //do work division here
    int alignment = VEC_LEN; //each group should have elements of multiple of this number
    start = roundUp((cinfo.unique_id + 0) * n / cinfo.total_groups, alignment); 
    end = roundUp((cinfo.unique_id + 1) * n / cinfo.total_groups, alignment); 
  }

  #else
  core_config_info_t cinfo = manycore_template(ptid_x, ptid_y, pdim_x, pdim_y);

  //do work division here
  start  = ( ( cinfo.unique_id + 0 ) * n ) / cinfo.total_groups;
  end    = ( ( cinfo.unique_id + 1 ) * n ) / cinfo.total_groups;
  #endif

  // get behavior of each core
  #ifdef _VEC
  int mask = getSIMDMask(&cinfo);
  #else
  int mask = 0;
  #endif

  // region based mask for scratchpad
#ifdef _VEC
  SET_PREFETCH_MASK(NUM_REGIONS, REGION_SIZE, &start_barrier);
#endif

  // only let certain tids continue
  if (cinfo.used == 0) return;

  // move stack onto scratchpad for faster local access than default on DRAM
  MOVE_STACK_ONTO_SCRATCHPAD();

#if defined USE_VECTOR_SIMD
  template_vec(mask);

#else
  template_manycore();
#endif

  // restore stack pointer to DRAM
  RECOVER_DRAM_STACK();
}

// helper functions
Kern_Args *construct_args(DTYPE *a, int n,
                          int tid_x, int tid_y, int dim_x, int dim_y)
{

  Kern_Args *args = (Kern_Args *)malloc(sizeof(Kern_Args));

  args->a = a;
  args->n = n;
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

  kernel(a->a, a->n,
         a->tid_x, a->tid_y, a->dim_x, a->dim_y);


  if (a->tid_x == 0 && a->tid_y == 0)
  {
    stats_off();
  }

  return NULL;
}
