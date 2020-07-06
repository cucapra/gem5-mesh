#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

#include "pthread_launch.h"
#include "vvadd.h"
#include "spad.h"
#include "bind_defs.h"
#include "vvadd_kernel.h"
#include "util.h"

void vvadd(DTYPE *a, DTYPE *b, DTYPE *c, int start, int end, 
    int ptid, int vtid, int dim, int unroll_len, int is_da, int origin) {
  
  for (int i = start + vtid; i < end; i+=unroll_len*dim) {
      DTYPE a_, b_, c_;
      a_ = a[i];
      b_ = b[i];
      // add and then store
      c_ = a_ + b_;
      STORE_NOACK(c_, c + i, 0);
  }
}

void __attribute__((optimize("-freorder-blocks-algorithm=simple"))) kernel(
    DTYPE *a, DTYPE *b, DTYPE *c, int n,
    int ptid_x, int ptid_y, int pdim_x, int pdim_y) {
  
  // start recording all stats (all cores)
  if (ptid_x == 0 && ptid_y == 0) {
    stats_on();
  }

  // linearize tid and dim
  int ptid = ptid_x + ptid_y * pdim_x;
  int pdim = pdim_x * pdim_y;

  int start  = 0;
  int end    = 0;

  // group construction
  #ifdef VECTOR_LEN

  #if VECTOR_LEN==4
  template_info_t tinfo = init_template_4x4_2x2();
  // template_info_t tinfo = init_template_debug();
  #elif VECTOR_LEN==16
  template_info_t tinfo = init_template_8x8_4x4();
  #endif
  core_config_info_t cinfo = vector_group_template(ptid_x, ptid_y, pdim_x, pdim_y, &tinfo);

  if (cinfo.used) {
    int alignment = 16 * VECTOR_LEN;
    start = roundUp((cinfo.unique_id + 0) * n / cinfo.total_groups, alignment);
    end   = roundUp((cinfo.unique_id + 1) * n / cinfo.total_groups, alignment);
  }

  #elif !defined(USE_VEC)
  core_config_info_t cinfo = manycore_template(ptid_x, ptid_y, pdim_x, pdim_y);

  //do work division here
  start  = ( ( cinfo.unique_id + 0 ) * n ) / cinfo.total_groups;
  end    = ( ( cinfo.unique_id + 1 ) * n ) / cinfo.total_groups;
  #endif

  // construct special mask for dae example
  #ifdef USE_VEC
  int mask = getSIMDMask(&cinfo);
  #endif

  // printf("ptid %d(%d,%d) vtid %d(%d,%d) dim %d(%d,%d) %d->%d\n", ptid, ptid_x, ptid_y, vtid, vtid_x, vtid_y, vdim, vdim_x, vdim_y, start, end); 

  #ifdef NUM_REGIONS
  SET_PREFETCH_MASK(NUM_REGIONS, REGION_SIZE, &start_barrier);
  #endif

  // only let certain tids continue
  #if defined(USE_VEC)
  if (cinfo.used == 0) return;
  #endif

  // run the actual kernel with the configuration
  #ifdef UNROLL
  int unroll_len = REGION_SIZE / 2;
  #else
  int unroll_len = 1;
  #endif

  // move stack onto scratchpad for faster local access than default on DRAM
  MOVE_STACK_ONTO_SCRATCHPAD();

  // configure
  #ifdef USE_VEC
  tril_vvadd(mask, a, b, c, start, end, ptid, get_vtid(&cinfo), get_vdim(&cinfo), cinfo.is_scalar);
  #else
  vvadd(a, b, c, start, end, ptid, vtid, vdim, unroll_len, is_da, orig);
  #endif

  // restore stack pointer to DRAM
  RECOVER_DRAM_STACK();

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
