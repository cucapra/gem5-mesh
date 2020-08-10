#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

#include "pthread_launch.h"
#include "conv3d.h"
#include "spad.h"
#include "bind_defs.h"
#include "conv3d_kernel.h"

/*
  Conv3d. weird kernel
*/

void conv3d_manycore(DTYPE *a, DTYPE *b, int NI, int NJ, int NK, int ptid, int pdim) {

  int eff_outer_dim = NI - (FILTER_DIM-1);
  int outer_start  = 1 + ( ( ptid + 0 ) * eff_outer_dim ) / pdim;
  int outer_end    = 1 + ( ( ptid + 1 ) * eff_outer_dim ) / pdim;

  DEF_WEIGHTS();

  #ifdef MANYCORE_PREFETCH
  int sp = 0;
  DTYPE* sp_ptr = (DTYPE*)getSpAddr(ptid, 0);
  for (int i = outer_start; i < outer_end; i++) {
    for (int j = 1; j < NJ - 1; j++) {
      for (int k = 1; k < NK - 1; k++) {
        prefetch_horiz_frame(a, i, j, k, NJ, NK, &sp);

        START_FRAME();
        DTYPE out = 
          CONV_15(
            a[IDX(i-1, j-1, k-1, NJ, NK)], a[IDX(i-1, j-1, k+1, NJ, NK)], 
            a[IDX(i-1, j+0, k+1, NJ, NK)], a[IDX(i-1, j+1, k+1, NJ, NK)], 
            a[IDX(i+0, j-1, k+0, NJ, NK)], a[IDX(i+0, j+0, k+0, NJ, NK)], 
            a[IDX(i+0, j+1, k+0, NJ, NK)], a[IDX(i+1, j-1, k-1, NJ, NK)], 
            a[IDX(i+1, j-1, k+1, NJ, NK)], a[IDX(i+1, j+0, k+1, NJ, NK)], 
            a[IDX(i+1, j+1, k+1, NJ, NK)]
          );
        END_FRAME();
        sp += REGION_SIZE;
        if (sp == POST_REGION_WORD) sp = 0;
        STORE_NOACK(out, &b[IDX(i, j, k, NJ, NK)], 0);
      }
    }
  }
  #else
  for (int i = outer_start; i < outer_end; i++) {
    for (int j = 1; j < NJ - 1; j++) {
      for (int k = 1; k < NK - 1; k++) {
        DTYPE out = 
          CONV_15(
            a[IDX(i-1, j-1, k-1, NJ, NK)], a[IDX(i-1, j-1, k+1, NJ, NK)], 
            a[IDX(i-1, j+0, k+1, NJ, NK)], a[IDX(i-1, j+1, k+1, NJ, NK)], 
            a[IDX(i+0, j-1, k+0, NJ, NK)], a[IDX(i+0, j+0, k+0, NJ, NK)], 
            a[IDX(i+0, j+1, k+0, NJ, NK)], a[IDX(i+1, j-1, k-1, NJ, NK)], 
            a[IDX(i+1, j-1, k+1, NJ, NK)], a[IDX(i+1, j+0, k+1, NJ, NK)], 
            a[IDX(i+1, j+1, k+1, NJ, NK)]
          );
        STORE_NOACK(out, &b[IDX(i, j, k, NJ, NK)], 0);
      }
    }
  }
  #endif
  asm volatile("fence\n\t");
}


void __attribute__((optimize("-freorder-blocks-algorithm=simple"))) kernel(
    DTYPE *a, DTYPE *b, int NI, int NJ, int NK,
    int ptid_x, int ptid_y, int pdim_x, int pdim_y) {
  
  // start recording all stats (all cores)
  if (ptid_x == 0 && ptid_y == 0) {
    stats_on();
  }

  // linearize tid and dim
  int ptid = ptid_x + ptid_y * pdim_x;
  int pdim = pdim_x * pdim_y;

  // split into physical and virtual tids + dim
  int vtid_x = 0;
  int vtid_y = 0;
  int vtid   = 0;
  int vdim_x = 0;
  int vdim_y = 0;
  int vdim   = 0;
  int unique_id = 0;
  int total_groups = 0;
  int used = 0;
  int start = 0;
  int end = 0;

  // group construction
  #ifdef USE_VEC

  #if VECTOR_LEN==4
  template_info_t tinfo = init_template_4x4_2x2();
  // template_info_t tinfo = init_template_debug();
  #elif VECTOR_LEN==16
  template_info_t tinfo = init_template_8x8_4x4();
  #endif
  core_config_info_t cinfo = vector_group_template(ptid_x, ptid_y, pdim_x, pdim_y, &tinfo);

  vtid = cinfo.vtid;
  vtid_x = cinfo.vtid_x;
  vtid_y = cinfo.vtid_y;
  vdim_x = cinfo.vdim_x;
  vdim_y = cinfo.vdim_y;
  unique_id = cinfo.unique_id;
  total_groups = cinfo.total_groups;
  used = cinfo.used;

  if (used) {
    start = 1 + ( (unique_id + 0) * (NI-2) ) / total_groups;
    end   = 1 + ( (unique_id + 1) * (NI-2) ) / total_groups;
    // printf("%d->%d\n", start, end); 
  }

  #elif !defined(USE_VEC)

  vdim_x = 1;
  vdim_y = 1;
  vtid_x = 0;
  vtid_y = 0;
  vtid   = 0;
  // start  = ( ( ptid + 0 ) * NI ) / pdim;
  // end    = ( ( ptid + 1 ) * NI ) / pdim;

  // printf("%d->%d\n", start, end); 

  #endif

  // linearize some fields
  vdim = vdim_x * vdim_y;

  // printf("ptid %d(%d,%d) vtid %d(%d,%d) dim %d(%d,%d) %d->%d\n", ptid, ptid_x, ptid_y, vtid, vtid_x, vtid_y, vdim, vdim_x, vdim_y, start, end); 

  #ifdef NUM_REGIONS
  #ifdef MANYCORE_PREFETCH
  core_config_info_t cinfo = manycore_template(ptid_x, ptid_y, pdim_x, pdim_y);
  int mask = getDebugMask(&cinfo);
  VECTOR_EPOCH(mask);
  #else
  int mask = getSIMDMask(&cinfo);
  #endif
  SET_PREFETCH_MASK(NUM_REGIONS, REGION_SIZE, &start_barrier);
  #endif

  // each vector group size is rated to do a certain problem size and multiples of that problem size
  // for the mod of this we need to do the rest on the flexible manycore version
  // int rated_size = 0;
  // #ifdef REUSE
  // rated_size = ( VECTOR_LEN * FILTER_DIM - (FILTER_DIM - 1) );
  // #elif defined(VERTICAL_LOADS)
  // rated_size = ( VECTOR_LEN * CORE_STEP );
  // #elif defined(VECTOR_LEN)
  // rated_size = ( VECTOR_LEN );
  // #else
  // rated_size = 1;
  // #endif

  // // cols without the edge case
  // int eff_len = NJ - (FILTER_DIM-1);
  // // mapped len is schedule on main config, unmapped will be scheduled on base manycore
  // int unmapped_len = eff_len % rated_size;
  // int mapped_len = eff_len - unmapped_len;

  // printf("%d %xd\n", mapped_len, unmapped_len);

  // move stack onto scratchpad for faster local access than default on DRAM
  MOVE_STACK_ONTO_SCRATCHPAD();

  #ifdef USE_VEC
  // do computation that we can map
  if (used)
    tril_conv3d(mask, a, b, start, end, NJ, NK,
      ptid, vtid_x, vtid_y, vdim_x, vdim_y);

  // do remainder of computation starting from offset
  // conv3d_manycore(a, b, NI, NJ, mapped_len + 1, ptid, pdim);
  #else
  conv3d_manycore(a, b, NI, NJ, NK, ptid, pdim);
  #endif

  // restore stack pointer to DRAM
  RECOVER_DRAM_STACK();

}


// helper functions
Kern_Args *construct_args(DTYPE *a, DTYPE *b, int NI, int NJ, int NK,
  int tid_x, int tid_y, int dim_x, int dim_y) {

  Kern_Args *args = (Kern_Args*)malloc(sizeof(Kern_Args));
  
  args->a = a;
  args->b = b;
  args->NI = NI;
  args->NJ = NJ;
  args->NK = NK;
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
  
  kernel(a->a, a->b, a->NI, a->NJ, a->NK,
      a->tid_x, a->tid_y, a->dim_x, a->dim_y);

  pthread_barrier_wait(&start_barrier);

  if (a->tid_x == 0 && a->tid_y == 0) {
    stats_off();
  }

  return NULL;
}
