#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

#include "pthread_launch.h"
#include "conv2d.h"
#include "spad.h"
#include "bind_defs.h"
#include "conv2d_kernel.h"

/*
  3x3 stencil with a single 3x3 filter
  Filter is cached in each spad.
  9 values are prefetched per frame
  Parallelize innermost loops (unrolled) so we can get away with codegen trick

  Reuse strategy - constraint is equal frame sizes and can't do remote stores to fill frames
  Each core gets 3 elements just like no reuse case but none are overlapping
  Its up to each core to do compute for those 3 elements and the two adjacents ones 
  Load x + 0, x + 1, x + 2
  Compute
  x - 1, x + 0, x + 1
         x + 0, x + 1, x + 2
                x + 1, x + 2, x + 3
  With this strategy you are the only core to have to load x + 1
  You have to reorder the data to implement! ( spaced out by strides )
  0 1 2 3 | 4 5 6 7  | 8 9 10 11 || 12 13 14 15
  0 3 6 9 | 1 4 7 10 | 2 5 8  11 || 12 15 18 21 

*/

void conv2d_manycore(DTYPE *a, DTYPE *b, int outer_dim, int inner_dim,
    int inner_start, int ptid, int pdim) {

  int eff_outer_dim = outer_dim - (FILTER_DIM-1);
  int outer_start  = 1 + ( ( ptid + 0 ) * eff_outer_dim ) / pdim;
  int outer_end    = 1 + ( ( ptid + 1 ) * eff_outer_dim ) / pdim;

  DEF_WEIGHTS();

  int NJ = inner_dim;



  for (int i = outer_start; i < outer_end; i++) {
    for (int j = inner_start; j < NJ - 1; j++) {
      // printf("%d->%d\n", inner_start, NJ-1);
      // TODO order in gpu version is outer dim first for some reason,
      // better to access in order below
      b[i*NJ + j] = CONV_3x3(
        a[(i - 1)*NJ + (j - 1)], a[(i - 1)*NJ + (j + 0)], a[(i - 1)*NJ + (j + 1)],
        a[(i + 0)*NJ + (j - 1)], a[(i + 0)*NJ + (j + 0)], a[(i + 0)*NJ + (j + 1)],
        a[(i + 1)*NJ + (j - 1)], a[(i + 1)*NJ + (j + 0)], a[(i + 1)*NJ + (j + 1)]
      );
    }
  }
}


void __attribute__((optimize("-freorder-blocks-algorithm=simple"))) kernel(
    DTYPE *a, DTYPE *b, int NI, int NJ,
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
  #ifdef VECTOR_LEN

  #if VECTOR_LEN==4
  // template_info_t tinfo = init_template_4x4_2x2();
  template_info_t tinfo = init_template_debug();
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
  }

  // printf("ptid %d(%d,%d) vtid %d(%d,%d) dim %d(%d,%d) %d->%d used? %d\n", ptid, ptid_x, ptid_y, vtid, vtid_x, vtid_y, 16, vdim_x, vdim_y, start, end, used); 

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

  #ifdef USE_VEC
  // volatile so dont reorder this function call
  int mask = getSIMDMask(&cinfo);
  #endif

  // printf("ptid %d(%d,%d) vtid %d(%d,%d) dim %d(%d,%d) %d->%d\n", ptid, ptid_x, ptid_y, vtid, vtid_x, vtid_y, vdim, vdim_x, vdim_y, start, end); 

  #ifdef NUM_REGIONS
  SET_PREFETCH_MASK(NUM_REGIONS, REGION_SIZE, &start_barrier);
  #endif

  // each vector group size is rated to do a certain problem size and multiples of that problem size
  // for the mod of this we need to do the rest on the flexible manycore version
  int rated_size = 0;
  #ifdef REUSE
  rated_size = ( VECTOR_LEN * FILTER_DIM - (FILTER_DIM - 1) );
  #elif defined(VERTICAL_LOADS)
  rated_size = ( VECTOR_LEN * CORE_STEP );
  #elif defined(VECTOR_LEN)
  rated_size = ( VECTOR_LEN );
  #else
  rated_size = 1;
  #endif

  // cols without the edge case
  int eff_len = NJ - (FILTER_DIM-1);
  // mapped len is schedule on main config, unmapped will be scheduled on base manycore
  int unmapped_len = eff_len % rated_size;
  int mapped_len = eff_len - unmapped_len;
  // mapped_len -= (FILTER_DIM-1);
  // unmapped_len -= (FILTER_DIM-1);

  // TODO better way to do this for arbitrary groups
  DTYPE *p_sp_ptr = NULL;
  DTYPE *n_sp_ptr = NULL;
  #ifdef REUSE
  // calculate prev and next spAddr for reuse
  if (vtid != 0) {
    if (vtid_x == 0) 
      p_sp_ptr = (DTYPE*)getSpAddr(ptid - (GRID_XDIM - (vdim_x - 1)), 0);
    else
      p_sp_ptr = (DTYPE*)getSpAddr(ptid - 1, 0);
  }
  if (vtid != vdim - 1) {
    if (vtid_x == vdim_x - 1)
      n_sp_ptr = (DTYPE*)getSpAddr(ptid + (GRID_XDIM - (vdim_x - 1)), 0);
    else 
      n_sp_ptr = (DTYPE*)getSpAddr(ptid + 1, 0);
  }
  #endif

  // printf("%d %xd\n", mapped_len, unmapped_len);

  // move stack onto scratchpad for faster local access than default on DRAM
  MOVE_STACK_ONTO_SCRATCHPAD();

  #ifdef USE_VEC
  // do computation that we can map
  if (used)
    tril_conv2d(mask, a, b, start, end, NJ, mapped_len, 
      ptid, vtid_x, vtid_y, vdim_x, vdim_y, p_sp_ptr, n_sp_ptr);

  // do remainder of computation starting from offset
  conv2d_manycore(a, b, NI, NJ, mapped_len + 1, ptid, pdim);
  #else
  conv2d_manycore(a, b, NI, NJ, 1, ptid, pdim);
  #endif

  // restore stack pointer to DRAM
  RECOVER_DRAM_STACK();

}


// helper functions
Kern_Args *construct_args(DTYPE *a, DTYPE *b, int NI, int NJ,
  int tid_x, int tid_y, int dim_x, int dim_y) {

  Kern_Args *args = (Kern_Args*)malloc(sizeof(Kern_Args));
  
  args->a = a;
  args->b = b;
  args->NI = NI;
  args->NJ = NJ;
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
  
  kernel(a->a, a->b, a->NI, a->NJ,
      a->tid_x, a->tid_y, a->dim_x, a->dim_y);

  pthread_barrier_wait(&start_barrier);

  if (a->tid_x == 0 && a->tid_y == 0) {
    stats_off();
  }

  return NULL;
}
