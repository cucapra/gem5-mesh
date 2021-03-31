#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

#include "pthread_launch.h"
#include "conv2d.h"
#include "spad.h"
#include "bind_defs.h"
#include "conv2d_kernel.h"
#include "util.h"

#ifdef PACKED_SIMD
#include <riscv_vector.h>
#endif

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

  #ifdef PACKED_SIMD
  int cEnd = ((NJ-1) / CORE_STEP) * CORE_STEP;

  for (int r = outer_start; r < outer_end; r++) {
  
    for (int c = inner_start; c < cEnd; c+=CORE_STEP) {
      // vlen = max, stuff that doesnt fit is done on manycore at end
      vsetvl_e32m1(HARDWARE_VECTOR_LEN);

      // load 3x16 section of image and produce 14 outputs
      vfloat32m1_t r1 = vle32_v_f32m1(&a[(r - 1)*NJ + (c - 1)]);
      vfloat32m1_t r2 = vle32_v_f32m1(&a[(r + 0)*NJ + (c - 1)]);
      vfloat32m1_t r3 = vle32_v_f32m1(&a[(r + 1)*NJ + (c - 1)]);
    
      // do three vector-scalar multiplications for each row (one for each filter element)
      vfloat32m1_t r11, r12, r13, r21, r22, r23, r31, r32, r33;
      // row 1
      r11 = vfmul_vf_f32m1(r1, c11);
      r21 = vfmul_vf_f32m1(r1, c21);
      r31 = vfmul_vf_f32m1(r1, c31);
      // row 2
      r12 = vfmul_vf_f32m1(r2, c12);
      r22 = vfmul_vf_f32m1(r2, c22);
      r32 = vfmul_vf_f32m1(r2, c32);
      // row 3
      r13 = vfmul_vf_f32m1(r3, c13);
      r23 = vfmul_vf_f32m1(r3, c23);
      r33 = vfmul_vf_f32m1(r3, c33);

      // shift vectors so when add get a partial product for the row
      // row 1
      r21 = vslidedown_vx_f32m1(r21, r21, 1);
      r31 = vslidedown_vx_f32m1(r31, r31, 2);
      // row 2
      r22 = vslidedown_vx_f32m1(r22, r22, 1);
      r32 = vslidedown_vx_f32m1(r32, r32, 2);
      // row 3
      r23 = vslidedown_vx_f32m1(r23, r23, 1);
      r33 = vslidedown_vx_f32m1(r33, r33, 2);

      // only want to do first 16
      // vsetvl_e32m1(CORE_STEP);

      // mask of last two elements
      vint32m1_t vmask = vmv_v_x_i32m1(1); // splat 0
      vfloat32m1_t voffmask = vfmv_v_f_f32m1(0.0f);
      vmask = vslidedown_vx_i32m1(vmask, vmask, (FILTER_DIM-1));
      vbool32_t mask = vmseq_vx_i32m1_b32(vmask, 1);

      // add every vector togehter for a set of 14 outputs (last 2 are duds)
      vfloat32m1_t ofmap = vfadd_vv_f32m1(r11, r21);
      ofmap = vfadd_vv_f32m1(ofmap, r31);
      ofmap = vfadd_vv_f32m1(ofmap, r12);
      ofmap = vfadd_vv_f32m1(ofmap, r22);
      ofmap = vfadd_vv_f32m1(ofmap, r32);
      ofmap = vfadd_vv_f32m1(ofmap, r13);
      ofmap = vfadd_vv_f32m1(ofmap, r23);
      ofmap = vfadd_vv_f32m1(ofmap, r33);

      // ofmap = vfadd_vv_f32m1_m(mask, voffmask, ofmap, r33);

      vse32_v_f32m1_m(mask, &b[r*NJ + c], ofmap);
    }

    // TODO not clear how much this matters
    // do rest that doesnt map into prefetch pattern
    #pragma GCC unroll(16)
    for (int c = cEnd; c < NJ-1; c++) {
      int i = r;
      int j = c;
      DTYPE out = CONV_3x3(
        a[(i - 1)*NJ + (j - 1)], a[(i - 1)*NJ + (j + 0)], a[(i - 1)*NJ + (j + 1)],
        a[(i + 0)*NJ + (j - 1)], a[(i + 0)*NJ + (j + 0)], a[(i + 0)*NJ + (j + 1)],
        a[(i + 1)*NJ + (j - 1)], a[(i + 1)*NJ + (j + 0)], a[(i + 1)*NJ + (j + 1)]
      );
      FSTORE_NOACK(out, &b[i*NJ + j], 0);
    }
  }
  #elif defined(MANYCORE_PREFETCH)
  int sp = 0;
  DTYPE* sp_ptr = (DTYPE*)getSpAddr(ptid, 0);

  int cEnd = ((NJ-1) / CORE_STEP) * CORE_STEP;

  for (int r = outer_start; r < outer_end; r++) {
  

    for (int c = inner_start; c < cEnd; c+=CORE_STEP) {
      prefetch_vert_frame(a, r, c, inner_dim, 1, &sp);

      FRAME_START(REGION_SIZE);
      // not actually 14iters, but like 6
      #pragma GCC unroll(14)
      for (int i = 0; i < CORE_STEP; i++) {
        int sp0 = sp + i;
        int sp1 = sp0 + LOAD_DEPTH;
        int sp2 = sp1 + LOAD_DEPTH;
        DTYPE out = CONV_3x3(
          sp_ptr[sp0 + 0], sp_ptr[sp0 + 1], sp_ptr[sp0 + 2],
          sp_ptr[sp1 + 0], sp_ptr[sp1 + 1], sp_ptr[sp1 + 2],
          sp_ptr[sp2 + 0], sp_ptr[sp2 + 1], sp_ptr[sp2 + 2]
        );
        FSTORE_NOACK(out, &b[r*NJ + c + i], 0);
      }
      END_FRAME();
      sp += REGION_SIZE;
      sp = sp % POST_REGION_WORD; 
    }

    // TODO not clear how much this matters
    // do rest that doesnt map into prefetch pattern
    #pragma GCC unroll(16)
    for (int c = cEnd; c < NJ-1; c++) {
      int i = r;
      int j = c;
      DTYPE out = CONV_3x3(
        a[(i - 1)*NJ + (j - 1)], a[(i - 1)*NJ + (j + 0)], a[(i - 1)*NJ + (j + 1)],
        a[(i + 0)*NJ + (j - 1)], a[(i + 0)*NJ + (j + 0)], a[(i + 0)*NJ + (j + 1)],
        a[(i + 1)*NJ + (j - 1)], a[(i + 1)*NJ + (j + 0)], a[(i + 1)*NJ + (j + 1)]
      );
      STORE_NOACK(out, &b[i*NJ + j], 0);
    }
  }
  #else
  for (int i = outer_start; i < outer_end; i++) {
    #pragma GCC unroll(16)
    for (int j = inner_start; j < NJ - 1; j++) {
      // printf("%d->%d\n", inner_start, NJ-1);
      // TODO order in gpu version is outer dim first for some reason,
      // better to access in order below
      DTYPE out = CONV_3x3(
        a[(i - 1)*NJ + (j - 1)], a[(i - 1)*NJ + (j + 0)], a[(i - 1)*NJ + (j + 1)],
        a[(i + 0)*NJ + (j - 1)], a[(i + 0)*NJ + (j + 0)], a[(i + 0)*NJ + (j + 1)],
        a[(i + 1)*NJ + (j - 1)], a[(i + 1)*NJ + (j + 0)], a[(i + 1)*NJ + (j + 1)]
      );
      FSTORE_NOACK(out, &b[i*NJ + j], 0);
    }
  }
  #endif
  FENCE();
}


void __attribute__((optimize("-fno-inline"))) conv2d(
    DTYPE *a, DTYPE *b,
    int ptid, int pdim, int vtid, 
    int NI, int NJ, int start, int end, int mapped_len,
    int mask, int used, DTYPE *p_sp_ptr, DTYPE *n_sp_ptr 
  ) {

  #ifdef USE_VEC
  // do computation that we can map
  if (used)
    tril_conv2d(mask, a, b, start, end, NJ, mapped_len, 
      ptid, vtid, p_sp_ptr, n_sp_ptr);

  // do remainder of computation starting from offset
  conv2d_manycore(a, b, NI, NJ, mapped_len + 1, ptid, pdim);
  #else
  conv2d_manycore(a, b, NI, NJ, 1, ptid, pdim);
  #endif


}


void __attribute__((optimize("-freorder-blocks-algorithm=simple"))) kernel(
    DTYPE *a, DTYPE *b, int NI, int NJ,
    int ptid_x, int ptid_y, int pdim_x, int pdim_y) {
  
  // start recording all stats (all cores)
  if (ptid_x == 0 && ptid_y == 0) {
    stats_on();
  }

  #if VECTOR_LEN==4
  SET_USEFUL_VARIABLES_V4(ptid_x, ptid_y, pdim_x, pdim_y);
  #elif VECTOR_LEN==16
  SET_USEFUL_VARIABLES_V16(ptid_x, ptid_y, pdim_x, pdim_y);
  #else
  SET_USEFUL_VARIABLES_MANYCORE(ptid_x, ptid_y, pdim_x, pdim_y);
  #endif

  int start = 0;
  int end = 0;
  #ifdef USE_VEC
  if (used) {
    start = 1 + ( (unique_id + 0) * (NI-2) ) / total_groups;
    end   = 1 + ( (unique_id + 1) * (NI-2) ) / total_groups;
  }
  #endif

  #ifdef NUM_REGIONS
  SET_PREFETCH_MASK(NUM_REGIONS, REGION_SIZE, &start_barrier);
  #endif

  // each vector group size is rated to do a certain problem size and multiples of that problem size
  // for the mod of this we need to do the rest on the flexible manycore version
  int rated_size = 0;
  #ifdef USE_VEC
  rated_size = C_STRIDE;
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
  // TODO get snaking pattern from the reduction tried to do on syrk
  DTYPE *p_sp_ptr = NULL;
  DTYPE *n_sp_ptr = NULL;
  #ifdef LONGLINES
  // calculate prev and next spAddr for reuse
  if (vtid != 0) {
    if (vtid_x == 0) 
      p_sp_ptr = (DTYPE*)getSpAddr(ptid - (GRID_XDIM - (vdim_x - 1)), 0);
    else
      p_sp_ptr = (DTYPE*)getSpAddr(ptid - 1, 0);

    // set offset
    p_sp_ptr += 3;
  }
  else {
    // loopback to self (set as next)
    p_sp_ptr = (DTYPE*)getSpAddr(ptid, 0);
    p_sp_ptr += 0;
  }
  if (vtid != vdim - 1) {
    if (vtid_x == vdim_x - 1)
      n_sp_ptr = (DTYPE*)getSpAddr(ptid + (GRID_XDIM - (vdim_x - 1)), 0);
    else 
      n_sp_ptr = (DTYPE*)getSpAddr(ptid + 1, 0);

    // set offset 
    n_sp_ptr += 0;
  }
  else {
    // loopback to self (set as prev)
    n_sp_ptr = (DTYPE*)getSpAddr(ptid, 0);
    n_sp_ptr += 3;
  }
  #endif


  // printf("%d %xd\n", mapped_len, unmapped_len);

  // move stack onto scratchpad for faster local access than default on DRAM
  MOVE_STACK_ONTO_SCRATCHPAD();

  conv2d(a, b, ptid, pdim, vtid, NI, NJ, start, end, mapped_len, mask, used,
    p_sp_ptr, n_sp_ptr 
  );

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

  // reset scratchpad config
  SET_PREFETCH_MASK(0, 0, &start_barrier);

  if (a->tid_x == 0 && a->tid_y == 0) {
    stats_off();
  }

  return NULL;
}
