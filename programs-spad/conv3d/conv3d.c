#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

#include "pthread_launch.h"
#include "conv3d.h"
#include "spad.h"
#include "bind_defs.h"
#include "conv3d_kernel.h"
#include "util.h"

#ifdef PER_CORE_SIMD
#include <riscv_vector.h>
#endif

/*
  Conv3d. weird kernel
*/

void conv3d_manycore(DTYPE *a, DTYPE *b, int NI, int NJ, int NK, int ptid, int pdim) {

  int eff_outer_dim = NI - (FILTER_DIM-1);
  int outer_start  = 1 + ( ( ptid + 0 ) * eff_outer_dim ) / pdim;
  int outer_end    = 1 + ( ( ptid + 1 ) * eff_outer_dim ) / pdim;

  DEF_WEIGHTS();

  #ifdef PER_CORE_SIMD
  for (int i = outer_start; i < outer_end; i++) {
    for (int j = 1; j < NJ - 1; j++) {
      for (int k = 1; k < NK - 1; k+=16) {

        int remLen = (NK-1) - k;
        vsetvl_e32m1(remLen);

        vfloat32m1_t a111 = vle32_v_f32m1(a + IDX(i-1, j-1, k-1, NJ, NK));
        vfloat32m1_t a113 = vle32_v_f32m1(a + IDX(i-1, j-1, k+1, NJ, NK));
        vfloat32m1_t a123 = vle32_v_f32m1(a + IDX(i-1, j+0, k+1, NJ, NK));
        vfloat32m1_t a133 = vle32_v_f32m1(a + IDX(i-1, j+1, k+1, NJ, NK));
        vfloat32m1_t a212 = vle32_v_f32m1(a + IDX(i+0, j-1, k+0, NJ, NK));
        vfloat32m1_t a222 = vle32_v_f32m1(a + IDX(i+0, j+0, k+0, NJ, NK));
        vfloat32m1_t a232 = vle32_v_f32m1(a + IDX(i+0, j+1, k+0, NJ, NK));
        vfloat32m1_t a311 = vle32_v_f32m1(a + IDX(i+1, j-1, k-1, NJ, NK));
        vfloat32m1_t a313 = vle32_v_f32m1(a + IDX(i+1, j-1, k+1, NJ, NK));
        vfloat32m1_t a323 = vle32_v_f32m1(a + IDX(i+1, j+0, k+1, NJ, NK));
        vfloat32m1_t a333 = vle32_v_f32m1(a + IDX(i+1, j+1, k+1, NJ, NK));
    
        // do vector-scalar multiplications for each element
        // stride is elementwise so this works? could reduce loads by doing shifts
        // afterwards, but more complicated than in conv2d and only saves 2/11 loads
        // so going to skip for now
        vfloat32m1_t b111_11, b111_21, b111_31, b113_11, b123_21, b133_31,
          b212_12, b222_22, b232_32, b311_13, b311_23, b311_33, b313_13,
          b323_23, b333_33;

        b111_11 = vfmul_vf_f32m1(a111, c11);
        b111_21 = vfmul_vf_f32m1(a111, c21);
        b111_31 = vfmul_vf_f32m1(a111, c31);
        b113_11 = vfmul_vf_f32m1(a113, c11);
        b123_21 = vfmul_vf_f32m1(a123, c21);
        b133_31 = vfmul_vf_f32m1(a133, c31);
        b212_12 = vfmul_vf_f32m1(a212, c12);
        b222_22 = vfmul_vf_f32m1(a222, c22);
        b232_32 = vfmul_vf_f32m1(a232, c32);
        b311_13 = vfmul_vf_f32m1(a311, c13);
        b311_23 = vfmul_vf_f32m1(a311, c23);
        b311_33 = vfmul_vf_f32m1(a311, c33);
        b313_13 = vfmul_vf_f32m1(a313, c13);
        b323_23 = vfmul_vf_f32m1(a323, c23);
        b333_33 = vfmul_vf_f32m1(a333, c33);

        // add every vector together
        vfloat32m1_t ofmap = vfadd_vv_f32m1(b111_11, b111_21);
        ofmap = vfadd_vv_f32m1(ofmap, b111_31);
        ofmap = vfadd_vv_f32m1(ofmap, b113_11);
        ofmap = vfadd_vv_f32m1(ofmap, b123_21);
        ofmap = vfadd_vv_f32m1(ofmap, b133_31);
        ofmap = vfadd_vv_f32m1(ofmap, b212_12);
        ofmap = vfadd_vv_f32m1(ofmap, b222_22);
        ofmap = vfadd_vv_f32m1(ofmap, b232_32);
        ofmap = vfadd_vv_f32m1(ofmap, b311_13);
        ofmap = vfadd_vv_f32m1(ofmap, b311_23);
        ofmap = vfadd_vv_f32m1(ofmap, b311_33);
        ofmap = vfadd_vv_f32m1(ofmap, b313_13);
        ofmap = vfadd_vv_f32m1(ofmap, b323_23);
        ofmap = vfadd_vv_f32m1(ofmap, b333_33);

        vse32_v_f32m1(&b[IDX(i, j, k, NJ, NK)], ofmap);
      }
    }
  }


  #elif defined(MANYCORE_PREFETCH)
  int sp = 0;
  DTYPE* sp_ptr = (DTYPE*)getSpAddr(ptid, 0);
  for (int i = outer_start; i < outer_end; i++) {
    for (int j = 1; j < NJ - 1; j++) {
      for (int k = 1; k < NK - 1; k+=UNROLL_LEN) {
        prefetch_horiz_frame(a, i, j, k, NJ, NK, &sp);

        FRAME_START(REGION_SIZE);
        // unroll doesn't seem to help here
        // #pragma GCC unroll(8)
        for (int u = 0; u < UNROLL_LEN; u++) {
          #ifdef AUDIT
          if (k + u >= NK - 1) break;
          int ul = UNROLL_LEN;
          DTYPE out = CONV_15(
            sp_ptr[sp + 0*ul + u], 
            sp_ptr[sp + 1*ul + u], 
            sp_ptr[sp + 2*ul + u],
            sp_ptr[sp + 3*ul + u], 
            sp_ptr[sp + 4*ul + u],
            sp_ptr[sp + 5*ul + u],
            sp_ptr[sp + 6*ul + u], 
            sp_ptr[sp + 7*ul + u], 
            sp_ptr[sp + 8*ul + u],
            sp_ptr[sp + 9*ul + u], 
            sp_ptr[sp + 10*ul + u]
          );
          #elif defined(AUDIT2)
          if (k + u >= NK - 1) break;
          int ul = UNROLL_LEN;
          int ml = UNROLL_LEN + 2;
          DTYPE out = CONV_15(
            sp_ptr[sp + 0*ul + u], 
            sp_ptr[sp + 0*ul + u + 2], 
            sp_ptr[sp + 1*ml + u],
            sp_ptr[sp + 1*ul+1*ml + u], 
            sp_ptr[sp + 2*ul+1*ml + u],
            sp_ptr[sp + 3*ul+1*ml + u],
            sp_ptr[sp + 4*ul+1*ml + u], 
            sp_ptr[sp + 5*ul+1*ml + u], 
            sp_ptr[sp + 5*ul+1*ml + u + 2],
            sp_ptr[sp + 5*ul+2*ml + u], 
            sp_ptr[sp + 6*ul+2*ml + u]
          );
          #else
          DTYPE out = CONV_15(
            sp_ptr[sp + 0], sp_ptr[sp + 1], sp_ptr[sp + 2],
            sp_ptr[sp + 3], sp_ptr[sp + 4], sp_ptr[sp + 5],
            sp_ptr[sp + 6], sp_ptr[sp + 7], sp_ptr[sp + 8],
            sp_ptr[sp + 9], sp_ptr[sp + 10]
          );
          #endif
          FSTORE_NOACK(out, &b[IDX(i, j, k + u, NJ, NK)], 0);
        }
        END_FRAME();
        sp += REGION_SIZE;
        if (sp == POST_REGION_WORD) sp = 0;
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
        FSTORE_NOACK(out, &b[IDX(i, j, k, NJ, NK)], 0);
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

  #if VECTOR_LEN==4
  SET_USEFUL_VARIABLES_V4(ptid_x, ptid_y, pdim_x, pdim_y);
  #elif VECTOR_LEN==16
  SET_USEFUL_VARIABLES_V16(ptid_x, ptid_y, pdim_x, pdim_y);
  #else
  SET_USEFUL_VARIABLES_MANYCORE(ptid_x, ptid_y, pdim_x, pdim_y);
  #endif

  #ifdef USE_VEC
  int start = 0;
  int end = 0;
  if (used) {
    start = 1 + ( (unique_id + 0) * (NI-2) ) / total_groups;
    end   = 1 + ( (unique_id + 1) * (NI-2) ) / total_groups;
    // printf("%d->%d\n", start, end); 
  }
  #endif


  #ifdef NUM_REGIONS
  SET_PREFETCH_MASK(NUM_REGIONS, REGION_SIZE, &start_barrier);
  #endif

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

  // reset scratchpad config
  SET_PREFETCH_MASK(0, 0, &start_barrier);

  if (a->tid_x == 0 && a->tid_y == 0) {
    stats_off();
  }

  return NULL;
}
