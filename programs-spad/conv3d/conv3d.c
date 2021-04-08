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
  // construct vector to help generate ending mask
  vsetvl_e32m1(HARDWARE_VECTOR_LEN);
  vint32m1_t cresendo = vmv_v_x_i32m1(0.0f);
  #pragma GCC unroll(16)
  for (int i = 0; i < PER_CORE_SIMD_LEN; i++) {
    cresendo = vslide1down_vx_i32m1(cresendo, i);
  }
  #endif

  #if defined(MANYCORE_PREFETCH)
  int sp = 0;
  DTYPE* sp_ptr = (DTYPE*)getSpAddr(ptid, 0);
  for (int i = outer_start; i < outer_end; i++) {
    for (int j = 1; j < NJ - 1; j++) {
      for (int k = 1; k < NK - 1; k+=UNROLL_LEN) {
        prefetch_horiz_frame(a, i, j, k, NJ, NK, &sp);

        FRAME_START(REGION_SIZE);
        // unroll doesn't seem to help here
        // #pragma GCC unroll(8)
        for (int u = 0; u < UNROLL_LEN; u+=PER_CORE_SIMD_LEN) {
          #ifdef USE_AUDIT1
          #ifdef PER_CORE_SIMD
          #error per core simd not supported for this config
          #endif
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
          #elif defined(USE_AUDIT2)
          
          #ifdef PER_CORE_SIMD
          vsetvl_e32m1(HARDWARE_VECTOR_LEN);
          int ul = UNROLL_LEN;
          int ml = UNROLL_LEN + 2;
          // check if exceeds k
          vbool32_t bmask2 = vmslt_vx_i32m1_b32(cresendo, (NK - 1) - (k + u));
          // check if exceeds the unroll amount
          vbool32_t bmask1 = vmslt_vx_i32m1_b32(cresendo, UNROLL_LEN - u);
          vbool32_t bmask = vmand_mm_b32(bmask1, bmask2);

          vfloat32m1_t a111 = vle32_v_f32m1(&sp_ptr[sp + 0*ul + u]);
          vfloat32m1_t a113 = vle32_v_f32m1(&sp_ptr[sp + 0*ul + u + 2]);
          vfloat32m1_t a123 = vle32_v_f32m1(&sp_ptr[sp + 1*ml + u]);
          vfloat32m1_t a133 = vle32_v_f32m1(&sp_ptr[sp + 1*ul+1*ml + u]);
          vfloat32m1_t a212 = vle32_v_f32m1(&sp_ptr[sp + 2*ul+1*ml + u]);
          vfloat32m1_t a222 = vle32_v_f32m1(&sp_ptr[sp + 3*ul+1*ml + u]);
          vfloat32m1_t a232 = vle32_v_f32m1(&sp_ptr[sp + 4*ul+1*ml + u]);
          vfloat32m1_t a311 = vle32_v_f32m1(&sp_ptr[sp + 5*ul+1*ml + u]);
          vfloat32m1_t a313 = vle32_v_f32m1(&sp_ptr[sp + 5*ul+1*ml + u + 2]);
          vfloat32m1_t a323 = vle32_v_f32m1(&sp_ptr[sp + 5*ul+2*ml + u]);
          vfloat32m1_t a333 = vle32_v_f32m1(&sp_ptr[sp + 6*ul+2*ml + u]);
      
          VCONV_15(a111, a113, a123, a133, a212, a222, a232, a311, a313, a323, a333);

          vse32_v_f32m1_m(bmask, &b[IDX(i, j, k + u, NJ, NK)], ofmap);
          #else
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
          #endif
          #else
          #ifdef PER_CORE_SIMD
          #error per core simd not supported for this config
          #endif
          DTYPE out = CONV_15(
            sp_ptr[sp + 0], sp_ptr[sp + 1], sp_ptr[sp + 2],
            sp_ptr[sp + 3], sp_ptr[sp + 4], sp_ptr[sp + 5],
            sp_ptr[sp + 6], sp_ptr[sp + 7], sp_ptr[sp + 8],
            sp_ptr[sp + 9], sp_ptr[sp + 10]
          );
          #endif

          #ifndef PER_CORE_SIMD
          FSTORE_NOACK(out, &b[IDX(i, j, k + u, NJ, NK)], 0);
          #endif
        }
        END_FRAME();
        sp += REGION_SIZE;
        if (sp == POST_REGION_WORD) sp = 0;
      }
    }
  }
  #elif defined(PER_CORE_SIMD)
  for (int i = outer_start; i < outer_end; i++) {
    for (int j = 1; j < NJ - 1; j++) {
      for (int k = 1; k < NK - 1; k+=HARDWARE_VECTOR_LEN) {

        int remLen = (NK-1) - k;
        vsetvl_e32m1(remLen);

        // stride is elementwise so this works? could reduce loads by doing shifts
        // afterwards, but more complicated than in conv2d and only saves 2/11 loads
        // so going to skip for now
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
    
        // do vector-scalar multiplications for each element and then add
        VCONV_15(a111, a113, a123, a133, a212, a222, a232, a311, a313, a323, a333);

        vse32_v_f32m1(&b[IDX(i, j, k, NJ, NK)], ofmap);
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

  #ifdef PER_CORE_SIMD
  vsetvl_e32m1(PER_CORE_SIMD_LEN);
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
