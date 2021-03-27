#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <math.h>

#include "pthread_launch.h"
#include "syr2k.h"
#include "spad.h"
#include "bind_defs.h"
#include "group_templates.h"
#include "syr2k_kernel.h"
#include "util.h"

#if defined(PACKED_SIMD) || defined(NESTED_SIMD)
#include <riscv_vector.h>
#endif

/*
  syrk kernel
*/

/*-----------------------------------------------------------------------------------
 * Manycore. Using PolyBench GPU parallelization strategy. No scratchpad use
 *---------------------------------------------------------------------------------*/

// compute s by parallezing the outerloop around reduction (reductions done within a single core)
void syr2k_manycore_baseline(DTYPE *a, DTYPE *b, DTYPE *c, int N, int M, int tid, int dim) {
  // could parallize over two dimensions. thats what gpu version does
  // just do 1d so easier
  int start = ((tid + 0) * N) / dim;
  int end   = ((tid + 1) * N) / dim;
  
  int sp = 0;
  DTYPE* sp_ptr = (DTYPE*)getSpAddr(tid, 0);

  for (int i = start; i < end; i++) {
    for (int j = 0; j < M; j++) {
      // TODO not prefetching outframe but should be negligable
      DTYPE c_ij = c[i * N + j] * beta;
      // c[i * N + j] *= beta;

      #ifdef PACKED_SIMD

      int chunk = M;
      for (size_t l; (l = vsetvl_e32m1(chunk)) > 0; chunk -= l) {
        l = vsetvl_e32m1(chunk);

        int base_k = M - chunk;

        vfloat32m1_t vai = vle32_v_f32m1(&a[i * M + base_k]);
        vfloat32m1_t vaj = vle32_v_f32m1(&a[j * M + base_k]);

        vfloat32m1_t vbi = vle32_v_f32m1(&b[i * M + base_k]);
        vfloat32m1_t vbj = vle32_v_f32m1(&b[j * M + base_k]);

        vfloat32m1_t vac = vfmul_vv_f32m1(vai, vbj);
        vac = vfmul_vf_f32m1(vac, alpha);

        vfloat32m1_t vbc = vfmul_vv_f32m1(vbi, vaj);
        vbc = vfmul_vf_f32m1(vbc, alpha);

        vfloat32m1_t vcij = vfadd_vv_f32m1(vac, vbc);

        // sum
        vfloat32m1_t vzero = vfmv_v_f_f32m1(0.0f); // splat 0
        vcij = vfredsum_vs_f32m1_f32m1(vcij, vcij, vzero);

        // update the accumulation
        c_ij += vfmv_f_s_f32m1_f32(vcij);
      }

      #elif defined(MANYCORE_PREFETCH)
      for (int k = 0; k < M; k+=INNER_PREFETCH_LEN) {
        prefetch_inner_frame(a, b, i, j, k, &sp, M);

        FRAME_START(INNER_FRAME_SIZE);
        #pragma GCC unroll(16)
        for (int kin = 0; kin < INNER_PREFETCH_LEN; kin++) {
          c_ij += alpha * sp_ptr[sp + INNER_PREFETCH_LEN*0 + kin] * 
                          sp_ptr[sp + INNER_PREFETCH_LEN*3 + kin] + 
                  alpha * sp_ptr[sp + INNER_PREFETCH_LEN*2 + kin] *
                          sp_ptr[sp + INNER_PREFETCH_LEN*1 + kin];
        }
        END_FRAME();

        sp += INNER_FRAME_SIZE;
        sp = sp % POST_FRAME_WORD;
      }
      #else
      #pragma GCC unroll(16)
      for (int k = 0; k < M; k++) {
        // c[i * N + j] += ALPHA * a[i * M + k] * b[j * M + k] + ALPHA * b[i * M + k] * a[j * M + k];
        c_ij += alpha * a[i * M + k] * b[j * M + k] + alpha * b[i * M + k] * a[j * M + k];
      }
      #endif
      FSTORE_NOACK(c_ij, &c[i * N + j], 0);
    }
  }
  asm volatile("fence\n\t");
}

#ifdef LONGLINES
// partial sum reduction offload
void mailer(DTYPE *c, int baseGroupId, int numGroups, int N, int M, 
    int ptid, int *fwders) {
  // chunk over vector groups. note all might not do the same amount of work
  int max_chunk_size = ceilToInt((float)N / (float)numGroups);

  // cache sp ptrs to avoid global load
  SETUP_REDUCTION_CORE(fwders, ptid);

  for (int cnt = 0; cnt < max_chunk_size; cnt++) {

    SETUP_GROUP_ITERATION_CHUNKED(baseGroupId, numGroups, cnt);

    for (int j = 0; j < M; j+=J_STRIDE*ACCUM_GRANULARITY) {

      // prefetch c into frame along with partial sums
      for (int g = 0; g < NUM_GROUPS_PER_PIPE; g++) {
        int i = group_start[g];
        if (i < 0) continue;
        for (int a = 0; a < ACCUM_GRANULARITY; a++) {
          VPREFETCH_L(sp_self + g + a * SUB_FRAME_SIZE, &c[i * N + j + a], 0, 1, TO_SELF);
        }
      }

      REDUCE_SYNC_WITH_SCALAR(group_start, spPtrs, flat_iter);

      // wait for frame and then do sum
      FRAME_START(expected_elements);

      for (int g = 0; g < NUM_GROUPS_PER_PIPE; g++) {

        #pragma GCC unroll(4)
        for (int a = 0; a < ACCUM_GRANULARITY; a++) {
          DTYPE sum = sp_ptr[sp_self + g + a * SUB_FRAME_SIZE] * beta;

          int sum_offset = MAILER_OFFSET + g * PER_CORE_MAILER_FRAME + a * SUB_FRAME_SIZE;

          #pragma GCC unroll(16)
          for (int k = 0; k < PER_CORE_MAILER_FRAME; k++) {
            sum += sp_ptr[sum_offset + sp_self + k];
          }

          int i = group_start[g];
          if (i < 0) continue;

          FSTORE_NOACK(sum, &c[i * N + j + a], 0);
        }

      }
      sp_self += MAILER_FRAME_SIZE;
      sp_self = sp_self % MAILER_POST_FRAME_WORD; // TOOD branch better??
      REMEM(expected_elements);
    }
  }
}
#endif

/*-----------------------------------------------------------------------------------
 * Staging
 *---------------------------------------------------------------------------------*/

void __attribute__((optimize("-fno-inline"))) syr2k(
    DTYPE *a, DTYPE *b, DTYPE *c,
    int ptid, int vtid, int dim, int N, int M, int groupId, int numGroups,
    int mask, int used, int ptidMailer, int isMailer, int *ptidFwder, int linkId
  ) {

    #ifndef USE_VEC
    syr2k_manycore_baseline(a, b, c, N, M, ptid, dim);
    #else
    if (used)
      tril_syr2k(mask, a, b, c, N, M, ptid, groupId, numGroups, vtid, 
        ptidMailer, linkId);
    #ifdef LONGLINES
    else if (isMailer)
      mailer(c, groupId, numGroups, N, M, ptid, ptidFwder);
    #endif
    #endif

}

void __attribute__((optimize("-freorder-blocks-algorithm=simple"))) kernel(
    DTYPE *a, DTYPE *b, DTYPE *c, int N, int M,
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

  #ifdef LONGLINES
  SETUP_REDUCE_CONFIG();
  #else
  SETUP_REDUCE_CONFIG_NULL();
  #endif

  // need to set vlen here so doesn't cause squash in vector core on change in value
  #ifdef NESTED_SIMD
  vsetvl_e32m1(NESTED_SIMD_VLEN);
  #endif

  #ifdef NUM_FRAMES
  // int mask = getDebugMask(&cinfo);
  if (isMailer) {
    SET_PREFETCH_MASK(MAILER_NUM_FRAMES, MAILER_FRAME_SIZE, &start_barrier); 
  }
  else { 
    SET_PREFETCH_MASK(NUM_FRAMES, INNER_FRAME_SIZE, &start_barrier);
  }
  #endif

  MOVE_STACK_ONTO_SCRATCHPAD();

  // do the kernel
  syr2k(a, b, c, ptid, vtid, pdim, N, M, unique_id, total_groups, mask, used,
    ptidMailer, isMailer, ptidFwders, linkId);

  // restore stack pointer
  RECOVER_DRAM_STACK();

}


// helper functions
Kern_Args *construct_args(DTYPE *a, DTYPE *b, DTYPE *c, int N, int M,
  int tid_x, int tid_y, int dim_x, int dim_y) {

  Kern_Args *args = (Kern_Args*)malloc(sizeof(Kern_Args));
  
  args->a = a;
  args->b = b;
  args->c = c;
  args->N = N;
  args->M = M;
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
  
  kernel(a->a, a->b, a->c, a->N, a->M,
      a->tid_x, a->tid_y, a->dim_x, a->dim_y);

  // reset scratchpad config
  SET_PREFETCH_MASK(0, 0, &start_barrier);

  if (a->tid_x == 0 && a->tid_y == 0) {
    stats_off();
  }

  return NULL;
}
