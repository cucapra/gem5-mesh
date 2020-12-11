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

#ifdef PACKED_SIMD
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

        FRAME_START();
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
      STORE_NOACK(c_ij, &c[i * N + j], 0);
    }
  }
  asm volatile("fence\n\t");
}

/*-----------------------------------------------------------------------------------
 * Staging
 *---------------------------------------------------------------------------------*/

void __attribute__((optimize("-fno-inline"))) syr2k(
    DTYPE *a, DTYPE *b, DTYPE *c,
    int ptid, int vtid, int dim, int N, int M, int groupId, int numGroups,
    int mask, int used
  ) {

    #ifndef USE_VEC
    syr2k_manycore_baseline(a, b, c, N, M, ptid, dim);
    #else
    if (used)
      tril_syr2k(mask, a, b, c, N, M, ptid, groupId, numGroups, vtid);
    #endif

}

void __attribute__((optimize("-freorder-blocks-algorithm=simple"))) kernel(
    DTYPE *a, DTYPE *b, DTYPE *c, int N, int M,
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
  int orig_x = 0;
  int orig_y = 0;
  int is_da  = 0;
  int master_x = 0;
  int master_y = 0;
  int unique_id = 0;
  int total_groups = 0;
  int used = 0;

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
  orig_x = cinfo.orig_x;
  orig_y = cinfo.orig_y;
  is_da  = cinfo.is_scalar;
  master_x = cinfo.master_x;
  master_y = cinfo.master_y;
  unique_id = cinfo.unique_id;
  total_groups = cinfo.total_groups;
  used = cinfo.used;

  // printf("ptid %d(%d,%d) da %d vtid %d(%d,%d) dim %d(%d,%d) %d->%d used? %d\n", ptid, ptid_x, ptid_y, is_da, vtid, vtid_x, vtid_y, 4, vdim_x, vdim_y, start, end, used);

  #elif !defined(USE_VEC)

  vdim_x = 1;
  vdim_y = 1;
  vtid_x = 0;
  vtid_y = 0;
  vtid   = 0;
  used   = 1;

  #endif

  // linearize some fields
  vdim = vdim_x * vdim_y;

  // get behavior of each core
  #ifdef NUM_FRAMES
  // setup up self prefetch
  #ifdef MANYCORE_PREFETCH
  core_config_info_t cinfo = manycore_template(ptid_x, ptid_y, pdim_x, pdim_y);
  int mask = getDebugMask(&cinfo);
  VECTOR_EPOCH(mask);
  #else
  int mask = getSIMDMask(&cinfo);
  #endif
  // int mask = getDebugMask(&cinfo);
  SET_PREFETCH_MASK(NUM_FRAMES, INNER_FRAME_SIZE, &start_barrier);
  #else
  int mask = 0;
  #endif

  MOVE_STACK_ONTO_SCRATCHPAD();

  // do the kernel
  syr2k(a, b, c, ptid, vtid, pdim, N, M, unique_id, total_groups, mask, used);

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

  pthread_barrier_wait(&start_barrier);

  if (a->tid_x == 0 && a->tid_y == 0) {
    stats_off();
  }

  return NULL;
}
