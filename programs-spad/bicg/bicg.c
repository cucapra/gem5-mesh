#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <math.h>

#include "pthread_launch.h"
#include "bicg.h"
#include "spad.h"
#include "bind_defs.h"
#include "group_templates.h"
#include "bicg_kernel.h"
#include "util.h"

#ifdef PER_CORE_SIMD
#include <riscv_vector.h>
#endif

/*
  big c
*/

/*-----------------------------------------------------------------------------------
 * Manycore. Using PolyBench GPU parallelization strategy. No scratchpad use
 *---------------------------------------------------------------------------------*/

// compute s by parallezing the outerloop around reduction (reductions done within a single core)
void compute_s_manycore_baseline(DTYPE *a, DTYPE *r, DTYPE *s, int NX, int NY, int tid, int dim) {

  // paralleize outer loop y
  int start = ((tid + 0) * NY) / dim;
  int end   = ((tid + 1) * NY) / dim;

  #ifdef MANYCORE_PREFETCH
  int sp = 0;
  DTYPE* sp_ptr = (DTYPE*)getSpAddr(tid, 0);
  #endif

  for (int j = start; j < end; j++) {
    // s[j] = 0.0f;
    DTYPE s_local = 0.0f;

    #if defined(MANYCORE_PREFETCH)

    #ifdef PER_CORE_SIMD
    vsetvl_e32m1(PER_CORE_SIMD_LEN);
    vfloat32m1_t vzero = vfmv_v_f_f32m1(0.0f);
    vfloat32m1_t accum = vzero;
    #endif

    for (int i = 0; i < NX; i+=Q_PREFETCH_LEN) {
      prefetch_s_frame(a, r, i, j, &sp, NY);
      #ifdef PER_CORE_SIMD
      vsetvl_e32m1(PER_CORE_SIMD_LEN);
      #endif
      FRAME_START(FRAME_SIZE);
      #pragma GCC unroll(16)
      for (int iin = 0; iin < Q_PREFETCH_LEN; iin+=PER_CORE_SIMD_LEN) {
        #ifdef PER_CORE_SIMD
        vfloat32m1_t va = vle32_v_f32m1(&sp_ptr[sp + iin]);
        vfloat32m1_t vr = vle32_v_f32m1(&sp_ptr[sp + Q_PREFETCH_LEN + iin]);
        vfloat32m1_t vs = vfmul_vv_f32m1(va, vr);
        accum = vfadd_vv_f32m1(accum, vs);
        #else
        s_local += sp_ptr[sp + iin] * sp_ptr[sp + Q_PREFETCH_LEN + iin];
        #endif
      }
      sp += FRAME_SIZE;
      sp = sp % POST_FRAME_WORD;
      END_FRAME();
    }

    #ifdef PER_CORE_SIMD
    vsetvl_e32m1(PER_CORE_SIMD_LEN);
    vfloat32m1_t vslocal = vfredsum_vs_f32m1_f32m1(accum, accum, vzero);
    s_local += vfmv_f_s_f32m1_f32(vslocal);
    #endif

    #elif defined(PER_CORE_SIMD)
    int chunk = NX;
    for (size_t l; (l = vsetvl_e32m1(chunk)) > 0; chunk -= l) {
      l = vsetvl_e32m1(chunk);

      int base_i = NX - chunk;

      // TODO should we try to do prefetching here??

      // strided load
      vfloat32m1_t va = vlse32_v_f32m1(&a[base_i * NY + j], NY * sizeof(float));

      // can vectorize load to r
      vfloat32m1_t vr = vle32_v_f32m1(&r[base_i]);

      // multiple together
      vfloat32m1_t vs = vfmul_vv_f32m1(va, vr);

      // sum
      vfloat32m1_t vzero = vfmv_v_f_f32m1(0.0f); // splat 0
      vs = vfredsum_vs_f32m1_f32m1(vs, vs, vzero);

      // update the accumulation
      float single_val = vfmv_f_s_f32m1_f32(vs);
      s_local += single_val;
    }
    #else
    #pragma GCC unroll(16)
    for (int i = 0; i < NX; i++) {
      // s[j] += a[i * NY + j] * r[i];
      s_local += a[i * NY + j] * r[i];
    }
    #endif
    // s[j] = s_local;
    FSTORE_NOACK(s_local, &s[j], 0);
  }

  asm volatile("fence\n\t");
}

// compute q by paralleization outerloop around reduction (reduction done within a single core)
// note loops are in the opposite order as in the previous kernel to allow for this strategy
void compute_q_manycore_baseline(DTYPE *a, DTYPE *p, DTYPE *q, int NX, int NY, int tid, int dim) {

  // paralleize outer loop x
  int start = ((tid + 0) * NX) / dim;
  int end   = ((tid + 1) * NX) / dim;

  #ifdef MANYCORE_PREFETCH
  int sp = 0;
  DTYPE* sp_ptr = (DTYPE*)getSpAddr(tid, 0);
  #endif

  for (int i = start; i < end; i++) {
    // q[i] = 0.0f;
    DTYPE q_local = 0.0f;

    #if defined(MANYCORE_PREFETCH)

    #ifdef PER_CORE_SIMD
    vsetvl_e32m1(PER_CORE_SIMD_LEN);
    vfloat32m1_t vzero = vfmv_v_f_f32m1(0.0f);
    vfloat32m1_t accum = vzero;
    #endif

    for (int j = 0; j < NY; j+=Q_PREFETCH_LEN) {
      prefetch_q_frame(a, p, i, j, &sp, NY);
      #ifdef PER_CORE_SIMD
      vsetvl_e32m1(PER_CORE_SIMD_LEN);
      #endif
      FRAME_START(FRAME_SIZE);
      #pragma GCC unroll(16)
      for (int jin = 0; jin < Q_PREFETCH_LEN; jin+=PER_CORE_SIMD_LEN) {
        #ifdef PER_CORE_SIMD
        vfloat32m1_t va = vle32_v_f32m1(&sp_ptr[sp + jin]);
        vfloat32m1_t vp = vle32_v_f32m1(&sp_ptr[sp + Q_PREFETCH_LEN + jin]);
        vfloat32m1_t vq = vfmul_vv_f32m1(va, vp);
        accum = vfadd_vv_f32m1(accum, vq);
        #else
        q_local += sp_ptr[sp + jin] * sp_ptr[sp + Q_PREFETCH_LEN + jin];
        #endif
      }
      sp += FRAME_SIZE;
      sp = sp % POST_FRAME_WORD;
      END_FRAME();
    }

    #ifdef PER_CORE_SIMD
    vsetvl_e32m1(PER_CORE_SIMD_LEN);
    vfloat32m1_t vqlocal = vfredsum_vs_f32m1_f32m1(accum, accum, vzero);
    q_local += vfmv_f_s_f32m1_f32(vqlocal);
    #endif

    #elif defined(PER_CORE_SIMD)
    int chunk = NY;
    for (size_t l; (l = vsetvl_e32m1(chunk)) > 0; chunk -= l) {
      l = vsetvl_e32m1(chunk);

      int base_j = NY - chunk;

      // TODO should we try to do prefetching here??

      // can vectorize load to a
      vfloat32m1_t va = vle32_v_f32m1(&a[i * NY + base_j]);

      // can vectorize load to p
      vfloat32m1_t vp = vle32_v_f32m1(&p[base_j]);

      // multiple together
      vfloat32m1_t vq = vfmul_vv_f32m1(va, vp);

      // sum
      vfloat32m1_t vzero = vfmv_v_f_f32m1(0.0f); // splat 0
      vq = vfredsum_vs_f32m1_f32m1(vq, vq, vzero);

      // update the accumulation
      float single_val = vfmv_f_s_f32m1_f32(vq);
      q_local += single_val;
    }
    #else
    #pragma GCC unroll(16)
    for (int j = 0; j < NY; j++) {
      // q[i] += a[i * NY + j] * p[j];
      q_local += a[i * NY + j] * p[j];
    }
    #endif
    // q[i] = q_local;
    FSTORE_NOACK(q_local, &q[i], 0);
  }

  asm volatile("fence\n\t");
}

/*-----------------------------------------------------------------------------------
 * Vector versions of the kernels.
 *---------------------------------------------------------------------------------*/

void __attribute__((optimize("-fno-inline"))) bicg(
    DTYPE *a, DTYPE *r, DTYPE *p, DTYPE *s, DTYPE *q, 
    int ptid, int vtid, int dim, int NX, int NY, int groupId, int numGroups,
    int mask, int used
  ) {
    #if defined(USE_VEC) || defined(MANYCORE_PREFETCH)
    SET_PREFETCH_MASK(NUM_FRAMES, FRAME_SIZE, &start_barrier);
    #endif

    #ifndef USE_VEC
    compute_s_manycore_baseline(a, r, s, NX, NY, ptid, dim);
    // don't need a barrier in between b/c s and q can be compute independently
    #ifdef MANYCORE_PREFETCH
    SET_PREFETCH_MASK(NUM_FRAMES, FRAME_SIZE, &start_barrier);
    #endif

    compute_q_manycore_baseline(a, p, q, NX, NY, ptid, dim);
    #else
    // if (groupId != 0) return;

    if (used)
      tril_bicg_s(mask, a, r, s, NX, NY, ptid, groupId, numGroups, vtid);

    // reset frames. technically don't have to do this is pass the last sp ptr between these two
    SET_PREFETCH_MASK(NUM_FRAMES, FRAME_SIZE, &start_barrier);

    if (used)
      tril_bicg_q(mask, a, p, q, NX, NY, ptid, groupId, numGroups, vtid);
    #endif

}

void __attribute__((optimize("-freorder-blocks-algorithm=simple"))) kernel(
    DTYPE *a, DTYPE *r, DTYPE *p, DTYPE *s, DTYPE *q,  int NX, int NY,
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
  #ifdef PER_CORE_SIMD
  vsetvl_e32m1(PER_CORE_SIMD_LEN);
  #endif

  MOVE_STACK_ONTO_SCRATCHPAD();

  // bicg
  bicg(a, r, p, s, q, ptid, vtid, pdim, NX, NY, unique_id, total_groups, mask, used);

  // restore stack pointer
  RECOVER_DRAM_STACK();

}


// helper functions
Kern_Args *construct_args(DTYPE *a, DTYPE *r, DTYPE *p, DTYPE *s, DTYPE *q, int NX, int NY,
  int tid_x, int tid_y, int dim_x, int dim_y) {

  Kern_Args *args = (Kern_Args*)malloc(sizeof(Kern_Args));
  
  args->a = a;
  args->r = r;
  args->p = p;
  args->s = s;
  args->q = q;
  args->NX = NX;
  args->NY  = NY;
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
  
  kernel(a->a, a->r, a->p, a->s, a->q, a->NX, a->NY,
      a->tid_x, a->tid_y, a->dim_x, a->dim_y);

  // reset scratchpad config
  SET_PREFETCH_MASK(0, 0, &start_barrier);

  if (a->tid_x == 0 && a->tid_y == 0) {
    stats_off();
  }

  return NULL;
}
