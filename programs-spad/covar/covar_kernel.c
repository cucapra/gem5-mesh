#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <math.h>

#include "pthread_launch.h"
#include "covar.h"
#include "spad.h"
#include "bind_defs.h"
#include "group_templates.h"
#include "util.h"

#ifdef NESTED_SIMD
#include <riscv_vector.h>
#endif

// #define SCALAR_CORE
// #define VECTOR_CORE

/*-----------------------------------------------------------------------------------
 * Vector versions of the kernels.
 *---------------------------------------------------------------------------------*/

#ifdef USE_VEC
// compute each mean across each vector (single dimension)
void tril_mean(int mask, DTYPE *mean, DTYPE *data, int N, int M, 
    int ptid, int groupId, int numGroups, int vtid, int ptidMailer, int linkId) {

  #ifdef SCALAR_CORE
  VECTOR_EPOCH(mask);

  int start = ((groupId + 0) * N) / numGroups;
  int end   = ((groupId + 1) * N) / numGroups;

  #ifdef LONGLINES
  int numCompleted = 0;
  volatile int *sp_ptr = (int*)getSpAddr(ptid, 0);
  #else
  // make it a factor of vector group mapping size
  start = roundUp(start, VECTOR_LEN);
  end   = roundUp(end  , VECTOR_LEN);
  #endif

  int startOffset = min(INIT_MEAN_OFFSET, N);
  int sp  = 0;

  ISSUE_VINST(init_label);
  #endif

  #ifdef VECTOR_CORE
  asm("trillium vissue_delim until_next vector_init");
  int start = ((groupId + 0) * N) / numGroups;
  #ifdef LONGLINES
  int sp_origin = (linkId * PER_CORE_MAILER_FRAME) + vtid;
  DTYPE* sp_origin_ptr = (DTYPE*)getSpAddr(ptidMailer, 0);
  #else
  start = roundUp(start, VECTOR_LEN);
  #endif
  int i = start + vtid;
  int j = 0;
  DTYPE mean_i = 0.0f;
  int sp = 0;
  DTYPE *sp_ptr = (DTYPE*)getSpAddr(ptid, 0);

  #ifdef NESTED_SIMD
  vsetvl_e32m1(NESTED_SIMD_VLEN);
  vfloat32m1_t vzero = vfmv_v_f_f32m1(0.0f); // splat 0
  #endif

  #endif

  #ifdef SCALAR_CORE

  #ifdef LONGLINES


  for (int i = start; i < end; i++) {

    ISSUE_VINST(vec_body_init_label);

    // initial round
    for (int j = 0; j < startOffset; j+=MEAN_J_STRIDE) {
      prefetch_mean_frame(data, i, j, &sp, M);
    }

    // steady state
    for (int j = startOffset; j < N; j+=MEAN_J_STRIDE) {
      prefetch_mean_frame(data, i, j, &sp, M);
      ISSUE_VINST(mean_body_label);
    }

    //cooldown
    for (int j = N - startOffset; j < N; j+=MEAN_J_STRIDE) {
      ISSUE_VINST(mean_body_label);
    }

    SCALAR_SYNC_WITH_REDUCTION(sp_ptr, numCompleted);

    ISSUE_VINST(vec_body_end_label);
  }

  #else

  for (int i = start; i < end; i+=VECTOR_LEN) {

    ISSUE_VINST(vec_body_init_label);

    // initial round
    for (int j = 0; j < startOffset; j+=MEAN_J_STRIDE) {
      // printf("bpf %d %d\n", i, j);
      prefetch_mean_frame(data, i, j, &sp, M);
    }

    // steady state
    for (int j = startOffset; j < N; j+=MEAN_J_STRIDE) {
      // printf("mpf %d %d\n", i, j);
      prefetch_mean_frame(data, i, j, &sp, M);
      ISSUE_VINST(mean_body_label);
    }

    //cooldown
    for (int j = N - startOffset; j < N; j+=MEAN_J_STRIDE) {
      // printf("epf %d %d\n", i, j);
      ISSUE_VINST(mean_body_label);
    }

    ISSUE_VINST(center_begin_label);


    // initial round
    for (int j = 0; j < startOffset; j+=MEAN_J_STRIDE) {
      prefetch_mean_frame(data, i, j, &sp, M);
    }

    // steady state
    for (int j = startOffset; j < N; j+=MEAN_J_STRIDE) {
      prefetch_mean_frame(data, i, j, &sp, M);
      ISSUE_VINST(center_body_label);
    }

    //cooldown
    for (int j = N - startOffset; j < N; j+=MEAN_J_STRIDE) {
      ISSUE_VINST(center_body_label);
    }

    ISSUE_VINST(vec_body_end_label);

    
  }
  #endif
  #endif

  #ifdef VECTOR_CORE
  volatile int BH;
  volatile int BHO;
  do {

    asm("trillium vissue_delim until_next vec_body_init");
    #ifdef NESTED_SIMD
    // NOTE MUST NEVER CHANGE THIS VALUE B/C CANT SQUASH IN VCORES!!!
    size_t l = vsetvl_e32m1(NESTED_SIMD_VLEN);
    #endif

    do {
      asm("trillium vissue_delim if_begin mean_body");

      #ifdef NESTED_SIMD
      l = vsetvl_e32m1(NESTED_SIMD_VLEN);
      vfloat32m1_t accum = vzero;
      #endif

      FRAME_START(MEAN_FRAME_SIZE);
      #pragma GCC unroll(16)
      for (int u = 0; u < MEAN_PREFETCH_LEN; u+=NESTED_SIMD_VLEN) {
        #ifdef NESTED_SIMD
        vfloat32m1_t vmean = vle32_v_f32m1(&sp_ptr[sp + u]);
        accum = vfadd_vv_f32m1(accum, vmean);
        #else
        mean_i += sp_ptr[sp + u];
        #endif
      }

      #ifdef NESTED_SIMD
      vfloat32m1_t vmean_partial = vfredsum_vs_f32m1_f32m1(accum, accum, vzero);
      mean_i += vfmv_f_s_f32m1_f32(vmean_partial);
      #endif

      sp+=MEAN_FRAME_SIZE;
      sp = sp % POST_FRAME_WORD;
      END_FRAME();
      // #if VECTOR_LEN==16
      // #pragma GCC unroll(16)
      // for (int n = 0; n < 1; n++) {
      //   asm volatile("nop\n\t");
      // }
      // #endif
      asm("trillium vissue_delim end at_jump");
    } while(BH);

    #ifndef LONGLINES
    asm("trillium vissue_delim until_next center_begin");
    mean_i /= (DTYPE)FLOAT_N;
    FSTORE_NOACK(mean_i, &mean[i], 0);

    do {
      asm("trillium vissue_delim if_begin center_body");
      FRAME_START(MEAN_FRAME_SIZE);
      #pragma GCC unroll(16)
      for (int u = 0; u < MEAN_PREFETCH_LEN; u++) {
        DTYPE dat = sp_ptr[sp + u] - mean_i;
        FSTORE_NOACK(dat, &data[i * N + j + u], 0);
      }
      j+=MEAN_PREFETCH_LEN;
      sp += MEAN_FRAME_SIZE;
      sp = sp % POST_FRAME_WORD;
      END_FRAME();
      asm("trillium vissue_delim end at_jump");
    } while(BH);
    #endif

    asm("trillium vissue_delim if_begin vec_body_end");
    #ifdef LONGLINES
    FSTORE_NOACK(mean_i, &sp_origin_ptr[sp_origin], 0);
    sp_origin+=SUB_FRAME_SIZE;
    sp_origin = sp_origin % MAILER_POST_FRAME_WORD;
    #else
    i+=VECTOR_LEN;
    j = 0;
    #endif
    asm volatile("fmv.s.x %[creg],zero\n\t" : [creg] "=f" (mean_i));
    asm("trillium vissue_delim end at_jump");

  } while (BHO);
  #endif


  // Clean up on the vector cores.
#ifdef SCALAR_CORE
  ISSUE_VINST(vector_return_label);
#elif defined VECTOR_CORE
  asm("trillium vissue_delim return vector_return");
  return;
#endif

#ifdef SCALAR_CORE
  // devec with unique tag
  DEVEC(devec_0);

  // we are doing lazy store acks, so use this to make sure all stores have commited to memory
  asm volatile("fence\n\t");
  asm("trillium vissue_delim return scalar_return");  // XXX is this real???
  return;
#endif

  // Glue points!
#ifdef SCALAR_CORE
init_label:
  asm("trillium glue_point vector_init");
vec_body_init_label:
  asm("trillium glue_point vec_body_init");
mean_body_label:
  asm("trillium glue_point mean_body");
#ifndef LONGLINES
center_begin_label:
  asm("trillium glue_point center_begin");
center_body_label:
  asm("trillium glue_point center_body");
#endif
vec_body_end_label:
  asm("trillium glue_point vec_body_end");
vector_return_label:
  asm("trillium glue_point vector_return");
#endif
  // }
  // else {
  // DTYPE *sp_ptr = (DTYPE*)getSpAddr(ptid, 0);
  // for (int j = start + vtid; j < end; j+=VECTOR_LEN) {
  //   DTYPE mean_j = 0.0f;
  //   for (int i = 1; i < (N+1); i++) {
  //     FRAME_START(MEAN_FRAME_SIZE);
  //     mean_j += sp_ptr[sp];
  //     REMEM(MEAN_FRAME_SIZE);
  //     sp++;
  //     if (sp == POST_FRAME_WORD) sp = 0;
  //   }
  //   mean_j /= (DTYPE)FLOAT_N;
  //   mean[j] = mean_j;
  // }

  // }
}

// compute the covariance matrix
void tril_covar(int mask, DTYPE *symmat, DTYPE *data, int N, int M, 
    int ptid, int groupId, int numGroups, int vtid, int ptidMailer, int linkId) {

  #ifdef SCALAR_CORE
  VECTOR_EPOCH(mask);

  int start = groupId;
  int stride = numGroups;
  int end = N;
  int numCompleted = 0;
  int startOffset = min(INIT_COVAR_OFFSET, N);
  int sp  = 0;

  #ifdef LONGLINES
  volatile int *sp_ptr = (int*)getSpAddr(ptid, 0);
  #endif

  ISSUE_VINST(init_label);
  #endif

  #ifdef VECTOR_CORE
  asm("trillium vissue_delim until_next vector_init");
  int start = groupId; // * VECTOR_LEN;
  // int j2 = j1;
  int i2;
  int stride = numGroups;// * VECTOR_LEN;
  int i1 = start - stride;
  int sp = 0;
  DTYPE* sp_ptr = (DTYPE*)getSpAddr(ptid, 0);

  int sp_origin = linkId * PER_CORE_MAILER_FRAME + vtid;
  DTYPE* sp_origin_ptr = (DTYPE*)getSpAddr(ptidMailer, 0);

  #ifdef NESTED_SIMD
  vsetvl_e32m1(NESTED_SIMD_VLEN);
  vfloat32m1_t vzero = vfmv_v_f_f32m1(0.0f); // splat 0
  #endif
  #endif

  #ifdef SCALAR_CORE

  #ifdef LONGLINES

  for (int i1 = start; i1 < end; i1+=stride) {

    for (int i2 = i1; i2 < N; i2++) {

      ISSUE_VINST(vec_body_init_label);

      // initial round
      for (int j = 0; j < startOffset; j+=COVAR_J_STRIDE) {
        prefetch_covar_frame(data, i1, i2, j, &sp, M);
      }

      // steady state
      for (int j = startOffset; j < M; j+=COVAR_J_STRIDE) {
        prefetch_covar_frame(data, i1, i2, j, &sp, M);
        ISSUE_VINST(vec_body_label);
      }

      // cooldown
      for (int j = N - startOffset; j < N; j+=COVAR_J_STRIDE) {
        ISSUE_VINST(vec_body_label);
      }

      SCALAR_SYNC_WITH_REDUCTION(sp_ptr, numCompleted);

      ISSUE_VINST(vec_body_end_label);
    }

    // depending on linkid might need a lil extra for strided
    for (int l = 0; l < linkId; l++) {
      SCALAR_SYNC_WITH_REDUCTION(sp_ptr, numCompleted);
      ISSUE_VINST(vec_body_end_label);
    }

  }

  #else

  for (int i1 = start; i1 < end; i1+=stride) {

    ISSUE_VINST(j2_begin_label);

    for (int i2 = i1; i2 < N; i2+=VECTOR_LEN) {

      ISSUE_VINST(vec_body_init_label);

      // initial round
      for (int j = 0; j < startOffset; j+=COVAR_J_STRIDE) {
        prefetch_covar_frame(data, i1, i2, j, &sp, M);
      }

       // steady state
      for (int j = startOffset; j < M; j+=COVAR_J_STRIDE) {
        prefetch_covar_frame(data, i1, i2, j, &sp, M);
        ISSUE_VINST(vec_body_label);
      }

      // cooldown
      for (int j = N - startOffset; j < N; j+=COVAR_J_STRIDE) {
        ISSUE_VINST(vec_body_label);
      }

      ISSUE_VINST(vec_body_end_label);
    }

    ISSUE_VINST(j2_end_label);
  }
  #endif
  #endif

  #ifdef VECTOR_CORE
  volatile int BH;
  volatile int BHO;
  #ifndef LONGLINES
  volatile int BHOO;
  do {
    asm("trillium vissue_delim until_next j2_begin");
    i1+=stride;
    i2 = i1 + vtid;
  #endif

  do {

    asm("trillium vissue_delim until_next vec_body_init");
    DTYPE symmat_idx = 0.0f;
    #ifdef NESTED_SIMD
    // NOTE MUST NEVER CHANGE THIS VALUE B/C CANT SQUASH IN VCORES!!!
    size_t l = vsetvl_e32m1(NESTED_SIMD_VLEN);
    #endif

    do {
      asm("trillium vissue_delim if_begin vec_body");

      #ifdef NESTED_SIMD
      l = vsetvl_e32m1(NESTED_SIMD_VLEN);
      vfloat32m1_t accum = vzero;
      #endif

      FRAME_START(COVAR_FRAME_SIZE);
      #pragma GCC unroll(16)
      for (int u = 0; u < COVAR_PREFETCH_LEN; u+=NESTED_SIMD_VLEN) {
        #ifdef NESTED_SIMD
        vfloat32m1_t vi1 = vle32_v_f32m1(&sp_ptr[sp + u]);
        vfloat32m1_t vi2 = vle32_v_f32m1(&sp_ptr[sp + COVAR_PREFETCH_LEN + u]);
        vfloat32m1_t vs = vfmul_vv_f32m1(vi1, vi2);
        accum = vfadd_vv_f32m1(accum, vs);
        #else
        symmat_idx += sp_ptr[sp + u] * sp_ptr[sp + COVAR_PREFETCH_LEN + u];
        #endif
      }

      #ifdef NESTED_SIMD
      vfloat32m1_t vpartial = vfredsum_vs_f32m1_f32m1(accum, accum, vzero);
      symmat_idx += vfmv_f_s_f32m1_f32(vpartial);
      #endif

      sp+=COVAR_FRAME_SIZE;
      sp = sp % POST_FRAME_WORD;
      REMEM(COVAR_FRAME_SIZE);
      // #if VECTOR_LEN==16
      // #pragma GCC unroll(16)
      // for (int n = 0; n < 1; n++) {
      //   asm volatile("nop\n\t");
      // }
      // #endif
      asm("trillium vissue_delim end at_jump");
    } while(BH);


    asm("trillium vissue_delim if_begin vec_body_end");
    #ifdef LONGLINES 
    FSTORE_NOACK(symmat_idx, &sp_origin_ptr[sp_origin], 0);
    sp_origin+=SUB_FRAME_SIZE;
    sp_origin = sp_origin % MAILER_POST_FRAME_WORD;
    asm volatile("fmv.s.x %[creg],zero\n\t" : [creg] "=f" (symmat_idx));
    #else
    int gt = (i2 >= N);
    PRED_EQ(gt, 0);
    FSTORE_NOACK(symmat_idx, &symmat[i2 * M + i1], 0);
    FSTORE_NOACK(symmat_idx, &symmat[i1 * M + i2], 0);
    // symmat[j2 * (M+1) + j1] = symmat_idx;
    // symmat[j1 * (M+1) + j2] = symmat_idx;
    PRED_EQ(i2, i2);
    i2+=VECTOR_LEN;
    #endif
    asm("trillium vissue_delim end at_jump");

  } while (BHO);


#ifndef LONGLINES
    asm("trillium vissue_delim if_begin j2_end");

    asm("trillium vissue_delim end at_jump");

  } while(BHOO);
#endif
  #endif


  // Clean up on the vector cores.
#ifdef SCALAR_CORE
  ISSUE_VINST(vector_return_label);
#elif defined VECTOR_CORE
  asm("trillium vissue_delim return vector_return");
  return;
#endif

#ifdef SCALAR_CORE
  // devec with unique tag
  DEVEC(devec_0);

  // we are doing lazy store acks, so use this to make sure all stores have commited to memory
  asm volatile("fence\n\t");
  asm("trillium vissue_delim return scalar_return");  // XXX is this real???
  return;
#endif

  // Glue points!
#ifdef SCALAR_CORE
init_label:
  asm("trillium glue_point vector_init");
  exit(1);
#ifndef LONGLINES
j2_begin_label:
  asm("trillium glue_point j2_begin");
  exit(1);
#endif
vec_body_init_label:
  asm("trillium glue_point vec_body_init");
  exit(1);
vec_body_label:
  asm("trillium glue_point vec_body");
  exit(1);
vec_body_end_label:
  asm("trillium glue_point vec_body_end");
  exit(1);
#ifndef LONGLINES
j2_end_label:
  asm("trillium glue_point j2_end");
  exit(1);
#endif
vector_return_label:
  asm("trillium glue_point vector_return");
  exit(1);
#endif

}

#ifdef LONGLINES
// compute each reduce across each vector (single dimension)
// only used in longlines version
void tril_reduce(int mask, DTYPE *mean, DTYPE *data, int N, int M, 
    int ptid, int groupId, int numGroups, int vtid, int ptidMailer, int linkId) {

  #ifdef SCALAR_CORE
  VECTOR_EPOCH(mask);

  int start = ((groupId + 0) * N) / numGroups;
  int end   = ((groupId + 1) * N) / numGroups;

  int startOffset = min(INIT_REDUCE_OFFSET, N);
  int sp  = 0;

  ISSUE_VINST(init_label);
  #endif

  #ifdef VECTOR_CORE
  asm("trillium vissue_delim until_next vector_init");
  int start = ((groupId + 0) * N) / numGroups;
  int i = start;
  int j = vtid*REDUCE_PREFETCH_LEN;
  int sp = 0;
  DTYPE *sp_ptr = (DTYPE*)getSpAddr(ptid, 0);
  #ifdef NESTED_SIMD
  vsetvl_e32m1(NESTED_SIMD_VLEN);
  #endif
  #endif

  #ifdef SCALAR_CORE

  for (int i = start; i < end; i++) {

    ISSUE_VINST(vec_body_init_label);

    // initial round
    for (int j = 0; j < startOffset; j+=REDUCE_J_STRIDE) {
      prefetch_reduce_frame(data, mean, i, j, &sp, M);
    }

    // steady state
    for (int j = startOffset; j < N; j+=REDUCE_J_STRIDE) {
      prefetch_reduce_frame(data, mean, i, j, &sp, M);
      ISSUE_VINST(vec_body_label);
    }

    //cooldown
    for (int j = N - startOffset; j < N; j+=REDUCE_J_STRIDE) {
      ISSUE_VINST(vec_body_label);
    }

    ISSUE_VINST(vec_body_end_label);
  }

  #endif

  #ifdef VECTOR_CORE
  volatile int BH;
  volatile int BHO;
  do {

    asm("trillium vissue_delim until_next vec_body_init");

    #ifdef NESTED_SIMD
    // NOTE MUST NEVER CHANGE THIS VALUE B/C CANT SQUASH IN VCORES!!!
    size_t l = vsetvl_e32m1(NESTED_SIMD_VLEN);
    #endif

    do {
      asm("trillium vissue_delim if_begin vec_body");

      #ifdef NESTED_SIMD
      l = vsetvl_e32m1(NESTED_SIMD_VLEN);
      #endif

      FRAME_START(REDUCE_FRAME_SIZE);
      #pragma GCC unroll(16)
      for (int u = 0; u < REDUCE_PREFETCH_LEN; u+=NESTED_SIMD_VLEN) {
        #ifdef NESTED_SIMD 
        // TODO don't do vector here b/c don't have store noack version?
        vfloat32m1_t vdata = vle32_v_f32m1(&sp_ptr[sp + u]);
        vfloat32m1_t vmean = vle32_v_f32m1(&sp_ptr[sp + REDUCE_PREFETCH_LEN + u]);

        vfloat32m1_t vnew = vfsub_vv_f32m1(vdata, vmean);
        vse32_v_f32m1(&data[i * M + j + u], vnew);
        #else
        DTYPE updated_data = sp_ptr[sp + u] - sp_ptr[sp + REDUCE_PREFETCH_LEN + u];
        FSTORE_NOACK(updated_data, &data[i * M + j + u], 0);
        #endif
      }
      j+=REDUCE_J_STRIDE;
      sp+=REDUCE_FRAME_SIZE;
      sp = sp % POST_FRAME_WORD;
      END_FRAME();
      asm("trillium vissue_delim end at_jump");
    } while(BH);

    asm("trillium vissue_delim if_begin vec_body_end");
    j = vtid*REDUCE_PREFETCH_LEN;
    i++;
    asm("trillium vissue_delim end at_jump");

  } while (BHO);
  #endif


  // Clean up on the vector cores.
#ifdef SCALAR_CORE
  ISSUE_VINST(vector_return_label);
#elif defined VECTOR_CORE
  asm("trillium vissue_delim return vector_return");
  return;
#endif

#ifdef SCALAR_CORE
  // devec with unique tag
  DEVEC(devec_0);

  // we are doing lazy store acks, so use this to make sure all stores have commited to memory
  asm volatile("fence\n\t");
  asm("trillium vissue_delim return scalar_return");  // XXX is this real???
  return;
#endif

  // Glue points!
#ifdef SCALAR_CORE
init_label:
  asm("trillium glue_point vector_init");
vec_body_init_label:
  asm("trillium glue_point vec_body_init");
vec_body_label:
  asm("trillium glue_point vec_body");
vec_body_end_label:
  asm("trillium glue_point vec_body_end");
vector_return_label:
  asm("trillium glue_point vector_return");
#endif
}
#endif


#endif