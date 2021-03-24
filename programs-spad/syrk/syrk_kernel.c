#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <math.h>

#include "pthread_launch.h"
#include "syrk.h"
#include "spad.h"
#include "bind_defs.h"
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

void tril_syrk(int mask, DTYPE *a, DTYPE *c, int N, int M, 
                  int ptid, int groupId, int numGroups, int vtid,
                  int ptidMailer, int linkId) {


  #ifdef SCALAR_CORE

  VECTOR_EPOCH(mask);

  // chunk over vector gorups
  int start = get_group_start(groupId, N, numGroups);
  int end   = get_group_end(groupId, N, numGroups);
  int numCompleted = 0;
  int startOffset = min(INIT_OFFSET, M);
  int sp = 0;

  #ifdef LONGLINES
  volatile int *sp_ptr = (int*)getSpAddr(ptid, 0);
  #endif

  ISSUE_VINST(init_label);
  #endif

  #ifdef VECTOR_CORE
  asm("trillium vissue_delim until_next vector_init");
  int start = get_group_start(groupId, N, numGroups);
  int i = start;
  int j = vtid;
  int sp = 0;
  DTYPE c_ij;
  DTYPE* sp_ptr = (DTYPE*)getSpAddr(ptid, 0);

  int sp_origin = MAILER_OFFSET + (linkId * PER_CORE_MAILER_FRAME) + vtid;
  DTYPE* sp_origin_ptr = (DTYPE*)getSpAddr(ptidMailer, 0);

  #ifdef NESTED_SIMD
  vsetvl_e32m1(NESTED_SIMD_VLEN);
  vfloat32m1_t vzero = vfmv_v_f_f32m1(0.0f); // splat 0
  #endif
  #endif

  #ifdef SCALAR_CORE

  for (int i = start; i < end; i++) {

    #ifdef LONGLINES

    int j = 0;

    // get ahead
    ISSUE_VINST(vec_body_init_label);

    for (int k = 0; k < startOffset; k+=K_STRIDE) {
      prefetch_inner_frame(a, i, j, k, &sp, M);
    }

    // do first inner loop
    for (int k = startOffset; k < M; k+=K_STRIDE) {
      prefetch_inner_frame(a, i, j, k, &sp, M);
      ISSUE_VINST(vec_body_label);
    }

    // steady state
    for (j = 1; j < M; j+=J_STRIDE) {

      for (int k = 0; k < M; k+=K_STRIDE) {

        prefetch_inner_frame(a, i, j, k, &sp, M);
        ISSUE_VINST(vec_body_label);

        // finish loop at a weird time
        if (k + K_STRIDE == startOffset) {

          // wait for mailer to be ready
          SCALAR_SYNC_WITH_REDUCTION(sp_ptr, numCompleted);

          ISSUE_VINST(vec_body_end_label);

          // reduce number of vinsts on critical path. TODO make sure 
          // end label does everything init label does (generally setting sum variable)
          // ISSUE_VINST(vec_body_init_label);
        }
      }

    }

    // draining. do the last vissue corresponding to the initial round of prefetch
    for (int k = M - startOffset; k < M; k+=K_STRIDE) {
      ISSUE_VINST(vec_body_label);
    }

    SCALAR_SYNC_WITH_REDUCTION(sp_ptr, numCompleted);

    ISSUE_VINST(vec_body_end_label);

    #else
    for (int j = 0; j < M; j+=J_STRIDE) {
      prefetch_outer_frame(c, i, j, &sp, N);
      ISSUE_VINST(vec_body_init_label);

      // warmup 
      for (int k = 0; k < startOffset; k+=K_STRIDE) {
        prefetch_inner_frame(a, i, j, k, &sp, M);
      }

      // steady-state
      for (int k = startOffset; k < M; k+=K_STRIDE) {
        prefetch_inner_frame(a, i, j, k, &sp, M);
        ISSUE_VINST(vec_body_label);
      }

      // cooldown
      for (int k = M - startOffset; k < M; k+=K_STRIDE) {
        ISSUE_VINST(vec_body_label);
      }

      ISSUE_VINST(vec_body_end_label);
    }
    #endif
  }

  #endif
  
  #ifdef VECTOR_CORE
  volatile int BH;
  volatile int BHO;
  do { 

    asm("trillium vissue_delim until_next vec_body_init");

    #ifndef LONGLINES
    FRAME_START(OUTER_FRAME_SIZE);
    // c[i * N + j] *= beta;
    // printf("c %f ?= %f\n", sp_ptr[sp], c[i*N+j]);
    c_ij = sp_ptr[sp + 0] * beta;
    REMEM(OUTER_FRAME_SIZE);
    // pad so num regions possible regions is lower
    asm volatile("nop\n\t");
    asm volatile("nop\n\t");
    asm volatile("nop\n\t");
    asm volatile("nop\n\t");
    asm volatile("nop\n\t");
    asm volatile("nop\n\t");
    asm volatile("nop\n\t");
    asm volatile("nop\n\t");
    asm volatile("nop\n\t");
    sp+=INNER_FRAME_SIZE;
    sp = sp % POST_FRAME_WORD;
    #else
    #ifdef NESTED_SIMD
    // NOTE MUST NEVER CHANGE THIS VALUE B/C CANT SQUASH IN VCORES!!!
    size_t l = vsetvl_e32m1(NESTED_SIMD_VLEN);
    #endif
    c_ij = 0;
    #endif

    do {
      asm("trillium vissue_delim if_begin vec_body");
  
      // do innermost loop body (k)
      #ifdef NESTED_SIMD
      l = vsetvl_e32m1(NESTED_SIMD_VLEN);
      vfloat32m1_t accum = vzero;
      #endif
      
      FRAME_START(INNER_FRAME_SIZE);

      // WARNING if less than INNER_PREFETCH_LEN, creates code gen problem
      // could always just not unroll
      #pragma GCC unroll(32)
      for (int k = 0; k < INNER_PREFETCH_LEN; k+=NESTED_SIMD_VLEN) {
        #ifdef NESTED_SIMD
        // load from scratchpad frame
        vfloat32m1_t vai = vle32_v_f32m1(&sp_ptr[sp + k]);
        vfloat32m1_t vaj = vle32_v_f32m1(&sp_ptr[sp + INNER_PREFETCH_LEN + k]);

        vfloat32m1_t vaa = vfmul_vv_f32m1(vai, vaj);
        vfloat32m1_t vaaa = vfmul_vf_f32m1(vaa, alpha);

        accum = vfadd_vv_f32m1(accum, vaaa);
        #else
        c_ij += alpha * sp_ptr[sp + k] * sp_ptr[sp + INNER_PREFETCH_LEN + k];
        #endif
      }

      #ifdef NESTED_SIMD
      // NOTE very important to do this outside of the loop otherwise won't
      // mix iterations together and will have poor depedency distances
      vfloat32m1_t vcij = vfredsum_vs_f32m1_f32m1(accum, accum, vzero);
      c_ij += vfmv_f_s_f32m1_f32(vcij);
      #endif

      sp+=INNER_FRAME_SIZE;
      sp = sp % POST_FRAME_WORD;

      // best to do remem at the end so can reorder as my instructions as possible
      REMEM(INNER_FRAME_SIZE); 
      asm("trillium vissue_delim end at_jump");
    } while(BH);

    asm("trillium vissue_delim if_begin vec_body_end");

    #ifdef LONGLINES
    // store partial sum to scalar core
    STORE_NOACK(c_ij, &sp_origin_ptr[sp_origin], 0);
    sp_origin+=SUB_FRAME_SIZE;
    sp_origin = sp_origin % MAILER_POST_FRAME_WORD;
    // force sum reset here
    asm volatile("fmv.s.x %[creg],zero\n\t" : [creg] "=f" (c_ij));
   
    #else

    STORE_NOACK(c_ij, &c[i * N + j], 0);

    // handle outer loops
    j+=J_STRIDE;
    if (j >= M) {
      j = vtid;
      i++;
    }
    #endif
    asm("trillium vissue_delim end at_jump");

  } while(BHO);
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
#if INIT_FRAMES==0
exit(1);
#endif
vec_body_init_label:
  asm("trillium glue_point vec_body_init");
#if INIT_FRAMES==0
exit(1);
#endif
vec_body_label:
  asm("trillium glue_point vec_body");
#if INIT_FRAMES==0
exit(1);
#endif
vec_body_end_label:
  asm("trillium glue_point vec_body_end");
#if INIT_FRAMES==0
exit(1);
#endif
vector_return_label:
  asm("trillium glue_point vector_return");
#if INIT_FRAMES==0
exit(1);
#endif
#endif

  // can we change where paralleization is to get more horiz prefetch?
  // for (int i = start; i < end; i++) {
  //   for (int j = 0; j < M; j++) {
  //     c[i * N + j] *= beta;

  //     for (int k = 0; k < M; k++) {
  //       c[i * N + j] += alpha * a[i * M + k] * a[j * M + k];
  //     }
  //   }
  // }

}
#endif