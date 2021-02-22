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

// TODO there are def oppurtuniteis to parallize inner loop instead of outer loop to get more horizontal prefetching
//
// for (int i = start + vtid; i < end; i+=VECTOR_LEN) {
//  for (int j = 0; j < M; j++) {
//    c[i * N + j]
//  }
// }
//
// should be 
//
// for (int i = start; i < end; i++{
//  for (int j = vtid; j < M; j+=VECTOR_LEN) {
//    c[i * N + j]
//  }
// }
//
// then can use horizontal prefetching

void tril_syrk(int mask, DTYPE *a, DTYPE *c, int N, int M, 
                  int ptid, int groupId, int numGroups, int vtid,
                  int ptidMailer, int linkId, int numGroupsPerMailer) {


  #ifdef SCALAR_CORE

  VECTOR_EPOCH(mask);

  // chunk over vector gorups
  int start = ((groupId + 0) * N) / numGroups;
  int end   = ((groupId + 1) * N) / numGroups;

  #ifndef LONGLINES
  // make it a factor of vector group mapping size
  start = roundUp(start, VECTOR_LEN);
  end   = roundUp(end  , VECTOR_LEN);
  #endif

  int startOffset = min(INIT_OFFSET, M);

  ISSUE_VINST(init_label);
  #endif

  #ifdef VECTOR_CORE
  asm("trillium vissue_delim until_next vector_init");
  int start = ((groupId + 0) * N) / numGroups;
  #ifndef LONGLINES
  start = roundUp(start, VECTOR_LEN);
  #endif
  int i = start;
  int j = vtid;
  int sp = 0;
  DTYPE c_ij;
  DTYPE* sp_ptr = (DTYPE*)getSpAddr(ptid, 0);

  int sp_origin = (linkId * SCALAR_FRAME_SIZE) + vtid;
  DTYPE* sp_origin_ptr = (DTYPE*)getSpAddr(ptidMailer, 0);

  #ifdef NESTED_SIMD
  vfloat32m1_t vzero = vfmv_v_f_f32m1(0.0f); // splat 0
  #endif
  #endif

  #ifdef SCALAR_CORE
  int sp = 0;
  int sp_self = 0;
  #ifdef SCALAR_IS_MAILER
  DTYPE *sp_ptr = (DTYPE*)getSpAddr(ptid, 0);
  #else
  volatile int *sp_ptr = (int*)getSpAddr(ptid, 0);
  #endif
  // int init_offset = min(INIT_OFFSET, M);

  for (int i = start; i < end; i++) {

    #ifdef LONGLINES
    #ifndef SCALAR_IS_MAILER
    while (1) {
      // needs volatile so doesn't optimize
      int wait_val = sp_ptr[POST_FRAME_WORD]; 
      if (wait_val == 1) break;
    }
    // printf("reset val %d\n", ptid);
    sp_ptr[POST_FRAME_WORD] = 0;
    #endif


    #if INIT_FRAMES==0
    for (int j = 0; j < M; j+=J_STRIDE) {
      ISSUE_VINST(vec_body_init_label);

      for (int k = 0; k < M; k+=K_STRIDE) {
        prefetch_inner_frame(a, i, j, k, &sp, M);
        ISSUE_VINST(vec_body_label);
      }

      #ifndef SCALAR_IS_MAILER
      // wait for mailer to be ready
      if (j != 1 && (j - 1) % FRAMES_TO_SYNC_AFTER == 0) {
        // printf("start reset value %d %d\n", ptid, j);
        while (1) {
          int wait_val = sp_ptr[POST_FRAME_WORD]; 
          if (wait_val == 1) break;
        }
        // printf("reset value %d %d\n", ptid, j);
        // sp_ptr[POST_FRAME_WORD] = 0;

        // TODO doesn't work. not sure if sync bug or NOACK to local spad not supported
        STORE_NOACK(0, &sp_ptr[POST_FRAME_WORD], 0);
      }
      #endif

      ISSUE_VINST(vec_body_end_label);

    }

    #else
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

          ISSUE_VINST(vec_body_end_label);

          #ifndef SCALAR_IS_MAILER
          // wait for mailer to be ready
          if (j != 1 && (j - 1) % FRAMES_TO_SYNC_AFTER == 0) {
            // printf("start reset value %d %d\n", ptid, j);
            while (1) {
              int wait_val = sp_ptr[POST_FRAME_WORD]; 
              if (wait_val == 1) break;
            }
            // printf("reset value %d %d\n", ptid, j);
            // sp_ptr[POST_FRAME_WORD] = 0;

            // TODO doesn't work. not sure if sync bug or NOACK to local spad not supported
            STORE_NOACK(0, &sp_ptr[POST_FRAME_WORD], 0);
          }
          #endif

          ISSUE_VINST(vec_body_init_label);
        }


      }

      #ifdef SCALAR_IS_MAILER
      // every so often need to complete the sum
      // maybe want to put this somewhere else, so in between vinst and such?
      if (j % ACCUM_GRANULARITY == 0)
        do_sum(c, i, j - ACCUM_GRANULARITY, N, sp_ptr, &sp_self);
      #endif
    }

    // draining. do the last vissue corresponding to the initial round of prefetch
    for (int k = M - startOffset; k < M; k+=K_STRIDE) {
      ISSUE_VINST(vec_body_label);
    }

    ISSUE_VINST(vec_body_end_label);
    #ifdef SCALAR_IS_MAILER
    do_sum(c, i, M - ACCUM_GRANULARITY, N, sp_ptr, &sp_self);
    #endif
    #endif

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

  // // get ahead
  // prefetch_outer_frame(c, start, 0, &sp, N);
  // for (int k = 0; k < INIT_OFFSET; k+=INNER_PREFETCH_LEN) {
  //   prefetch_inner_frame(a, start, 0, k, &sp, M);
  // }

  // // do first inner loop
  // for (int k = INIT_OFFSET; k < M; k+=INNER_PREFETCH_LEN) {
  //   prefetch_inner_frame(a, start, 0, k, &sp, M);
  //   ISSUE_VINST(vec_body_label);
  // }

  // // steady state
  // for (int i = start; i < end; i++) {
  //   int startJ = 0;
  //   if (i == start) startJ += VECTOR_LEN;
  //   for (int j = startJ; j < M; j+=VECTOR_LEN) {
  //     prefetch_outer_frame(c, i, j, &sp, N);

  //     for (int k = 0; k < M; k+=INNER_PREFETCH_LEN) {
  //       prefetch_inner_frame(a, i, j, k, &sp, M);
  //       ISSUE_VINST(vec_body_label);
  //     }
  //   }
  // }

  // // draining. do the last vissue corresponding to the initial round of prefetch
  // for (int k = N - INIT_OFFSET; k < N; k+=INNER_PREFETCH_LEN) {
  //   ISSUE_VINST(vec_body_label);
  // }
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
    sp+=OUTER_FRAME_SIZE;
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
      #endif
      
      FRAME_START(INNER_FRAME_SIZE);

      #pragma GCC unroll(16)
      for (int k = 0; k < INNER_PREFETCH_LEN; k+=NESTED_SIMD_VLEN) {
        #ifdef NESTED_SIMD
        // load from scratchpad frame
        vfloat32m1_t vai = vle32_v_f32m1(&sp_ptr[sp + k]);
        vfloat32m1_t vaj = vle32_v_f32m1(&sp_ptr[sp + INNER_PREFETCH_LEN + k]);

        vfloat32m1_t vaa = vfmul_vv_f32m1(vai, vaj);
        vfloat32m1_t vaaa = vfmul_vf_f32m1(vaa, alpha); 

        vfloat32m1_t vcij = vfredsum_vs_f32m1_f32m1(vaaa, vaaa, vzero);
        c_ij += vfmv_f_s_f32m1_f32(vcij);
        #else
        c_ij += alpha * sp_ptr[sp + k] * sp_ptr[sp + INNER_PREFETCH_LEN + k];
        #endif
      }

      // printf("a %f ?= %f %f ?= %f\n", sp_ptr[sp + 0], a[i * M + k], sp_ptr[sp + 1], a[j * M +k]);
      // c[i * N + j] += alpha * a[i * M + k] * a[j * M + k];
      // c_ij += alpha * sp_ptr[sp + 0] * sp_ptr[sp + 1];
      REMEM(INNER_FRAME_SIZE);
      sp+=INNER_FRAME_SIZE;
      sp = sp % POST_FRAME_WORD;
      asm("trillium vissue_delim end at_jump");
    } while(BH);

    asm("trillium vissue_delim if_begin vec_body_end");
    #ifdef LONGLINES
    // store partial sum to scalar core
    STORE_NOACK(c_ij, &sp_origin_ptr[sp_origin], 0);
    sp_origin+=SCALAR_FRAME_SIZE*numGroupsPerMailer;
    sp_origin = sp_origin % POST_FRAME_WORD;
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