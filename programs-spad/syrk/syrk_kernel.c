#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <math.h>

#include "pthread_launch.h"
#include "syrk.h"
#include "spad.h"
#include "bind_defs.h"
#include "util.h"

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


// do reduction on scalar core
// each core in vector group should have sent a message in a frame
inline void do_sum(DTYPE *c, int i, int j, int N, DTYPE *sp_ptr, int *sp_idx) {
  DTYPE sum = c[i * N + j] * beta;
  FRAME_START(SCALAR_FRAME_SIZE);
  for (int i = 0; i < SCALAR_FRAME_SIZE; i++) {
    sum += sp_ptr[*sp_idx + i];
  }
  REMEM(SCALAR_FRAME_SIZE);
  STORE_NOACK(sum, &c[i * N + j], 0);
  (*sp_idx)+=SCALAR_FRAME_SIZE;
  *sp_idx = *sp_idx % POST_FRAME_WORD;
}

void tril_syrk(int mask, DTYPE *a, DTYPE *c, int N, int M, 
                  int ptid, int groupId, int numGroups, int vtid,
                  int ptidOrigin) {


  #ifdef SCALAR_CORE

  VECTOR_EPOCH(mask);

  // chunk over vector gorups
  int start = ((groupId + 0) * N) / numGroups;
  int end   = ((groupId + 1) * N) / numGroups;

  // TODO don't think needed for this??
  // make it a factor of vector group mapping size
  start = roundUp(start, VECTOR_LEN);
  end   = roundUp(end  , VECTOR_LEN);

  int startOffset = min(INIT_OFFSET, M);

  ISSUE_VINST(init_label);
  #endif

  #ifdef VECTOR_CORE
  asm("trillium vissue_delim until_next vector_init");
  int start = ((groupId + 0) * N) / numGroups;
  start = roundUp(start, VECTOR_LEN);
  int i = start;
  int j = vtid;
  int sp = 0;
  DTYPE c_ij;
  DTYPE* sp_ptr = (DTYPE*)getSpAddr(ptid, 0);

  int sp_origin = vtid;
  DTYPE* sp_origin_ptr = (DTYPE*)getSpAddr(ptidOrigin, 0);
  #endif

  #ifdef SCALAR_CORE
  int sp = 0;
  int sp_self = 0;
  DTYPE *sp_ptr = (DTYPE*)getSpAddr(ptid, 0);

  // int init_offset = min(INIT_OFFSET, M);

  for (int i = start; i < end; i++) {
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

      #ifdef LONGLINES
      // do reduction of previous iter now while loads are in flight for next frame
      if (j > 0) {
        do_sum(c, i, j - 1, N, sp_ptr, &sp_self);
      }
      #endif

      // cooldown
      for (int k = M - startOffset; k < M; k+=K_STRIDE) {
        ISSUE_VINST(vec_body_label);
      }

      ISSUE_VINST(vec_body_end_label);

      // do the last of j iter sum here (no load in flight, but only single iter so w/e)
      #ifdef LONGLINES
      if (j == M - 1) {
        do_sum(c, i, j, N, sp_ptr, &sp_self);
      }
      #endif
    }
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
    c_ij = 0;
    #endif

    do {
      asm("trillium vissue_delim if_begin vec_body");
      // do innermost loop body (k)
      FRAME_START(INNER_FRAME_SIZE);

      #pragma GCC unroll(16)
      for (int k = 0; k < INNER_PREFETCH_LEN; k++) {
        c_ij += alpha * sp_ptr[sp + k] * sp_ptr[sp + INNER_PREFETCH_LEN + k];
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
    // TODO store NO ACK
    // c[i * N + j] = c_ij;

    #ifdef LONGLINES
    // store partial sum to scalar core
    STORE_NOACK(c_ij, &sp_origin_ptr[sp_origin], 0);
    // sp_origin_ptr[sp_origin] = c_ij; // TODO STORE_NOACK doesn't work. check remote load req path, maybe not considering flag
    sp_origin+=SCALAR_FRAME_SIZE;
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
vec_body_init_label:
  asm("trillium glue_point vec_body_init");
vec_body_label:
  asm("trillium glue_point vec_body");
vec_body_end_label:
  asm("trillium glue_point vec_body_end");
vector_return_label:
  asm("trillium glue_point vector_return");
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