#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <math.h>

#include "pthread_launch.h"
#include "syr2k.h"
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

void tril_syr2k(int mask, DTYPE *a, DTYPE *b, DTYPE *c, int N, int M, 
                  int ptid, int groupId, int numGroups, int vtid) {


  #ifdef SCALAR_CORE

  VECTOR_EPOCH(mask);

  // chunk over vector gorups
  int start = ((groupId + 0) * N) / numGroups;
  int end   = ((groupId + 1) * N) / numGroups;

  // make it a factor of vector group mapping size
  // start = roundUp(start, VECTOR_LEN);
  // end   = roundUp(end  , VECTOR_LEN);

  ISSUE_VINST(init_label);
  #endif

  #ifdef VECTOR_CORE
  asm("trillium vissue_delim until_next vector_init");
  int start = ((groupId + 0) * N) / numGroups;
  // start = roundUp(start, VECTOR_LEN);
  int i = start;
  int j = vtid;
  // int k = 0;
  int sp = 0;
  DTYPE c_ij;
  DTYPE* sp_ptr = (DTYPE*)getSpAddr(ptid, 0);
  #endif

  #ifdef SCALAR_CORE
  int sp = 0;


  for (int i = start; i < end; i++) {
    for (int j = 0; j < M; j+=VECTOR_LEN) {
      prefetch_outer_frame(c, i, j, &sp, N);
      ISSUE_VINST(vec_body_init_label);

      // warmup 
      for (int k = 0; k < INIT_OFFSET; k+=INNER_PREFETCH_LEN) {
        prefetch_inner_frame(a, b, i, j, k, &sp, M);
      }

      // steady-state
      for (int k = INIT_OFFSET; k < M; k+=INNER_PREFETCH_LEN) {
        prefetch_inner_frame(a, b, i, j, k, &sp, M);
        ISSUE_VINST(vec_body_label);
      }

      // cooldown
      for (int k = M - INIT_OFFSET; k < M; k+=INNER_PREFETCH_LEN) {
        ISSUE_VINST(vec_body_label);
      }

      ISSUE_VINST(vec_body_end_label);
    }
  }
  #endif
  
  #ifdef VECTOR_CORE
  volatile int BH;
  volatile int BHO;
  do {
    asm("trillium vissue_delim until_next vec_body_init");
    FRAME_START(OUTER_FRAME_SIZE);
      // c[i * N + j] *= beta;
      // printf("c %f ?= %f\n", sp_ptr[sp], c[i*N+j]);
    c_ij = sp_ptr[sp + 0] * beta;
    REMEM(OUTER_FRAME_SIZE);
    #pragma GCC unroll(22)
    for (int n = 0; n < 22; n++) {
          asm volatile("nop\n\t");
    }
    sp+=OUTER_FRAME_SIZE;
    sp = sp % POST_FRAME_WORD;

    do {

      asm("trillium vissue_delim if_begin vec_body");
      // loop header
      // if ( k == 0 ) {
      //   FRAME_START(OUTER_FRAME_SIZE);
      //     // c[i * N + j] *= beta;
      //     // printf("c %f ?= %f\n", sp_ptr[sp], c[i*N+j]);
      //   c_ij = sp_ptr[sp + 0] * beta;
      //   REMEM(OUTER_FRAME_SIZE);
      //   sp+=OUTER_FRAME_SIZE;
      //   sp = sp % POST_FRAME_WORD;
      //   // if (sp == POST_FRAME_WORD) sp = 0;
      // }

      // do innermost loop body (k)
      FRAME_START(INNER_FRAME_SIZE);
      // printf("a %f ?= %f %f ?= %f\n", sp_ptr[sp + 0], a[i * M + k], sp_ptr[sp + 1], a[j * M +k]);
      // c[i * N + j] += alpha * a[i * M + k] * a[j * M + k];
      #pragma GCC unroll(8)
      for (int k = 0; k < INNER_PREFETCH_LEN; k++) {
        c_ij += alpha * sp_ptr[sp + INNER_PREFETCH_LEN*0 + k] * 
                        sp_ptr[sp + INNER_PREFETCH_LEN*3 + k] + 
                alpha * sp_ptr[sp + INNER_PREFETCH_LEN*2 + k] *
                        sp_ptr[sp + INNER_PREFETCH_LEN*1 + k];
      }
      REMEM(INNER_FRAME_SIZE);
      sp+=INNER_FRAME_SIZE;
      sp = sp % POST_FRAME_WORD;
      // if (sp == POST_FRAME_WORD) sp = 0;
      asm("trillium vissue_delim end at_jump");

    } while(BH);

    asm("trillium vissue_delim if_begin vec_body_end");
    // TODO store NO ACK
    // c[i * N + j] = c_ij;
    STORE_NOACK(c_ij, &c[i * N + j], 0);

    // handle outer loops
    j+=VECTOR_LEN;
    if (j >= M) {
      j = vtid;
      i++;
    }
    asm("trillium vissue_delim end at_jump");

    // // loop footer
    // k++;
    // if ( k == M ) {
    //   k = 0;

    //   // TODO store NO ACK
    //   c[i * N + j] = c_ij;

    //   // handle outer loops
    //   j+=VECTOR_LEN;
    //   if (j >= M) {
    //     j = vtid;
    //     i++;
    //   }
    // }
    // asm("trillium vissue_delim end at_jump");
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
