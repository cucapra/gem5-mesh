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

// prefetch c
// pad out to the frame size (1->2 currently)
// maybe don't have to prefetch this
inline void prefetch_outer_frame(DTYPE *c, int i, int j, int *sp, int N) {
  for (int core = 0; core < VECTOR_LEN; core++) {
    VPREFETCH_L(*sp + 0, &c[i * N + j + core], core, OUTER_PREFETCH_LEN, VERTICAL);

    // pad out
    VPREFETCH_L(*sp + 1, &c[i * N + j + core], core, OUTER_PREFETCH_LEN, VERTICAL);
  }

  *sp = *sp + 2*OUTER_PREFETCH_LEN;
  if (*sp == POST_FRAME_WORD) *sp = 0;
}

// prefetch a
inline void prefetch_inner_frame(DTYPE *a, int i, int j, int k, int *sp, int M) {
  for (int core = 0; core < VECTOR_LEN; core++) {
    // TODO redundant
    VPREFETCH_L(*sp + 0, &a[i * M + k], core, INNER_PREFETCH_LEN, VERTICAL);

    // TODO this can be horizontal?
    VPREFETCH_L(*sp + 1, &a[(j + core) * M + k], core, INNER_PREFETCH_LEN, VERTICAL);
  }

  *sp = *sp + 2*INNER_PREFETCH_LEN;
  if (*sp == POST_FRAME_WORD) *sp = 0;
}

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
                  int ptid, int groupId, int numGroups, int vtid) {


  // chunk over vector gorups
  int start = ((groupId + 0) * N) / numGroups;
  int end   = ((groupId + 1) * N) / numGroups;

  // make it a factor of vector group mapping size
  start = roundUp(start, VECTOR_LEN);
  end   = roundUp(end  , VECTOR_LEN);

  VECTOR_EPOCH(mask);

  int sp = 0;

  if (ptid == 0) {
  // ISSUE_VINST(fable0);
    
  // get ahead
  prefetch_outer_frame(c, start, 0, &sp, N);
  for (int k = 0; k < INIT_OFFSET; k+=INNER_PREFETCH_LEN) {
    prefetch_inner_frame(a, start, 0, k, &sp, M);
  }

  // do first inner loop
  for (int k = INIT_OFFSET; k < M; k+=INNER_PREFETCH_LEN) {
    prefetch_inner_frame(a, start, 0, k, &sp, M);
    // ISSUE_VINST(fable1);
  }

  // steady state
  for (int i = start; i < end; i++) {
    int startJ = 0;
    if (i == start) startJ += VECTOR_LEN;
    for (int j = startJ; j < M; j+=VECTOR_LEN) {
      prefetch_outer_frame(c, i, j, &sp, N);

      for (int k = 0; k < M; k+=INNER_PREFETCH_LEN) {
        prefetch_inner_frame(a, i, j, k, &sp, M);
        // ISSUE_VINST(fable1);
      }
    }
  }

  // draining. do the last vissue corresponding to the initial round of prefetch
  for (int k = N - INIT_OFFSET; k < N; k+=INNER_PREFETCH_LEN) {
    // ISSUE_VINST(fable1);
  }

  // // devec with unique tag
  // DEVEC(devec_0);
  }
  else {
  DTYPE *sp_ptr = (DTYPE*)getSpAddr(ptid, 0);
  for (int i = start; i < end; i++) {
    for (int j = vtid; j < M; j+=VECTOR_LEN) {
      FRAME_START(OUTER_FRAME_SIZE);
      // c[i * N + j] *= beta;
      // printf("c %f ?= %f\n", sp_ptr[sp], c[i*N+j]);
      DTYPE c_ij = sp_ptr[sp + 0] * beta;
      REMEM(OUTER_FRAME_SIZE);
      sp+=2;
      if (sp == POST_FRAME_WORD) sp = 0;

      for (int k = 0; k < M; k++) {
        FRAME_START(INNER_FRAME_SIZE);
        // printf("a %f ?= %f %f ?= %f\n", sp_ptr[sp + 0], a[i * M + k], sp_ptr[sp + 1], a[j * M +k]);
        // c[i * N + j] += alpha * a[i * M + k] * a[j * M + k];
        c_ij += alpha * sp_ptr[sp + 0] * sp_ptr[sp + 1];
        REMEM(INNER_FRAME_SIZE);
        sp+=2;
        if (sp == POST_FRAME_WORD) sp = 0;
      }
      // TODO store_noack
      c[i * N + j] = c_ij;
    }
  }
  }


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