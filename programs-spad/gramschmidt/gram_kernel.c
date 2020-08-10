#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <math.h>

#include "pthread_launch.h"
#include "gramschmidt.h"
#include "spad.h"
#include "bind_defs.h"
#include "token_queue.h"
#include "group_templates.h"
#include "util.h"

// #define SCALAR_CORE
// #define VECTOR_CORE

/*-----------------------------------------------------------------------------------
 * Vector versions of the kernels. Same memory access pattern as baseline but do prefetching
 *---------------------------------------------------------------------------------*/

#ifdef USE_VEC

void tril_u_normalize(int mask, DTYPE *a, DTYPE *r, DTYPE *q, 
    int numVectors, int vectorLen, int k, int ptid, int groupId, int numGroups, int vtid) {


#ifdef SCALAR_CORE
  VECTOR_EPOCH(mask);

  // chunk over vector gorups
  int start = ((groupId + 0) * vectorLen) / numGroups;
  int end   = ((groupId + 1) * vectorLen) / numGroups;

  // make it a factor of vector group mapping size
  start = roundUp(start, STRIDE_NORM);
  end   = roundUp(end  , STRIDE_NORM);

  // issue header block
  ISSUE_VINST(init_label);
#endif

#ifdef VECTOR_CORE
  asm("trillium vissue_delim until_next vector_init");
  int start = ((groupId + 0) * vectorLen) / numGroups;
  start = roundUp(start, STRIDE_NORM);
  int i = start + vtid;
  DTYPE r_cache = r[k * numVectors + k];
  int sp = 0;
  DTYPE* sp_ptr = (DTYPE*)getSpAddr(ptid, 0);
#endif

#ifdef SCALAR_CORE
  int sp = 0;

  int init_i_dist = min(end - start, INIT_OFFSET_NORM);

  for (int i = start; i < start + init_i_dist; i+=STRIDE_NORM) {
    prefetch_normalize_frame(a, i, k, numVectors, &sp);
  }

  for (int i = start + init_i_dist; i < end; i+=STRIDE_NORM) {
    prefetch_normalize_frame(a, i, k, numVectors, &sp);
    ISSUE_VINST(vec_body_label);
  }

  for (int i = end - init_i_dist; i < end; i+=STRIDE_NORM) {
    ISSUE_VINST(vec_body_label);
  }


#endif

#ifdef VECTOR_CORE
  volatile int BH;
  do {
    asm("trillium vissue_delim if_begin vec_body");
    // q[i * numVectors + k] = a[i * numVectors + k] / r_cache;
    START_FRAME();
    #pragma GCC unroll(2)
    for (int u = 0; u < UNROLL_LEN_NORM; u++) {
      DTYPE val =  sp_ptr[sp + u] / r_cache;
      STORE_NOACK(val, &q[(i + u*VECTOR_LEN) * numVectors + k], 0);
    }
    END_FRAME();
    i+=STRIDE_NORM;
    sp+=FRAME_SIZE_NORM;
    sp = sp % POST_FRAME_WORD_NORM;
    asm("trillium vissue_delim end at_jump");
  } while(BH);

#endif


  // Clean up on the vector cores.
#ifdef SCALAR_CORE
  ISSUE_VINST(vector_return_label);
#elif defined VECTOR_CORE
  asm("trillium vissue_delim return vector_return");
  return;
#endif

#ifdef SCALAR_CORE
  DEVEC(devec_0);
  asm volatile("fence\n\t");
  asm("trillium vissue_delim return scalar_return");  // XXX is this real???
  return;
#endif

  // Glue points!
#ifdef SCALAR_CORE
init_label:
  asm("trillium glue_point vector_init");
vec_body_label:
  asm("trillium glue_point vec_body");
vector_return_label:
  asm("trillium glue_point vector_return");
#endif
}

void tril_u_dot_subtract(int mask, DTYPE *a, DTYPE *r, DTYPE *q, 
    int numVectors, int vectorLen, int k, int ptid, int groupId, int numGroups, int vtid) {

  // TODO going to need predictation b/c prob won't be nice factor


#ifdef SCALAR_CORE
  VECTOR_EPOCH(mask);

  // chunk over vector groups
  // ignores all vectors before the current orthonormal one
  int numProjs = numVectors - ( k + 1 );
  int start = ( ( groupId + 0 ) * numProjs ) / numGroups;
  int end   = ( ( groupId + 1 ) * numProjs ) / numGroups;

  // make it a factor of vector group mapping size
  start += k + 1;
  end   += k + 1;
  // start = ( k + 1 ) + roundUp(start, VECTOR_LEN);
  // end   = ( k + 1 ) + roundUp(end  , VECTOR_LEN);

  // printf("%d->%d k %d\n", start, end, k);

  // issue header block
  ISSUE_VINST(init_label);
#endif

#ifdef VECTOR_CORE
  asm("trillium vissue_delim until_next vector_init");
  // chunk over vector groups
  // ignores all vectors before the current orthonormal one
  int numProjs = numVectors - ( k + 1 );
  int start = ( ( groupId + 0 ) * numProjs ) / numGroups;
  int end   = ( ( groupId + 1 ) * numProjs ) / numGroups;

  // make it a factor of vector group mapping size
  start += k + 1;
  end   += k + 1;

  int j = start + vtid;
  j -=VECTOR_LEN;
  int i = 0;
  // int i = 0;
  // DTYPE r_cache = 0.0f;

  int sp = 0;
  DTYPE* sp_ptr = (DTYPE*)getSpAddr(ptid, 0);
#endif

#ifdef SCALAR_CORE
  int sp = 0;

  // TODO remove if's from vector code in favor of more labels here
  for (int j = start; j < end; j+=VECTOR_LEN) {

    // initial prefetch
    int init_i_dist = min(INIT_OFFSET_SUB, vectorLen);
    for (int i = 0; i < init_i_dist; i+=UNROLL_LEN_SUB) {
      prefetch_dot_frame(q, a, i, j, k, numVectors, &sp);
    }

    ISSUE_VINST(vec_body_1_init_label);
    for (int i = init_i_dist; i < vectorLen; i+=UNROLL_LEN_SUB) {
      prefetch_dot_frame(q, a, i, j, k, numVectors, &sp);
      ISSUE_VINST(vec_body_1_label);
    }

    for (int i = vectorLen - init_i_dist; i < vectorLen; i+=UNROLL_LEN_SUB) {
      ISSUE_VINST(vec_body_1_label);
    }

    for (int i = 0; i < init_i_dist; i+=UNROLL_LEN_SUB) {
      prefetch_dot_frame(q, a, i, j, k, numVectors, &sp);
    }

    ISSUE_VINST(vec_body_2_init_label);

    for (int i = init_i_dist; i < vectorLen; i+=UNROLL_LEN_SUB) {
      prefetch_dot_frame(q, a, i, j, k, numVectors, &sp);
      ISSUE_VINST(vec_body_2_label);
    }

    for (int i = vectorLen - init_i_dist; i < vectorLen; i+=UNROLL_LEN_SUB) {
      ISSUE_VINST(vec_body_2_label);
    }

    ISSUE_VINST(vec_body_2_end_label);
  }
#endif

// first inner loop (dot product)
#ifdef VECTOR_CORE
  volatile int WH;
  do {
  // header for first inner loop
  asm("trillium vissue_delim until_next vec_body_1_init");
  i = 0;
  DTYPE r_cache = 0.0f;
  j+=VECTOR_LEN;

  volatile int BH;
  do {
    asm("trillium vissue_delim if_begin vec_body_1");
    // get data regardless of predication
    START_FRAME();

    // predicate loop
    int gt = (j >= end);
    PRED_EQ(gt, 0);
    #pragma GCC unroll(4)
    for (int u = 0; u < UNROLL_LEN_SUB; u++) {
      DTYPE val = sp_ptr[sp + u]  * sp_ptr[sp + UNROLL_LEN_SUB + u];
      r_cache += val;
    }
    PRED_EQ(gt, gt);
    END_FRAME();
    sp = (sp + FRAME_SIZE_SUB) % POST_FRAME_WORD_SUB;
    asm("trillium vissue_delim end at_jump");
  } while(BH);
#endif

// a0 in above block is to spad address
// but a0 below should be to global (not being reset in between)

// second inner loop
#ifdef VECTOR_CORE
  // header for second inner loop
  asm("trillium vissue_delim until_next vec_body_2_init");
  i = 0;

  volatile int BH2;
  do {
    asm("trillium vissue_delim if_begin vec_body_2");

    START_FRAME();

    int gt = (j >= end);
    PRED_EQ(gt, 0);
    #pragma GCC unroll(4)
    for (int u = 0; u < UNROLL_LEN_SUB; u++) {
      DTYPE val = sp_ptr[sp + UNROLL_LEN_SUB + u] - sp_ptr[sp + u] * r_cache;
      STORE_NOACK(val, &a[(i + u) * numVectors + j], 0);
    }
    PRED_EQ(gt, gt);

    END_FRAME();
    sp = (sp + FRAME_SIZE_SUB) % POST_FRAME_WORD_SUB;
    i+=UNROLL_LEN_SUB;

    // DTYPE val = a[i * numVectors + j] - q[i * numVectors + k] * r_cache;

  
    asm("trillium vissue_delim end at_jump");
  } while(BH2);

  asm("trillium vissue_delim until_next vec_body_2_end");


  } while(WH);
#endif


  // Clean up on the vector cores.
#ifdef SCALAR_CORE
  ISSUE_VINST(vector_return_label);
#elif defined VECTOR_CORE
  asm("trillium vissue_delim return vector_return");
  return;
#endif

#ifdef SCALAR_CORE
  DEVEC(devec_0);
  asm volatile("fence\n\t");
  asm("trillium vissue_delim return scalar_return");  // XXX is this real???
  return;
#endif

  // Glue points!
#ifdef SCALAR_CORE
init_label:
  asm("trillium glue_point vector_init");
vec_body_1_init_label:
  asm("trillium glue_point vec_body_1_init");
vec_body_1_label:
  asm("trillium glue_point vec_body_1");
vec_body_2_init_label:
  asm("trillium glue_point vec_body_2_init");
vec_body_2_label:
  asm("trillium glue_point vec_body_2");
vec_body_2_end_label:
  asm("trillium glue_point vec_body_2_end");
vector_return_label:
  asm("trillium glue_point vector_return");
#endif
}

#endif