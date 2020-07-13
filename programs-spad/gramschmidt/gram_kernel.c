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

inline void prefetch_normalize_frame(DTYPE *a, int i, int k, int numVectors, int *sp) {
  for (int core = 0; core < VECTOR_LEN; core++) {
    VPREFETCH_L(*sp, &a[(i + core) * numVectors + k], core, 1, VERTICAL);
  }
  *sp = *sp + 1;
  if (*sp == POST_FRAME_WORD) *sp = 0;
}


void tril_u_normalize(int mask, DTYPE *a, DTYPE *r, DTYPE *q, 
    int numVectors, int vectorLen, int k, int ptid, int groupId, int numGroups, int vtid) {


#ifdef SCALAR_CORE
  VECTOR_EPOCH(mask);

  // chunk over vector gorups
  int start = ((groupId + 0) * vectorLen) / numGroups;
  int end   = ((groupId + 1) * vectorLen) / numGroups;

  // make it a factor of vector group mapping size
  start = roundUp(start, VECTOR_LEN);
  end   = roundUp(end  , VECTOR_LEN);

  // issue header block
  ISSUE_VINST(init_label);
#endif

#ifdef VECTOR_CORE
  asm("trillium vissue_delim until_next vector_init");
  int start = ((groupId + 0) * vectorLen) / numGroups;
  start = roundUp(start, VECTOR_LEN);
  int i = start + vtid;
  DTYPE r_cache = r[k * numVectors + k];
  int sp = 0;
  DTYPE* sp_ptr = (DTYPE*)getSpAddr(ptid, 0);
#endif

#ifdef SCALAR_CORE
  int sp = 0;
  // TODO prefetch
  for (int i = start; i < end; i+=VECTOR_LEN) {
    prefetch_normalize_frame(a, i, k, numVectors, &sp);
    ISSUE_VINST(vec_body_label);
  }
#endif

#ifdef VECTOR_CORE
  volatile int BH;
  do {
    asm("trillium vissue_delim if_begin vec_body");
    // q[i * numVectors + k] = a[i * numVectors + k] / r_cache;
    START_FRAME();
    DTYPE val = sp_ptr[sp] / r_cache;
    END_FRAME();
    STORE_NOACK(val, &q[i * numVectors + k], 0);
    i+=VECTOR_LEN;
    sp++;
    if (sp == POST_FRAME_WORD) sp = 0;
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
  int i = 0;
  DTYPE r_cache = 0.0f;
#endif

#ifdef SCALAR_CORE
  // TODO prefetch
  // TODO remove if's from vector code in favor of more labels here
  for (int j = start; j < end; j+=VECTOR_LEN) {
    for (int i = 0; i < vectorLen; i++) {
      ISSUE_VINST(vec_body_1_label);
    }
    for (int i = 0; i < vectorLen; i++) {
      ISSUE_VINST(vec_body_2_label);
    }
  }
#endif

// first inner loop (dot product)
#ifdef VECTOR_CORE
  volatile int BH;
  do {
    asm("trillium vissue_delim if_begin vec_body_1");
    // TODO PRED_GRE
    int gt = (j >= end);
    PRED_EQ(gt, 0);
    r_cache += q[i * numVectors + k] * a[i * numVectors + j];
    i++;
    // PRED_EQ(i, vectorLen);
    // i = 0; // DCE on this and above b/c always 0
    // PRED_EQ(i, i);
    if (i == vectorLen) i = 0;
    PRED_EQ(i, i);
    asm("trillium vissue_delim end at_jump");
  } while(BH);
#endif

// second inner loop
#ifdef VECTOR_CORE
  volatile int BH2;
  do {
    asm("trillium vissue_delim if_begin vec_body_2");
    int gt = (j >= end);
    PRED_EQ(gt, 0);
    DTYPE val = a[i * numVectors + j] - q[i * numVectors + k] * r_cache;
    STORE_NOACK(val, &a[i * numVectors + j], 0);
    i++;
    // PRED_EQ(i, vectorLen);
    // i = 0;
    // r_cache = 0.0f;
    // j+=VECTOR_LEN;
    // PRED_EQ(j, j);
    if (i == vectorLen) {
      i = 0;
      r_cache = 0.0f;
      j+=VECTOR_LEN;
    }
    PRED_EQ(j, j);
    asm("trillium vissue_delim end at_jump");
  } while(BH2);
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
vec_body_1_label:
  asm("trillium glue_point vec_body_1");
vec_body_2_label:
  asm("trillium glue_point vec_body_2");
vector_return_label:
  asm("trillium glue_point vector_return");
#endif
}

#endif