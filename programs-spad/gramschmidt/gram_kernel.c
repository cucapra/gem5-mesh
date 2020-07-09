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
#endif

#ifdef SCALAR_CORE
  // TODO prefetch
  for (int i = start; i < end; i+=VECTOR_LEN) {
    ISSUE_VINST(vec_body_label);
  }
#endif

#ifdef VECTOR_CORE
  volatile int BH;
  do {
    asm("trillium vissue_delim if_begin vec_body");
    q[i * numVectors + k] = a[i * numVectors + k] / r_cache;
    // TODO consider ST_NOACK
    i+=VECTOR_LEN;
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
  int numProjs = numVectors - ( k + 1 );
  int start = ( ( groupId + 0 ) * numProjs ) / numGroups;
  int end   = ( ( groupId + 1 ) * numProjs ) / numGroups;

  // make it a factor of vector group mapping size
  // first term ignores all vectors before the current orthonormal one
  start = ( k + 1 ) + roundUp(start, VECTOR_LEN);
  end   = ( k + 1 ) + roundUp(end  , VECTOR_LEN);

  // issue header block
  ISSUE_VINST(init_label);
#endif

#ifdef VECTOR_CORE
  asm("trillium vissue_delim until_next vector_init");
  int numProjs = numVectors - ( k + 1 );
  int start = ( ( groupId + 0 ) * numProjs ) / numGroups;
  start = (k + 1) + roundUp(start, VECTOR_LEN);
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

// first inner loop
#ifdef VECTOR_CORE
  volatile int BH;
  do {
    asm("trillium vissue_delim if_begin vec_body_1");
    r_cache += q[i * numVectors + k] * a[i * numVectors + j];
    i++;
    if (i == vectorLen) i = 0;
    asm("trillium vissue_delim end at_jump");
  } while(BH);
#endif

// second inner loop
#ifdef VECTOR_CORE
  volatile int BH2;
  do {
    asm("trillium vissue_delim if_begin vec_body_2");
    DTYPE val = a[i * numVectors + j] - q[i * numVectors + k] * r_cache;
    STORE_NOACK(val, &a[i * numVectors + j], 0);
    i++;
    if (i == vectorLen) {
      i = 0;
      r_cache = 0.0f;
      j+=VECTOR_LEN;
    }
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