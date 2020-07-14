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
  *sp = *sp + FRAME_SIZE_NORM;
  if (*sp == POST_FRAME_WORD_NORM) *sp = 0;
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
    DTYPE val =  sp_ptr[sp] / r_cache;
    END_FRAME();
    STORE_NOACK(val, &q[i * numVectors + k], 0);
    i+=VECTOR_LEN;
    sp+=FRAME_SIZE_NORM;
    if (sp == POST_FRAME_WORD_NORM) sp = 0;
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


inline void prefetch_dot_frame(DTYPE *q, DTYPE *a, int i, int j, int k, int numVectors, int *sp) {
  // fetch the same q to each core
  for (int core = 0; core < VECTOR_LEN; core++) {
    VPREFETCH_L(*sp + 0, &q[i * numVectors + k], core, 1, VERTICAL);
  }

  VPREFETCH_L(*sp + 1, &a[i * numVectors + j], 0, VECTOR_LEN, HORIZONTAL);
  VPREFETCH_R(*sp + 1, &a[i * numVectors + j], 0, VECTOR_LEN, HORIZONTAL);
  

// q[i * numVectors + k] * a[i * numVectors + j];

  *sp = *sp + FRAME_SIZE_SUB;
  if (*sp == POST_FRAME_WORD_SUB) *sp = 0;
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
    ISSUE_VINST(vec_body_1_init_label);
    for (int i = 0; i < vectorLen; i++) {
      prefetch_dot_frame(q, a, i, j, k, numVectors, &sp);
      ISSUE_VINST(vec_body_1_label);
    }
    ISSUE_VINST(vec_body_2_init_label);
    for (int i = 0; i < vectorLen; i++) {
      prefetch_dot_frame(q, a, i, j, k, numVectors, &sp);
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
    // TODO PRED_GRE

    // get data regardless of predication
    START_FRAME();
    // volatile needed or else does fmadd with r_cache += val
    volatile DTYPE val = sp_ptr[sp + 0]  * sp_ptr[sp + 1];
    // volatile DTYPE val = q[i * numVectors + k] * a[i * numVectors + j];
    // volatile DTYPE val = q[i * numVectors + k] * sp_ptr[sp + 1];
    END_FRAME();

    sp = (sp + FRAME_SIZE_SUB) % POST_FRAME_WORD_SUB;

    // volatile int j_ = j;
    int gt = (j >= end);
    PRED_EQ(gt, 0);
    // if (BH) {
    // r_cache += q[i * numVectors + k] * a[i * numVectors + j];
    r_cache += val;
    // i++;

    // PRED_EQ(i, vectorLen);
    // i = 0; // DCE on this and above b/c always 0
    // PRED_EQ(i, i);
    // if (i == vectorLen) i = 0;
    // }
    PRED_EQ(gt, gt);

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
    volatile DTYPE val = sp_ptr[sp + 1] - sp_ptr[sp + 0] * r_cache;
    END_FRAME();
    sp = (sp + FRAME_SIZE_SUB) % POST_FRAME_WORD_SUB;

    // volatile int j_ = j;
    int gt = (j >= end);
    PRED_EQ(gt, 0);
    // if (BH2) {
    // DTYPE val = a[i * numVectors + j] - q[i * numVectors + k] * r_cache;
    STORE_NOACK(val, &a[i * numVectors + j], 0);
    i++;
    // PRED_EQ(i, vectorLen);
    // i = 0;
    // r_cache = 0.0f;
    // j+=VECTOR_LEN;
    // PRED_EQ(j, j);
    // if (i == vectorLen) {
    //   i = 0;
    //   r_cache = 0.0f;
    //   j+=VECTOR_LEN;
    // }
    // }
    PRED_EQ(i, i);
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