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

/*-----------------------------------------------------------------------------------
 * Vector versions of the kernels. Same memory access pattern as baseline but do prefetching
 *---------------------------------------------------------------------------------*/

#ifdef USE_VEC

void tril_u_normalize(int mask, DTYPE *a, DTYPE *r, DTYPE *q, 
    int numVectors, int vectorLen, int k, int ptid, int groupId, int numGroups, int vtid) {

  // chunk over vector gorups
  int start = ((groupId + 0) * vectorLen) / numGroups;
  int end   = ((groupId + 1) * vectorLen) / numGroups;

  // make it a factor of vector group mapping size
  start = roundUp(start, VECTOR_LEN);
  end   = roundUp(end  , VECTOR_LEN);

  // prevents code from being reordered :|
  volatile int ohjeez = 1;
  if (ohjeez) {

  // goto vector mode
  VECTOR_EPOCH(mask);
  
  // issue header block
  ISSUE_VINST(fable0);

  // issue loop body block
  for (int i = start; i < end; i+=VECTOR_LEN) {
    ISSUE_VINST(fable1);
  }

  // devec with unique tag
  DEVEC(devec_0);

  asm volatile("nop\n\t");

  // we are doing lazy store acks, so use this to make sure all stores have commited to memory
  asm volatile("fence\n\t");
  return;
  }

  // vector engine code

  // declarations
  int i;
  DTYPE *qPtr, *aPtr;
  DTYPE r_cache;

  // header
  fable0:
    i = start + vtid;
    // aPtr = a + (start + vtid) * numVectors + k;
    // qPtr = q + (start + vtid) * numVectors + k;
    // make sure r is cached
    r_cache = r[k * numVectors + k];

  // body
  fable1:
    q[i * numVectors + k] = a[i * numVectors + k] / r_cache;
    // TODO consider ST_NOACK
    i+=VECTOR_LEN;
    asm volatile goto("j %l[fable1]\n\t"::::fable1);
}

void tril_u_dot_subtract(int mask, DTYPE *a, DTYPE *r, DTYPE *q, 
    int numVectors, int vectorLen, int k, int ptid, int groupId, int numGroups, int vtid) {

  // chunk over vector groups
  int numProjs = numVectors - ( k + 1 );
  int start = ( ( groupId + 0 ) * numProjs ) / numGroups;
  int end   = ( ( groupId + 1 ) * numProjs ) / numGroups;

  // make it a factor of vector group mapping size
  start = roundUp(start, VECTOR_LEN);
  end   = roundUp(end  , VECTOR_LEN);

  // ignore all vectors before the current orthonormal one
  start += k + 1;
  end   += k + 1;

  // prevents code from being reordered :|
  volatile int ohjeez = 1;
  if (ohjeez) {

  // goto vector mode
  VECTOR_EPOCH(mask);
  
  // issue header block
  ISSUE_VINST(fable2);

  // issue loop body block
  for (int j = start; j < end; j+=VECTOR_LEN) {

    // reset dot prod accum and init dot interator i
    ISSUE_VINST(fable3);

    // dot product
    for (int i = 0; i < vectorLen; i++) {
      ISSUE_VINST(fable4);
    }

    // init subtract iterator i
    ISSUE_VINST(fable5);

    // substract
    for (int i = 0; i < vectorLen; i++) {
      ISSUE_VINST(fable6);
    }

    // increment j
    ISSUE_VINST(fable7);

  }

  // devec with unique tag
  DEVEC(devec_1);

  asm volatile("nop\n\t");

  // we are doing lazy store acks, so use this to make sure all stores have commited to memory
  asm volatile("fence\n\t");
  return;
  }

  // vector engine code

  // declarations
  int i, j; // iterators
  // DTYPE *qPtr, *aPtr;
  DTYPE r_cache;

  // outer loop header
  fable2:
    j = start + vtid;

  // init dot product header
  fable3:
    r_cache = 0.0f;
    i = 0;

  // dot product loop
  fable4:
    r_cache += q[i * numVectors + k] * a[i * numVectors + j];
    asm volatile goto("j %l[fable4]\n\t"::::fable4);

  // init substract loop
  fable5:
    i = 0;

  // subtract loop
  fable6:
    a[i * numVectors + j] -= q[i * numVectors + k] * r_cache;
    asm volatile goto("j %l[fable6]\n\t"::::fable6);

  // end outer loop
  fable7:
    j += VECTOR_LEN;
    asm volatile goto("j %l[fable7]\n\t"::::fable7);
}

#endif