#include "vvadd_kernel.h"

// #define SCALAR_CORE
// #define VECTOR_CORE

inline int min(int a, int b)
{
  if (a > b)
  {
    return b;
  }
  else
  {
    return a;
  }
}

inline void prefetch_frame(DTYPE *a, DTYPE *b, int i, int *sp, int dim, int start) {
  // should be a constant from static analysis of dim
  int pRatio = VECTOR_LEN / PREFETCH_LEN;
  
  #ifdef VERTICAL_LOADS
  for (int core = 0; core < dim; core++) {
    int memIdx = start + i * dim + LOAD_LEN * core;
    VPREFETCH_L(*sp + 0       , a + memIdx, core, LOAD_LEN, 1);
    VPREFETCH_L(*sp + LOAD_LEN, b + memIdx, core, LOAD_LEN, 1);
  }
  #elif defined(SPATIAL_UNROLL)
  for (int j = 0; j < LOAD_LEN; j++) {
    for (int p = 0; p < pRatio; p++) {
      VPREFETCH_L(*sp + j * 2 + 0, a + start + ((i + j) * dim + p * PREFETCH_LEN), p * PREFETCH_LEN, PREFETCH_LEN, 0);
      VPREFETCH_L(*sp + j * 2 + 1, b + start + ((i + j) * dim + p * PREFETCH_LEN), p * PREFETCH_LEN, PREFETCH_LEN, 0);
    }
  }
  #else
  #if PREFETCH_LEN != VECTOR_LEN
  for (int p = 0; p < pRatio; p++) {
    VPREFETCH_L(*sp + 0, a + start + (i * dim + p * PREFETCH_LEN), p * PREFETCH_LEN, PREFETCH_LEN, 0);
    VPREFETCH_L(*sp + 1, b + start + (i * dim + p * PREFETCH_LEN), p * PREFETCH_LEN, PREFETCH_LEN, 0);
  }
  #else
  VPREFETCH_L(*sp + 0, a + start + (i * dim), 0, PREFETCH_LEN, 0);
  VPREFETCH_L(*sp + 1, b + start + (i * dim), 0, PREFETCH_LEN, 0);
  #endif
  #endif

  (*sp)+=REGION_SIZE;
  if (*sp == POST_REGION_WORD) *sp = 0;
}

inline void vvadd_body(DTYPE *spadAddr, DTYPE **cPtr, int *sp, int dim) {
      // unrolled version when doing vertical loads
    #ifdef VERTICAL_LOADS
    FRAME_START(REGION_SIZE);

    // load values from scratchpad
    #pragma GCC unroll(16)
    for (int i = 0; i < LOAD_LEN; i++) {
      DTYPE a = spadAddr[*sp + i + 0];
      DTYPE b = spadAddr[*sp + i + LOAD_LEN];

      // compute and store
      DTYPE c = a + b;
      STORE_NOACK(c, *cPtr + i, 0);
    }

    REMEM(REGION_SIZE);

    (*cPtr) += LOAD_LEN * dim;
    *sp = (*sp + REGION_SIZE) % POST_REGION_WORD;
    #elif defined(SPATIAL_UNROLL)
    FRAME_START(REGION_SIZE);

    // load values from scratchpad
    #pragma GCC unroll(16)
    for (int i = 0; i < LOAD_LEN; i++) {
      DTYPE a = spadAddr[*sp + i * 2 + 0];
      DTYPE b = spadAddr[*sp + i * 2 + 1];

      // compute and store
      DTYPE c = a + b;
      STORE_NOACK(c, *cPtr + i * dim, 0);
    }

    REMEM(REGION_SIZE);

    (*cPtr) += LOAD_LEN * dim;
    *sp = (*sp + REGION_SIZE) % POST_REGION_WORD;
    #else

    FRAME_START(REGION_SIZE);

    // load values from scratchpad
    DTYPE a = spadAddr[*sp + 0];
    DTYPE b = spadAddr[*sp + 1];

    // remem as soon as possible, so don't stall loads for next iterations
    // currently need to stall for remem b/c need to issue LWSPEC with a stable remem cnt
    REMEM(REGION_SIZE);

    // compute and store
    DTYPE c = a + b;
    STORE_NOACK(c, *cPtr, 0);
    (*cPtr) += dim;
    *sp = (*sp + REGION_SIZE) % POST_REGION_WORD;

    #endif
}

void tril_vvadd(int mask, DTYPE *a, DTYPE *b, DTYPE *c, int start, int end, int ptid, int vtid, int dim, int is_master)
{

  int prefetchFrames = INIT_FRAMES; // BE carful about prefetching, this + queue size >= num hardware frames

  #if defined(VERTICAL_LOADS) || defined(SPATIAL_UNROLL)
  int numInitFetch = LOAD_LEN * prefetchFrames;
  int step = LOAD_LEN;
  #else
  int numInitFetch = prefetchFrames;
  int step = 1;
  #endif
  
  int sp = 0;

#ifdef SCALAR_CORE
  // enter vector epoch within function, b/c vector-simd can't have control flow
  VECTOR_EPOCH(mask);

  // do a bunch of prefetching in the beginning to get ahead
  int totalIter = (end - start) / dim;
  int beginIter = min(numInitFetch, totalIter);
  for (int i = 0; i < beginIter; i+=step)
  {
    prefetch_frame(a, b, i, &sp, dim, start);
  }

  // issue header instructions
  ISSUE_VINST(vector_init_label);
#elif defined VECTOR_CORE
  asm("trillium vissue_delim until_next vector_init");
  #ifdef VERTICAL_LOADS
  DTYPE *cPtr = c + start + vtid * LOAD_LEN;
  #else
  DTYPE *cPtr = c + start + vtid;
  #endif
  int *spadAddr = (int *)getSpAddr(ptid, 0);
#endif

#ifdef SCALAR_CORE
  for (int i = beginIter; i < totalIter; i+=step)
  {
#elif defined VECTOR_CORE
  volatile int bh1;
  while (bh1)
  {
#endif

#ifdef SCALAR_CORE
    // prefetch for future iterations
    prefetch_frame(a, b, i, &sp, dim, start);

    ISSUE_VINST(vector_body_label);

#elif defined VECTOR_CORE
  asm("trillium vissue_delim until_next vector_body");
    vvadd_body(spadAddr, &cPtr, &sp, dim);
#endif
  }

#ifdef SCALAR_CORE
// issue the rest
  for (int i = totalIter - beginIter; i < totalIter; i+=step)
  {
    ISSUE_VINST(vector_body_label);
  }

  ISSUE_VINST(vector_return_label);
  // devec with unique tag
  DEVEC(devec_0);
  // we are doing lazy store acks, so use this to make sure all stores have commited to memory
  asm volatile("fence\n\t");
  asm("trillium vissue_delim return scalar_return");
  return;
vector_init_label:
  asm("trillium glue_point vector_init");

vector_body_label:
  asm("trillium glue_point vector_body");

vector_return_label:
  asm("trillium glue_point vector_return");

#elif defined VECTOR_CORE

asm("trillium vissue_delim return vector_return");
return;

#endif

}
