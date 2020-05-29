#include "vvadd_kernel.h"

// #define SCALAR_CORE
// #define VECTOR_CORE

#define REGION_SIZE 2
#define NUM_REGIONS 256

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

void vvadd_execute_simd(int mask, DTYPE *a, DTYPE *b, DTYPE *c, int start, int end, int ptid, int vtid, int dim, int is_master)
{

#ifdef SCALAR_CORE
  // enter vector epoch within function, b/c vector-simd can't have control flow
  VECTOR_EPOCH(mask);


  // do a bunch of prefetching in the beginning to get ahead
  int totalIter = (end - start) / dim;
  int numInitFetch = 16;
  int beginIter = min(numInitFetch, totalIter);
  for (int i = 0; i < beginIter; i++)
  {

    VPREFETCH_L(i * 2 + 0, a + start + (i * dim), 0, 4, 0);
    VPREFETCH_R(i * 2 + 0, a + start + (i * dim), 0, 4, 0);
    VPREFETCH_L(i * 2 + 1, b + start + (i * dim), 0, 4, 0);
    VPREFETCH_R(i * 2 + 1, b + start + (i * dim), 0, 4, 0);
  }

  // issue header instructions
  ISSUE_VINST(fable0);
#elif defined VECTOR_CORE
  asm("trillium vissue_delim until_next vector_init");
  volatile int bh1, bh2; // while loop variables
  DTYPE a_, b_, c_;
  int64_t iter = 0;
  DTYPE *cPtr = c + start + vtid;
  int *spadAddr = (int *)getSpAddr(ptid, 0);
#endif

#ifdef SCALAR_CORE
  int localIter = beginIter * 2;

#ifdef SIMD_BCAST
  int deviceIter = 0;
#endif
#endif

#ifdef SCALAR_CORE
  for (int i = beginIter; i < totalIter; i++)
  {
#elif defined VECTOR_CORE
  while (bh1)
  {
#endif

#ifdef SCALAR_CORE
#ifdef SIMD_BCAST
    // broadcast values needed to execute
    // in this case the spad loc
    BROADCAST(t0, deviceIter, 0);
#endif

    // issue fable1
    ISSUE_VINST(fable1);

    // prefetch for future iterations
    // VPREFETCH(spadAddr + localIter + 0, a + start + (i * dim), 0);
    // VPREFETCH(spadAddr + localIter + 1, b + start + (i * dim), 0);
    VPREFETCH_L(localIter + 0, a + start + (i * dim), 0, 4, 0);
    VPREFETCH_R(localIter + 0, a + start + (i * dim), 0, 4, 0);
    VPREFETCH_L(localIter + 1, b + start + (i * dim), 0, 4, 0);
    VPREFETCH_R(localIter + 1, b + start + (i * dim), 0, 4, 0);

    localIter += 2;
    if (localIter == (NUM_REGIONS * 2))
    {
      localIter = 0;
    }

#ifdef SIMD_BCAST
    deviceIter += 2;
    if (deviceIter == (NUM_REGIONS * 2))
    {
      deviceIter = 0;
    }
#endif

#elif defined VECTOR_CORE
  asm("trillium vissue_delim until_next vector_body");
#ifdef SIMD_BCAST
    // try to get compiler to use register that will recv broadcasted values
    // can make compiler pass
    asm volatile(
        "add %[var], t0, x0\n\t"
        : [ var ] "=r"(iter));
#endif

    FRAME_START(REGION_SIZE);
    // load values from scratchpad
    // LWSPEC(a_, spadAddr + iter, 0);
    // LWSPEC(b_, spadAddr + iter + 1, 0);

    a_ = *(spadAddr + iter);
    b_ = *(spadAddr + iter + 1);

    // remem as soon as possible, so don't stall loads for next iterations
    // currently need to stall for remem b/c need to issue LWSPEC with a stable remem cnt
    REMEM(REGION_SIZE);

    // compute and store
    c_ = a_ + b_;
    STORE_NOACK(c_, cPtr, 0);
    cPtr += dim;

#ifndef SIMD_BCAST
    iter = (iter + 2) % (NUM_REGIONS * 2);
#endif
#endif
  }

#ifdef SCALAR_CORE
// issue the rest
  for (int i = totalIter - beginIter; i < totalIter; i++)
  {

#ifdef SIMD_BCAST
    BROADCAST(t0, deviceIter, 0);
#endif

    ISSUE_VINST(fable1);

#ifdef SIMD_BCAST
    deviceIter += 2;
    if (deviceIter == (NUM_REGIONS * 2))
    {
      deviceIter = 0;
    }
#endif
  }

  ISSUE_VINST(fable2);
  // devec with unique tag
  DEVEC(devec_0);
  // we are doing lazy store acks, so use this to make sure all stores have commited to memory
  asm volatile("fence\n\t");
  asm("trillium vissue_delim return scalar_return");
  return;
fable0:
  asm("trillium glue_point vector_init");

fable1:
  asm("trillium glue_point vector_body");

fable2:
  asm("trillium glue_point vector_return");

#elif defined VECTOR_CORE

asm("trillium vissue_delim return vector_return");
return;

#endif

}
