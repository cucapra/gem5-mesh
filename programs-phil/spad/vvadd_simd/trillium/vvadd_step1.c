#include "vvadd_kernel.h"

// #define SCALAR_CORE
// #define SIMD_BCAST
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


// TODO: codegen w/trillium
/* void vvadd_execute_simd(int mask, DTYPE *a, DTYPE *b, DTYPE *c, int start, int end, int ptid, int vtid, int dim, int is_master) */
/* { */
#pragma trillium vec_simd begin vvadd
  #ifdef SCALAR_CORE
    // enter vector epoch within function, b/c vector-simd can't have control flow
    VECTOR_EPOCH(mask);

    // do a bunch of prefetching in the beginning to get ahead
    int totalIter = (end - start) / dim;
    int numInitFetch = 16;
    int beginIter = min(numInitFetch, totalIter);

    for (int i = 0; i < beginIter; i++)
    {
      // VPREFETCH(spadAddr + i * 2 + 0, a + start + (i * dim), 0);
      // VPREFETCH(spadAddr + i * 2 + 1, b + start + (i * dim), 0);

      VPREFETCH_L(i * 2 + 0, a + start + (i * dim), 0, 4);
      VPREFETCH_R(i * 2 + 0, a + start + (i * dim), 0, 4);
      VPREFETCH_L(i * 2 + 1, b + start + (i * dim), 0, 4);
      VPREFETCH_R(i * 2 + 1, b + start + (i * dim), 0, 4);
    }

    // issue header instructions
    ISSUE_VINST(fable0);

    int localIter = beginIter * 2;

    #ifdef SIMD_BCAST
      int deviceIter = 0;
    #endif

    for (int i = beginIter; i < totalIter; i++)
    {
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
      VPREFETCH_L(localIter + 0, a + start + (i * dim), 0, 4);
      VPREFETCH_R(localIter + 0, a + start + (i * dim), 0, 4);
      VPREFETCH_L(localIter + 1, b + start + (i * dim), 0, 4);
      VPREFETCH_R(localIter + 1, b + start + (i * dim), 0, 4);

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
    }

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
    /* // devec with unique tag */
    /* --- TODO: codegen w/trillium ---
    /* DEVEC(devec_0); */
    /*  */
    /* asm volatile("fence\n\t"); */
    /* asm("scalar_return"); */
    return;

    fable0:
      asm("vector_init");

    fable1:
      asm("vector_body");

    fable2:
      asm("vector_return");


  #elif defined VECTOR_CORE
    asm("vector_init");
    volatile int bh; // while loop variables
    DTYPE a_, b_, c_;
    int64_t iter = 0;
    DTYPE *cPtr = c + start + vtid;
    int *spadAddr = (int *)getSpAddr(ptid, 0);

    while (bh)
    {
      asm("vector_body");
      #ifdef SIMD_BCAST
        // try to get compiler to use register that will recv broadcasted values
        // can make compiler pass
        asm volatile(
            "add %[var], t0, x0\n\t"
            : [ var ] "=r"(iter));
      #endif

      // load values from scratchpad
      LWSPEC(a_, spadAddr + iter, 0);
      LWSPEC(b_, spadAddr + iter + 1, 0);

      // remem as soon as possible, so don't stall loads for next iterations
      // currently need to stall for remem b/c need to issue LWSPEC with a stable remem cnt
      REMEM(0);

      // compute and store
      c_ = a_ + b_;
      STORE_NOACK(c_, cPtr, 0);
      cPtr += dim;

      #ifndef SIMD_BCAST
      iter = (iter + 2) % (NUM_REGIONS * 2);
      #endif
    }
    // TODO: codegen w/trillium
    /* asm("vector_return"); */
    return;
  #endif
#pragma trillium vec_simd end
