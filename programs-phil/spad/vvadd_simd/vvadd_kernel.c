#include "vvadd_kernel.h"


// #define SCALAR_CORE
// #define VECTOR_CORE

#define REGION_SIZE 2
#define NUM_REGIONS 256

inline int min(int a, int b) {
  if (a > b) {
    return b;
  }
  else {
    return a;
  }
}

void vvadd_execute_simd(DTYPE *a, DTYPE *b, DTYPE *c, int start, int end, int ptid, int vtid, int dim, int mask, int is_master) {

  
  int *spadAddr = (int*)getSpAddr(ptid, 0);
  // enter vector epoch within function, b/c vector-simd can't have control flow
  VECTOR_EPOCH(mask);

  #ifdef SCALAR_CORE
  
  // do a bunch of prefetching in the beginning to get ahead
  int totalIter = (end - start) / dim;
  int numInitFetch = 16;
  int beginIter = min(numInitFetch, totalIter);
  for (int i = 0; i < beginIter; i++) {
    VPREFETCH(spadAddr + i * 2 + 0, a + start + (i * dim), 0);
    VPREFETCH(spadAddr + i * 2 + 1, b + start + (i * dim), 0);
  }
  
  // issue header instructions
  ISSUE_VINST(fable0);
  #elif defined VECTOR_CORE
  volatile int bh1,bh2; // while loop variables
  DTYPE a_,b_,c_;
  int64_t iter = 0;
  DTYPE* cPtr = c + start + vtid;
  #endif

  #ifdef SCALAR_CORE
  int localIter = beginIter * 2;

  #ifdef SIMD_BCAST
  int deviceIter = 0;
  #endif
  #endif

  #ifdef SCALAR_CORE
  for (int i = beginIter; i < totalIter; i++) {
  #elif defined VECTOR_CORE
  while (bh1){
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
    VPREFETCH(spadAddr + localIter + 0, a + start + (i * dim), 0);
    VPREFETCH(spadAddr + localIter + 1, b + start + (i * dim), 0);
    localIter+=2;
    if (localIter == (NUM_REGIONS * 2)) {
      localIter = 0;
    }

    #ifdef SIMD_BCAST
    deviceIter+=2;
    if (deviceIter == (NUM_REGIONS * 2)) {
      deviceIter = 0;
    }
    #endif

  #elif defined VECTOR_CORE
    #ifdef SIMD_BCAST
    // try to get compiler to use register that will recv broadcasted values
    // can make compiler pass
    asm volatile(
      "add %[var], t0, x0\n\t"
      : [var] "=r" (iter)
    );
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
  #endif
  }

  // issue the rest
  #ifdef SCALAR_CORE
  for (int i = totalIter - beginIter; i < totalIter; i++) {
  #elif defined VECTOR_CORE
  while (bh2){
  #endif

  #ifdef SCALAR_CORE
    #ifdef SIMD_BCAST
    BROADCAST(t0, deviceIter, 0);
    #endif

    ISSUE_VINST(fable1);

    #ifdef SIMD_BCAST
    deviceIter+=2;
    if (deviceIter == (NUM_REGIONS * 2)) {
      deviceIter = 0;
    }
    #endif
  #elif defined VECTOR_CORE
    #ifdef SIMD_BCAST
    // try to get compiler to use register that will recv broadcasted values
    // can make compiler pass
    asm volatile(
      "add %[var], t0, x0\n\t"
      : [var] "=r" (iter)
    );
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
  #endif
  }

  #ifdef SCALAR_CORE
  // devec with unique tag
  DEVEC(devec_0);
  #endif

  // we are doing lazy store acks, so use this to make sure all stores have commited to memory
  asm volatile("fence\n\t");

  return;

#ifdef SCALAR_CORE
  fable0:
  asm("nop");

  fable1:
  asm("nop");

#endif


/*
  // vector engine code

  // declarations
  DTYPE a_, b_, c_;
  int64_t iter; // avoids sext.w instruction when doing broadcast // TODO maybe should be doing rv32
  DTYPE *cPtr;

  // entry block
  // NOTE need to do own loop-invariant code hoisting?
  fable0:
    iter = 0;
    cPtr = c + start + vtid;
  
  // loop body block
  fable1:
    #ifdef SIMD_BCAST
    // try to get compiler to use register that will recv broadcasted values
    // can make compiler pass
    asm volatile(
      "add %[var], t0, x0\n\t"
      : [var] "=r" (iter)
    );
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

    // need this jump to create loop carry dependencies
    // an assembly pass will remove this instruction
    asm volatile goto("j %l[fable1]\n\t"::::fable1);

    */

  return;
}