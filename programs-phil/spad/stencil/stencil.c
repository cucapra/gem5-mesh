#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

#include "pthread_launch.h"
#include "stencil.h"
#include "spad.h"
#include "../../common/bind_defs.h"

/*
  3x3 stencil with a single 3x3 filter
  Filter is cached in each spad.
  9 values are prefetched per frame
  Parallelize innermost loops (unrolled) so we can get away with codegen trick
*/

#define CACHELINE_WORDS 16

// one of these should be defined to dictate config
// #define NO_VEC 1
#define VEC_4_SIMD 1
// #define VEC_4_SIMD_BCAST 1

// vvadd_execute config directives
#if defined(VEC_4_SIMD) || defined(VEC_4_SIMD_BCAST)
#define USE_VEC 1
#endif
#if defined(VEC_4_SIMD) || defined(VEC_4_SIMD_BCAST)
#define USE_VECTOR_SIMD 1
#endif

// vector grouping directives
#if defined(VEC_4_SIMD) || defined(VEC_4_SIMD_BCAST)
#define VEC_SIZE_4_SIMD 1
#endif

// prefetch sizings
#if defined(VEC_4_DA) || defined(NO_VEC_DA) || defined(VEC_16_UNROLL) || defined(VEC_4_UNROLL) || defined(VEC_16_UNROLL_SERIAL) \
 || defined(SIM_DA_VLOAD_SIZE_1)
#define REGION_SIZE 32
#define NUM_REGIONS 16
#elif defined(USE_VECTOR_SIMD)
#define REGION_SIZE 3
#define NUM_REGIONS 256
#define POST_REGION_WORD NUM_REGIONS * REGION_SIZE
#endif

// https://stackoverflow.com/questions/3407012/c-rounding-up-to-the-nearest-multiple-of-a-number
int roundUp(int numToRound, int multiple) {
  if (multiple == 0) {
    return numToRound;
  }

  int remainder = abs(numToRound) % multiple;
  if (remainder == 0) {
    return numToRound;
  }

  if (numToRound < 0) {
    return -(abs(numToRound) - remainder);
  }
  else {
    return numToRound + multiple - remainder;
  }
}

inline int min(int a, int b) {
  if (a > b) {
    return b;
  }
  else {
    return a;
  }
}

// NOTE optimize("-fno-inline") prevents return block from being at the end, which is kind of needed for the scheme
// ACTUALLY any second label causes a problem???
#ifdef USE_VECTOR_SIMD
void __attribute__((optimize("-fno-reorder-blocks")))
stencil(
    DTYPE *a, DTYPE *b, DTYPE *c, int nrows, int ncols,
    int ptid, int vtid, int dim, int mask, int is_master) {

  int *spadAddr = (int*)getSpAddr(ptid, 0);

  // enter vector epoch within function, b/c vector-simd can't have control flow
  VECTOR_EPOCH(mask);

  // have r = 0 for now
  // will need broadcast to support r > 0
  int spadIdx = 0;

  ISSUE_VINST(fable0);

  // do initial batch of prefetching
  int prefetchFrames = 8;
  int beginCol = min(prefetchFrames * dim, ncols);
  for (int r = 0; r < nrows - (FILTER_DIM - 1); r++) {
    for (int c = 0; c < beginCol; c+=dim) {
      // // manually unroll how to prefetch over a cacheline?
      // int aIdx = r * ncols + c;
      // // k1 = 0
      // VPREFETCH(spadAddr + spadIdx, a + aIdx, 0, 4);
      // spadIdx++;
      // aIdx++;
      // VPREFETCH(spadAddr + spadIdx, a + aIdx, 0, 4);
      // spadIdx++;
      // aIdx++;
      // VPREFETCH(spadAddr + spadIdx, a + aIdx, 0, 4);
      // spadIdx++;
      // aIdx+=ncols;
      // // k1 = 1
      // VPREFETCH(spadAddr + spadIdx, a + aIdx, 0, 4);
      // spadIdx++;
      // aIdx++;
      // VPREFETCH(spadAddr + spadIdx, a + aIdx, 0, 4);
      // spadIdx++;
      // aIdx++;
      // VPREFETCH(spadAddr + spadIdx, a + aIdx, 0, 4);
      // spadIdx++;
      // aIdx+=ncols;
      // // k1 = 2
      // VPREFETCH(spadAddr + spadIdx, a + aIdx, 0, 4);
      // spadIdx++;
      // aIdx++;
      // VPREFETCH(spadAddr + spadIdx, a + aIdx, 0, 4);
      // spadIdx++;
      // aIdx++;
      // VPREFETCH(spadAddr + spadIdx, a + aIdx, 0, 4);
      // spadIdx++;
      // aIdx+=ncols;



      for (int k1 = 0; k1 < FILTER_DIM; k1++) {
        for (int k2 = 0; k2 < 1; k2++) {
          int aIdx = (r + k1) * ncols + (c + k2);
          // TODO Can't have variable load offsets...
          // Seems like might want to make this an rtype instruction???
          // Need to unroll 
          if (k2 == 0) {
            // VPREFETCH(spadAddr + spadIdx, a + aIdx, 0, 4);
            VPREFETCH(spadAddr + spadIdx, a + aIdx + 0, 0, 1);
            VPREFETCH(spadAddr + spadIdx, a + aIdx + 1, 1, 2);
            VPREFETCH(spadAddr + spadIdx, a + aIdx + 2, 2, 3);
            VPREFETCH(spadAddr + spadIdx, a + aIdx + 3, 3, 4);
          }
          else {
            uint32_t baseCacheLinePos = (c % CACHELINE_WORDS) + k2;
            int overShoot = (baseCacheLinePos + dim) - CACHELINE_WORDS;
            // can't have variables as vprefetch settings b/c takes immediate!
            // although these are induction variables so if unroll can get them in
            // if (overShoot > 0) {
            //   VPREFETCH(spadAddr + spadIdx, a + aIdx, 0, 4 - overShoot);
            //   VPREFETCH(spadAddr + spadIdx, a + aIdx + overShoot, 4 - overShoot, 4);
            // }
            // printf("c %d, k2 %d endPos %d overshoot %d\n", c, k2, baseCacheLinePos + dim, overShoot);
            // instead have to have one of these for every single vec length
            // also very reliant on cacheline alignment i.e. row ends at factor of 16
            if (overShoot <= 0) {
              VPREFETCH(spadAddr + spadIdx, a + aIdx, 0, 4);
            }
            else if (overShoot == 1) {
              VPREFETCH(spadAddr + spadIdx, a + aIdx, 0, 3);
              VPREFETCH(spadAddr + spadIdx, a + aIdx + 3, 3, 4);
            }
            else if (overShoot == 2) {
              VPREFETCH(spadAddr + spadIdx, a + aIdx, 0, 2);
              VPREFETCH(spadAddr + spadIdx, a + aIdx + 2, 2, 4);
            }
            else /*if (overShoot == 3)*/ {
              VPREFETCH(spadAddr + spadIdx, a + aIdx, 0, 1);
              VPREFETCH(spadAddr + spadIdx, a + aIdx + 1, 1, 4);
            }
          }
          spadIdx++;
        }
      }
    }
  }

  for (int r = 0; r < nrows - (FILTER_DIM - 1); r++) {
    for (int c = beginCol; c < ncols /*- (FILTER_DIM - 1)*/; c+=dim) {
      // prefetch all 9 values required for computation
      // prevent unroll b/c doesnt work well if VISSUE in the loop
      // #pragma GCC unroll 0
      // for (int k1 = 0; k1 < FILTER_DIM; k1++) {
      //   #pragma GCC unroll 0
      //   for (int k2 = 0; k2 < FILTER_DIM; k2++) {
      //     int aIdx = (r + k1) * ncols + (c + k2);
      //     // TODO can't handle when this inevitably goes off cacheline
      //     VPREFETCH(spadAddr + spadIdx, a + aIdx, 0);

      //     // spad is circular buffer so do cheap mod here
      //     spadIdx++;
      //     if (spadIdx == POST_REGION_WORD) {
      //       spadIdx = 0;
      //     }
      //   }
      // }

      // just do stencil with 3 values prefetched by row
      for (int k1 = 0; k1 < FILTER_DIM; k1++) {
        for (int k2 = 0; k2 < 1; k2++) {
          int aIdx = (r + k1) * ncols + (c + k2);
          // TODO can't handle when this inevitably goes off cacheline
          VPREFETCH(spadAddr + spadIdx, a + aIdx, 0, 4);

          // spad is circular buffer so do cheap mod here
          spadIdx++;
          if (spadIdx == POST_REGION_WORD) {
            spadIdx = 0;
          }
        }
      }

      ISSUE_VINST(fable1);
    }
  }

  // issue the rest of blocks
  for (int r = 0; r < nrows - (FILTER_DIM - 1); r++) {
    for (int c = ncols  /*- (FILTER_DIM - 1)*/ - beginCol; c < ncols /*- (FILTER_DIM - 1)*/; c+=dim) {
      ISSUE_VINST(fable1);
    }
  }

  // devec with unique tag
  DEVEC(devec_0);

  // we are doing lazy store acks, so use this to make sure all stores have commited to memory
  asm volatile("fence\n\t");

  return;

  // vector engine code

  // declarations
  DTYPE a_, b_, c_;
  int64_t iter; // avoids sext.w instruction when doing broadcast // TODO maybe should be doing rv32
  DTYPE *cPtr;
  DTYPE b0, b1, b2, b3, b4, b5, b6, b7, b8;

  // entry block
  // load the full filter into spad
  fable0:
  //   #pragma GCC unroll 9
  //   for (int i = 0; i < FILTER_DIM * FILTER_DIM; i++) {
  //     spadAddr[POST_REGION_WORD + i] = b[i];
  //     // b_ = b[i]; // keep filter in regfile
  //   }
  // fable0b:
    iter = 0;
    cPtr = c + /*(startRow * ncols + startCol)*/ + vtid;
    // b0 = b[0];
    // b1 = b[1];
    // b2 = b[2];
    // b3 = b[3];
    // b4 = b[4];
    // b5 = b[5];
    // b6 = b[6];
    // b7 = b[7];
    // b8 = b[8];
    b0 = b[0];
    b1 = b[3];
    b2 = b[6];
    
  
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

    c_ = 0;
    // #pragma GCC unroll 9
    // for (int i = 0; i < FILTER_DIM * FILTER_DIM; i++) {
    //   LWSPEC(a_, spadAddr + i, 0);
    //   b_ = spadAddr[POST_REGION_WORD + i];
    //   c_ += a_ * b_;
    // }
    LWSPEC(a_, spadAddr + (iter * 3) + 0, 0);
    c_ += b0 * a_;
    LWSPEC(a_, spadAddr + (iter * 3) + 1, 0);
    c_ += b1 * a_;
    LWSPEC(a_, spadAddr + (iter * 3) + 2, 0);
    c_ += b2 * a_;
    // LWSPEC(a_, spadAddr + 3, 0);
    // c_ += b3 * a_;
    // LWSPEC(a_, spadAddr + 4, 0);
    // c_ += b4 * a_;
    // LWSPEC(a_, spadAddr + 5, 0);
    // c_ += b5 * a_;
    // LWSPEC(a_, spadAddr + 6, 0);
    // c_ += b6 * a_;
    // LWSPEC(a_, spadAddr + 7, 0);
    // c_ += b7 * a_;
    // LWSPEC(a_, spadAddr + 8, 0);
    // c_ += b8 * a_;

    REMEM(0);
    STORE_NOACK(c_, cPtr, 0);
    // do no reuse version for now
    cPtr += dim;
    iter = (iter + 1) % (NUM_REGIONS);
    

    // need this jump to create loop carry dependencies
    // an assembly pass will remove this instruction
    asm volatile goto("j %l[fable1]\n\t"::::fable1);
  
  return;
}
#else
void /*__attribute__((optimize("-freorder-blocks-algorithm=simple"), optimize("-fno-inline"))) */
stencil(DTYPE *a, DTYPE *b, DTYPE *c, int nrows, int ncols, int ptid, int vtid, int dim) {
   for (int row = 0; row < nrows - (FILTER_DIM - 1); row++) {
    for (int col = vtid; col < ncols /*- (FILTER_DIM - 1)*/; col+=dim) {
      int temp = 0;
      for (int k1 = 0; k1 < FILTER_DIM; k1++) {
        for (int k2 = 0; k2 < 1; k2++) {
          int aIdx = (row + k1) * ncols + (col + k2);
          int bIdx = k1 * FILTER_DIM + k2;
          temp += a[aIdx] * b[bIdx];
        }
      }
      int cIdx = row * ncols + col;
      c[cIdx] = temp;
    }
  }
}
#endif // VECTOR_SIMD


void __attribute__((optimize("-freorder-blocks-algorithm=simple"))) kernel(
    DTYPE *a, DTYPE *b, DTYPE *c, int nrows, int ncols,
    int tid_x, int tid_y, int dim_x, int dim_y) {
  
  // start recording all stats (all cores)
  if (tid_x == 0 && tid_y == 0) {
    stats_on();
  }

  // for now we're just doing one row
  // so n can be ncols - 2
  int n = ncols - (FILTER_DIM - 1);

  // linearize tid and dim
  int tid = tid_x + tid_y * dim_x;
  int dim = dim_x * dim_y;

  // split into physical and virtual tids + dim
  int ptid_x = tid_x;
  int ptid_y = tid_y;
  int ptid   = tid;
  int pdim_x = dim_x;
  int pdim_y = dim_y;
  int pdim   = dim;
  int vtid_x = 0;
  int vtid_y = 0;
  int vtid   = 0;
  int vdim_x = 0;
  int vdim_y = 0;
  int vdim   = 0;
  int start  = 0;
  int end    = 0;
  int orig_x = 0;
  int orig_y = 0;
  int is_da  = 0;
  int master_x = 0;
  int master_y = 0;

  #if defined(VEC_SIZE_4_SIMD)
    // virtual group dimension
  vdim_x = 2;
  vdim_y = 2;

  int alignment = 16 * vdim_x * vdim_y;

  // group 1 top left (master = 0)
  if (ptid == 1) vtid = 0;
  if (ptid == 2) vtid = 1;
  if (ptid == 5) vtid = 2;
  if (ptid == 6) vtid = 3;
  if (ptid == 0) is_da = 1;
  if (ptid == 0 || ptid == 1 || ptid == 2 || ptid == 5 || ptid == 6) {
    start = 0;
    end = ncols - (FILTER_DIM - 1); //roundUp(n / 3, alignment); // make sure aligned to cacheline 
    orig_x = 1;
    orig_y = 0;
    master_x = 0;
    master_y = 0;
  }

  // group 2 bot left (master == 4)
  if (ptid == 8) vtid = 0;
  if (ptid == 9) vtid = 1;
  if (ptid == 12) vtid = 2;
  if (ptid == 13) vtid = 3;
  if (ptid == 4) is_da = 1;
  if (ptid == 4 || ptid == 8 || ptid == 9 || ptid == 12 || ptid == 13) {
    start = roundUp(n / 3, alignment);
    end = roundUp(2 * n / 3, alignment);
    orig_x = 0;
    orig_y = 2;
    master_x = 0;
    master_y = 1;
    // TODO for some reason can't return here...
  }

  // group 3 bottom right (master == 7)
  if (ptid == 10)  vtid = 0;
  if (ptid == 11) vtid = 1;
  if (ptid == 14) vtid = 2;
  if (ptid == 15) vtid = 3;
  if (ptid == 7) is_da = 1;
  if (ptid == 7 || ptid == 10 || ptid == 11 || ptid == 14 || ptid == 15) {
    start = roundUp(2 * n / 3, alignment);
    end = n;
    orig_x = 2;
    orig_y = 2;
    master_x = 3;
    master_y = 1;
  }

  // unused core
  // if (ptid == 3) return;

  vtid_x = vtid % vdim_x;
  vtid_y = vtid / vdim_y;


  #elif !defined(USE_VEC)

  vdim_x = 1;
  vdim_y = 1;
  vtid_x = 0;
  vtid_y = 0;
  vtid   = 0;
  start  = ptid * ( n / pdim );
  end    = ( ptid + 1 ) * ( n / pdim );

  #endif

  // linearize some fields
  vdim = vdim_x * vdim_y;
  int orig = orig_x + orig_y * dim_x;

  #ifdef USE_VECTOR_SIMD
  // volatile so dont reorder this function call
  int mask = getSIMDMask(master_x, master_y, orig_x, orig_y, vtid_x, vtid_y, vdim_x, vdim_y, is_da);
  #endif

  // printf("ptid %d(%d,%d) vtid %d(%d,%d) dim %d(%d,%d) %d->%d\n", ptid, ptid_x, ptid_y, vtid, vtid_x, vtid_y, vdim, vdim_x, vdim_y, start, end); 

  #ifdef NUM_REGIONS
  int prefetchMask = (NUM_REGIONS << PREFETCH_NUM_REGION_SHAMT) | (REGION_SIZE << PREFETCH_REGION_SIZE_SHAMT);
  PREFETCH_EPOCH(prefetchMask);

  // make sure all cores have done this before begin kernel section --> do thread barrier for now
  // TODO hoping for a cleaner way to do this
  pthread_barrier_wait(&start_barrier);
  #endif

  // only let certain tids continue
  #if defined(USE_VECTOR_SIMD)
  if (ptid != 0 && ptid != 1 && ptid != 2 && ptid != 5 && ptid != 6) return;
  if (ptid == 3) return;
  #else
  if (ptid == 0 || ptid == 1 || ptid == 2 || ptid == 3) {
    vtid = ptid;
    vdim = 4;
  }
  else {
    return;
  }
  #endif

  // save the stack pointer to top of spad and change the stack pointer to point into the scratchpad
  // reset after the kernel is done
  // do before the function call so the arg stack frame is on the spad
  // store the the current spAddr to restore later 
  unsigned long long *spTop = getSpTop(ptid);
  // guess the remaining of the part of the frame that might be needed??
  spTop -= 4;

  unsigned long long stackLoc;
  asm volatile (
    // copy part of the stack onto the scratchpad in case there are any loads to scratchpad right before
    // function call
    "ld t0, 0(sp)\n\t"
    "sd t0, 0(%[spad])\n\t"
    "ld t0, 8(sp)\n\t"
    "sd t0, 8(%[spad])\n\t"
    "ld t0, 16(sp)\n\t"
    "sd t0, 16(%[spad])\n\t"
    "ld t0, 24(sp)\n\t"
    "sd t0, 24(%[spad])\n\t"
    // save the stack ptr
    "addi %[dest], sp, 0\n\t" 
    // overwrite stack ptr
    "addi sp, %[spad], 0\n\t"
    : [dest] "=r" (stackLoc)
    : [spad] "r" (spTop)
  );

  #ifdef USE_VECTOR_SIMD
  stencil(a, b, c, nrows, ncols, ptid, vtid, vdim, mask, is_da);
  #else
  stencil(a, b, c, nrows, ncols, ptid, vtid, vdim);
  #endif

  // restore stack pointer
  asm volatile (
    "addi sp, %[stackTop], 0\n\t" :: [stackTop] "r" (stackLoc)
  );

}


// helper functions
Kern_Args *construct_args(DTYPE *a, DTYPE *b, DTYPE *c, int nrows, int ncols,
  int tid_x, int tid_y, int dim_x, int dim_y) {

  Kern_Args *args = (Kern_Args*)malloc(sizeof(Kern_Args));
  
  args->a = a;
  args->b = b;
  args->c = c;
  args->nrows = nrows;
  args->ncols = ncols;
  args->tid_x = tid_x;
  args->tid_y = tid_y;
  args->dim_x = dim_x;
  args->dim_y = dim_y;
  
  return args;
      
}

void *pthread_kernel(void *args) {
  // guarentee one thread goes to each core, by preventing any threads
  // from finishing early
  pthread_barrier_wait(&start_barrier);
  
  // call the spmd kernel
  Kern_Args *a = (Kern_Args*)args;
  
  kernel(a->a, a->b, a->c, a->nrows, a->ncols, 
      a->tid_x, a->tid_y, a->dim_x, a->dim_y);
      
  pthread_barrier_wait(&start_barrier);

  if (a->tid_x == 0 && a->tid_y == 0) {
    stats_off();
  }

  // BUG: note this printf fails if have the VECTOR_EPOCH(0), but mayber just timing thing
  // printf("ptid (%d,%d)\n", a->tid_x, a->tid_y);

  return NULL;
}
