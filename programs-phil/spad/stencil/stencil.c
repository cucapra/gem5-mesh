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

// one of these should be defined to dictate config
// #define NO_VEC 1
#define VEC_4_SIMD 1
// #define VEC_4_SIMD_BCAST 1
// #define VEC_4_SIMD_REUSE 1
// #define VEC_4_SIMD_SINGLE_PREFETCH 1

// vvadd_execute config directives
#if defined(VEC_4_SIMD) || defined(VEC_4_SIMD_BCAST) || defined(VEC_4_SIMD_SINGLE_PREFETCH) || defined(VEC_4_SIMD_REUSE)
#define USE_VEC 1
#endif
#if defined(VEC_4_SIMD) || defined(VEC_4_SIMD_BCAST) || defined(VEC_4_SIMD_SINGLE_PREFETCH) || defined(VEC_4_SIMD_REUSE)
#define USE_VECTOR_SIMD 1
#endif

// vector grouping directives
#if defined(VEC_4_SIMD) || defined(VEC_4_SIMD_BCAST) || defined(VEC_4_SIMD_SINGLE_PREFETCH) || defined(VEC_4_SIMD_REUSE)
#define VEC_SIZE_4_SIMD 1
#endif

// kernel settings 
#if defined(VEC_4_SIMD_SINGLE_PREFETCH)
#define SINGLE_PREFETCH 1
#endif
#if defined(VEC_4_SIMD_REUSE)
#define REUSE 1
#endif

// prefetch sizings
#if defined(USE_VECTOR_SIMD)
#define REGION_SIZE FILTER_DIM * FILTER_DIM
#define NUM_REGIONS 64
#define POST_REGION_WORD NUM_REGIONS * REGION_SIZE
#endif

inline int min(int a, int b) {
  if (a > b) {
    return b;
  }
  else {
    return a;
  }
}

#ifdef USE_VECTOR_SIMD
void __attribute__((optimize("-fno-reorder-blocks")))
stencil(
    DTYPE *a, DTYPE *b, DTYPE *c, int start_row, int end_row, int ncols,
    int ptid, int vtid, int dim, int mask) {

  int *spadAddr = (int*)getSpAddr(ptid, 0);

  #ifdef REUSE
  // calculate next spAddr for reuse
  int *prevSpAddr = NULL;
  if (vtid == 1 || vtid == 3) prevSpAddr = (int*)getSpAddr(ptid - 1, 0);
  else if (vtid == 2) prevSpAddr = (int*)getSpAddr(ptid - 3, 0); // GRID_DIM = 4 - 1 = 3
  // vtid=3 doesn't do this and must be predicated
  int frameSize = FILTER_DIM;
  if (vtid == 0) frameSize = FILTER_DIM * FILTER_DIM;
  int *leftCol = prevSpAddr + 0;
  int *midCol = prevSpAddr + 3;
  int *rightCol = spadAddr;
  if (vtid == 0) {
    leftCol = spadAddr + 3;
    midCol = spadAddr + 6;
  }
  #else
  int frameSize = FILTER_DIM * FILTER_DIM;
  #endif 

  // enter vector epoch within function, b/c vector-simd can't have control flow
  VECTOR_EPOCH(mask);

  // have r = 0 for now
  // will need broadcast to support r > 0
  // int spadIdx = 0;

  ISSUE_VINST(fable0);
  
  // how much we're actually going to do (ignore edges)
  // TODO can we use predication instead?
  int effCols = ncols - (FILTER_DIM - 1);

  // do initial batch of prefetching. only prefetch part of the first row
  #ifdef REUSE
  int beginCol = 0;
  #else
  int prefetchFrames = 8;
  int beginCol = min(prefetchFrames * dim, effCols);
  for (int r = start_row; r < start_row + 1; r++) {
    for (int c = 0; c < beginCol; c+=dim) {
      int frameIdx = 0;
      for (int k1 = 0; k1 < FILTER_DIM; k1++) {
        for (int k2 = 0; k2 < FILTER_DIM; k2++) {
          int aIdx = (r + k1) * ncols + (c + k2);
          VPREFETCH_L(frameIdx, a + aIdx, 0, 4);
          VPREFETCH_R(frameIdx, a + aIdx, 0, 4);
          frameIdx++;
        }
      }
      REMEM(0);
    }
  }
  #endif

  for (int r = start_row; r < end_row; r++) {
    int startCol = 0;
    // we've prefetch part of the first row to get ahead
    if (r == start_row) startCol = beginCol;
    for (int c = startCol; c < effCols; c+=dim) {

      #ifdef REUSE
      int frameIdx = 0;

      // load the last value into each core which cannot be received from prev core
      // these need to go first b/c all cores will recv this in their frame
      // a2 a5 a8 | a0 a1 a3 a4 a6 a7
      for (int k1 = 0; k1 < FILTER_DIM; k1++) {
        for (int k2 = FILTER_DIM - 1; k2 < FILTER_DIM; k2++) {
          int aIdx = (r + k1) * ncols + (c + k2);

          VPREFETCH_L(frameIdx, a + aIdx, 0, 4);
          VPREFETCH_R(frameIdx, a + aIdx, 0, 4);
          frameIdx++;
        }
      }

      // load the first two columns just into vtid0 to be shared
      for (int k2 = 0; k2 < FILTER_DIM - 1; k2++) {
        for (int k1 = 0; k1 < FILTER_DIM; k1++) {
          int aIdx = (r + k1) * ncols + (c + k2);
          
          VPREFETCH_L(frameIdx, a + aIdx, 0, 1);
          frameIdx++;
        }
      }



      #else
      // prefetch all 9 values required for computation
      int frameIdx = 0;
      for (int k1 = 0; k1 < FILTER_DIM; k1++) {
        for (int k2 = 0; k2 < FILTER_DIM; k2++) {
          int aIdx = (r + k1) * ncols + (c + k2);
          
          #ifdef SINGLE_PREFETCH
          VPREFETCH_L(frameIdx, a + aIdx + 0, 0, 1);
          VPREFETCH_L(frameIdx, a + aIdx + 1, 1, 1);
          VPREFETCH_L(frameIdx, a + aIdx + 2, 2, 1);
          VPREFETCH_L(frameIdx, a + aIdx + 3, 3, 1);
          #else
          VPREFETCH_L(frameIdx, a + aIdx, 0, 4);
          VPREFETCH_R(frameIdx, a + aIdx, 0, 4);
          #endif

          frameIdx++;
          // spad is circular buffer so do cheap mod here
          // spadIdx++;
          // if (spadIdx == POST_REGION_WORD) {
          //   spadIdx = 0;
          // }
        }
      }
      #endif

      ISSUE_VINST(fable1);

      REMEM(0);
    }
  }

  #ifndef REUSE
  // issue the rest of blocks
  for (int r = start_row; r < end_row; r++) {
    // take some loads off the last row b/c already prefetched
    int colStart = effCols;
    if (r == end_row - 1) colStart = effCols - beginCol;
    for (int c = colStart; c < effCols; c+=dim) {
      ISSUE_VINST(fable1);
    }
  }
  #endif

  // devec with unique tag
  DEVEC(devec_0);

  // we are doing lazy store acks, so use this to make sure all stores have commited to memory
  asm volatile("fence\n\t");

  return;

  // vector engine code

  // declarations
  DTYPE a_, b_, c_;
  int64_t iter, baseIdx, frameStart; // avoids sext.w instruction when doing broadcast // TODO maybe should be doing rv32
  DTYPE *cPtr;
  DTYPE b0, b1, b2, b3, b4, b5, b6, b7, b8;
  DTYPE a0, a1, a2, a3, a4, a5, a6, a7, a8;

  // entry block
  // load the full filter into spad
  fable0:
  //   #pragma GCC unroll 9
  //   for (int i = 0; i < FILTER_DIM * FILTER_DIM; i++) {
  //     spadAddr[POST_REGION_WORD + i] = b[i];
  //     // b_ = b[i]; // keep filter in regfile
  //   }
    iter = 0;
    cPtr = c + (start_row * ncols) + vtid;
    b0 = b[0];
    b1 = b[1];
    b2 = b[2];
    b3 = b[3];
    b4 = b[4];
    b5 = b[5];
    b6 = b[6];
    b7 = b[7];
    b8 = b[8];
  
  // loop body block
  fable1:
    c_ = 0;
    frameStart = iter * frameSize;
    baseIdx = iter * FILTER_DIM * FILTER_DIM;

    // start consumption of frame (stall unless we have the tokens we need)
    FRAME_START(frameSize);

    #ifdef REUSE

    // shared computation of first part
    // note since this part has common memory structure
    // a2 a5 a8 | a0 a3 a6 a1 a4 a7
    // LWSPEC(a2, spadAddr + frameStart + 0, 0);
    // LWSPEC(a5, spadAddr + frameStart + 1, 0);
    // LWSPEC(a8, spadAddr + frameStart + 2, 0);
    a2 = rightCol[frameStart + 0];
    a5 = rightCol[frameStart + 1];
    a8 = rightCol[frameStart + 2];
    c_ += a2 * b2;
    c_ += a5 * b5;
    c_ += a8 * b8;

    // vectorize load (diff address but same number of loads) so don't need predication
    a0 = leftCol[baseIdx + 0];
    a3 = leftCol[baseIdx + 1];
    a6 = leftCol[baseIdx + 2];
    c_ += a0 * b0;
    c_ += a3 * b3;
    c_ += a6 * b6;

    a1 = midCol[baseIdx + 1];
    a4 = midCol[baseIdx + 2];
    a7 = midCol[baseIdx + 3];
    c_ += a1 * b1;
    c_ += a4 * a4;
    c_ += a7 * a7;

    #else
    // #pragma GCC unroll 9
    // for (int i = 0; i < FILTER_DIM * FILTER_DIM; i++) {
    //   LWSPEC(a_, spadAddr + i, 0);
    //   b_ = spadAddr[POST_REGION_WORD + i];
    //   c_ += a_ * b_;
    // }
    c_ += b0 * spadAddr[baseIdx + 0];
    c_ += b1 * spadAddr[baseIdx + 1];
    c_ += b2 * spadAddr[baseIdx + 2];
    c_ += b3 * spadAddr[baseIdx + 3];
    c_ += b4 * spadAddr[baseIdx + 4];
    c_ += b5 * spadAddr[baseIdx + 5];
    c_ += b6 * spadAddr[baseIdx + 6];
    c_ += b7 * spadAddr[baseIdx + 7];
    c_ += b8 * spadAddr[baseIdx + 8];
    #endif

    REMEM(frameSize);

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
    // #pragma GCC unroll 4
    for (int col = vtid; col < ncols - (FILTER_DIM - 1); col+=dim) {
      int temp = 0;
      #pragma GCC unroll 3
      for (int k1 = 0; k1 < FILTER_DIM; k1++) {
        #pragma GCC unroll 3
        for (int k2 = 0; k2 < FILTER_DIM; k2++) {
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

  // chunk across rows
  int effRows = nrows - (FILTER_DIM - 1);

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

  // group 1 top left (master = 0)
  if (ptid == 1) vtid = 0;
  if (ptid == 2) vtid = 1;
  if (ptid == 5) vtid = 2;
  if (ptid == 6) vtid = 3;
  if (ptid == 0) is_da = 1;
  if (ptid == 0 || ptid == 1 || ptid == 2 || ptid == 5 || ptid == 6) {
    start = 0;
    end = effRows; //(float)effRows / 3.0f;
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
    start = (float)effRows / 3.0f;
    end = (float)(2 * effRows) / 3.0f;
    orig_x = 0;
    orig_y = 2;
    master_x = 0;
    master_y = 1;
  }

  // group 3 bottom right (master == 7)
  if (ptid == 10)  vtid = 0;
  if (ptid == 11) vtid = 1;
  if (ptid == 14) vtid = 2;
  if (ptid == 15) vtid = 3;
  if (ptid == 7) is_da = 1;
  if (ptid == 7 || ptid == 10 || ptid == 11 || ptid == 14 || ptid == 15) {
    start = (float)(2 * effRows) / 3.0f;
    end = effRows;
    orig_x = 2;
    orig_y = 2;
    master_x = 3;
    master_y = 1;
  }

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
  spTop -= 6;

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
    "ld t0, 32(sp)\n\t"
    "sd t0, 32(%[spad])\n\t"
    "ld t0, 40(sp)\n\t"
    "sd t0, 40(%[spad])\n\t"
    // save the stack ptr
    "addi %[dest], sp, 0\n\t" 
    // overwrite stack ptr
    "addi sp, %[spad], 0\n\t"
    : [dest] "=r" (stackLoc)
    : [spad] "r" (spTop)
  );

  #ifdef USE_VECTOR_SIMD
  stencil(a, b, c, start, end, ncols, ptid, vtid, vdim, mask);
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
