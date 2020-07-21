#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

#include "pthread_launch.h"
#include "conv2d.h"
#include "spad.h"
#include "bind_defs.h"
#include "util.h"
#include "conv2d_kernel.h"

/*
  3x3 stencil with a single 3x3 filter
  Filter is cached in each spad.
  9 values are prefetched per frame
  Parallelize innermost loops (unrolled) so we can get away with codegen trick

  Reuse strategy - constraint is equal frame sizes and can't do remote stores to fill frames
  Each core gets 3 elements just like no reuse case but none are overlapping
  Its up to each core to do compute for those 3 elements and the two adjacents ones 
  Load x + 0, x + 1, x + 2
  Compute
  x - 1, x + 0, x + 1
         x + 0, x + 1, x + 2
                x + 1, x + 2, x + 3
  With this strategy you are the only core to have to load x + 1
  You have to reorder the data to implement! ( spaced out by strides )
  0 1 2 3 | 4 5 6 7  | 8 9 10 11 || 12 13 14 15
  0 3 6 9 | 1 4 7 10 | 2 5 8  11 || 12 15 18 21 

*/

#ifdef USE_VEC

// #define SCALAR_CORE
// #define VECTOR_CORE

#ifdef VERTICAL_LOADS
inline void prefetch_vert_frame(DTYPE *a, int r, int c, int ncols, int dim, int *spadIdx) {
  for (int k1 = -1; k1 <= 1; k1++) {
    for (int core = 0; core < dim; core++) {
      int aIdx = (r + k1) * ncols + c - 1 + core * CORE_STEP;
      // int aIdx = core * 16; // at least for size 4, only use 4/8 caches so less bw
      // printf("mid issue r %d c %d k1 %d core %d, depth %d, aIdx %d\n", r, c, k1, core, LOAD_DEPTH, aIdx);
      VPREFETCH_L(*spadIdx, a + aIdx, core, LOAD_DEPTH, 1);
      VPREFETCH_R(*spadIdx, a + aIdx, core, LOAD_DEPTH, 1);
    }
    (*spadIdx)+=LOAD_DEPTH;
  }

  if (*spadIdx == POST_REGION_WORD) *spadIdx = 0;
}
#else
inline void prefetch_horiz_frame(DTYPE *a, int r, int c, int ncols, int pRatio, int *spadIdx) {
  // prefetch all 9 values required for computation
  // #pragma GCC unroll 3
  for (int k1 = -1; k1 <= 1; k1++) {
    // #pragma GCC unroll 3
    for (int k2 = -1; k2 <= 1; k2++) {
      int aIdx = (r + k1) * ncols + (c + k2);
      
      #if PREFETCH_LEN != VECTOR_LEN
      for (int p = 0; p < pRatio; p++) {
        VPREFETCH_L(*spadIdx, a + aIdx + p * PREFETCH_LEN, p * PREFETCH_LEN, PREFETCH_LEN, 0);
        VPREFETCH_R(*spadIdx, a + aIdx + p * PREFETCH_LEN, p * PREFETCH_LEN, PREFETCH_LEN, 0);
      }
      #else
      VPREFETCH_L(*spadIdx, a + aIdx, 0, PREFETCH_LEN, 0);
      VPREFETCH_R(*spadIdx, a + aIdx, 0, PREFETCH_LEN, 0);
      #endif

      (*spadIdx)++;
      
    }
  }

  // spad is circular buffer so do cheap mod here
  if (*spadIdx == POST_REGION_WORD) {
    *spadIdx = 0;
  }
}
#endif

void tril_conv2d(int mask,
    DTYPE *a, DTYPE *b, int outer_start, int outer_end, int inner_dim, int eff_inner_dim,
    int ptid, int vtid_x, int vtid_y, int vdim_x, int vdim_y,
    DTYPE *p_sp_ptr, DTYPE *n_sp_ptr) {

  #ifdef SCALAR_CORE
  VECTOR_EPOCH(mask);

  int dim = vdim_x * vdim_y;

  #ifdef REUSE
  int step = dim*FILTER_DIM - (FILTER_DIM - 1);
  #elif defined(VERTICAL_LOADS)
  int step = CORE_STEP*dim;
  #else
  int step = dim;
  #endif

  //   int unmappedColLen = inner_dim - eff_inner_dim;
  // printf("%d %d %d %d %d\n", outer_start, outer_end, inner_dim, eff_inner_dim,
  //   unmappedColLen);

  // should be a constant from static analysis of dim
  int pRatio = VECTOR_LEN / PREFETCH_LEN;

  ISSUE_VINST(init_label);

  #endif


  #ifdef VECTOR_CORE
  asm("trillium vissue_delim until_next vector_init");
  
	DEF_WEIGHTS();

  int vtid = vtid_x + vtid_y * vdim_x;
  int dim = vdim_x * vdim_y;


  #ifdef REUSE
  int step = dim*FILTER_DIM - (FILTER_DIM - 1);
  #elif defined(VERTICAL_LOADS)
  int step = CORE_STEP*dim;
  #else
  int step = dim;
  #endif

  #ifdef REUSE
  int startOffset = vtid * FILTER_DIM - 1;
  #else
  int startOffset = vtid * (step/dim);
  #endif

  DTYPE *bPtr = b + outer_start * inner_dim + startOffset + 1;
  int unmappedColLen = inner_dim - eff_inner_dim;


  int sp = 0;
  DTYPE* sp_ptr = (DTYPE*)getSpAddr(ptid, 0);
  #endif

  #ifdef SCALAR_CORE
  int sp = 0;
  int beginCol = min(INIT_FRAMES * step, eff_inner_dim);

  for (int r = outer_start; r < outer_end; r++) {
    // initial warmup
    for (int c = 1; c < 1 + beginCol; c+=step) {
      #ifdef VERTICAL_LOADS
      // exhibit temporal reuse within a frame in a cacheline (16) can do 16-2=14 3x1 filters
      // TODO spatial should also do reuse maybe between frames (by putting in temporal storage). 
      // But maybe can't do memory layout restrictions
      prefetch_vert_frame(a, r, c, inner_dim, dim, &sp);
      #else
      prefetch_horiz_frame(a, r, c, inner_dim, pRatio, &sp);
      #endif
    }

    // steady state
    for (int c = 1 + beginCol; c < 1 + eff_inner_dim; c+=step) {
      #ifdef VERTICAL_LOADS
      prefetch_vert_frame(a, r, c, inner_dim, dim, &sp);
      #else
      prefetch_horiz_frame(a, r, c, inner_dim, pRatio, &sp);
      #endif
      ISSUE_VINST(vec_body_label);
    }

    // cooldown
    for (int c = 1 + eff_inner_dim - beginCol; c < 1 + eff_inner_dim; c+=step) {
      ISSUE_VINST(vec_body_label);
    }

    ISSUE_VINST(vec_body_end_label);
  }
  #endif

  #ifdef VECTOR_CORE
  volatile int BH;
  volatile int BHO;
  do {
    do {
    asm("trillium vissue_delim if_begin vec_body");

      FRAME_START();

      #ifdef REUSE
      volatile int ohjeez = 1;
      // note we need to unroll in order to get cPtr indexing to work b/c it goes +1 +1 +3*dim
      // potentially can move routine into a function call?
      // also could access an indirection array that gives and then have counter mod 3
      // or could even have 3 seperate issue block

      // center computation with local values
      // important to put non-predicated first so any shared values between pred blocks
      // are not masked out... really need compiler help on this
      // c_ = 0;
      // c_ += b0 * spData0;
      // c_ += b1 * spData1;
      // c_ += b2 * spData2;
      // c_ += b3 * spData3;
      // c_ += b4 * spData4;
      // c_ += b5 * spData5;
      // c_ += b6 * spData6;
      // c_ += b7 * spData7;
      // c_ += b8 * spData8;
      DTYPE out = CONV_3x3(
        sp_ptr[sp + 0], sp_ptr[sp + 1], sp_ptr[sp + 2],
        sp_ptr[sp + 3], sp_ptr[sp + 4], sp_ptr[sp + 5],
        sp_ptr[sp + 6], sp_ptr[sp + 7], sp_ptr[sp + 8]
      );
      STORE_NOACK(out, bPtr + 1, 0);

      // if swap following two pred blocks core0 pred works, but then core3 pred doesn't work
      // definetly something wrong with pred...
      // fetch one column from the left to perform leftmost computation
      PRED_NEQ(vtid, 0);
      if (ohjeez) {
      // c_ = 0;
      // c_ += b0 * prevSpadAddr[spIdx + 2];
      // c_ += b1 * spData0;
      // c_ += b2 * spData1;
      // c_ += b3 * prevSpadAddr[spIdx + 5];
      // c_ += b4 * spData3;
      // c_ += b5 * spData4;
      // c_ += b6 * prevSpadAddr[spIdx + 8];
      // c_ += b7 * spData6;
      // c_ += b8 * spData7;
      DTYPE out = CONV_3x3(
        p_sp_ptr[sp + 2], sp_ptr[sp + 0], sp_ptr[sp + 1],
        p_sp_ptr[sp + 5], sp_ptr[sp + 3], sp_ptr[sp + 4],
        p_sp_ptr[sp + 8], sp_ptr[sp + 6], sp_ptr[sp + 7]
      );
      STORE_NOACK(out, bPtr, 0);
      }
      PRED_EQ(vtid, vtid);

      // fetch one column from the right to perform rightmost computation
      PRED_NEQ(vtid, dim - 1); // last core in group can't do this
      if (ohjeez) { 
      // c_ = 0;
      // c_ += b0 * spData1;
      // c_ += b1 * spData2;
      // c_ += b2 * nextSpadAddr[spIdx + 0];
      // c_ += b3 * spData4;
      // c_ += b4 * spData5;
      // c_ += b5 * nextSpadAddr[spIdx + 3];
      // c_ += b6 * spData7;
      // c_ += b7 * spData8;
      // c_ += b8 * nextSpadAddr[spIdx + 6];
      DTYPE out = CONV_3x3(
        sp_ptr[sp + 1], sp_ptr[sp + 2], n_sp_ptr[sp + 0],
        sp_ptr[sp + 4], sp_ptr[sp + 5], n_sp_ptr[sp + 3],
        sp_ptr[sp + 7], sp_ptr[sp + 8], n_sp_ptr[sp + 6]
      );
      STORE_NOACK(out, bPtr + 2, 0);
      }
      PRED_EQ(vtid, vtid);

      END_FRAME();

      // 10 results are computed per reuse iteration
      bPtr += step;

      #elif defined(VERTICAL_LOADS)
      #pragma GCC unroll(14)
      for (int i = 0; i < CORE_STEP; i++) {
        int sp0 = sp + i;
        int sp1 = sp0 + LOAD_DEPTH;
        int sp2 = sp1 + LOAD_DEPTH;
        DTYPE out = CONV_3x3(
          sp_ptr[sp0 + 0], sp_ptr[sp0 + 1], sp_ptr[sp0 + 2],
          sp_ptr[sp1 + 0], sp_ptr[sp1 + 1], sp_ptr[sp1 + 2],
          sp_ptr[sp2 + 0], sp_ptr[sp2 + 1], sp_ptr[sp2 + 2]
        );
        STORE_NOACK(out, bPtr + i, 0);
      }
      END_FRAME();

      bPtr += step;
      #else
      DTYPE out = CONV_3x3(
        sp_ptr[sp + 0], sp_ptr[sp + 1], sp_ptr[sp + 2],
        sp_ptr[sp + 3], sp_ptr[sp + 4], sp_ptr[sp + 5],
        sp_ptr[sp + 6], sp_ptr[sp + 7], sp_ptr[sp + 8]
      );

      END_FRAME();

      STORE_NOACK(out, bPtr, 0);
      bPtr += dim;
      #endif

      sp += REGION_SIZE;
      // sp = sp % POST_REGION_WORD; // not a power of 2 --> if cheaper than mod
      if (sp == POST_REGION_WORD) sp = 0;
      asm("trillium vissue_delim end at_jump");
    } while (BH);

    asm("trillium vissue_delim if_begin vec_body_end");
    bPtr += unmappedColLen;
    asm("trillium vissue_delim end at_jump");


  } while (BHO);
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
exit(1);
vec_body_label:
  asm("trillium glue_point vec_body");
exit(1);
vec_body_end_label:
  asm("trillium glue_point vec_body_end");
exit(1);
vector_return_label:
  asm("trillium glue_point vector_return");
exit(1);
#endif

  return;
}
#endif