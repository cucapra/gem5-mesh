#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

#include "pthread_launch.h"
#include "conv3d.h"
#include "spad.h"
#include "bind_defs.h"
#include "util.h"
#include "conv3d_kernel.h"

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

#define SCALAR_CORE
#define VECTOR_CORE

// vec parallel across k
// group parallel across i
// j along for the ride?

#define VPREFETCH_LR(sp, memIdx, core, len, style)  \
  VPREFETCH_L(sp, memIdx, core, len, style);        \
  VPREFETCH_R(sp, memIdx, core, len, style)


inline void prefetch_horiz_frame(DTYPE *a, int i, int j, int k, int NJ, int NK, int *sp) {
  // prefetch all 15 values required for computation
            // a[IDX(i-1, j-1, k-1, NJ, NK)], a[IDX(i-1, j-1, k+1, NJ, NK)], 
            // a[IDX(i-1, j+0, k+1, NJ, NK)], a[IDX(i-1, j+1, k+1, NJ, NK)], 
            // a[IDX(i+0, j-1, k+0, NJ, NK)], a[IDX(i+0, j+0, k+0, NJ, NK)], 
            // a[IDX(i+0, j+1, k+0, NJ, NK)], a[IDX(i+1, j-1, k-1, NJ, NK)], 
            // a[IDX(i+1, j-1, k+1, NJ, NK)], a[IDX(i+1, j+0, k+1, NJ, NK)], 
            // a[IDX(i+1, j+1, k+1, NJ, NK)];

  VPREFETCH_LR(*sp + 0 , a + IDX(i-1, j-1, k-1, NJ, NK), 0, PREFETCH_LEN, HORIZONTAL);
  VPREFETCH_LR(*sp + 1 , a + IDX(i-1, j-1, k+1, NJ, NK), 0, PREFETCH_LEN, HORIZONTAL);
  VPREFETCH_LR(*sp + 2 , a + IDX(i-1, j+0, k+1, NJ, NK), 0, PREFETCH_LEN, HORIZONTAL);
  VPREFETCH_LR(*sp + 3 , a + IDX(i-1, j+1, k+1, NJ, NK), 0, PREFETCH_LEN, HORIZONTAL);
  VPREFETCH_LR(*sp + 4 , a + IDX(i+0, j-1, k+0, NJ, NK), 0, PREFETCH_LEN, HORIZONTAL);
  VPREFETCH_LR(*sp + 5 , a + IDX(i+0, j+0, k+0, NJ, NK), 0, PREFETCH_LEN, HORIZONTAL);
  VPREFETCH_LR(*sp + 6 , a + IDX(i+0, j+1, k+0, NJ, NK), 0, PREFETCH_LEN, HORIZONTAL);
  VPREFETCH_LR(*sp + 7 , a + IDX(i+1, j-1, k-1, NJ, NK), 0, PREFETCH_LEN, HORIZONTAL);
  VPREFETCH_LR(*sp + 8 , a + IDX(i+1, j-1, k+1, NJ, NK), 0, PREFETCH_LEN, HORIZONTAL);
  VPREFETCH_LR(*sp + 9 , a + IDX(i+1, j+0, k+1, NJ, NK), 0, PREFETCH_LEN, HORIZONTAL);
  VPREFETCH_LR(*sp + 10, a + IDX(i+1, j+1, k+1, NJ, NK), 0, PREFETCH_LEN, HORIZONTAL);

  *sp = (*sp + REGION_SIZE);

  // spad is circular buffer so do cheap mod here
  if (*sp == POST_REGION_WORD) {
    *sp = 0;
  }
}

void tril_conv3d(int mask,
    DTYPE *a, DTYPE *b, int outer_start, int outer_end, int NJ, int NK,
    int ptid, int vtid_x, int vtid_y, int vdim_x, int vdim_y) {

  #ifdef SCALAR_CORE
  VECTOR_EPOCH(mask);
  int dim = vdim_x * vdim_y;
  int step = dim;
  ISSUE_VINST(init_label);
  #endif

  #ifdef VECTOR_CORE
  asm("trillium vissue_delim until_next vector_init");
  
	DEF_WEIGHTS();

  int vtid = vtid_x + vtid_y * vdim_x;
  int dim = vdim_x * vdim_y;
  int step = dim;

  DTYPE *bPtr = b + outer_start * NJ * NK + vtid + 1;
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
  volatile int BH; // TODO are 3 needed? or 1 suffices?
  volatile int BHO;
  volatile int BHOO;
  do {
    do {
      do {
      asm("trillium vissue_delim if_begin vec_body");

        FRAME_START();

        DTYPE out = CONV_3x3(
          sp_ptr[sp + 0], sp_ptr[sp + 1], sp_ptr[sp + 2],
          sp_ptr[sp + 3], sp_ptr[sp + 4], sp_ptr[sp + 5],
          sp_ptr[sp + 6], sp_ptr[sp + 7], sp_ptr[sp + 8]
        );

        END_FRAME();

        STORE_NOACK(out, bPtr, 0);
        bPtr += dim;
        sp += REGION_SIZE;
        if (sp == POST_REGION_WORD) sp = 0;
        asm("trillium vissue_delim end at_jump");
      } while (BH);

      asm("trillium vissue_delim if_begin vec_body_end");
      bPtr += unmappedColLen;
      asm("trillium vissue_delim end at_jump");


    } while (BHO);

    asm("trillium vissue_delim if_begin vec_body_end");
    bPtr += unmappedColLen;
    asm("trillium vissue_delim end at_jump");

  } while (BHOO);
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