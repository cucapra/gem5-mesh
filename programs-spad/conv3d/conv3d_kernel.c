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

// #define SCALAR_CORE
// #define VECTOR_CORE

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
    DTYPE *a, DTYPE *b, int outer_start, int outer_end, int NJ, int NK, //int eff_NK,
    int ptid, int vtid_x, int vtid_y, int vdim_x, int vdim_y) {

  // int is_da = (mask != 0);

  #ifdef SCALAR_CORE
  // if (is_da)
  VECTOR_EPOCH(mask);

  // printf("start b %d\n", IDX(outer_start, 1, 1, NJ, NK));

  ISSUE_VINST(init_label);
  #endif

  #ifdef VECTOR_CORE
  asm("trillium vissue_delim until_next vector_init");
  
	DEF_WEIGHTS();

  int vtid = vtid_x + vtid_y * vdim_x;

  int bIdx = IDX(outer_start, 1, vtid + 1, NJ, NK); //outer_start * NJ * NK + NK + vtid + 1;
  int unmappedJ = (FILTER_DIM-1);
  int unmappedK = (FILTER_DIM-1);//NK - eff_NK;


  int sp = 0;
  DTYPE* sp_ptr = (DTYPE*)getSpAddr(ptid, 0);
  #endif

  #ifdef SCALAR_CORE
  // if (is_da) {
  int sp = 0;
  int eff_NK = NK - (FILTER_DIM-1);
  int beginK = min(INIT_FRAMES * VECTOR_LEN, eff_NK);

  for (int i = outer_start; i < 2 /*outer_end*/; i++) {

    // ISSUE_VINST(j_body_begin_label);

    for (int j = 1; j < 2/*NJ - 1*/; j++) {

      ISSUE_VINST(k_body_begin_label);

      // initial warmup
      for (int k = 1; k < 1 + beginK; k+=VECTOR_LEN) {
        prefetch_horiz_frame(a, i, j, k, NJ, NK, &sp);
      }

      // steady state
      for (int k = 1 + beginK; k < 1 + eff_NK; k+=VECTOR_LEN) {
        prefetch_horiz_frame(a, i, j, k, NJ, NK, &sp);
        ISSUE_VINST(vec_body_label);
      }

      // cooldown
      for (int k = 1 + eff_NK - beginK; k < 1 + eff_NK; k+=VECTOR_LEN) {
        ISSUE_VINST(vec_body_label);
      }

      ISSUE_VINST(k_body_end_label);
    }

    ISSUE_VINST(j_body_end_label);
  }
  // }
  #endif

  // if (!is_da) {

  // int vtid = vtid_x + vtid_y * vdim_x;
  // int sp = 0;
  // DTYPE* sp_ptr = (DTYPE*)getSpAddr(ptid, 0);


  // for (int i = outer_start; i < 2; i++) {
  //   for (int j = 1; j < 2; j++) {
  //     for (int k = 1 + vtid; k < NK - 1; k+=VECTOR_LEN) {
  //       START_FRAME();
  //       DTYPE out = CONV_15(
  //         sp_ptr[sp + 0], sp_ptr[sp + 1], sp_ptr[sp + 2],
  //         sp_ptr[sp + 3], sp_ptr[sp + 4], sp_ptr[sp + 5],
  //         sp_ptr[sp + 6], sp_ptr[sp + 7], sp_ptr[sp + 8],
  //         sp_ptr[sp + 9], sp_ptr[sp + 10]
  //       );
  //       b[IDX(i, j, k, NJ, NK)] = out;
  //       END_FRAME();
  //       sp = (sp + REGION_SIZE);

  //       // spad is circular buffer so do cheap mod here
  //       if (sp == POST_REGION_WORD) {
  //         sp = 0;
  //       }
  //     }
  //   }
  // }
  // }

  #ifdef VECTOR_CORE
  volatile int BH; // TODO are 3 needed? or 1 suffices?
  volatile int BHO;
  volatile int BHOO;
  do {

    // TODO needed? check if empty
    // asm("trillium vissue_delim until_next j_body_begin");

    do {

      asm("trillium vissue_delim until_next k_body_begin");

      do {
      asm("trillium vissue_delim if_begin vec_body");

        FRAME_START();


        // prefetching all 0s
        DTYPE out = CONV_15(
          sp_ptr[sp + 0], sp_ptr[sp + 1], sp_ptr[sp + 2],
          sp_ptr[sp + 3], sp_ptr[sp + 4], sp_ptr[sp + 5],
          sp_ptr[sp + 6], sp_ptr[sp + 7], sp_ptr[sp + 8],
          sp_ptr[sp + 9], sp_ptr[sp + 10]
        );

        END_FRAME();

        STORE_NOACK(out, b + bIdx, 0);
        bIdx += VECTOR_LEN;
        sp += REGION_SIZE;
        if (sp == POST_REGION_WORD) sp = 0;
        asm("trillium vissue_delim end at_jump");
      } while (BH);

      asm("trillium vissue_delim if_begin k_body_end");
      bIdx += unmappedK;
      asm("trillium vissue_delim end at_jump");


    } while (BHO);

    asm("trillium vissue_delim if_begin j_body_end");
    bIdx += unmappedJ;
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
k_body_end_label:
  asm("trillium glue_point k_body_end");
exit(1);
j_body_end_label:
  asm("trillium glue_point j_body_end");
exit(1);
k_body_begin_label:
  asm("trillium glue_point k_body_begin");
exit(1);
// j_body_begin_label:
//   asm("trillium glue_point j_body_begin");
// exit(1);
vector_return_label:
  asm("trillium glue_point vector_return");
exit(1);
#endif

  return;
}
#endif