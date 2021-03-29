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

void tril_conv2d(int mask,
    DTYPE *a, DTYPE *b, int outer_start, int outer_end, int inner_dim, int eff_inner_dim,
    int ptid, int vtid_x, int vtid_y, int vdim_x, int vdim_y, DTYPE *p_sp_ptr, DTYPE *n_sp_ptr) {

  #ifdef SCALAR_CORE
  VECTOR_EPOCH(mask);
  int sp = 0;
  int beginCol = min(INIT_FRAMES * C_STRIDE, eff_inner_dim);

  ISSUE_VINST(init_label);

  #endif


  #ifdef VECTOR_CORE
  asm("trillium vissue_delim until_next vector_init");
  
	DEF_WEIGHTS();

  int vtid = vtid_x + vtid_y * vdim_x;
  int startOffset = vtid * CORE_STEP;

  DTYPE *bPtr = b + outer_start * inner_dim + startOffset + 1;
  bPtr -= OUT_PTR_OFFSET;

  int unmappedColLen = inner_dim - eff_inner_dim;


  int sp = 0;
  DTYPE* sp_ptr = (DTYPE*)getSpAddr(ptid, 0);
  #endif

  #ifdef SCALAR_CORE
  for (int r = outer_start; r < outer_end; r++) {
    // initial warmup
    for (int c = 1; c < 1 + beginCol; c+=C_STRIDE) {

      // exhibit temporal reuse within a frame in a cacheline (16) can do 16-2=14 3x1 filters
      // TODO spatial should also do reuse maybe between frames (by putting in temporal storage). 
      // But maybe can't do memory layout restrictions
      prefetch_vert_frame(a, r, c, inner_dim, VECTOR_LEN, &sp);

    }

    // steady state
    for (int c = 1 + beginCol; c < 1 + eff_inner_dim; c+=C_STRIDE) {
      prefetch_vert_frame(a, r, c, inner_dim, VECTOR_LEN, &sp);
      ISSUE_VINST(vec_body_label);
    }

    // cooldown
    for (int c = 1 + eff_inner_dim - beginCol; c < 1 + eff_inner_dim; c+=C_STRIDE) {
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

      FRAME_START(REGION_SIZE);

      int sp0 = sp;
      int sp1 = sp0 + LOAD_DEPTH;
      int sp2 = sp1 + LOAD_DEPTH;

      #pragma GCC unroll(14)
      for (int i = 0; i < CENTER_ITERS; i++) {
        int sp0i = sp0 + i;
        int sp1i = sp1 + i;
        int sp2i = sp2 + i;

        DTYPE out = CONV_3x3(
          sp_ptr[sp0i + 0], sp_ptr[sp0i + 1], sp_ptr[sp0i + 2],
          sp_ptr[sp1i + 0], sp_ptr[sp1i + 1], sp_ptr[sp1i + 2],
          sp_ptr[sp2i + 0], sp_ptr[sp2i + 1], sp_ptr[sp2i + 2]
        );
        FSTORE_NOACK(out, bPtr + OUT_PTR_OFFSET + i, 0);
      }

      #ifdef LONGLINES

      // // fetch one column from the left to perform leftmost computation
      // volatile int ohjeez = 1;
      // if (ohjeez) { 
      // PRED_NEQ(vtid, 0);
      // // if (ohjeez) {

      // // prev vals at end of fetch for each row
      // int p_sp0 = sp0 + LOAD_DEPTH_M1;
      // int p_sp1 = sp1 + LOAD_DEPTH_M1;
      // int p_sp2 = sp2 + LOAD_DEPTH_M1;

      // DTYPE out_l = CONV_3x3(
      //   p_sp_ptr[p_sp0], sp_ptr[sp0 + 0], sp_ptr[sp0 + 1],
      //   p_sp_ptr[p_sp1], sp_ptr[sp1 + 0], sp_ptr[sp1 + 1],
      //   p_sp_ptr[p_sp2], sp_ptr[sp2 + 0], sp_ptr[sp2 + 1]
      // );
      // FSTORE_NOACK(out_l, bPtr, 0);
      // PRED_EQ(vtid, vtid);
      // }

      // // fetch one column from the right to perform rightmost computation
      // if (ohjeez) { 
      // PRED_NEQ(vtid, VECTOR_LEN - 1); // last core in group can't do this
   

      // // use end vals
      // int e_sp0 = sp0 + LOAD_DEPTH_M1 - 1;
      // int e_sp1 = sp1 + LOAD_DEPTH_M1 - 1;
      // int e_sp2 = sp2 + LOAD_DEPTH_M1 - 1;

      // DTYPE out_r = CONV_3x3(
      //   sp_ptr[e_sp0 + 0], sp_ptr[e_sp0 + 1], n_sp_ptr[sp0 + 0],
      //   sp_ptr[e_sp1 + 0], sp_ptr[e_sp1 + 1], n_sp_ptr[sp1 + 0],
      //   sp_ptr[e_sp2 + 0], sp_ptr[e_sp2 + 1], n_sp_ptr[sp2 + 0]
      // );
      // FSTORE_NOACK(out_r, bPtr + LOAD_DEPTH_M1, 0);
      // PRED_EQ(vtid, vtid);
      // }

      #endif

      bPtr += C_STRIDE;
     
      sp += REGION_SIZE;
      sp = sp % POST_REGION_WORD; // not a power of 2 --> if cheaper than mod?
      // if (sp == POST_REGION_WORD) sp = 0;
      END_FRAME();
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