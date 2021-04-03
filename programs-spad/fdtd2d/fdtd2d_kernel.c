#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <math.h>

#include "pthread_launch.h"
#include "fdtd2d.h"
#include "spad.h"
#include "bind_defs.h"
#include "group_templates.h"
#include "util.h"
#include "fdtd2d_kernel.h"

// #define SCALAR_CORE
// #define VECTOR_CORE

/*-----------------------------------------------------------------------------------
 * Vector versions of the kernels.
 *---------------------------------------------------------------------------------*/

#ifdef USE_VEC

void tril_fdtd_step1(int mask,
  DTYPE *fict, DTYPE *ex, DTYPE *ey, DTYPE *hz, int t, int NX, int NY,
  int ptid, int groupId, int numGroups, int vtid) {

  #ifdef SCALAR_CORE
  VECTOR_EPOCH(mask);

  int start = VECTOR_LEN + ((groupId + 0) * NX) / numGroups;
  int end   = VECTOR_LEN + ((groupId + 1) * NX) / numGroups;

  #ifndef LONGLINES
  // make it a factor of vector group mapping size
  start = roundUp(start, VECTOR_LEN);
  end   = roundUp(end  , VECTOR_LEN);
  #endif

  int sp = 0;
  int init_len = min(INIT_FRAMES*STEP1_J_STRIDE, NY);

  ISSUE_VINST(init_label);
  #endif

  #ifdef VECTOR_CORE
  asm("trillium vissue_delim until_next vector_init");
  int start = VECTOR_LEN + ((groupId + 0) * NX) / numGroups;
  #ifndef LONGLINES
  start = roundUp(start, VECTOR_LEN);
  int i = vtid + start;
  #else
  int i = start;
  #endif

  int sp = 0;
  DTYPE *sp_ptr = (DTYPE*)getSpAddr(ptid, 0);
  #endif


  #ifdef SCALAR_CORE

  for (int i = start; i < end; i+=STEP1_I_STRIDE) {

    ISSUE_VINST(vec_body_init_in0_label);

    // warmup
    for (int j = 0; j < init_len; j+=STEP1_J_STRIDE) {
      prefetch_step1_frame_in0(ey, hz, i, j, NY, &sp);
    }

    // steady state
    for (int j = init_len; j < NY; j+=STEP1_J_STRIDE) {
      prefetch_step1_frame_in0(ey, hz, i, j, NY, &sp);
      ISSUE_VINST(vec_body_in0_label);
    }

    // cooldown
    for (int j = NY - init_len; j < NY; j+=STEP1_J_STRIDE) {
      ISSUE_VINST(vec_body_in0_label);
    }

    ISSUE_VINST(vec_body_end_in0_label);
  }
  
  #endif

  #ifdef VECTOR_CORE
  volatile int BH;
  volatile int BHO;

  do {
    asm("trillium vissue_delim until_next vec_body_init_in0");
    #ifdef LONGLINES
    int j = vtid * STEP1_UNROLL_LEN;
    #else
    int j = 0;
    #endif

    // i != 0
    do {

      asm("trillium vissue_delim if_begin vec_body_in0");
      FRAME_START(STEP1_REGION_SIZE);
      #pragma GCC unroll(16)
      for (int u = 0; u < STEP1_UNROLL_LEN; u++) {
        int u0 = u;
        int u1 = STEP1_UNROLL_LEN+u;
        int u2 = 2*STEP1_UNROLL_LEN+u;
        DTYPE out = sp_ptr[sp + u0] - 0.5f * (sp_ptr[sp + u1] - sp_ptr[sp + u2]);
        int idx = i * NY + j;
        FSTORE_NOACK(out, ey + idx + u, 0);
      }
      END_FRAME();
      j += STEP1_J_STRIDE;
      sp += STEP1_REGION_SIZE;
      sp = sp % STEP1_POST_FRAME_WORD;
      // if (sp == POST_FRAME_WORD) sp = 0;
      asm("trillium vissue_delim end at_jump");

    } while(BH);

    asm("trillium vissue_delim if_begin vec_body_end_in0");
    i += STEP1_I_STRIDE;
    asm("trillium vissue_delim end at_jump");

  } while(BHO);
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
  asm("trillium vissue_delim return scalar_return");
  return;
#endif

  // Glue points!
#ifdef SCALAR_CORE
init_label:
  asm("trillium glue_point vector_init");
vec_body_init_in0_label:
  asm("trillium glue_point vec_body_init_in0");
vec_body_in0_label:
  asm("trillium glue_point vec_body_in0");
vec_body_end_in0_label:
  asm("trillium glue_point vec_body_end_in0");
vector_return_label:
  asm("trillium glue_point vector_return");
#endif

}

// weird bounds so need to only do up to eff_NY
void tril_fdtd_step2(int mask,
  DTYPE *ex, DTYPE *ey, DTYPE *hz, int t, int NX, int NY,
  int ptid, int groupId, int numGroups, int vtid) {
  #ifdef SCALAR_CORE
  VECTOR_EPOCH(mask);

  int start = ((groupId + 0) * NX) / numGroups;
  int end   = ((groupId + 1) * NX) / numGroups;
  #ifndef LONGLINES
  start = roundUp(start, VECTOR_LEN);
  end   = roundUp(end  , VECTOR_LEN);
  #endif

  int sp = 0;
  int init_len = min(INIT_FRAMES*STEP2_J_STRIDE, NY-1);

  ISSUE_VINST(init_label);
  #endif

  #ifdef VECTOR_CORE
  asm("trillium vissue_delim until_next vector_init");
  int start = ((groupId + 0) * NX) / numGroups;
  #ifndef LONGLINES
  start = roundUp(start, VECTOR_LEN);
  int i = vtid + start;
  #else
  int i = start;
  #endif


  int sp = 0;
  DTYPE *sp_ptr = (DTYPE*)getSpAddr(ptid, 0);
  #endif


  #ifdef SCALAR_CORE

  for (int i = start; i < end; i+=STEP2_I_STRIDE) {

    ISSUE_VINST(vec_body_init_label);

    // warmup
    for (int j = 1; j < 1 + init_len; j+=STEP2_J_STRIDE) {
      prefetch_step2_frame(ex, hz, i, j, NY, &sp);
    }

    // steady-state
    for (int j = 1 + init_len; j < NY; j+=STEP2_J_STRIDE) {
      prefetch_step2_frame(ex, hz, i, j, NY, &sp);
      ISSUE_VINST(vec_body_label);
    }

    // coolodown
    for (int j = NY - init_len; j < NY; j+=STEP2_J_STRIDE) {
      ISSUE_VINST(vec_body_label);
    }

    ISSUE_VINST(vec_body_end_label);
  }
  
  #endif

  // TODO do first row in scalar and no vector core. maybe want another kernel?

  #ifdef VECTOR_CORE
  volatile int BH;
  volatile int BHO;

  do {

    asm("trillium vissue_delim until_next vec_body_init");
    #ifdef LONGLINES
    int j = 1 + vtid * STEP2_UNROLL_LEN;
    #else
    int j = 1;
    #endif

    do {

      asm("trillium vissue_delim if_begin vec_body");
      FRAME_START(STEP2_REGION_SIZE);
      #pragma GCC unroll(16)
      for (int u = 0; u < STEP2_UNROLL_LEN; u++) {
        int u0 = u;
        int u1 = STEP2_UNROLL_LEN + u;
        DTYPE out = sp_ptr[sp + u0] - 
          0.5f * (sp_ptr[sp + u1 + 1] - sp_ptr[sp + u1]);
        int idx = i * (NY+1) + j + u;
        int gt = (j + u >= NY);
        PRED_EQ_FSTORE_NOACK(gt, 0, out, ex + idx, 0);
      }
      END_FRAME();

      j += STEP2_J_STRIDE;
      sp += STEP2_REGION_SIZE;
      sp = sp % STEP2_POST_FRAME_WORD;
      // if (sp == POST_FRAME_WORD) sp = 0;
      asm("trillium vissue_delim end at_jump");

    } while(BH);

      asm("trillium vissue_delim if_begin vec_body_end");
      i+=STEP2_I_STRIDE;
      asm("trillium vissue_delim end at_jump");

  } while(BHO);
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
  asm("trillium vissue_delim return scalar_return");
  return;
#endif

  // Glue points!
#ifdef SCALAR_CORE
init_label:
  asm("trillium glue_point vector_init");
vec_body_init_label:
  asm("trillium glue_point vec_body_init");
vec_body_label:
  asm("trillium glue_point vec_body");
vec_body_end_label:
  asm("trillium glue_point vec_body_end");
vector_return_label:
  asm("trillium glue_point vector_return");
#endif
}

void tril_fdtd_step3(int mask,
  DTYPE *ex, DTYPE *ey, DTYPE *hz, int t, int NX, int NY, 
  int ptid, int groupId, int numGroups, int vtid) {
   #ifdef SCALAR_CORE
  VECTOR_EPOCH(mask);

  int start = ((groupId + 0) * NX) / numGroups;
  int end   = ((groupId + 1) * NX) / numGroups;
  start = roundUp(start, VECTOR_LEN);
  end   = roundUp(end  , VECTOR_LEN);

  ISSUE_VINST(init_label);
  #endif

  #ifdef VECTOR_CORE
  asm("trillium vissue_delim until_next vector_init");
  int start = ((groupId + 0) * NX) / numGroups;
  start = roundUp(start, VECTOR_LEN);
  
  int stride = (VECTOR_LEN-1)*NY; // b/c already went +1 row
  int idx = (start + vtid) * NY;

  int sp = 0;
  DTYPE *sp_ptr = (DTYPE*)getSpAddr(ptid, 0);
  #endif


  #ifdef SCALAR_CORE
  int sp = 0;
  int init_len = min(INIT_FRAMES*STEP3_UNROLL_LEN, NY);

  for (int i = start; i < end; i+=VECTOR_LEN) {

    ISSUE_VINST(vec_body_init_label);

    // warmup
    for (int j = 0; j < init_len; j+=STEP3_UNROLL_LEN) {
      prefetch_step3_frame(ex, ey, hz, i, j, NY, &sp);
    }

    // steady-state
    for (int j = init_len; j < NY; j+=STEP3_UNROLL_LEN) {
      prefetch_step3_frame(ex, ey, hz, i, j, NY, &sp);
      ISSUE_VINST(vec_body_label);
    }

    // coolodown
    for (int j = NY - init_len; j < NY; j+=STEP3_UNROLL_LEN) {
      ISSUE_VINST(vec_body_label);
    }

    ISSUE_VINST(vec_body_end_label);
  }
  
  #endif

  // TODO do first row in scalar and no vector core. maybe want another kernel?

  #ifdef VECTOR_CORE
  volatile int BH;
  volatile int BHO;

  do {

    asm("trillium vissue_delim until_next vec_body_init");
    // idx += unmappedLen;

    do {

      asm("trillium vissue_delim if_begin vec_body");
      FRAME_START(STEP3_REGION_SIZE);
      #pragma GCC unroll(16)
      for (int u = 0; u < STEP3_UNROLL_LEN; u++) {
        int u0 = u;
        int u1 = STEP3_UNROLL_LEN + u;
        int u2 = 2*STEP3_UNROLL_LEN+1 + u;
        int u3 = 3*STEP3_UNROLL_LEN+1 + u;
        DTYPE out = sp_ptr[sp + u0] - 0.7f * 
          (sp_ptr[sp + u1+1] - sp_ptr[sp + u1] + sp_ptr[sp + u2] - sp_ptr[sp + u3]);
        FSTORE_NOACK(out, &hz[idx + u], 0); 
      }
      END_FRAME();
      sp += STEP3_REGION_SIZE;
      sp = sp % STEP3_POST_FRAME_WORD;
      idx += STEP3_UNROLL_LEN;
      // if (sp == POST_FRAME_WORD) sp = 0;
      asm("trillium vissue_delim end at_jump");

    } while(BH);

    asm("trillium vissue_delim if_begin vec_body_end");
    idx += stride;
    asm("trillium vissue_delim end at_jump");

  } while(BHO);
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
  asm("trillium vissue_delim return scalar_return");
  return;
#endif

  // Glue points!
#ifdef SCALAR_CORE
init_label:
  asm("trillium glue_point vector_init");
vec_body_init_label:
  asm("trillium glue_point vec_body_init");
vec_body_label:
  asm("trillium glue_point vec_body");
vec_body_end_label:
  asm("trillium glue_point vec_body_end");
vector_return_label:
  asm("trillium glue_point vector_return");
#endif
}

#endif