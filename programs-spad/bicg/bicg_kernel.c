#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <math.h>

#include "pthread_launch.h"
#include "bicg.h"
#include "spad.h"
#include "bind_defs.h"
#include "util.h"

// #define SCALAR_CORE
// #define VECTOR_CORE

/*-----------------------------------------------------------------------------------
 * Vector versions of the kernels.
 *---------------------------------------------------------------------------------*/

#ifdef USE_VEC

// prefetch a and r
inline void prefetch_s_frame(DTYPE *a, DTYPE *r, int i, int j, int *sp, int NY) {
  // don't think need prefetch_R here?
  VPREFETCH_L(*sp, &a[i * NY + j], 0, S_PREFETCH_LEN, HORIZONTAL);
  // VPREFETCH_R(*sp, &a[i * NY + j], 0, S_PREFETCH_LEN, HORIZONTAL);
  // printf("horiz pf i %d j %d idx %d sp %d val %f %f %f %f\n", i, j, i * NY + j, *sp, a[i* NY + j], a[i*NY+j+1], a[i*NY+j+2], a[i*NY+j+3]);
  *sp = *sp + 1;

  // TODO potentially some reuse here b/c fetch the same thing for everyone
  for (int core = 0; core < VECTOR_LEN; core++) {
    VPREFETCH_L(*sp, &r[i], core, Q_PREFETCH_LEN, VERTICAL);
    // VPREFETCH_R(*sp, &r[i], core, Q_PREFETCH_LEN, VERTICAL);
  }

  *sp = *sp + Q_PREFETCH_LEN;

  if (*sp == POST_FRAME_WORD) *sp = 0;
}

void tril_bicg_s(int mask, DTYPE *a, DTYPE *r, DTYPE *s, int NX, int NY, int ptid, int groupId, int numGroups, int vtid) {

  // CANT DO CODE HERE, also can't share code between scalar and vector here
  // shared code should happen before go into this function
  // // chunk over vector gorups
  // int start = ((groupId + 0) * NY) / numGroups;
  // int end   = ((groupId + 1) * NY) / numGroups;

  // // make it a factor of vector group mapping size
  // start = roundUp(start, VECTOR_LEN);
  // end   = roundUp(end  , VECTOR_LEN);

  #ifdef SCALAR_CORE
  VECTOR_EPOCH(mask);

  // chunk over vector gorups
  int start = ((groupId + 0) * NY) / numGroups;
  int end   = ((groupId + 1) * NY) / numGroups;

  // make it a factor of vector group mapping size
  start = roundUp(start, VECTOR_LEN);
  end   = roundUp(end  , VECTOR_LEN);

  // issue header block
  ISSUE_VINST(init_label);
  #endif

  #ifdef VECTOR_CORE
  asm("trillium vissue_delim until_next vector_init");
  int start = ((groupId + 0) * NY) / numGroups;
  start = roundUp(start, VECTOR_LEN);

  int i = 0;
  int j = start + vtid;
  DTYPE s_local = 0.0f;
  int sp = 0;
  DTYPE* sp_ptr = (DTYPE*)getSpAddr(ptid, 0);
  #endif

  #ifdef SCALAR_CORE
  int sp = 0;
  
  // do initial prefetching for a small amount of first row
  for (int i = 0; i < INIT_FRAMES*Q_PREFETCH_LEN; i+=Q_PREFETCH_LEN) {
    prefetch_s_frame(a, r, i, start, &sp, NY);
  }

  // do modified first row that has already been prefetch
  for (int i = INIT_FRAMES*Q_PREFETCH_LEN; i < NX; i+=Q_PREFETCH_LEN) {
    prefetch_s_frame(a, r, i, start, &sp, NY);
    ISSUE_VINST(vec_body_label);
  }

  // steady state
  for (int j = start + VECTOR_LEN; j < end; j+=VECTOR_LEN) {
    for (int i = 0; i < NX; i+=Q_PREFETCH_LEN) {
      prefetch_s_frame(a, r, i, j, &sp, NY);
      ISSUE_VINST(vec_body_label);
    }
  }

  // draining. do the last vissue corresponding to the initial round of prefetch
  for (int i = NX - INIT_FRAMES*Q_PREFETCH_LEN; i < NX; i+=Q_PREFETCH_LEN) {
    ISSUE_VINST(vec_body_label);
  }
#endif

#ifdef VECTOR_CORE

  volatile int BH;
  do {
    asm("trillium vissue_delim if_begin vec_body");
    FRAME_START(FRAME_SIZE);
    // s_local += a[i * NY + j] * r[i];
    s_local += sp_ptr[sp + 0] * sp_ptr[sp + 1];
    i++; // TODO when this is inlined is the dereference removed?
    // do loop check here, to take load off scalar core?
    // does reduce vector core utilization
    if (i == NX) {
      STORE_NOACK(s_local, &s[j], 0);
      i = 0;
      j+=VECTOR_LEN;
      s_local = 0.0f;
    }
    sp+=2;
    if (sp == POST_FRAME_WORD) sp = 0;
    REMEM(FRAME_SIZE);
    asm("trillium vissue_delim end at_jump");
  } while(BH);

#endif


  // Clean up on the vector cores.
#ifdef SCALAR_CORE
  ISSUE_VINST(vector_return_label);
#elif defined VECTOR_CORE
  asm("trillium vissue_delim return vector_return");
  return;
#endif

#ifdef SCALAR_CORE
  // devec with unique tag
  DEVEC(devec_0);

  // we are doing lazy store acks, so use this to make sure all stores have commited to memory
  asm volatile("fence\n\t");
  asm("trillium vissue_delim return scalar_return");  // XXX is this real???
  return;
#endif

  // Glue points!
#ifdef SCALAR_CORE
init_label:
  asm("trillium glue_point vector_init");
vec_body_label:
  asm("trillium glue_point vec_body");
vector_return_label:
  asm("trillium glue_point vector_return");
#endif
}

// prefetch a and p
inline void prefetch_q_frame(DTYPE *a, DTYPE *p, int i, int j, int *sp, int NY) {
  // don't think need prefetch R here?
  for (int core = 0; core < VECTOR_LEN; core++) {
    int icore = i + core;
    VPREFETCH_L(*sp, &a[icore * NY + j], core, Q_PREFETCH_LEN, VERTICAL);
    VPREFETCH_R(*sp, &a[icore * NY + j], core, Q_PREFETCH_LEN, VERTICAL);
  }

  *sp = *sp + Q_PREFETCH_LEN;

  // TODO potentially some reuse here b/c fetch the same thing for everyone
  for (int core = 0; core < VECTOR_LEN; core++) {
    VPREFETCH_L(*sp, &p[j], core, Q_PREFETCH_LEN, VERTICAL);
    VPREFETCH_R(*sp, &p[j], core, Q_PREFETCH_LEN, VERTICAL);
  }

  *sp = *sp + Q_PREFETCH_LEN;

  if (*sp == POST_FRAME_WORD) *sp = 0;
}

void tril_bicg_q(int mask, DTYPE *a, DTYPE *p, DTYPE *q, int NX, int NY, int ptid, int groupId, int numGroups, int vtid) {

  // // chunk over vector gorups
  // int start = ((groupId + 0) * NX) / numGroups;
  // int end   = ((groupId + 1) * NX) / numGroups;

  // // make it a factor of vector group mapping size
  // start = roundUp(start, VECTOR_LEN);
  // end   = roundUp(end  , VECTOR_LEN);

  #ifdef SCALAR_CORE
  // goto vector mode
  VECTOR_EPOCH(mask);

  // chunk over vector gorups
  int start = ((groupId + 0) * NX) / numGroups;
  int end   = ((groupId + 1) * NX) / numGroups;

  // make it a factor of vector group mapping size
  start = roundUp(start, VECTOR_LEN);
  end   = roundUp(end  , VECTOR_LEN);
  
  // issue header block
  ISSUE_VINST(init_label);
  #endif

  #ifdef VECTOR_CORE
    asm("trillium vissue_delim until_next vector_init");
    int start = ((groupId + 0) * NX) / numGroups;
    start = roundUp(start, VECTOR_LEN);

    int i = start + vtid;
    int j = 0;
    DTYPE q_local = 0.0f;
    int sp = 0;
    DTYPE* sp_ptr = (DTYPE*)getSpAddr(ptid, 0);
  #endif

  // TODO seems like can use c functors to implement this pattern
  // just need to give functions for prefetch and vissue?
  #ifdef SCALAR_CORE
  int sp = 0;

  // this kernel needs to do vertical loads due to access pattern of A
  // currently doing a one wide load. want to increase size, but then also have
  // to unroll vector code

  // TODO merge INIT_FRAMES*Q_PREFEATCH_LEN

  // do initial prefetching for a small amount of first row
  for (int j = 0; j < INIT_FRAMES*Q_PREFETCH_LEN; j+=Q_PREFETCH_LEN) {
    prefetch_q_frame(a, p, start, j, &sp, NY);
  }

  // do modified first row that has already been prefetch
  for (int j = INIT_FRAMES*Q_PREFETCH_LEN; j < NY; j+=Q_PREFETCH_LEN) {
    prefetch_q_frame(a, p, start, j, &sp, NY);
    ISSUE_VINST(vec_body_label);
  }

  // steady state
  for (int i = start + VECTOR_LEN; i < end; i+=VECTOR_LEN) {
    for (int j = 0; j < NY; j+=Q_PREFETCH_LEN) {
      prefetch_q_frame(a, p, i, j, &sp, NY);
      ISSUE_VINST(vec_body_label);
    }
  }

  // draining. do the last vissue corresponding to the initial round of prefetch
  for (int j = NY - INIT_FRAMES*Q_PREFETCH_LEN; j < NY; j+=Q_PREFETCH_LEN) {
    ISSUE_VINST(vec_body_label);
  }
  #endif

#ifdef VECTOR_CORE
  volatile int BH;
  do {
    asm("trillium vissue_delim if_begin vec_body");
    FRAME_START(FRAME_SIZE);
    // q_local += a[i * NY + j] * p[j];
    q_local += sp_ptr[sp + 0] * sp_ptr[sp + 1];
    j++;
    // do loop check here, to take load off scalar core?
    // does reduce vector core utilization
    if (j == NY) {
      STORE_NOACK(q_local, &q[i], 0);
      i += VECTOR_LEN;
      j = 0;
      q_local = 0.0f;
    }
    sp += 2;
    if (sp == POST_FRAME_WORD) sp = 0;
    REMEM(FRAME_SIZE);
    asm("trillium vissue_delim end at_jump");
  } while(BH);
#endif

  // Clean up on the vector cores.
#ifdef SCALAR_CORE
  ISSUE_VINST(vector_return_label);
#elif defined VECTOR_CORE
  asm("trillium vissue_delim return vector_return");
  return;
#endif

#ifdef SCALAR_CORE
  // devec with unique tag
  DEVEC(devec_0);

  // we are doing lazy store acks, so use this to make sure all stores have commited to memory
  asm volatile("fence\n\t");
  asm("trillium vissue_delim return scalar_return");  // XXX is this real???
  return;
#endif

  // Glue points!
#ifdef SCALAR_CORE
init_label:
  asm("trillium glue_point vector_init");
vec_body_label:
  asm("trillium glue_point vec_body");
vector_return_label:
  asm("trillium glue_point vector_return");
#endif
}

#endif