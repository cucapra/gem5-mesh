#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <math.h>

#include "pthread_launch.h"
#include "covar.h"
#include "spad.h"
#include "bind_defs.h"
#include "group_templates.h"
#include "util.h"

// #define SCALAR_CORE
// #define VECTOR_CORE

/*-----------------------------------------------------------------------------------
 * Vector versions of the kernels.
 *---------------------------------------------------------------------------------*/

#ifdef USE_VEC

inline void prefetch_mean_frame(DTYPE *data, int i, int j, int *sp, int M) {
  VPREFETCH_L(*sp, &data[i * (M+1) + j], 0, MEAN_PREFETCH_LEN, HORIZONTAL);
  VPREFETCH_R(*sp, &data[i * (M+1) + j], 0, MEAN_PREFETCH_LEN, HORIZONTAL);

  *sp = *sp + 1;
  if (*sp == POST_FRAME_WORD) *sp = 0;
}

// compute each mean across each vector (single dimension)
void tril_mean(int mask, DTYPE *mean, DTYPE *data, int N, int M, 
    int ptid, int groupId, int numGroups, int vtid) {

  #ifdef SCALAR_CORE
  VECTOR_EPOCH(mask);

  int start = ((groupId + 0) * M) / numGroups;
  int end   = ((groupId + 1) * M) / numGroups;

  // make it a factor of vector group mapping size
  start = 1 + roundUp(start, VECTOR_LEN);
  end   = 1 + roundUp(end  , VECTOR_LEN);

  ISSUE_VINST(init_label);
  #endif

  #ifdef VECTOR_CORE
  asm("trillium vissue_delim until_next vector_init");
  int start = ((groupId + 0) * M) / numGroups;
  start = 1 + roundUp(start, VECTOR_LEN);
  // int i = 1;
  int j = start + vtid;
  DTYPE mean_j = 0.0f;
  int sp = 0;
  DTYPE *sp_ptr = (DTYPE*)getSpAddr(ptid, 0);
  #endif

  #ifdef SCALAR_CORE

  int sp  = 0;

  for (int j = start; j < end; j+=VECTOR_LEN) {

    ISSUE_VINST(vec_body_init_label);

    // initial round
    for (int i = 1; i < 1 + INIT_MEAN_OFFSET; i++) {
      prefetch_mean_frame(data, i, start, &sp, M);
    }

    // steady state
    for (int i = 1; i < (N+1); i++) {
      prefetch_mean_frame(data, i, j, &sp, M);
      ISSUE_VINST(vec_body_label);
    }

    //cooldown
    for (int i = (N+1) - INIT_MEAN_OFFSET; i < (N+1); i++) {
      ISSUE_VINST(vec_body_label);
    }

    ISSUE_VINST(vec_body_end_label);
  }
  #endif

  #ifdef VECTOR_CORE
  volatile int BH;
  volatile int BHO;
  do {

    asm("trillium vissue_delim until_next vec_body_init");

    do {
      asm("trillium vissue_delim if_begin vec_body");
      START_FRAME();
      mean_j += sp_ptr[sp];
      END_FRAME();
      asm volatile("nop\n\t");
      asm volatile("nop\n\t");
      asm volatile("nop\n\t");
      sp+=1;
      sp = sp % POST_FRAME_WORD;
      asm("trillium vissue_delim end at_jump");
    } while(BH);

    asm("trillium vissue_delim if_begin vec_body_end");
    mean_j /= (DTYPE)FLOAT_N;
    STORE_NOACK(mean_j, &mean[j], 0);
    // i = 1;
    j+=VECTOR_LEN;
    mean_j = 0.0f;
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
vec_body_init_label:
  asm("trillium glue_point vec_body_init");
vec_body_label:
  asm("trillium glue_point vec_body");
vec_body_end_label:
  asm("trillium glue_point vec_body_end");
vector_return_label:
  asm("trillium glue_point vector_return");
#endif
  // }
  // else {
  // DTYPE *sp_ptr = (DTYPE*)getSpAddr(ptid, 0);
  // for (int j = start + vtid; j < end; j+=VECTOR_LEN) {
  //   DTYPE mean_j = 0.0f;
  //   for (int i = 1; i < (N+1); i++) {
  //     FRAME_START(MEAN_FRAME_SIZE);
  //     mean_j += sp_ptr[sp];
  //     REMEM(MEAN_FRAME_SIZE);
  //     sp++;
  //     if (sp == POST_FRAME_WORD) sp = 0;
  //   }
  //   mean_j /= (DTYPE)FLOAT_N;
  //   mean[j] = mean_j;
  // }

  // }
}

inline void prefetch_center_frame(DTYPE *data, DTYPE *mean, int i, int j, int *sp, int M) {
  // fetch data
  for (int core = 0; core < VECTOR_LEN; core++) {
    VPREFETCH_L(*sp, &data[(i + core) * (M+1) + j], core, CENTER_PREFETCH_LEN, VERTICAL);
  }

  *sp = *sp + CENTER_PREFETCH_LEN;

  // TODO should do more than 1 here
  for (int core = 0; core < VECTOR_LEN; core++) {
    VPREFETCH_L(*sp, &mean[j], core, CENTER_PREFETCH_LEN, VERTICAL);
  }
  *sp = *sp + CENTER_PREFETCH_LEN;
  if (*sp == POST_FRAME_WORD) *sp = 0;
}

// subract mean from data to "center"
void tril_center(int mask, DTYPE *mean, DTYPE *data, int N, int M,
    int ptid, int groupId, int numGroups, int vtid) {

  #ifdef SCALAR_CORE
  VECTOR_EPOCH(mask);

  int start = ((groupId + 0) * N) / numGroups;
  int end   = ((groupId + 1) * N) / numGroups;

  // make it a factor of vector group mapping size
  start = 1 + roundUp(start, VECTOR_LEN);
  end   = 1 + roundUp(end  , VECTOR_LEN);

  ISSUE_VINST(init_label);
  #endif

  #ifdef VECTOR_CORE
  asm("trillium vissue_delim until_next vector_init");
  int start = ((groupId + 0) * N) / numGroups;
  start = 1 + roundUp(start, VECTOR_LEN);
  int i = start + vtid;
  int j = 1;
  int sp = 0;
  DTYPE* sp_ptr = (DTYPE*)getSpAddr(ptid, 0);
  #endif


  #ifdef SCALAR_CORE
  int sp  = 0;

  for (int i = start; i < end; i+=VECTOR_LEN) {

    ISSUE_VINST(vec_body_init_label);

    // initial prefetching for the column
    for (int j = 1; j < 1 + INIT_CENTER_OFFSET; j++) {
      prefetch_center_frame(data, mean, i, j, &sp, M);
    }

    // steady state
    for (int j = 1 + INIT_CENTER_OFFSET; j < (M+1); j++) {
      prefetch_center_frame(data, mean, i, j, &sp, M);
      ISSUE_VINST(vec_body_label);  
    }

    // cooldown
    for (int j = (M+1) - INIT_CENTER_OFFSET; j < (M+1); j++) {
      ISSUE_VINST(vec_body_label);
    }

    // tick reset loop
    ISSUE_VINST(vec_body_end_label);

  }
  #endif

  #ifdef VECTOR_CORE
  volatile int BH;
  volatile int BHO;
  do {

    asm("trillium vissue_delim until_next vec_body_init");

    do {
      asm("trillium vissue_delim if_begin vec_body");
      START_FRAME();
      data[i * (M+1) + j] = sp_ptr[sp + 0] - sp_ptr[sp + 1];
      END_FRAME();
      sp+=CENTER_FRAME_SIZE;
      sp = sp % POST_FRAME_WORD;
      j++;
      asm("trillium vissue_delim end at_jump");
    } while(BH);

    asm("trillium vissue_delim if_begin vec_body_end");
    j = 1;
    i+=VECTOR_LEN;
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
vec_body_init_label:
  asm("trillium glue_point vec_body_init");
vec_body_label:
  asm("trillium glue_point vec_body");
vec_body_end_label:
  asm("trillium glue_point vec_body_end");
vector_return_label:
  asm("trillium glue_point vector_return");
#endif

  // }
  // else {
  // DTYPE *sp_ptr = (DTYPE*)getSpAddr(ptid, 0);
  // for (int i = start + vtid; i < end; i+=VECTOR_LEN) {
  //   for (int j = 1; j < (M+1); j++) {
  //     FRAME_START(CENTER_FRAME_SIZE);
  //     data[i * (M+1) + j] = sp_ptr[sp + 0] - sp_ptr[sp + 1];
  //     REMEM(CENTER_FRAME_SIZE);
  //     sp+=2;
  //     if (sp == POST_FRAME_WORD) sp = 0;
  //   }
  // }
  // }

  // // unlike gpu version only parallelize outer loop? find if N dimension big enough which prob is
  // // or should we use same method as gpu to be fair?

  // // I think just paralleize outer here? and maybe optimize gpu later?
  // int start = ((tid + 0) * N) / dim;
  // int end   = ((tid + 1) * N) / dim;

  // for (int i = start + 1; i < end + 1; i++) {
  //   for (int j = 1; j < (M+1); j++) {
  //     data[i * (M+1) + j] -= mean[j];	
  //   }
  // }
}

inline void prefetch_covar_frame(DTYPE *data, int i, int j1, int j2, int *sp, int M) {
  // everyone in groups gets the same j1. could share and/or do vertical
  for (int core = 0; core < VECTOR_LEN; core++) {
    VPREFETCH_L(*sp, &data[i * (M+1) + j1], core, COVAR_J1_PREFETCH_LEN, VERTICAL);
  }
  *sp = *sp + COVAR_J1_PREFETCH_LEN;

  VPREFETCH_L(*sp, &data[i * (M+1) + j2], 0, COVAR_J2_PREFETCH_LEN, HORIZONTAL);
  VPREFETCH_R(*sp, &data[i * (M+1) + j2], 0, COVAR_J2_PREFETCH_LEN, HORIZONTAL);
  *sp = *sp + 1;

  if (*sp == POST_FRAME_WORD) *sp = 0;
}

// compute the covariance matrix
void tril_covar(int mask, DTYPE *symmat, DTYPE *data, int N, int M, 
    int ptid, int groupId, int numGroups, int vtid) {

  #ifdef SCALAR_CORE
  VECTOR_EPOCH(mask);

  // int start = ((groupId + 0) * M) / numGroups;
  // int end   = ((groupId + 1) * M) / numGroups;

  int start = 1 + groupId * VECTOR_LEN;
  int stride = numGroups * VECTOR_LEN;
  int end = M + 1;

  ISSUE_VINST(init_label);
  #endif

  #ifdef VECTOR_CORE
  asm("trillium vissue_delim until_next vector_init");
  int start = 1 + groupId * VECTOR_LEN;
  int j1 = start;
  int j2 = j1;
  int stride = numGroups * VECTOR_LEN;
  int sp = 0;
  DTYPE* sp_ptr = (DTYPE*)getSpAddr(ptid, 0);
  #endif


  #ifdef SCALAR_CORE
  int sp  = 0;

  for (int j1 = start; j1 < end; j1+=stride) {
    for (int j2 = j1; j2 < (M+1); j2+=VECTOR_LEN) {

      ISSUE_VINST(vec_body_init_label);

      // initial round
      for (int i = 1; i < 1 + INIT_COVAR_OFFSET; i++) {
        prefetch_covar_frame(data, i, start, start, &sp, M);
      }

       // steady state
      for (int i = 1 + INIT_COVAR_OFFSET; i < (N+1); i++) {
        prefetch_covar_frame(data, i, start, start, &sp, M);
        ISSUE_VINST(vec_body_label);
      }

      // cooldown
      for (int i = (N+1) - INIT_MEAN_OFFSET; i < (N+1); i++) {
        ISSUE_VINST(vec_body_label);
      }

      ISSUE_VINST(vec_body_end_label);
    }

    ISSUE_VINST(j2_end_label);
  }
  #endif

  #ifdef VECTOR_CORE
  volatile int BH;
  volatile int BHO;
  volatile int BHOO;
  do {
    asm("trillium vissue_delim until_next vec_body_init");
    int j2_idx = j2 + vtid;
    DTYPE symmat_idx = 0.0f;

  do {



    do {
      
      asm("trillium vissue_delim if_begin vec_body");
      FRAME_START(COVAR_FRAME_SIZE);
      symmat_idx += sp_ptr[sp + 0] * sp_ptr[sp + 1];
      REMEM(COVAR_FRAME_SIZE);
      sp+=2;
      sp = sp % POST_FRAME_WORD;
      asm("trillium vissue_delim end at_jump");
      // if (j2_idx < (M+1)) {
      //   symmat[j2_idx * (M+1) + j1] = symmat_idx;
      //   symmat[j1 * (M+1) + j2_idx] = symmat_idx;
      // }
    } while(BH);


    asm("trillium vissue_delim if_begin vec_body_end");
    int gt = (j2_idx >= (M+1));
    PRED_EQ(gt, 0);
    symmat[j2_idx * (M+1) + j1] = symmat_idx;
    symmat[j1 * (M+1) + j2_idx] = symmat_idx;
    PRED_EQ(j2, j2);
    j2+=VECTOR_LEN;
    asm("trillium vissue_delim end at_jump");

  } while (BHO);

    asm("trillium vissue_delim if_begin j2_end");
    j1+=stride;
    j2 = j1;
    asm("trillium vissue_delim end at_jump");

  } while(BHOO);
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
vec_body_init_label:
  asm("trillium glue_point vec_body_init");
vec_body_label:
  asm("trillium glue_point vec_body");
vec_body_end_label:
  asm("trillium glue_point vec_body_end");
j2_end_label:
  asm("trillium glue_point j2_end");
vector_return_label:
  asm("trillium glue_point vector_return");
#endif


  // if (ptid == 0) {
  
  // ISSUE_VINST()

  // // initial round
  // for (int i = 1; i < 1 + INIT_COVAR_OFFSET; i++) {
  //   prefetch_covar_frame(data, i, start, start, &sp, M);
  // }

  // // first row
  // for (int i = 1 + INIT_COVAR_OFFSET; i < (N+1); i++) {
  //   prefetch_covar_frame(data, i, start, start, &sp, M);
  //   // ISSUE_VINST()
  // }

  // // steady state
  // for (int j1 = start; j1 < end; j1++) {
  //   int startJ2 = j1;
  //   if (j1 == start) startJ2 += VECTOR_LEN;
  //   for (int j2 = startJ2; j2 < (M+1); j2+=VECTOR_LEN) {
  //     for (int i = 1; i < (N+1); i++) {
  //       prefetch_covar_frame(data, i, j1, j2, &sp, M);
  //       // ISSUE_VINST()
  //     }
  //   }
  // }

  // // cooldown
  // for (int i = N - INIT_MEAN_OFFSET; i < (N+1); i++) {
  //   // ISSUE_VINST()
  // }

  // }
  // else {
  // DTYPE *sp_ptr = (DTYPE*)getSpAddr(ptid, 0);
  // for (int j1 = start; j1 < end; j1++) {
  //   // for (int j2 = j1 + vtid; j2 < (M+1); j2+=VECTOR_LEN) { // TODO needs predication on this loop
  //   for (int j2 = j1; j2 < (M+1); j2+=VECTOR_LEN) {
  //     int j2_idx = j2 + vtid;
  //     DTYPE symmat_idx = 0.0f;
  //     for (int i = 1; i < (N+1); i++) {
  //       FRAME_START(COVAR_FRAME_SIZE);
  //       // printf("j1 %d j2 %d i %d vtid %d %f ?= %f | %f ?= %f\n", j1, j2, i, vtid, sp_ptr[sp+0], data[i *(M+1) + j1], sp_ptr[sp+1], data[i *(M+1) + j2]);
  //       symmat_idx += sp_ptr[sp + 0] * sp_ptr[sp + 1]; // not prefetching the right stuff here
  //       REMEM(COVAR_FRAME_SIZE);
  //       sp+=2;
  //       if (sp == POST_FRAME_WORD) sp = 0;
  //     }

  //     if (j2_idx < (M+1)) {
  //       symmat[j2_idx * (M+1) + j1] = symmat_idx;
  //       symmat[j1 * (M+1) + j2_idx] = symmat_idx;
  //     }
  //   }
  // }

  // }

  // for (int j1 = start + 1; j1 < (end+1); j1++) {
  //   for (int j2 = j1; j2 < (M+1); j2++) {
  //     DTYPE symmat_idx = 0.0f;
  //     for (int i = 1; i < (N+1); i++) {
  //       symmat_idx += data[i *(M+1) + j1] * data[i *(M+1) + j2];
  //     }
  //     symmat[j2 * (M+1) + j1] = symmat_idx;
  //     symmat[j1 * (M+1) + j2] = symmat_idx;
  //   }
  // }
}
#endif