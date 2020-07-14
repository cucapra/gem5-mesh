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
  int start = ((groupId + 0) * M) / numGroups;
  int end   = ((groupId + 1) * M) / numGroups;

  // make it a factor of vector group mapping size
  start = 1 + roundUp(start, VECTOR_LEN);
  end   = 1 + roundUp(end  , VECTOR_LEN);

  // prevents code from being reordered :|
  volatile int ohjeez = 1;
  if (ohjeez) {

  VECTOR_EPOCH(mask);

  int sp  = 0;

  // printf("ptid %d range %d->%d enter\n", ptid, start, end);

  // if (ptid == 0) {

  ISSUE_VINST(fable0);

  // initial round
  for (int i = 1; i < 1 + INIT_MEAN_OFFSET; i++) {
    prefetch_mean_frame(data, i, start, &sp, M);
  }

  // first row
  for (int i = 1 + INIT_MEAN_OFFSET; i < (N+1); i++) {
    prefetch_mean_frame(data, i, start, &sp, M);
    ISSUE_VINST(fable1);
  }

  // steady state
  for (int j = start + VECTOR_LEN; j < end; j+=VECTOR_LEN) {
    for (int i = 1; i < (N+1); i++) {
      prefetch_mean_frame(data, i, j, &sp, M);
      ISSUE_VINST(fable1);
    }
  }

  // cooldown
  for (int i = (N+1) - INIT_MEAN_OFFSET; i < (N+1); i++) {
    ISSUE_VINST(fable1);
  }

  // devec with unique tag
  DEVEC(devec_0);

  // we are doing lazy store acks, so use this to make sure all stores have commited to memory
  asm volatile("fence\n\t");
  return;

  }

  // vector engine code

  // declarations
  int i, j;
  int sp;
  DTYPE *sp_ptr;
  DTYPE mean_j;

  // header
  fable0:
    i = 1;
    j = start + vtid;
    mean_j = 0.0f;
    sp = 0;
    sp_ptr = (DTYPE*)getSpAddr(ptid, 0);

  // body
  fable1:
    FRAME_START(MEAN_FRAME_SIZE);
    mean_j += sp_ptr[sp];
    i++;
    // do loop check here, to take load off scalar core?
    // does reduce vector core utilization
    if (i == (N+1)) {
      mean_j /= (DTYPE)FLOAT_N;
      STORE_NOACK(mean_j, &mean[j], 0);
      i = 1;
      j+=VECTOR_LEN;
      mean_j = 0.0f;
    }
    sp+=1;
    if (sp == POST_FRAME_WORD) sp = 0;
    REMEM(MEAN_FRAME_SIZE);
    asm volatile goto("j %l[fable1]\n\t"::::fable1);

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
  int start = ((groupId + 0) * N) / numGroups;
  int end   = ((groupId + 1) * N) / numGroups;

  // make it a factor of vector group mapping size
  start = 1 + roundUp(start, VECTOR_LEN);
  end   = 1 + roundUp(end  , VECTOR_LEN);

  int sp  = 0;


  VECTOR_EPOCH(mask);

  if (ptid == 0) {
  
  // ISSUE_VINST()

  // initial round
  for (int j = 1; j < 1 + INIT_CENTER_OFFSET; j++) {
    prefetch_center_frame(data, mean, start, j, &sp, M);
  }

  // first row
  for (int j = 1 + INIT_CENTER_OFFSET; j < (M+1); j++) {
    prefetch_center_frame(data, mean, start, j, &sp, M);
    // ISSUE_VINST()
  }

  // steady state
  for (int i = start + VECTOR_LEN; i < end; i+=VECTOR_LEN) {
    for (int j = 1; j < (M+1); j++) {
      prefetch_center_frame(data, mean, i, j, &sp, M);
      // ISSUE_VINST()   
    }
  }

  // cooldown
  for (int j = M - INIT_CENTER_OFFSET; j < (M+1); j++) {
    // ISSUE_VINST()
  }

  }
  else {
  DTYPE *sp_ptr = (DTYPE*)getSpAddr(ptid, 0);
  for (int i = start + vtid; i < end; i+=VECTOR_LEN) {
    for (int j = 1; j < (M+1); j++) {
      FRAME_START(CENTER_FRAME_SIZE);
      data[i * (M+1) + j] = sp_ptr[sp + 0] - sp_ptr[sp + 1];
      REMEM(CENTER_FRAME_SIZE);
      sp+=2;
      if (sp == POST_FRAME_WORD) sp = 0;
    }
  }

  }

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
  int start = ((groupId + 0) * M) / numGroups;
  int end   = ((groupId + 1) * M) / numGroups;

  // make it a factor of vector group mapping size
  start = 1 + roundUp(start, VECTOR_LEN);
  end   = 1 + roundUp(end  , VECTOR_LEN);

  int sp  = 0;

  VECTOR_EPOCH(mask);

  if (ptid == 0) {
  
  // ISSUE_VINST()

  // initial round
  for (int i = 1; i < 1 + INIT_COVAR_OFFSET; i++) {
    prefetch_covar_frame(data, i, start, start, &sp, M);
  }

  // first row
  for (int i = 1 + INIT_COVAR_OFFSET; i < (N+1); i++) {
    prefetch_covar_frame(data, i, start, start, &sp, M);
    // ISSUE_VINST()
  }

  // steady state
  for (int j1 = start; j1 < end; j1++) {
    int startJ2 = j1;
    if (j1 == start) startJ2 += VECTOR_LEN;
    for (int j2 = startJ2; j2 < (M+1); j2+=VECTOR_LEN) {
      for (int i = 1; i < (N+1); i++) {
        prefetch_covar_frame(data, i, j1, j2, &sp, M);
        // ISSUE_VINST()
      }
    }
  }

  // cooldown
  for (int i = N - INIT_MEAN_OFFSET; i < (N+1); i++) {
    // ISSUE_VINST()
  }

  }
  else {
  DTYPE *sp_ptr = (DTYPE*)getSpAddr(ptid, 0);
  for (int j1 = start; j1 < end; j1++) {
    // for (int j2 = j1 + vtid; j2 < (M+1); j2+=VECTOR_LEN) { // TODO needs predication on this loop
    for (int j2 = j1; j2 < (M+1); j2+=VECTOR_LEN) {
      int j2_idx = j2 + vtid;
      DTYPE symmat_idx = 0.0f;
      for (int i = 1; i < (N+1); i++) {
        FRAME_START(COVAR_FRAME_SIZE);
        // printf("j1 %d j2 %d i %d vtid %d %f ?= %f | %f ?= %f\n", j1, j2, i, vtid, sp_ptr[sp+0], data[i *(M+1) + j1], sp_ptr[sp+1], data[i *(M+1) + j2]);
        symmat_idx += sp_ptr[sp + 0] * sp_ptr[sp + 1]; // not prefetching the right stuff here
        REMEM(COVAR_FRAME_SIZE);
        sp+=2;
        if (sp == POST_FRAME_WORD) sp = 0;
      }

      if (j2_idx < (M+1)) {
        symmat[j2_idx * (M+1) + j1] = symmat_idx;
        symmat[j1 * (M+1) + j2_idx] = symmat_idx;
      }
    }
  }

  }

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