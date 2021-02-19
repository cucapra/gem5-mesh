#ifndef __SYRK_KERNEL_H__
#define __SYRK_KERNEL_H__

#include "syrk.h"

#ifdef USE_VEC
// do reduction on scalar core
// each core in vector group should have sent a message in a frame
inline void do_sum(DTYPE *c, int i, int j_base, int N, DTYPE *sp_ptr, int *sp_idx) {
  // start fetching now so dont want for frame
  DTYPE sum[ACCUM_GRANULARITY];
  #pragma GCC unroll(8)
  for (int j = 0; j < ACCUM_GRANULARITY; j++) {
    int j_idx = j + j_base;
    sum[j] = c[i * N + j_idx] * beta;
  }
  
  for (int j = 0; j < ACCUM_GRANULARITY; j++) {
    FRAME_START(SCALAR_FRAME_SIZE);
    for (int i = 0; i < SCALAR_FRAME_SIZE; i++) {
      sum[j] += sp_ptr[*sp_idx + i];
    }
    int j_idx = j + j_base;
    STORE_NOACK(sum[j], &c[i * N + j_idx], 0);
    REMEM(SCALAR_FRAME_SIZE);
    (*sp_idx)+=SCALAR_FRAME_SIZE;
    *sp_idx = *sp_idx % POST_FRAME_WORD;
  }
}
#endif


void tril_syrk(int mask, DTYPE *a, DTYPE *c, int N, int M, 
                  int ptid, int groupId, int numGroups, int vtid,
                  int ptidMailer, int linkId, int numGroupsPerMailer);

#endif