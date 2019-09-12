#ifndef __WEIRDO_KERNEL_H__
#define __WEIRDO_KERNEL_H__

#include "sparse_mat.h"

// pthread argument for the kernel
typedef struct sddmm_args_t {
  data_t *a, *b_t;
  sparse_mat_t *c, *mask;
  int m, n, t;
  int tid_x, tid_y, threads_x, threads_y;
  int start_row, end_row;
} sddmm_args_t;

sddmm_args_t *construct_sddmm_args(data_t *a, data_t *b_t, sparse_mat_t *c,
    sparse_mat_t *mask, int m, int n, int t, int tid_x, int tid_y, 
    int threads_x, int threads_y, int start_row, int end_row);

// parallel call
void *pthread_sddmm_fused_div(void *args);

// only do the dot product for output matrix coords that will be non-zero
// also fuse in the elementwise division
// dubbed the 'weirdo-kernel'
void sddmm_fused_div(data_t *a, data_t *b_t, sparse_mat_t *c,
    sparse_mat_t *mask, int m, int n, int t, int tid_x, int tid_y, 
    int threads_x, int threads_y, int start_row, int end_row);



#endif
