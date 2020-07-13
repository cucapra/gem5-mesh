#ifndef __FUSED_SDDMM_H__
#define __FUSED_SDDMM_H__

#include "sparse_mat.h"

// pthread argument for the kernel
typedef struct Kern_Args {
  data_t *a, *b_t;
  sparse_mat_t *c, *mask;
  int m, n, t;
  int tid_x, tid_y, threads_x, threads_y;
  int start_row, end_row;
} Kern_Args;

Kern_Args *construct_args(data_t *a, data_t *b_t, sparse_mat_t *c,
    sparse_mat_t *mask, int m, int n, int t, int tid_x, int tid_y, 
    int threads_x, int threads_y, int start_row, int end_row);

// pthread call
void *pthread_kernel(void *args);

// only do the dot product for output matrix coords that will be non-zero
// also fuse in the elementwise division
// dubbed the 'weirdo-kernel'
void kernel(data_t *a, data_t *b_t, sparse_mat_t *c,
    sparse_mat_t *mask, int m, int n, int t, int tid_x, int tid_y, 
    int threads_x, int threads_y, int start_row, int end_row);



#endif
