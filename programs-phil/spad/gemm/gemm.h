#ifndef __VVADD_H__
#define __VVADD_H__

// pthread argument for the kernel
typedef struct Kern_Args {
  float *a, *b, *c;
  int m, n, t;
  int blk_dim;
  int tid_x, tid_y;
  int dim_x, dim_y;
} Kern_Args;

// helper to pack vvadd args
Kern_Args *construct_args(
    float *a, float *b, float *c, int m, int n, int t, int blk_dim,
    int tid_x, int tid_y, int dim_x, int dim_y
  );

// pthread call
void *pthread_kernel(void *args);

// vvadd kernel
void kernel(
    float *a, float *b, float *c, int m, int n, int t, int blk_dim,
    int tid_x, int tid_y, int dim_x, int dim_y
  );

#endif
