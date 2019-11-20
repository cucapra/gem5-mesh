#ifndef __VVADD_H__
#define __VVADD_H__

// pthread argument for the kernel
typedef struct Kern_Args {
  int *a, *b, *c, *d;
  int n;
  int tid_x, tid_y;
  int dim_x, dim_y;
} Kern_Args;

// helper to pack vvadd args
Kern_Args *construct_args(
    int *a, int *b, int *c, int *d, int n,
    int tid_x, int tid_y, int dim_x, int dim_y
  );

// pthread call
void *pthread_kernel(void *args);

// vvadd kernel
void kernel(
    int *a, int *b, int *c, int *d, int n,
    int tid_x, int tid_y, int dim_x, int dim_y
  );

#endif
