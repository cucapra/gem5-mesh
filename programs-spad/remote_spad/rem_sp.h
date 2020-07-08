#ifndef __REMSP_H__
#define __REMSP_H__

typedef float DTYPE;

// pthread argument for the kernel
typedef struct Kern_Args {
  DTYPE *a, *c;
  int n;
  int tid_x, tid_y;
  int dim_x, dim_y;
} Kern_Args;



// helper to pack vvadd args
Kern_Args *construct_args(
    DTYPE *a, DTYPE *c, int n,
    int tid_x, int tid_y, int dim_x, int dim_y
  );

// pthread call
void *pthread_kernel(void *args);

// vvadd kernel
void kernel(
    DTYPE *a, DTYPE *c, int n,
    int tid_x, int tid_y, int dim_x, int dim_y
  );

#endif
