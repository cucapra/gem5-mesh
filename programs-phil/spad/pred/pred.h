#ifndef __PRED_H__
#define __PRED_H__

// data type to do computation with
#define DTYPE int

// pthread argument for the kernel
typedef struct Kern_Args {
  DTYPE *a, *b, *c;
  int size;
  int tid_x, tid_y;
  int dim_x, dim_y;
} Kern_Args;

// helper to pack vvadd args
Kern_Args *construct_args(
    DTYPE *a, DTYPE *b, DTYPE *c, int size,
    int tid_x, int tid_y, int dim_x, int dim_y
  );

// pthread call
void *pthread_kernel(void *args);

// vvadd kernel
void kernel(
    DTYPE *a, DTYPE *b, DTYPE *c, int size,
    int tid_x, int tid_y, int dim_x, int dim_y
  );

#endif
