#ifndef __3MM_H__
#define __3MM_H__

// #define VEC_LEN 4
#ifdef VEC_LEN
#define _VEC
#endif

#define BLK_DIM 4 //tile size

#define MANYCORE_PREFETCH

#define REGION_SIZE (BLK_DIM * 2)
#define NUM_REGIONS (512 / REGION_SIZE)


#define ALPHA 1
#define BETA 0

typedef int DTYPE;

// pthread argument for the kernel
typedef struct Kern_Args
{
  DTYPE *a, *b, *e, *c, *d, *f, *eT, *g;
  int m, t1, k ,t2, n;
  int tid_x, tid_y;
  int dim_x, dim_y;
} Kern_Args;

// helper to pack vvadd args
Kern_Args *construct_args(
    DTYPE *a, DTYPE *b, DTYPE *e, DTYPE *c, DTYPE *d, DTYPE *f, DTYPE *eT, DTYPE *g, 
    int m, int t1, int k, int t2, int n, int tid_x, int tid_y, int dim_x, int dim_y);

// pthread call
void *pthread_kernel(void *args);

// vvadd kernel
void kernel(
    DTYPE *a, DTYPE *b, DTYPE *e, DTYPE *c, DTYPE *d, DTYPE *f, DTYPE *eT, DTYPE *g, 
    int m, int t1, int k, int t2, int n, int tid_x, int tid_y, int dim_x, int dim_y);


void transpose(DTYPE *a, int row, int col, DTYPE *aT);

#endif
