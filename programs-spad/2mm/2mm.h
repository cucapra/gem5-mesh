#ifndef __2MM_H__
#define __2MM_H__

// #define VEC_LEN 4
#ifdef VEC_LEN
#define _VEC
#endif

#define BLK_DIM 4 //tile size

// #define MANYCORE_PREFETCH

#define REGION_SIZE (BLK_DIM * 2)
#define NUM_REGIONS (512 / REGION_SIZE)


#define ALPHA 1
#define BETA 0

typedef float DTYPE;

// pthread argument for the kernel
typedef struct Kern_Args
{
  DTYPE *a, *b, *c, *cT, *d, *e;
  int m,n,t1,t2;
  int tid_x, tid_y;
  int dim_x, dim_y;
} Kern_Args;

// helper to pack vvadd args
Kern_Args *construct_args(
    DTYPE *a, DTYPE *b, DTYPE *c, DTYPE *cT, DTYPE *d, DTYPE *e, int m, int n, int t1, int t2,
    int tid_x, int tid_y, int dim_x, int dim_y);

// pthread call
void *pthread_kernel(void *args);

// vvadd kernel
void kernel(
    DTYPE *a, DTYPE *b, DTYPE *c, DTYPE *cT, DTYPE *d, DTYPE *e, int m, int n, int t1, int t2,
    int ptid_x, int ptid_y, int pdim_x, int pdim_y);

#endif
