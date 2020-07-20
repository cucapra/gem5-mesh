#ifndef __TEMP_H__
#define __TEMP_H__

#include "gemm_common.h"

// #define _VEC
#define VEC_LEN 4 //vec group size
#define BLK_DIM 4 //tile size

#if VEC_LEN==4
#define DIM_X 2
#elif VEC_LEN==16
#define DIM_X 4
#endif

#undef SHARING
#undef C_PREFETCH 
#define MANYCORE_PREFETCH

#ifdef SHARING
#define REGION_SIZE (BLK_DIM*2)/DIM_X
#define NUM_REGIONS (512 / REGION_SIZE)
#else
#define REGION_SIZE (BLK_DIM * 2)
#define NUM_REGIONS (512 / REGION_SIZE)
#endif

#define ALPHA 1
#define BETA 0

typedef int DTYPE;

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
