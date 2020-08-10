#ifndef __VVADD_H__
#define __VVADD_H__

// #define VEC_LEN 4
#ifdef VEC_LEN
#define _VEC
#endif

#define BLK_DIM 4 //tile size

#if VEC_LEN==4
#define DIM_X 2
#elif VEC_LEN==16
#define DIM_X 4
#endif

#define ALPHA 4
#define BETA 5

// #define SHARING
// #define C_PREFETCH 
// #define MANYCORE_PREFETCH

#define INIT_FRAMES 2

#ifdef SHARING
#define REGION_SIZE (BLK_DIM*2)/DIM_X
#define NUM_REGIONS (512 / REGION_SIZE)
#else
#define REGION_SIZE (BLK_DIM * 2)
#define NUM_REGIONS (512 / REGION_SIZE)
#endif

typedef float DTYPE;

// pthread argument for the kernel
typedef struct Kern_Args
{
  DTYPE *a, *b, *c;
  int m, n, t;
  int tid_x, tid_y;
  int dim_x, dim_y;
} Kern_Args;

// helper to pack vvadd args
Kern_Args *construct_args(
    DTYPE *a, DTYPE *b, DTYPE *c, int m, int n, int t,
    int tid_x, int tid_y, int dim_x, int dim_y);

// pthread call
void *pthread_kernel(void *args);

// vvadd kernel
void kernel(
    DTYPE *a, DTYPE *b, DTYPE *c, int m, int n, int t,
    int tid_x, int tid_y, int dim_x, int dim_y);

#endif
