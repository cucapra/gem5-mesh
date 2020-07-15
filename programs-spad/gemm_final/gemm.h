#ifndef __VVADD_H__
#define __VVADD_H__

#include "util.h"

// #define _VEC
#define VEC_LEN 4 //vec group size

#define BLK_DIM 4
#define REGION_SIZE (2*BLK_DIM) //configure using LCM of required frame/region sizes
#define NUM_REGIONS (512 / REGION_SIZE) // (0,512) in this case is the hardware region area 

#define ALPHA 4
#define BETA 5

// #define MANYCORE_PREFETCH

typedef int DTYPE;

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
