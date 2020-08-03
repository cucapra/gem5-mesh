#ifndef __GESUMMV_H__
#define __GESUMMV_H__

// #define VEC_LEN 4
#ifdef VEC_LEN
#define _VEC
#endif

#define REGION_SIZE 12 //configure using LCM of required frame/region sizes
#define NUM_REGIONS 50 

#define ALPHA 3
#define BETA 2

typedef float DTYPE;

// pthread argument for the kernel
typedef struct Kern_Args
{
  DTYPE *a, *b, *x, *tmp, *y;
  int n;
  int tid_x, tid_y;
  int dim_x, dim_y;
} Kern_Args;

// helper to pack vvadd args
Kern_Args *construct_args(
    DTYPE *a, DTYPE *b, DTYPE *x, DTYPE *tmp, DTYPE *y, int n,
    int tid_x, int tid_y, int dim_x, int dim_y);

// pthread call
void *pthread_kernel(void *args);

// vvadd kernel
void kernel(
    DTYPE *a, DTYPE *b, DTYPE *x, DTYPE *tmp, DTYPE *y, int n,
    int tid_x, int tid_y, int dim_x, int dim_y);

#endif
