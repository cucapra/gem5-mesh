#ifndef __TEMP_H__
#define __TEMP_H__

// #define VEC_LEN 4
#ifdef VEC_LEN
#define _VEC
#endif

// #define MANYCORE_PREFETCH


// #define POLYBENCH_VERSION
// #define REDUCE_VERSION

#ifndef INIT_FRAMES
#define INIT_FRAMES 2
#endif

#define PREFETCH_LEN 16
#define REGION_SIZE (PREFETCH_LEN * 2)
#define NUM_REGIONS (512 / REGION_SIZE)

typedef float DTYPE;

// pthread argument for the kernel
typedef struct Kern_Args
{
  DTYPE *a, *_x, *_y, *ax, *_y_partial;
  int nx, ny;
  int tid_x, tid_y;
  int dim_x, dim_y;
} Kern_Args;

// helper to pack vvadd args
Kern_Args *construct_args(
    DTYPE *a, DTYPE *_x, DTYPE *_y, DTYPE *ax, DTYPE* _y_partial, int nx, int ny,
    int tid_x, int tid_y, int dim_x, int dim_y);

// pthread call
void *pthread_kernel(void *args);

// vvadd kernel
void kernel(
    DTYPE *a, DTYPE *_x, DTYPE *_y, DTYPE *ax, DTYPE *_y_partial, int nx, int ny,
    int tid_x, int tid_y, int dim_x, int dim_y);

#endif
