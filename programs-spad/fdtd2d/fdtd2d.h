#ifndef __FDTD2D_H__
#define __FDTD2D_H__

// data type to do computation with
#define DTYPE float

// one of these should be defined to dictate config
// #define NO_VEC 1
#define VEC_4_SIMD 1
// #define VEC_16_SIMD 1

// vvadd_execute config directives
#if !defined(NO_VEC)
#define USE_VEC 1
#endif

// vector grouping directives
#if defined(VEC_4_SIMD)
#define VECTOR_LEN 4
#endif
#if defined(VEC_16_SIMD)
#define VECTOR_LEN 16
#endif

// prefetch sizing
#ifdef USE_VEC
// dedicate a quarter of scratchpad to frames
#define POST_FRAME_WORD 120

// number of frames to get ahead
#define INIT_FRAMES 1

// prefetch sizings for step1
#define STEP1_REGION_SIZE 3
#define STEP1_NUM_REGIONS (POST_FRAME_WORD / STEP1_REGION_SIZE)
#endif

// covariance specific value
#define FLOAT_N 3214212.01

// grid dim xy assuming always a square
#if _N_SPS==16
#define GRID_XDIM 4
#define GRID_YDIM 4
#elif _N_SPS==64
#define GRID_XDIM 8
#define GRID_YDIM 8
#endif

// pthread argument for the kernel
typedef struct Kern_Args {
  DTYPE *fict, *ex, *ey, *hz;
  int NX, NY, tmax;
  int tid_x, tid_y;
  int dim_x, dim_y;
} Kern_Args;

// helper to pack args
Kern_Args *construct_args(
    DTYPE *fict, DTYPE *ex, DTYPE *ey, DTYPE *hz,
    int NX, int NY, int tmax,
    int tid_x, int tid_y, int dim_x, int dim_y
  );

// pthread call
void *pthread_kernel(void *args);

// kernel
void kernel(
    DTYPE *fict, DTYPE *ex, DTYPE *ey, DTYPE *hz,
    int NX, int NY, int tmax,
    int tid_x, int tid_y, int dim_x, int dim_y
  );

#endif
