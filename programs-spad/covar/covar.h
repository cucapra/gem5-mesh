#ifndef __COVAR_H__
#define __COVAR_H__

// data type to do computation with
#define DTYPE float

// one of these should be defined to dictate config
// #define NO_VEC 1
// #define VEC_4_SIMD 1
// #define VEC_16_SIMD 1

// vvadd_execute config directives
#if !defined(NO_VEC)
#define USE_VEC 1
#endif

// vector grouping directives
#if defined(VEC_4_SIMD)
#define VECTOR_LEN 4
#define INIT_FRAMES 1
#endif
#if defined(VEC_16_SIMD)
#define VECTOR_LEN 16
#define INIT_FRAMES 0
#endif

// prefetch sizing
#ifdef USE_VEC
// dedicate a quarter of scratchpad to frames
#define POST_FRAME_WORD 256

// number of frames to get ahead
// #define INIT_FRAMES 1

// prefetch config for mean kernel
#define MEAN_FRAME_SIZE 1
#define NUM_MEAN_FRAMES (POST_FRAME_WORD / MEAN_FRAME_SIZE)
#define MEAN_PREFETCH_LEN VECTOR_LEN
#define INIT_MEAN_OFFSET (INIT_FRAMES * 1)

// prefetch config for center kernel
#define CENTER_FRAME_SIZE 2
#define NUM_CENTER_FRAMES (POST_FRAME_WORD / CENTER_FRAME_SIZE)
#define CENTER_PREFETCH_LEN 1
#define INIT_CENTER_OFFSET (INIT_FRAMES * 1)

// prefetch config for covar kernel
#define COVAR_FRAME_SIZE 2
#define NUM_COVAR_FRAMES (POST_FRAME_WORD / COVAR_FRAME_SIZE)
#define COVAR_J1_PREFETCH_LEN 1
#define COVAR_J2_PREFETCH_LEN VECTOR_LEN
#define INIT_COVAR_OFFSET (INIT_FRAMES * 1)
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
  DTYPE *data, *mean, *symmat;
  int N, M;
  int tid_x, tid_y;
  int dim_x, dim_y;
} Kern_Args;

// helper to pack args
Kern_Args *construct_args(
    DTYPE *data, DTYPE *mean, DTYPE *symmat,
    int N, int M,
    int tid_x, int tid_y, int dim_x, int dim_y
  );

// pthread call
void *pthread_kernel(void *args);

// kernel
void kernel(
    DTYPE *data, DTYPE *mean, DTYPE *symmat,
    int N, int M,
    int tid_x, int tid_y, int dim_x, int dim_y
  );

#endif
