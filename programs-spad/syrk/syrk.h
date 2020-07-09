#ifndef __SYRK_H__
#define __SYRK_H__

// data type to do computation with
#define DTYPE float

/* Declared constant values for alpha and beta specfically for SYRK bench */
#define alpha 12435
#define beta 4546

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
#define POST_FRAME_WORD 256

// number of frames to get ahead
#define INIT_FRAMES 4

// prefetch config for inner kernel
#define INNER_FRAME_SIZE 2
#define NUM_FRAMES (POST_FRAME_WORD / INNER_FRAME_SIZE)
#define INNER_PREFETCH_LEN 1
#define INIT_OFFSET (INIT_FRAMES * 1)

// frame size to get the c to accumulate on
#define OUTER_FRAME_SIZE INNER_FRAME_SIZE
#define OUTER_PREFETCH_LEN 1
#endif

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
  DTYPE *a, *c;
  int N, M;
  int tid_x, tid_y;
  int dim_x, dim_y;
} Kern_Args;

// helper to pack args
Kern_Args *construct_args(
    DTYPE *a, DTYPE *c,
    int N, int M,
    int tid_x, int tid_y, int dim_x, int dim_y
  );

// pthread call
void *pthread_kernel(void *args);

// kernel
void kernel(
    DTYPE *a, DTYPE *c,
    int N, int M,
    int tid_x, int tid_y, int dim_x, int dim_y
  );

#endif
