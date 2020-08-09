#ifndef __COVAR_H__
#define __COVAR_H__

#include "bind_defs.h"

// data type to do computation with
#define DTYPE float

// one of these should be defined to dictate config
// #define NO_VEC 1
// #define VEC_4_SIMD 1
// #define VEC_16_SIMD 1
// #define MANYCORE_PREFETCH

// vvadd_execute config directives
#if !defined(NO_VEC) && !defined(MANYCORE_PREFETCH)
#define USE_VEC 1
#endif

// vector grouping directives
#if defined(VEC_4_SIMD)
#define VECTOR_LEN 4
#define INIT_FRAMES 1
#endif
#if defined(VEC_16_SIMD)
#define VECTOR_LEN 16
#define INIT_FRAMES 1
#endif
#if defined(MANYCORE_PREFETCH)
#define VECTOR_LEN 1
#define INIT_FRAMES 0
#endif

// prefetch sizing
#if defined(USE_VEC) || defined(MANYCORE_PREFETCH)
// dedicate a quarter of scratchpad to frames
#define POST_FRAME_WORD 256

// number of frames to get ahead
// #define INIT_FRAMES 1

// prefetch config for mean kernel
#define MEAN_UNROLL_LEN 16
#define MEAN_FRAME_SIZE MEAN_UNROLL_LEN
#define NUM_MEAN_FRAMES (POST_FRAME_WORD / MEAN_FRAME_SIZE)
#define MEAN_PREFETCH_LEN VECTOR_LEN
#define INIT_MEAN_OFFSET (INIT_FRAMES * MEAN_FRAME_SIZE)

// prefetch config for center kernel
#define CENTER_PREFETCH_LEN 16
#define CENTER_FRAME_SIZE (2*CENTER_PREFETCH_LEN)
#define NUM_CENTER_FRAMES (POST_FRAME_WORD / CENTER_FRAME_SIZE)
#define INIT_CENTER_OFFSET (INIT_FRAMES * CENTER_PREFETCH_LEN)

// prefetch config for covar kernel
#define COVAR_UNROLL_LEN 8
#define COVAR_J1_PREFETCH_LEN 1
#define COVAR_FRAME_SIZE (2*COVAR_UNROLL_LEN)
#define NUM_COVAR_FRAMES (POST_FRAME_WORD / COVAR_FRAME_SIZE)
#define COVAR_J2_PREFETCH_LEN VECTOR_LEN
#define INIT_COVAR_OFFSET (INIT_FRAMES * COVAR_UNROLL_LEN)


inline void prefetch_mean_frame(DTYPE *data, int i, int j, int *sp, int M) {
  // can't merge into a vprefetch but can still unroll the old fashioned way
  for (int u = 0; u < MEAN_UNROLL_LEN; u++) {
    VPREFETCH_LR(*sp + u, &data[(i + u) * (M+1) + j], 0, MEAN_PREFETCH_LEN, HORIZONTAL);
  }
  // VPREFETCH_R(*sp, &data[i * (M+1) + j], 0, MEAN_PREFETCH_LEN, HORIZONTAL);

  #ifndef MANYCORE_PREFETCH
  *sp = *sp + MEAN_FRAME_SIZE;
  if (*sp == POST_FRAME_WORD) *sp = 0;
  #endif
}

inline void prefetch_center_frame(DTYPE *data, DTYPE *mean, int i, int j, int *sp, int M) {
  // fetch data
  for (int core = 0; core < VECTOR_LEN; core++) {
    VPREFETCH_LR(*sp, &data[(i + core) * (M+1) + j], core, CENTER_PREFETCH_LEN, VERTICAL);
  }

  // TODO should do more than 1 here
  for (int core = 0; core < VECTOR_LEN; core++) {
    VPREFETCH_LR(*sp + CENTER_PREFETCH_LEN, &mean[j], core, CENTER_PREFETCH_LEN, VERTICAL);
  }

  #ifndef MANYCORE_PREFETCH
  *sp = *sp + CENTER_FRAME_SIZE;
  if (*sp == POST_FRAME_WORD) *sp = 0;
  #endif
}

inline void prefetch_covar_frame(DTYPE *data, int i, int j1, int j2, int *sp, int M) {
  // everyone in groups gets the same j1. could share and/or do vertical
  
  for (int u = 0; u < COVAR_UNROLL_LEN; u++) {
    for (int core = 0; core < VECTOR_LEN; core++) {
      VPREFETCH_LR(*sp + u, &data[(i + u) * (M+1) + j1], core, COVAR_J1_PREFETCH_LEN, VERTICAL);
    }
  }
  // printf("%d %f\n", *sp, data[i * (M+1) + j1]);
  // *sp = *sp + COVAR_J1_PREFETCH_LEN;

  for (int u = 0; u < COVAR_UNROLL_LEN; u++) {
    VPREFETCH_LR(*sp + COVAR_UNROLL_LEN + u, &data[(i + u) * (M+1) + j2], 0, COVAR_J2_PREFETCH_LEN, HORIZONTAL);
  }
  // VPREFETCH_R(*sp, &data[i * (M+1) + j2], 0, COVAR_J2_PREFETCH_LEN, HORIZONTAL);
  // printf("%d %f\n", *sp, data[i * (M+1) + j2]);

  #ifndef MANYCORE_PREFETCH
  *sp = *sp + COVAR_FRAME_SIZE;
  if (*sp == POST_FRAME_WORD) *sp = 0;
  #endif
}



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
