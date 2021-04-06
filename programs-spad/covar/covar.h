#ifndef __COVAR_H__
#define __COVAR_H__

#include "bind_defs.h"

// data type to do computation with
#define DTYPE float

// vector grouping directives

// --------------------------------------------------------------------------------
// NO_VEC
// MANYCORE_PREFETCH
// PER_CORE_SIMD
// [VECTOR_LEN=4,16 + any combo of PER_CORE_SIMD, LONGLINES] 

#ifdef VECTOR_LEN
#define USE_VEC
#endif
#ifdef MANYCORE_PREFETCH
#define VECTOR_LEN (1)
#endif

// --------------------------------------------------------------------------------

// prefetch sizing
#if defined(USE_VEC) || defined(MANYCORE_PREFETCH)
// dedicate a quarter of scratchpad to frames
#define POST_FRAME_WORD 512

// number of frames to get ahead
#ifndef INIT_FRAMES
#define INIT_FRAMES 1
#endif

// prefetch config for mean kernel
#ifdef LONGLINES
  #define MEAN_PREFETCH_LEN (CACHE_LINE_SIZE / sizeof(DTYPE) / VECTOR_LEN)
  #define MEAN_J_STRIDE (MEAN_PREFETCH_LEN * VECTOR_LEN)
#else
  #define MEAN_PREFETCH_LEN (16)
  #define MEAN_J_STRIDE (MEAN_PREFETCH_LEN)
#endif

#define MEAN_FRAME_SIZE (MEAN_PREFETCH_LEN)
#define NUM_MEAN_FRAMES (POST_FRAME_WORD / MEAN_FRAME_SIZE)
#define INIT_MEAN_OFFSET (INIT_FRAMES * MEAN_J_STRIDE)

// prefetch config for reduce kernel
#ifdef LONGLINES
  #define REDUCE_PREFETCH_LEN (CACHE_LINE_SIZE / sizeof(DTYPE) / VECTOR_LEN)
  #define REDUCE_J_STRIDE (REDUCE_PREFETCH_LEN * VECTOR_LEN)

  #define REDUCE_FRAME_SIZE (2*REDUCE_PREFETCH_LEN)
  #define NUM_REDUCE_FRAMES (POST_FRAME_WORD / REDUCE_FRAME_SIZE)
  #define INIT_REDUCE_OFFSET (INIT_FRAMES * REDUCE_J_STRIDE)
#endif

// prefetch config for covar kernel
#ifdef LONGLINES
  #define COVAR_PREFETCH_LEN (CACHE_LINE_SIZE / sizeof(DTYPE) / VECTOR_LEN)
  #define COVAR_J_STRIDE (COVAR_PREFETCH_LEN * VECTOR_LEN)
#else
  #define COVAR_PREFETCH_LEN (16)
  #define COVAR_J_STRIDE (COVAR_PREFETCH_LEN)
#endif

#define COVAR_FRAME_SIZE (2*COVAR_PREFETCH_LEN)
#define NUM_COVAR_FRAMES (POST_FRAME_WORD / COVAR_FRAME_SIZE)
#define INIT_COVAR_OFFSET (INIT_FRAMES * COVAR_J_STRIDE)

// -------------------------------------------------------------------------------
#ifndef ACCUM_GRANULARITY
// default to coarser b/c can only help. although 2 is probably sufficient
#define ACCUM_GRANULARITY 2
#endif
#if VECTOR_LEN == 4
#define NUM_GROUPS_PER_PIPE (3)
#else
#define NUM_GROUPS_PER_PIPE (1)
#endif
#define SUB_FRAME_SIZE (VECTOR_LEN * NUM_GROUPS_PER_PIPE)
#define MAILER_FRAME_SIZE (SUB_FRAME_SIZE * ACCUM_GRANULARITY)
#define MAILER_NUM_FRAMES (POST_FRAME_WORD / MAILER_FRAME_SIZE)
#define MAILER_POST_FRAME_WORD (MAILER_FRAME_SIZE * MAILER_NUM_FRAMES)
#if MAILER_NUM_FRAMES < 5
  #define FRAMES_TO_SYNC_AFTER (MAILER_NUM_FRAMES)
#else
  #define FRAMES_TO_SYNC_AFTER (5)
#endif
#define PER_CORE_MAILER_FRAME (VECTOR_LEN)
#define PER_CORE_FULL_MAILER_FRAME (PER_CORE_MAILER_FRAME)
#ifdef MANYCORE_PREFETCH
  #define VERTICAL_FETCH_TYPE (TO_SELF)
#else
  #define VERTICAL_FETCH_TYPE (TO_ONE_CORE)
#endif
// ------------------------------------------------------------------------------

inline void prefetch_mean_frame(DTYPE *data, int i, int j, int *sp, int M) {
  #ifdef LONGLINES
  VPREFETCH_LR(*sp + 0, &data[i * M + j], 0, 
    MEAN_PREFETCH_LEN, TO_ALL_CORES);
  #else
  for (int core = 0; core < VECTOR_LEN; core++) {
    VPREFETCH_LR(*sp + 0, &data[(i + core) * M + j], core, MEAN_PREFETCH_LEN, VERTICAL_FETCH_TYPE);
  }
  #endif

  #ifndef MANYCORE_PREFETCH
  *sp = *sp + MEAN_FRAME_SIZE;
  if (*sp == POST_FRAME_WORD) *sp = 0;
  #endif
}

inline void prefetch_covar_frame(DTYPE *data, int i1, int i2, int j, int *sp, int M) {
  #ifdef LONGLINES
  VPREFETCH_LR(*sp + 0, &data[i1 * M + j], 0, 
    COVAR_PREFETCH_LEN, TO_ALL_CORES);
  VPREFETCH_LR(*sp + COVAR_PREFETCH_LEN, &data[i2 * M + j], 0, 
    COVAR_PREFETCH_LEN, TO_ALL_CORES);
  #else
  
  // everyone in groups gets the same j1. could share and/or do vertical
  
  for (int core = 0; core < VECTOR_LEN; core++) {
    VPREFETCH_LR(*sp + 0, &data[i1 * M + j], core, COVAR_PREFETCH_LEN, VERTICAL_FETCH_TYPE);
  }

  for (int core = 0; core < VECTOR_LEN; core++) {
    VPREFETCH_LR(*sp + COVAR_PREFETCH_LEN, &data[(i2 + core) * M + j], core, COVAR_PREFETCH_LEN, VERTICAL_FETCH_TYPE);
  }
  #endif

  // VPREFETCH_R(*sp, &data[i * (M+1) + j2], 0, COVAR_J2_PREFETCH_LEN, HORIZONTAL);
  // printf("%d %f\n", *sp, data[i * (M+1) + j2]);

  #ifndef MANYCORE_PREFETCH
  *sp = *sp + COVAR_FRAME_SIZE;
  if (*sp == POST_FRAME_WORD) *sp = 0;
  #endif
}

#ifdef LONGLINES
inline void prefetch_reduce_frame(DTYPE *data, DTYPE *mean, int i, int j, int *sp, int M) {
  VPREFETCH_LR(*sp + 0, &data[i * M + j], 0, 
    REDUCE_PREFETCH_LEN, TO_ALL_CORES);
  VPREFETCH_LR(*sp + REDUCE_PREFETCH_LEN, &mean[j], 0, 
    REDUCE_PREFETCH_LEN, TO_ALL_CORES);

  *sp = *sp + REDUCE_FRAME_SIZE;
  if (*sp == POST_FRAME_WORD) *sp = 0;
}
#endif


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
  DTYPE *data, *dataT, *mean, *symmat;
  int N, M;
  int tid_x, tid_y;
  int dim_x, dim_y;
} Kern_Args;

// helper to pack args
Kern_Args *construct_args(
    DTYPE *data, DTYPE *dataT, DTYPE *mean, DTYPE *symmat,
    int N, int M,
    int tid_x, int tid_y, int dim_x, int dim_y
  );

// pthread call
void *pthread_kernel(void *args);

// kernel
void kernel(
    DTYPE *data, DTYPE *dataT, DTYPE *mean, DTYPE *symmat,
    int N, int M,
    int tid_x, int tid_y, int dim_x, int dim_y
  );

#endif
