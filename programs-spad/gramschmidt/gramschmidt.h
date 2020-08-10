#ifndef __GRAMSCHMIDT_H__
#define __GRAMSCHMIDT_H__

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
#endif
#if defined(VEC_16_SIMD)
#define VECTOR_LEN 16
#endif
#if defined(MANYCORE_PREFETCH)
#define VECTOR_LEN 1
#endif

// prefetch sizings
#if defined(USE_VEC) || defined(MANYCORE_PREFETCH)

#define INIT_FRAMES 1

#define POST_FRAME_WORD_NORM 256
#define PREFETCH_LEN_NORM 1
#define UNROLL_LEN_NORM 2
#define FRAME_SIZE_NORM (UNROLL_LEN_NORM)
#define NUM_FRAMES_NORM (POST_FRAME_WORD_NORM / FRAME_SIZE_NORM)
#define STRIDE_NORM (UNROLL_LEN_NORM*VECTOR_LEN)
#define INIT_OFFSET_NORM (INIT_FRAMES*STRIDE_NORM)

#define POST_FRAME_WORD_SUB 256
#define UNROLL_LEN_SUB 4
#define FRAME_SIZE_SUB (2*UNROLL_LEN_SUB)
#define NUM_FRAMES_SUB (POST_FRAME_WORD_SUB / FRAME_SIZE_SUB)
#define INIT_OFFSET_SUB (INIT_FRAMES*UNROLL_LEN_SUB)


inline void prefetch_normalize_frame(DTYPE *a, int i, int k, int numVectors, int *sp) {
  for (int u = 0; u < UNROLL_LEN_NORM; u++) {
    for (int core = 0; core < VECTOR_LEN; core++) {
      int i_idx = i + core + u*VECTOR_LEN;
      VPREFETCH_L(*sp + u, &a[i_idx * numVectors + k], core, PREFETCH_LEN_NORM, VERTICAL);
    }
  }

  #ifndef MANYCORE_PREFETCH
  *sp = *sp + FRAME_SIZE_NORM;
  if (*sp == POST_FRAME_WORD_NORM) *sp = 0;
  #endif
}

inline void prefetch_dot_frame(DTYPE *q, DTYPE *a, int i, int j, int k, int numVectors, int *sp) {
  // fetch the same q to each core
  for (int u = 0; u < UNROLL_LEN_SUB; u++) {
    for (int core = 0; core < VECTOR_LEN; core++) {
      VPREFETCH_L(*sp + u, &q[(i + u) * numVectors + k], core, 1, VERTICAL);
    }
  }

  for (int u = 0; u < UNROLL_LEN_SUB; u++) {
    VPREFETCH_LR(*sp + UNROLL_LEN_SUB + u, &a[(i + u) * numVectors + j], 0, VECTOR_LEN, HORIZONTAL);
  }

// q[i * numVectors + k] * a[i * numVectors + j];

  #ifndef MANYCORE_PREFETCH
  *sp = *sp + FRAME_SIZE_SUB;
  if (*sp == POST_FRAME_WORD_SUB) *sp = 0;
  #endif
}

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
  DTYPE *a, *r, *q;
  int numVectors, vectorLen;
  int tid_x, tid_y;
  int dim_x, dim_y;
} Kern_Args;

// helper to pack args
Kern_Args *construct_args(
    DTYPE *a, DTYPE *r, DTYPE *q, 
    int numVectors, int vectorLen,
    int tid_x, int tid_y, int dim_x, int dim_y
  );

// pthread call
void *pthread_kernel(void *args);

// kernel
void kernel(
    DTYPE *a, DTYPE *r, DTYPE *q, 
    int numVectors, int vectorLen,
    int tid_x, int tid_y, int dim_x, int dim_y
  );

#endif
