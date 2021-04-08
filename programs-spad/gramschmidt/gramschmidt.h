#ifndef __GRAMSCHMIDT_H__
#define __GRAMSCHMIDT_H__

#include "bind_defs.h"

// data type to do computation with
#define DTYPE float

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

// prefetch sizings
#if defined(USE_VEC) || defined(MANYCORE_PREFETCH)

#ifndef INIT_FRAMES
#if VECTOR_LEN==16
// fails if INIT_FRAMES 1, due to cache overload
#define INIT_FRAMES 0
#else
#define INIT_FRAMES 1
#endif
#endif

#define POST_FRAME_WORD_NORM 256
#define PREFETCH_LEN_NORM 1
#define UNROLL_LEN_NORM 1
#define FRAME_SIZE_NORM (UNROLL_LEN_NORM)
#define NUM_FRAMES_NORM (POST_FRAME_WORD_NORM / FRAME_SIZE_NORM)
#define STRIDE_NORM (UNROLL_LEN_NORM*VECTOR_LEN)
#define INIT_OFFSET_NORM (INIT_FRAMES*STRIDE_NORM)

#define POST_FRAME_WORD_SUB 256
#define UNROLL_LEN_SUB 1
#define FRAME_SIZE_SUB (2*UNROLL_LEN_SUB)
#define NUM_FRAMES_SUB (POST_FRAME_WORD_SUB / FRAME_SIZE_SUB)
#define INIT_OFFSET_SUB (INIT_FRAMES*UNROLL_LEN_SUB)


#ifdef MANYCORE_PREFETCH
#define VPREFETCH_LR_FAIR(sp, memIdx, core, len, style)  \
  VPREFETCH_L(sp, memIdx, core, len, style)
#else
#define VPREFETCH_LR_FAIR(sp, memIdx, core, len, style)  \
  VPREFETCH_LR(sp, memIdx, core, len, style)
#endif

#ifdef MANYCORE_PREFETCH
  #define VERTICAL_FETCH_TYPE (TO_SELF)
  #define HORIZONTAL_FETCH_TYPE (TO_SELF)
#else
  #define VERTICAL_FETCH_TYPE (TO_ONE_CORE)
  #define HORIZONTAL_FETCH_TYPE (TO_ALL_CORES)
#endif


inline void prefetch_normalize_frame(DTYPE *a, int i, int k, int numVectors, int *sp) {
  for (int u = 0; u < UNROLL_LEN_NORM; u++) {
    for (int core = 0; core < VECTOR_LEN; core++) {
      int i_idx = i + core + u*VECTOR_LEN;
      VPREFETCH_L(*sp + u, &a[i_idx * numVectors + k], core, PREFETCH_LEN_NORM, VERTICAL_FETCH_TYPE);
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
      VPREFETCH_L(*sp + u, &q[(i + u) * numVectors + k], core, 1, VERTICAL_FETCH_TYPE);
    }
  }

  for (int u = 0; u < UNROLL_LEN_SUB; u++) {
    VPREFETCH_LR_FAIR(*sp + UNROLL_LEN_SUB + u, &a[(i + u) * numVectors + j], 0, 1, HORIZONTAL_FETCH_TYPE);
  }

// q[i * numVectors + k] * a[i * numVectors + j];

  #ifndef MANYCORE_PREFETCH
  *sp = *sp + FRAME_SIZE_SUB;
  if (*sp == POST_FRAME_WORD_SUB) *sp = 0;
  #endif
}

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
