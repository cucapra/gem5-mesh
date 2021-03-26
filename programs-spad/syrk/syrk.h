#ifndef __SYRK_H__
#define __SYRK_H__

#include "bind_defs.h"

// data type to do computation with
#define DTYPE float

/* Declared constant values for alpha and beta specfically for SYRK bench */
#define alpha 12435
#define beta 4546

// one of these should be defined to dictate config
// #define NO_VEC 1
// #define VEC_4_SIMD 1
// #define VEC_16_SIMD 1
// #define MANYCORE_PREFETCH
// #define PACKED_SIMD
// #define VEC_16_LONGLINES
// #define NESTED_SIMD_4_4
// #define VEC_4_LONGLINES

// vvadd_execute config directives
#if !defined(NO_VEC) && !defined(MANYCORE_PREFETCH) && !defined(PACKED_SIMD)
#define USE_VEC 1
#endif

// vector grouping directives
#if defined(VEC_4_SIMD) || defined(NESTED_SIMD_4_4) || defined(VEC_4_LONGLINES)
#define VECTOR_LEN 4
#endif
#if defined(VEC_16_SIMD) || defined(VEC_16_LONGLINES)
#define VECTOR_LEN 16
#endif
#if defined(MANYCORE_PREFETCH)
#define VECTOR_LEN 1
#endif

#if defined(VEC_16_LONGLINES) || defined(NESTED_SIMD_4_4) || defined(VEC_4_LONGLINES)
#define LONGLINES
#endif

#if defined(NESTED_SIMD_4_4)
#define NESTED_SIMD_VLEN 4
#else
#define NESTED_SIMD_VLEN 1
#endif

#if NESTED_SIMD_VLEN > 1
#define NESTED_SIMD
#endif

// prefetch sizing
#if defined(USE_VEC) || defined(MANYCORE_PREFETCH)
// dedicate a quarter of scratchpad to frames
#define POST_FRAME_WORD 512

// number of frames to get ahead
#ifndef INIT_FRAMES
#define INIT_FRAMES 1
#endif

// prefetch config for inner kernel

#ifndef ACCUM_GRANULARITY
// default to coarser b/c can onlt help. although 2 is probably sufficient
#define ACCUM_GRANULARITY 8
#endif

#ifdef LONGLINES
  #define INNER_PREFETCH_LEN (CACHE_LINE_SIZE / sizeof(DTYPE) / VECTOR_LEN)
  #define J_STRIDE (1)
  #define K_STRIDE (INNER_PREFETCH_LEN * VECTOR_LEN)
#else
  #define INNER_PREFETCH_LEN (16)
  #define J_STRIDE (VECTOR_LEN)
  #define K_STRIDE (INNER_PREFETCH_LEN)
#endif

#define INNER_FRAME_SIZE (2*INNER_PREFETCH_LEN)
#define NUM_FRAMES (POST_FRAME_WORD / INNER_FRAME_SIZE)
#define INIT_OFFSET (INIT_FRAMES * K_STRIDE)

// TODO hardcode this based on spipe
#if VECTOR_LEN == 4
#define NUM_GROUPS_PER_PIPE (3)
#else
#define NUM_GROUPS_PER_PIPE (1)
#endif

#define MAILER_OFFSET (NUM_GROUPS_PER_PIPE)
#define SUB_FRAME_SIZE (MAILER_OFFSET + VECTOR_LEN * NUM_GROUPS_PER_PIPE)
#define MAILER_FRAME_SIZE (SUB_FRAME_SIZE * ACCUM_GRANULARITY)
#define MAILER_NUM_FRAMES (POST_FRAME_WORD / MAILER_FRAME_SIZE)
#define MAILER_POST_FRAME_WORD (MAILER_FRAME_SIZE * MAILER_NUM_FRAMES)

// needs to be maxed at number of frame counters
#if MAILER_NUM_FRAMES < 5
#define FRAMES_TO_SYNC_AFTER (MAILER_NUM_FRAMES)
#else
#define FRAMES_TO_SYNC_AFTER (5)
#endif

// how much vector core will write
#define PER_CORE_MAILER_FRAME (VECTOR_LEN)
// includes also if any base value to the sum
#define PER_CORE_FULL_MAILER_FRAME (PER_CORE_MAILER_FRAME + 1)

// frame size to get the c to accumulate on
#define OUTER_PREFETCH_LEN INNER_PREFETCH_LEN
#define OUTER_FRAME_SIZE OUTER_PREFETCH_LEN

#ifdef MANYCORE_PREFETCH
  #define VERTICAL_FETCH_TYPE (TO_SELF)
#else
  #define VERTICAL_FETCH_TYPE (TO_ONE_CORE)
#endif

// prefetch c
// pad out to the frame size (1->2 currently)
// maybe don't have to prefetch this
inline void prefetch_outer_frame(DTYPE *c, int i, int j, int *sp, int N) {
  #ifdef LONGLINES
  // nothing
  #else
  for (int core = 0; core < VECTOR_LEN; core++) {
    VPREFETCH_LR(*sp + 0, &c[i * N + j + core], core, OUTER_PREFETCH_LEN, VERTICAL_FETCH_TYPE);

    // pad out
    // VPREFETCH_LR(*sp + OUTER_PREFETCH_LEN, &c[i * N + j + core], core, OUTER_PREFETCH_LEN, TO_ONE_CORE);
  }

  *sp = (*sp + INNER_FRAME_SIZE) % POST_FRAME_WORD;
  #endif
}

// prefetch a
inline void prefetch_inner_frame(DTYPE *a, int i, int j, int k, int *sp, int M) {
  #ifdef LONGLINES
  VPREFETCH_L(*sp + 0, 
    &a[i * M + k], 0, INNER_PREFETCH_LEN, TO_ALL_CORES);
  VPREFETCH_L(*sp + INNER_PREFETCH_LEN, 
    &a[j * M + k], 0, INNER_PREFETCH_LEN, TO_ALL_CORES);
  #else
  for (int core = 0; core < VECTOR_LEN; core++) {
    // TODO redundant across cores
    VPREFETCH_L(*sp + 0, &a[i * M + k], core, INNER_PREFETCH_LEN, VERTICAL_FETCH_TYPE);

    // TODO this can be horizontal?
    VPREFETCH_L(*sp + INNER_PREFETCH_LEN, &a[(j + core) * M + k], core, INNER_PREFETCH_LEN, VERTICAL_FETCH_TYPE);
  }
  #endif

  // will be done manually for manycore prefetch
  #ifndef MANYCORE_PREFETCH
  *sp = (*sp + INNER_FRAME_SIZE) % POST_FRAME_WORD;
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
