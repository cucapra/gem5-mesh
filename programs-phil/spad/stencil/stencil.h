#ifndef __STENCIL_H__
#define __STENCIL_H__

// filter size
#define FILTER_DIM 3

// data type to do computation with
#define DTYPE int

// one of these should be defined to dictate config
// #define NO_VEC 1
// #define VEC_4_SIMD 1
#define VEC_4_SIMD_VERTICAL 1
// #define VEC_4_SIMD_BCAST 1
// #define VEC_4_REUSE 1
// #define VEC_4_SIMD_SINGLE_PREFETCH 1
// #define VEC_4_SIMD_LARGE_FRAME 1

// vvadd_execute config directives
#if defined(VEC_4_SIMD) || defined(VEC_4_SIMD_BCAST) || defined(VEC_4_SIMD_SINGLE_PREFETCH) || defined(VEC_4_REUSE) || defined(VEC_4_SIMD_LARGE_FRAME) \
  || defined(VEC_4_SIMD_VERTICAL)
#define USE_VEC 1
#endif

// vector grouping directives
#if defined(VEC_4_SIMD) || defined(VEC_4_SIMD_BCAST) || defined(VEC_4_SIMD_SINGLE_PREFETCH) || defined(VEC_4_REUSE) || defined(VEC_4_SIMD_LARGE_FRAME) \
  || defined(VEC_4_SIMD_VERTICAL)
#define VEC_SIZE_4_SIMD 1
#endif

// kernel settings 
#if defined(VEC_4_SIMD_SINGLE_PREFETCH)
#define SINGLE_PREFETCH 1
#endif
#if defined(VEC_4_REUSE)
#define REUSE 1
#endif
#if defined(VEC_4_SIMD_LARGE_FRAME)
#define LARGE_FRAME 1
#endif
#if defined(VEC_4_SIMD_VERTICAL)
#define VERTICAL_LOADS 1
#endif

// prefetch sizings
#if defined(USE_VEC)
#if defined(LARGE_FRAME)
#define FRAMES_PER_REGION 8
#define REGION_SIZE (FILTER_DIM * FILTER_DIM * FRAMES_PER_REGION)
#define NUM_REGIONS 8
#define POST_REGION_WORD (REGION_SIZE * NUM_REGIONS)
#elif defined(VERTICAL_LOADS)
#define LOAD_DEPTH 8
#define REGION_SIZE (LOAD_DEPTH*FILTER_DIM)
#define NUM_REGIONS 16
#define POST_REGION_WORD (REGION_SIZE * NUM_REGIONS)
#else
#define REGION_SIZE (FILTER_DIM * FILTER_DIM)
#define NUM_REGIONS 64
#define POST_REGION_WORD (REGION_SIZE * NUM_REGIONS)
#endif
#endif

// pthread argument for the kernel
typedef struct Kern_Args {
  DTYPE *a, *b, *c;
  int nrows, ncols;
  int tid_x, tid_y;
  int dim_x, dim_y;
} Kern_Args;

// helper to pack vvadd args
Kern_Args *construct_args(
    DTYPE *a, DTYPE *b, DTYPE *c, int nrows, int ncols,
    int tid_x, int tid_y, int dim_x, int dim_y
  );

// pthread call
void *pthread_kernel(void *args);

// vvadd kernel
void kernel(
    DTYPE *a, DTYPE *b, DTYPE *c, int nrows, int ncols,
    int tid_x, int tid_y, int dim_x, int dim_y
  );

#endif
