#ifndef __CONV2D_H__
#define __CONV2D_H__

// filter size
#define FILTER_DIM 3

// data type to do computation with
#define DTYPE float

#define DEF_WEIGHTS() \
  DTYPE c11, c12, c13, c21, c22, c23, c31, c32, c33; \
	c11 = +0.2;  c21 = +0.5;  c31 = -0.8; \
	c12 = -0.3;  c22 = +0.6;  c32 = -0.9; \
	c13 = +0.4;  c23 = +0.7;  c33 = +0.10

#define CONV_3x3(a11, a21, a31, a12, a22, a32, a13, a23, a33) \
  c11 * a11 + c21 * a21 + c31 * a31 + \
  c12 * a12 + c22 * a22 + c32 * a32 + \
  c13 * a13 + c23 * a23 + c33 * a33


// one of these should be defined to dictate config
// #define NO_VEC 1
#define VEC_4_SIMD 1
// #define VEC_4_SIMD_VERTICAL 1
// #define VEC_4_REUSE_VERTICAL 1
// #define VEC_16_SIMD 1
// #define VEC_16_SIMD_VERTICAL 1
// #define VEC_16_REUSE_VERTICAL 1

// vvadd_execute config directives
#if !defined(NO_VEC)
#define USE_VEC 1
#endif

// vector grouping directives
#if defined(VEC_4_SIMD) || defined(VEC_4_SIMD_BCAST) || defined(VEC_4_SIMD_SINGLE_PREFETCH) || defined(VEC_4_REUSE) || defined(VEC_4_SIMD_LARGE_FRAME) \
  || defined(VEC_4_SIMD_VERTICAL) || defined(VEC_4_REUSE_VERTICAL)
#define VECTOR_LEN 4
#endif
#if defined(VEC_16_SIMD) || defined(VEC_16_SIMD_VERTICAL) || defined(VEC_16_REUSE_VERTICAL)
#define VECTOR_LEN 16
#endif

// grid dim xy assuming always a square
#if _N_SPS==16
#define GRID_XDIM 4
#define GRID_YDIM 4
#elif _N_SPS==64
#define GRID_XDIM 8
#define GRID_YDIM 8
#endif

// kernel settings 
#if defined(VEC_4_REUSE) || defined(VEC_4_REUSE_VERTICAL) || defined(VEC_16_REUSE_VERTICAL)
#define REUSE 1
#endif
#if defined(VEC_4_SIMD_VERTICAL) || defined(VEC_16_SIMD_VERTICAL) || defined(VEC_4_REUSE_VERTICAL) || defined(VEC_16_REUSE_VERTICAL)
#define VERTICAL_LOADS 1
#endif

// prefetch sizings
#if defined(USE_VEC)
#define POST_REGION_WORD 256
#define INIT_FRAMES 2
#if defined(REUSE)
#define LOAD_DEPTH 3
#define REGION_SIZE (LOAD_DEPTH*FILTER_DIM)
#define NUM_REGIONS (POST_REGION_WORD / REGION_SIZE)
#elif defined(VERTICAL_LOADS)
#define LOAD_DEPTH 8
#define REGION_SIZE (LOAD_DEPTH*FILTER_DIM)
#define NUM_REGIONS (POST_REGION_WORD / REGION_SIZE)
#else
#define REGION_SIZE (FILTER_DIM * FILTER_DIM)
#define NUM_REGIONS (POST_REGION_WORD / REGION_SIZE)
#endif
#endif

// define prefetch len externally
#ifdef PF
#define PREFETCH_LEN PF
#elif defined(VECTOR_LEN)
// default size is the vlen
#define PREFETCH_LEN VECTOR_LEN
#endif


// pthread argument for the kernel
typedef struct Kern_Args {
  DTYPE *a, *b;
  int NI, NJ;
  int tid_x, tid_y;
  int dim_x, dim_y;
} Kern_Args;

// helper to pack vvadd args
Kern_Args *construct_args(
    DTYPE *a, DTYPE *b, int NI, int NJ,
    int tid_x, int tid_y, int dim_x, int dim_y
  );

// pthread call
void *pthread_kernel(void *args);

// vvadd kernel
void kernel(
    DTYPE *a, DTYPE *b, int NI, int NJ,
    int tid_x, int tid_y, int dim_x, int dim_y
  );

#endif
