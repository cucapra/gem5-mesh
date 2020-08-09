#ifndef __CONV3D_H__
#define __CONV3D_H__

// filter size
#define FILTER_DIM 3

// data type to do computation with
#define DTYPE float

#define DEF_WEIGHTS() \
  DTYPE c11, c12, c13, c21, c22, c23, c31, c32, c33;  \
	c11 = +2;  c21 = +5;  c31 = -8;                     \
	c12 = -3;  c22 = +6;  c32 = -9;                     \
	c13 = +4;  c23 = +7;  c33 = +10

#define CONV_15(a111, a113, a123, a133, a212, a222, a232, a311, a313, a323, a333) \
  c11 * a111 + c21 * a111 + c31 * a111 +  \
  c11 * a113 +                            \
  c21 * a123 +                            \
  c31 * a133 +                            \
  c12 * a212 +                            \
  c22 * a222 +                            \
  c32 * a232 +                            \
  c13 * a311 + c23 * a311 + c33 * a311 +  \
  c13 * a313 +                            \
  c23 * a323 +                            \
  c33 * a333                           

#define IDX(i, j, k, NJ, NK) \
  ((i) * NK * NJ + (j) * NK + (k))                     



// one of these should be defined to dictate config
// #define NO_VEC 1
// #define VEC_4_SIMD 1
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
// can guarentee power of 2 works
#define POST_REGION_WORD 121
#define INIT_FRAMES 5
#define REGION_SIZE 11
#define NUM_REGIONS (POST_REGION_WORD / REGION_SIZE)

#endif

// define prefetch len externally
#ifdef PF
#define PREFETCH_LEN PF
#elif defined(VECTOR_LEN)
// default size is the vlen
#define PREFETCH_LEN VECTOR_LEN
#endif

#ifdef VERTICAL_LOADS
// number of filters done per iteration per core
#ifdef REUSE
#define CORE_STEP LOAD_DEPTH
#else
#define CORE_STEP (LOAD_DEPTH - (FILTER_DIM - 1))
#endif
#endif


// pthread argument for the kernel
typedef struct Kern_Args {
  DTYPE *a, *b;
  int NI, NJ, NK;
  int tid_x, tid_y;
  int dim_x, dim_y;
} Kern_Args;

// helper to pack vvadd args
Kern_Args *construct_args(
    DTYPE *a, DTYPE *b, int NI, int NJ, int NK,
    int tid_x, int tid_y, int dim_x, int dim_y
  );

// pthread call
void *pthread_kernel(void *args);

// vvadd kernel
void kernel(
    DTYPE *a, DTYPE *b, int NI, int NJ, int NK,
    int tid_x, int tid_y, int dim_x, int dim_y
  );

#endif
