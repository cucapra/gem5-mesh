#ifndef __CONV3D_H__
#define __CONV3D_H__

#include "bind_defs.h"

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


// #define AUDIT
#define AUDIT2

// one of these should be defined to dictate config
// #define NO_VEC 1
// #define MANYCORE_PREFETCH

// #define VEC_4_SIMD 1
// #define VEC_4_SIMD_VERTICAL 1
// #define VEC_4_REUSE_VERTICAL 1

// #define VEC_16_SIMD 1
// #define VEC_16_SIMD_VERTICAL 1
// #define VEC_16_REUSE_VERTICAL 1

// vvadd_execute config directives
#if !defined(NO_VEC) && !defined(MANYCORE_PREFETCH)
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
#if defined(MANYCORE_PREFETCH)
#define VECTOR_LEN 1
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
#if defined(USE_VEC) || defined(MANYCORE_PREFETCH)
#ifndef INIT_FRAMES
#define INIT_FRAMES 2
#endif

// define prefetch len externally
#ifdef AUDIT
#define UNROLL_LEN 16
#define PREFETCH_LEN (UNROLL_LEN)
#define REGION_SIZE (11*UNROLL_LEN)
#define NUM_REGIONS 2
#elif defined(AUDIT2)
#define UNROLL_LEN 14
#define PREFETCH_LEN (UNROLL_LEN)
#define REGION_SIZE (7*UNROLL_LEN+2*(UNROLL_LEN+2))
#define NUM_REGIONS 2
#else
#define UNROLL_LEN 1
#define PREFETCH_LEN VECTOR_LEN
#define REGION_SIZE 11
#define NUM_REGIONS 11
#endif


#define POST_REGION_WORD (REGION_SIZE * NUM_REGIONS)



#ifdef VERTICAL_LOADS
// number of filters done per iteration per core
#ifdef REUSE
#define CORE_STEP LOAD_DEPTH
#else
#define CORE_STEP (LOAD_DEPTH - (FILTER_DIM - 1))
#endif
#endif

#ifdef MANYCORE_PREFETCH
#define VPREFETCH_LR_FAIR(sp, memIdx, core, len, style)  \
  VPREFETCH_L(sp, memIdx, core, len, style)
#else
#define VPREFETCH_LR_FAIR(sp, memIdx, core, len, style)  \
  VPREFETCH_LR(sp, memIdx, core, len, style)
#endif

inline void prefetch_horiz_frame(DTYPE *a, int i, int j, int k, int NJ, int NK, int *sp) {
  // prefetch all 15 values required for computation
            // a[IDX(i-1, j-1, k-1, NJ, NK)], a[IDX(i-1, j-1, k+1, NJ, NK)], 
            // a[IDX(i-1, j+0, k+1, NJ, NK)], a[IDX(i-1, j+1, k+1, NJ, NK)], 
            // a[IDX(i+0, j-1, k+0, NJ, NK)], a[IDX(i+0, j+0, k+0, NJ, NK)], 
            // a[IDX(i+0, j+1, k+0, NJ, NK)], a[IDX(i+1, j-1, k-1, NJ, NK)], 
            // a[IDX(i+1, j-1, k+1, NJ, NK)], a[IDX(i+1, j+0, k+1, NJ, NK)], 
            // a[IDX(i+1, j+1, k+1, NJ, NK)];

  #if defined(AUDIT) && defined(MANYCORE_PREFETCH)
  int ul = UNROLL_LEN;
  for (int core = 0; core < VECTOR_LEN; core++) {
    VPREFETCH_LR(*sp + 0*ul , a + IDX(i-1, j-1+core, k-1, NJ, NK), core, PREFETCH_LEN, VERTICAL); // merge
    VPREFETCH_LR(*sp + 1*ul , a + IDX(i-1, j-1+core, k+1, NJ, NK), core, PREFETCH_LEN, VERTICAL); //
    VPREFETCH_LR(*sp + 2*ul , a + IDX(i-1, j+0+core, k+1, NJ, NK), core, PREFETCH_LEN, VERTICAL);
    VPREFETCH_LR(*sp + 3*ul , a + IDX(i-1, j+1+core, k+1, NJ, NK), core, PREFETCH_LEN, VERTICAL);
    VPREFETCH_LR(*sp + 4*ul , a + IDX(i+0, j-1+core, k+0, NJ, NK), core, PREFETCH_LEN, VERTICAL);
    VPREFETCH_LR(*sp + 5*ul , a + IDX(i+0, j+0+core, k+0, NJ, NK), core, PREFETCH_LEN, VERTICAL);
    VPREFETCH_LR(*sp + 6*ul , a + IDX(i+0, j+1+core, k+0, NJ, NK), core, PREFETCH_LEN, VERTICAL);
    VPREFETCH_LR(*sp + 7*ul , a + IDX(i+1, j-1+core, k-1, NJ, NK), core, PREFETCH_LEN, VERTICAL); // merge
    VPREFETCH_LR(*sp + 8*ul , a + IDX(i+1, j-1+core, k+1, NJ, NK), core, PREFETCH_LEN, VERTICAL); // 
    VPREFETCH_LR(*sp + 9*ul , a + IDX(i+1, j+0+core, k+1, NJ, NK), core, PREFETCH_LEN, VERTICAL);
    VPREFETCH_LR(*sp + 10*ul, a + IDX(i+1, j+1+core, k+1, NJ, NK), core, PREFETCH_LEN, VERTICAL);
  }
  #elif defined(AUDIT2) && defined(MANYCORE_PREFETCH)
  int ul = UNROLL_LEN;
  int ml = ul + 2;
  for (int core = 0; core < VECTOR_LEN; core++) {
    // VPREFETCH_LR(*sp + 0*ul , a + IDX(i-1, j-1+core, k-1, NJ, NK), core, PREFETCH_LEN, VERTICAL); // merge
    // VPREFETCH_LR(*sp + 1*ul , a + IDX(i-1, j-1+core, k+1, NJ, NK), core, PREFETCH_LEN, VERTICAL); //
    VPREFETCH_LR(*sp + 0*ul   , a + IDX(i-1, j-1+core, k-1, NJ, NK), core, PREFETCH_LEN + 2, VERTICAL); // merge

    VPREFETCH_LR(*sp + 1*ml   , a + IDX(i-1, j+0+core, k+1, NJ, NK), core, PREFETCH_LEN, VERTICAL);
    VPREFETCH_LR(*sp + ul+ml  , a + IDX(i-1, j+1+core, k+1, NJ, NK), core, PREFETCH_LEN, VERTICAL);
    VPREFETCH_LR(*sp + 2*ul+ml, a + IDX(i+0, j-1+core, k+0, NJ, NK), core, PREFETCH_LEN, VERTICAL);
    VPREFETCH_LR(*sp + 3*ul+ml, a + IDX(i+0, j+0+core, k+0, NJ, NK), core, PREFETCH_LEN, VERTICAL);
    VPREFETCH_LR(*sp + 4*ul+ml, a + IDX(i+0, j+1+core, k+0, NJ, NK), core, PREFETCH_LEN, VERTICAL);

    // VPREFETCH_LR(*sp + 7*ul , a + IDX(i+1, j-1+core, k-1, NJ, NK), core, PREFETCH_LEN, VERTICAL); // merge
    // VPREFETCH_LR(*sp + 8*ul , a + IDX(i+1, j-1+core, k+1, NJ, NK), core, PREFETCH_LEN, VERTICAL); // 
    VPREFETCH_LR(*sp + 5*ul+ml, a + IDX(i+1, j-1+core, k-1, NJ, NK), core, PREFETCH_LEN + 2, VERTICAL);

    VPREFETCH_LR(*sp + 5*ul+2*ml , a + IDX(i+1, j+0+core, k+1, NJ, NK), core, PREFETCH_LEN, VERTICAL);
    VPREFETCH_LR(*sp + 6*ul+2*ml, a + IDX(i+1, j+1+core, k+1, NJ, NK), core, PREFETCH_LEN, VERTICAL);
  }
  #else
  VPREFETCH_LR_FAIR(*sp + 0 , a + IDX(i-1, j-1, k-1, NJ, NK), 0, PREFETCH_LEN, HORIZONTAL);
  VPREFETCH_LR_FAIR(*sp + 1 , a + IDX(i-1, j-1, k+1, NJ, NK), 0, PREFETCH_LEN, HORIZONTAL);
  VPREFETCH_LR_FAIR(*sp + 2 , a + IDX(i-1, j+0, k+1, NJ, NK), 0, PREFETCH_LEN, HORIZONTAL);
  VPREFETCH_LR_FAIR(*sp + 3 , a + IDX(i-1, j+1, k+1, NJ, NK), 0, PREFETCH_LEN, HORIZONTAL);
  VPREFETCH_LR_FAIR(*sp + 4 , a + IDX(i+0, j-1, k+0, NJ, NK), 0, PREFETCH_LEN, HORIZONTAL);
  VPREFETCH_LR_FAIR(*sp + 5 , a + IDX(i+0, j+0, k+0, NJ, NK), 0, PREFETCH_LEN, HORIZONTAL);
  VPREFETCH_LR_FAIR(*sp + 6 , a + IDX(i+0, j+1, k+0, NJ, NK), 0, PREFETCH_LEN, HORIZONTAL);
  VPREFETCH_LR_FAIR(*sp + 7 , a + IDX(i+1, j-1, k-1, NJ, NK), 0, PREFETCH_LEN, HORIZONTAL);
  VPREFETCH_LR_FAIR(*sp + 8 , a + IDX(i+1, j-1, k+1, NJ, NK), 0, PREFETCH_LEN, HORIZONTAL);
  VPREFETCH_LR_FAIR(*sp + 9 , a + IDX(i+1, j+0, k+1, NJ, NK), 0, PREFETCH_LEN, HORIZONTAL);
  VPREFETCH_LR_FAIR(*sp + 10, a + IDX(i+1, j+1, k+1, NJ, NK), 0, PREFETCH_LEN, HORIZONTAL);
  #endif


  #ifndef MANYCORE_PREFETCH
  *sp = (*sp + REGION_SIZE);

  // spad is circular buffer so do cheap mod here
  if (*sp == POST_REGION_WORD) {
    *sp = 0;
  }
  #endif
}

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
