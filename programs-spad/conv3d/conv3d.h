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

// vector conv15
// implicit inputs (c's are weights)
// implicit outputs is ofmap
#define VCONV_15(a111, a113, a123, a133, a212, a222, a232, a311, a313, a323, a333)  \
  vfloat32m1_t b111_11, b111_21, b111_31, b113_11, b123_21, b133_31,                \
  b212_12, b222_22, b232_32, b311_13, b311_23, b311_33, b313_13,                    \
  b323_23, b333_33;                                                                 \
  b111_11 = vfmul_vf_f32m1(a111, c11);                                              \
  b111_21 = vfmul_vf_f32m1(a111, c21);                                              \
  b111_31 = vfmul_vf_f32m1(a111, c31);                                              \
  b113_11 = vfmul_vf_f32m1(a113, c11);                                              \
  b123_21 = vfmul_vf_f32m1(a123, c21);                                              \
  b133_31 = vfmul_vf_f32m1(a133, c31);                                              \
  b212_12 = vfmul_vf_f32m1(a212, c12);                                              \
  b222_22 = vfmul_vf_f32m1(a222, c22);                                              \
  b232_32 = vfmul_vf_f32m1(a232, c32);                                              \
  b311_13 = vfmul_vf_f32m1(a311, c13);                                              \
  b311_23 = vfmul_vf_f32m1(a311, c23);                                              \
  b311_33 = vfmul_vf_f32m1(a311, c33);                                              \
  b313_13 = vfmul_vf_f32m1(a313, c13);                                              \
  b323_23 = vfmul_vf_f32m1(a323, c23);                                              \
  b333_33 = vfmul_vf_f32m1(a333, c33);                                              \
  vfloat32m1_t ofmap = vfadd_vv_f32m1(b111_11, b111_21);                            \
  ofmap = vfadd_vv_f32m1(ofmap, b111_31);                                           \
  ofmap = vfadd_vv_f32m1(ofmap, b113_11);                                           \
  ofmap = vfadd_vv_f32m1(ofmap, b123_21);                                           \
  ofmap = vfadd_vv_f32m1(ofmap, b133_31);                                           \
  ofmap = vfadd_vv_f32m1(ofmap, b212_12);                                           \
  ofmap = vfadd_vv_f32m1(ofmap, b222_22);                                           \
  ofmap = vfadd_vv_f32m1(ofmap, b232_32);                                           \
  ofmap = vfadd_vv_f32m1(ofmap, b311_13);                                           \
  ofmap = vfadd_vv_f32m1(ofmap, b311_23);                                           \
  ofmap = vfadd_vv_f32m1(ofmap, b311_33);                                           \
  ofmap = vfadd_vv_f32m1(ofmap, b313_13);                                           \
  ofmap = vfadd_vv_f32m1(ofmap, b323_23);                                           \
  ofmap = vfadd_vv_f32m1(ofmap, b333_33)

#define IDX(i, j, k, NJ, NK) \
  ((i) * NK * NJ + (j) * NK + (k))                     


// #define AUDIT
#define AUDIT2



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
#define INIT_FRAMES 2
#endif

#if defined(AUDIT) && defined(MANYCORE_PREFETCH)
#define USE_AUDIT1
#endif

#if defined(AUDIT2) && (defined(MANYCORE_PREFETCH) || ((defined(PER_CORE_SIMD) || (VECTOR_LEN==4)) && defined(USE_VEC)))
#define USE_AUDIT2
#endif

// define prefetch len externally
#ifdef USE_AUDIT1
#define UNROLL_LEN 16
#define PREFETCH_LEN (UNROLL_LEN)
#define REGION_SIZE (11*UNROLL_LEN)
#define NUM_REGIONS 2
#elif defined(USE_AUDIT2)
#define UNROLL_LEN 14
#define PREFETCH_LEN (UNROLL_LEN)
#define REGION_SIZE (7*UNROLL_LEN+2*(UNROLL_LEN+2))
#define NUM_REGIONS 5
#else
#define UNROLL_LEN 1
#define PREFETCH_LEN VECTOR_LEN
#define REGION_SIZE 11
#define NUM_REGIONS 11
#endif

#define VEC_K_STRIDE (VECTOR_LEN*UNROLL_LEN)


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

#ifdef MANYCORE_PREFETCH
  #define VERTICAL_FETCH_TYPE (TO_SELF)
  #define HORIZONTAL_FETCH_TYPE (TO_SELF)
#else
  #define VERTICAL_FETCH_TYPE (TO_ONE_CORE)
  #define HORIZONTAL_FETCH_TYPE (TO_ALL_CORES)
#endif

inline void prefetch_horiz_frame(DTYPE *a, int i, int j, int k, int NJ, int NK, int *sp) {
  // prefetch all 15 values required for computation
            // a[IDX(i-1, j-1, k-1, NJ, NK)], a[IDX(i-1, j-1, k+1, NJ, NK)], 
            // a[IDX(i-1, j+0, k+1, NJ, NK)], a[IDX(i-1, j+1, k+1, NJ, NK)], 
            // a[IDX(i+0, j-1, k+0, NJ, NK)], a[IDX(i+0, j+0, k+0, NJ, NK)], 
            // a[IDX(i+0, j+1, k+0, NJ, NK)], a[IDX(i+1, j-1, k-1, NJ, NK)], 
            // a[IDX(i+1, j-1, k+1, NJ, NK)], a[IDX(i+1, j+0, k+1, NJ, NK)], 
            // a[IDX(i+1, j+1, k+1, NJ, NK)];

  #ifdef USE_AUDIT1
  int ul = UNROLL_LEN;
  for (int core = 0; core < VECTOR_LEN; core++) {
    VPREFETCH_LR(*sp + 0*ul , a + IDX(i-1, j-1+core, k-1, NJ, NK), core, PREFETCH_LEN, VERTICAL_FETCH_TYPE); // merge
    VPREFETCH_LR(*sp + 1*ul , a + IDX(i-1, j-1+core, k+1, NJ, NK), core, PREFETCH_LEN, VERTICAL_FETCH_TYPE); //
    VPREFETCH_LR(*sp + 2*ul , a + IDX(i-1, j+0+core, k+1, NJ, NK), core, PREFETCH_LEN, VERTICAL_FETCH_TYPE);
    VPREFETCH_LR(*sp + 3*ul , a + IDX(i-1, j+1+core, k+1, NJ, NK), core, PREFETCH_LEN, VERTICAL_FETCH_TYPE);
    VPREFETCH_LR(*sp + 4*ul , a + IDX(i+0, j-1+core, k+0, NJ, NK), core, PREFETCH_LEN, VERTICAL_FETCH_TYPE);
    VPREFETCH_LR(*sp + 5*ul , a + IDX(i+0, j+0+core, k+0, NJ, NK), core, PREFETCH_LEN, VERTICAL_FETCH_TYPE);
    VPREFETCH_LR(*sp + 6*ul , a + IDX(i+0, j+1+core, k+0, NJ, NK), core, PREFETCH_LEN, VERTICAL_FETCH_TYPE);
    VPREFETCH_LR(*sp + 7*ul , a + IDX(i+1, j-1+core, k-1, NJ, NK), core, PREFETCH_LEN, VERTICAL_FETCH_TYPE); // merge
    VPREFETCH_LR(*sp + 8*ul , a + IDX(i+1, j-1+core, k+1, NJ, NK), core, PREFETCH_LEN, VERTICAL_FETCH_TYPE); // 
    VPREFETCH_LR(*sp + 9*ul , a + IDX(i+1, j+0+core, k+1, NJ, NK), core, PREFETCH_LEN, VERTICAL_FETCH_TYPE);
    VPREFETCH_LR(*sp + 10*ul, a + IDX(i+1, j+1+core, k+1, NJ, NK), core, PREFETCH_LEN, VERTICAL_FETCH_TYPE);
  }
  #elif defined(USE_AUDIT2)
  int ul = UNROLL_LEN;
  int ml = ul + 2;
  for (int core = 0; core < VECTOR_LEN; core++) {
    // VPREFETCH_LR(*sp + 0*ul , a + IDX(i-1, j-1+core, k-1, NJ, NK), core, PREFETCH_LEN, VERTICAL); // merge
    // VPREFETCH_LR(*sp + 1*ul , a + IDX(i-1, j-1+core, k+1, NJ, NK), core, PREFETCH_LEN, VERTICAL); //
    VPREFETCH_LR(*sp + 0*ul   , a + IDX(i-1, j-1, k-1+core, NJ, NK), core, PREFETCH_LEN + 2, VERTICAL_FETCH_TYPE); // merge

    VPREFETCH_LR(*sp + 1*ml   , a + IDX(i-1, j+0, k+1+core, NJ, NK), core, PREFETCH_LEN, VERTICAL_FETCH_TYPE);
    VPREFETCH_LR(*sp + ul+ml  , a + IDX(i-1, j+1, k+1+core, NJ, NK), core, PREFETCH_LEN, VERTICAL_FETCH_TYPE);
    VPREFETCH_LR(*sp + 2*ul+ml, a + IDX(i+0, j-1, k+0+core, NJ, NK), core, PREFETCH_LEN, VERTICAL_FETCH_TYPE);
    VPREFETCH_LR(*sp + 3*ul+ml, a + IDX(i+0, j+0, k+0+core, NJ, NK), core, PREFETCH_LEN, VERTICAL_FETCH_TYPE);
    VPREFETCH_LR(*sp + 4*ul+ml, a + IDX(i+0, j+1, k+0+core, NJ, NK), core, PREFETCH_LEN, VERTICAL_FETCH_TYPE);

    // VPREFETCH_LR(*sp + 7*ul , a + IDX(i+1, j-1+core, k-1, NJ, NK), core, PREFETCH_LEN, VERTICAL); // merge
    // VPREFETCH_LR(*sp + 8*ul , a + IDX(i+1, j-1+core, k+1, NJ, NK), core, PREFETCH_LEN, VERTICAL); // 
    VPREFETCH_LR(*sp + 5*ul+ml, a + IDX(i+1, j-1, k-1+core, NJ, NK), core, PREFETCH_LEN + 2, VERTICAL_FETCH_TYPE);

    VPREFETCH_LR(*sp + 5*ul+2*ml , a + IDX(i+1, j+0, k+1+core, NJ, NK), core, PREFETCH_LEN, VERTICAL_FETCH_TYPE);
    VPREFETCH_LR(*sp + 6*ul+2*ml, a + IDX(i+1, j+1, k+1+core, NJ, NK), core, PREFETCH_LEN, VERTICAL_FETCH_TYPE);
  }
  #else
  // NOTE vector config uses this
  VPREFETCH_LR_FAIR(*sp + 0 , a + IDX(i-1, j-1, k-1, NJ, NK), 0, 1, HORIZONTAL_FETCH_TYPE);
  VPREFETCH_LR_FAIR(*sp + 1 , a + IDX(i-1, j-1, k+1, NJ, NK), 0, 1, HORIZONTAL_FETCH_TYPE);
  VPREFETCH_LR_FAIR(*sp + 2 , a + IDX(i-1, j+0, k+1, NJ, NK), 0, 1, HORIZONTAL_FETCH_TYPE);
  VPREFETCH_LR_FAIR(*sp + 3 , a + IDX(i-1, j+1, k+1, NJ, NK), 0, 1, HORIZONTAL_FETCH_TYPE);
  VPREFETCH_LR_FAIR(*sp + 4 , a + IDX(i+0, j-1, k+0, NJ, NK), 0, 1, HORIZONTAL_FETCH_TYPE);
  VPREFETCH_LR_FAIR(*sp + 5 , a + IDX(i+0, j+0, k+0, NJ, NK), 0, 1, HORIZONTAL_FETCH_TYPE);
  VPREFETCH_LR_FAIR(*sp + 6 , a + IDX(i+0, j+1, k+0, NJ, NK), 0, 1, HORIZONTAL_FETCH_TYPE);
  VPREFETCH_LR_FAIR(*sp + 7 , a + IDX(i+1, j-1, k-1, NJ, NK), 0, 1, HORIZONTAL_FETCH_TYPE);
  VPREFETCH_LR_FAIR(*sp + 8 , a + IDX(i+1, j-1, k+1, NJ, NK), 0, 1, HORIZONTAL_FETCH_TYPE);
  VPREFETCH_LR_FAIR(*sp + 9 , a + IDX(i+1, j+0, k+1, NJ, NK), 0, 1, HORIZONTAL_FETCH_TYPE);
  VPREFETCH_LR_FAIR(*sp + 10, a + IDX(i+1, j+1, k+1, NJ, NK), 0, 1, HORIZONTAL_FETCH_TYPE);
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
