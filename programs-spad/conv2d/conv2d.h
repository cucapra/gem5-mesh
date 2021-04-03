#ifndef __CONV2D_H__
#define __CONV2D_H__

#include "bind_defs.h"

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

// 1: do three vector-scalar multiplications for each row (one for each filter element)
// 2: shift vectors so when add get a partial product for the row
// 3: add every vector togehter for a set of VLEN-(FILTER_DIM-1) outputs (last 2 are duds)
#define VECTOR_CONV_3x3(r1, r2, r3, bPtr, bmask)                \
  vfloat32m1_t r11, r12, r13, r21, r22, r23, r31, r32, r33;     \
  r11 = vfmul_vf_f32m1(r1, c11);                                \
  r21 = vfmul_vf_f32m1(r1, c21);                                \
  r31 = vfmul_vf_f32m1(r1, c31);                                \
  r12 = vfmul_vf_f32m1(r2, c12);                                \
  r22 = vfmul_vf_f32m1(r2, c22);                                \
  r32 = vfmul_vf_f32m1(r2, c32);                                \
  r13 = vfmul_vf_f32m1(r3, c13);                                \
  r23 = vfmul_vf_f32m1(r3, c23);                                \
  r33 = vfmul_vf_f32m1(r3, c33);                                \
  r21 = vslidedown_vx_f32m1(r21, r21, 1);                       \
  r31 = vslidedown_vx_f32m1(r31, r31, 2);                       \
  r22 = vslidedown_vx_f32m1(r22, r22, 1);                       \
  r32 = vslidedown_vx_f32m1(r32, r32, 2);                       \
  r23 = vslidedown_vx_f32m1(r23, r23, 1);                       \
  r33 = vslidedown_vx_f32m1(r33, r33, 2);                       \
  vfloat32m1_t ofmap = vfadd_vv_f32m1(r11, r21);                \
  ofmap = vfadd_vv_f32m1(ofmap, r31);                           \
  ofmap = vfadd_vv_f32m1(ofmap, r12);                           \
  ofmap = vfadd_vv_f32m1(ofmap, r22);                           \
  ofmap = vfadd_vv_f32m1(ofmap, r32);                           \
  ofmap = vfadd_vv_f32m1(ofmap, r13);                           \
  ofmap = vfadd_vv_f32m1(ofmap, r23);                           \
  ofmap = vfadd_vv_f32m1(ofmap, r33);                           \
  vse32_v_f32m1_m(bmask, bPtr, ofmap)


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

#ifdef PACKED_SIMD
#define CORE_STEP (HARDWARE_VECTOR_LEN - (FILTER_DIM - 1))
#endif

// prefetch sizings
#if defined(USE_VEC) || defined(MANYCORE_PREFETCH)
#ifdef LONGLINES
  #define POST_REGION_WORD 300
#else
  #define POST_REGION_WORD 288
#endif

#ifndef INIT_FRAMES
#define INIT_FRAMES 2
#endif

#ifdef LONGLINES
  #define LOAD_DEPTH 8
  // (CACHE_LINE_SIZE / sizeof(DTYPE) / VECTOR_LEN / 2)
  #define CORE_STEP (LOAD_DEPTH)
  #define C_STRIDE (CORE_STEP*VECTOR_LEN - (FILTER_DIM-1))
  #define OUT_PTR_OFFSET (1)
  #define CENTER_ITERS (CORE_STEP - 2)
  #define LOAD_DEPTH_M1 (LOAD_DEPTH - 1)
#else
  #define LOAD_DEPTH 8
  #define CORE_STEP (LOAD_DEPTH - (FILTER_DIM - 1))
  #define C_STRIDE (CORE_STEP*VECTOR_LEN)
  #define OUT_PTR_OFFSET (0)
  #define CENTER_ITERS (CORE_STEP)
#endif

// requirement CORE_STEP % NESTED_SIMD_VLEN == 0
#ifdef NESTED_SIMD
#if CORE_STEP % NESTED_SIMD_VLEN != 0
#error Load length and valu len mismatch
#endif
#define NESTED_SIMD_STRIDE (NESTED_SIMD_VLEN-(FILTER_DIM-1))
#else
#define NESTED_SIMD_STRIDE (1)
#endif


#ifdef LONGLINES
  #define SHARED_REGION_SIZE (2*FILTER_DIM)
  #define REGION_SIZE (LOAD_DEPTH*FILTER_DIM + SHARED_REGION_SIZE)
  #define LOADED_REGION_SIZE (REGION_SIZE - SHARED_REGION_SIZE)
  #define SHARED_OFF (LOADED_REGION_SIZE)
#else
  #define REGION_SIZE (LOAD_DEPTH*FILTER_DIM)
#endif

#define NUM_REGIONS (POST_REGION_WORD / REGION_SIZE)

#ifdef MANYCORE_PREFETCH
  #define VERTICAL_FETCH_TYPE (TO_SELF)
#else
  #define VERTICAL_FETCH_TYPE (TO_ONE_CORE)
#endif


inline void prefetch_vert_frame(DTYPE *a, int r, int c, int ncols, int dim, int *spadIdx) {
  for (int k1 = -1; k1 <= 1; k1++) {
    #ifdef LONGLINES
      int aIdx = (r + k1) * ncols + c - 1;
      VPREFETCH_LR(*spadIdx + (k1+1)*LOAD_DEPTH, a + aIdx, 0, 
        LOAD_DEPTH, TO_ALL_CORES);
    #else
    for (int core = 0; core < dim; core++) {
      int aIdx = (r + k1) * ncols + c - 1 + core * CORE_STEP;
      // int aIdx = core * 16; // at least for size 4, only use 4/8 caches so less bw
      // printf("mid issue r %d c %d k1 %d core %d, depth %d, aIdx %d\n", r, c, k1, core, LOAD_DEPTH, aIdx);
      VPREFETCH_LR(*spadIdx + (k1+1)*LOAD_DEPTH, a + aIdx, core, LOAD_DEPTH, VERTICAL_FETCH_TYPE);
    }
    // (*spadIdx)+=LOAD_DEPTH;
    #endif
  }

  #ifndef MANYCORE_PREFETCH
  (*spadIdx) += REGION_SIZE;
  if (*spadIdx == POST_REGION_WORD) *spadIdx = 0;
  #endif
}

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
