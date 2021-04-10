#ifndef __FDTD2D_H__
#define __FDTD2D_H__

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

#ifdef MANYCORE_PREFETCH
  #define VERTICAL_FETCH_TYPE (TO_SELF)
#else
  #define VERTICAL_FETCH_TYPE (TO_ONE_CORE)
#endif

// prefetch sizing
#if defined(USE_VEC) || defined(MANYCORE_PREFETCH)
// dedicate a quarter of scratchpad to frames
// #define POST_FRAME_WORD 120

// number of frames to get ahead
#ifndef INIT_FRAMES
#define INIT_FRAMES 1
#endif



// prefetch sizings for step1
#define STEP1_UNROLL_LEN 16
#ifdef LONGLINES
  #define STEP1_J_STRIDE (STEP1_UNROLL_LEN*VECTOR_LEN)
  #define STEP1_I_STRIDE (1)
#else
  #define STEP1_J_STRIDE (STEP1_UNROLL_LEN)
  #define STEP1_I_STRIDE (VECTOR_LEN)
#endif
#define STEP1_REGION_SIZE (3*STEP1_UNROLL_LEN)
#define STEP1_NUM_REGIONS (8)
#define STEP1_POST_FRAME_WORD (STEP1_REGION_SIZE*STEP1_NUM_REGIONS)


// prefetch sizings for step2
#define STEP2_UNROLL_LEN 16
#ifdef LONGLINES
  #define STEP2_J_STRIDE (STEP2_UNROLL_LEN*VECTOR_LEN)
  #define STEP2_I_STRIDE (1)
#else
  #define STEP2_J_STRIDE (STEP2_UNROLL_LEN)
  #define STEP2_I_STRIDE (VECTOR_LEN)
#endif
#define STEP2_REGION_SIZE (2*STEP2_UNROLL_LEN+1)
#define STEP2_NUM_REGIONS (8)
#define STEP2_POST_FRAME_WORD (STEP2_REGION_SIZE*STEP2_NUM_REGIONS)

// prefetch sizings for step3
#define STEP3_UNROLL_LEN 16
#ifdef LONGLINES
  #define STEP3_J_STRIDE (STEP3_UNROLL_LEN*VECTOR_LEN)
  #define STEP3_I_STRIDE (1)
#else
  #define STEP3_J_STRIDE (STEP3_UNROLL_LEN)
  #define STEP3_I_STRIDE (VECTOR_LEN)
#endif
#define STEP3_REGION_SIZE (4*STEP3_UNROLL_LEN+1)
#define STEP3_NUM_REGIONS (8)
#define STEP3_POST_FRAME_WORD (STEP3_REGION_SIZE*STEP3_NUM_REGIONS)

inline void prefetch_step1_frame_i0(DTYPE *fict, int t, int *sp) {
  // pad out to region size (3). also only fetch one element
  for (int core = 0; core < 1; core++) {
    VPREFETCH_LR(*sp + 0*STEP1_UNROLL_LEN, fict + t, core, STEP1_UNROLL_LEN, VERTICAL_FETCH_TYPE);
    VPREFETCH_LR(*sp + 1*STEP1_UNROLL_LEN, fict + t, core, STEP1_UNROLL_LEN, VERTICAL_FETCH_TYPE);
    VPREFETCH_LR(*sp + 2*STEP1_UNROLL_LEN, fict + t, core, STEP1_UNROLL_LEN, VERTICAL_FETCH_TYPE);
  }

  #ifndef MANYCORE_PREFETCH
  *sp += STEP1_REGION_SIZE;
  if (*sp == STEP1_POST_FRAME_WORD) *sp = 0;
  #endif
}

inline void prefetch_step1_frame_in0(DTYPE *ey, DTYPE *hz, int i, int j, int NY, int *sp) {
  #ifdef LONGLINES
  VPREFETCH_LR(*sp + 0*STEP1_UNROLL_LEN, ey + ( i )     * NY + j, 0, STEP1_UNROLL_LEN, TO_ALL_CORES);
  VPREFETCH_LR(*sp + 1*STEP1_UNROLL_LEN, hz + ( i )     * NY + j, 0, STEP1_UNROLL_LEN, TO_ALL_CORES);
  VPREFETCH_LR(*sp + 2*STEP1_UNROLL_LEN, hz + ( i - 1 ) * NY + j, 0, STEP1_UNROLL_LEN, TO_ALL_CORES);
  #else
  for (int core = 0; core < VECTOR_LEN; core++) {
    VPREFETCH_LR(*sp + 0*STEP1_UNROLL_LEN, ey + (i + core)     * NY + j, core, STEP1_UNROLL_LEN, VERTICAL_FETCH_TYPE);
    VPREFETCH_LR(*sp + 1*STEP1_UNROLL_LEN, hz + (i + core)     * NY + j, core, STEP1_UNROLL_LEN, VERTICAL_FETCH_TYPE);
    VPREFETCH_LR(*sp + 2*STEP1_UNROLL_LEN, hz + (i + core - 1) * NY + j, core, STEP1_UNROLL_LEN, VERTICAL_FETCH_TYPE);
  }
  #endif

  #ifndef MANYCORE_PREFETCH
  *sp += STEP1_REGION_SIZE;
  if (*sp == STEP1_POST_FRAME_WORD) *sp = 0;
  #endif
}

inline void prefetch_step2_frame(DTYPE *ex, DTYPE *hz, int i, int j, int NY, int *sp) {
  #ifdef LONGLINES
  VPREFETCH_LR(*sp + 0               , ex + ( i ) * (NY+1) + j, 0, STEP2_UNROLL_LEN    , TO_ALL_CORES);
  
  // this +1 is a problem b/c not the same as j stride -> use unoptimized option
  // VPREFETCH_LR(*sp + STEP2_UNROLL_LEN, hz + ( i ) * NY + (j-1), 0, STEP2_UNROLL_LEN + 1, TO_ALL_CORES);
  for (int core = 0; core < VECTOR_LEN; core++) {
    int j_idx = j + core * STEP2_UNROLL_LEN;
    VPREFETCH_LR(*sp + STEP2_UNROLL_LEN, hz + ( i ) * NY + (j_idx-1), core, STEP2_UNROLL_LEN + 1, TO_ONE_CORE);
  }
  #else
  for (int core = 0; core < VECTOR_LEN; core++) {
    VPREFETCH_LR(*sp + 0               , ex + (i + core) * (NY+1) + j, core, STEP2_UNROLL_LEN    , VERTICAL_FETCH_TYPE);
    VPREFETCH_LR(*sp + STEP2_UNROLL_LEN, hz + (i + core) * NY + (j-1), core, STEP2_UNROLL_LEN + 1, VERTICAL_FETCH_TYPE);
  }
  #endif

  #ifndef MANYCORE_PREFETCH
  *sp += STEP2_REGION_SIZE;
  if (*sp == STEP2_POST_FRAME_WORD) *sp = 0;
  #endif
}

inline void prefetch_step3_frame(DTYPE *ex, DTYPE *ey, DTYPE *hz, 
      int i, int j, int NY, int *sp) {
  int ul = STEP3_UNROLL_LEN;

  #ifdef LONGLINES
  VPREFETCH_L (*sp + 0*ul  , hz + ( i )     * NY     + j, 0, STEP3_UNROLL_LEN    , TO_ALL_CORES);
  VPREFETCH_L (*sp + 2*ul+1, ey + ( i + 1 ) * NY     + j, 0, STEP3_UNROLL_LEN    , TO_ALL_CORES);
  VPREFETCH_L (*sp + 3*ul+1, ey + ( i )     * NY     + j, 0, STEP3_UNROLL_LEN    , TO_ALL_CORES);

  for (int core = 0; core < VECTOR_LEN; core++) {
    int j_idx = j + core * ul;
    VPREFETCH_LR(*sp + 1*ul  , ex + ( i )     * (NY+1) + j_idx, core, STEP3_UNROLL_LEN + 1, TO_ONE_CORE);
  }
  #else
  for (int core = 0; core < VECTOR_LEN; core++) {
    VPREFETCH_L (*sp + 0*ul  , hz + (i + core)     * NY     + j, core, STEP3_UNROLL_LEN    , VERTICAL_FETCH_TYPE);
    VPREFETCH_LR(*sp + 1*ul  , ex + (i + core)     * (NY+1) + j, core, STEP3_UNROLL_LEN + 1, VERTICAL_FETCH_TYPE);
    VPREFETCH_L (*sp + 2*ul+1, ey + (i + core + 1) * NY     + j, core, STEP3_UNROLL_LEN    , VERTICAL_FETCH_TYPE);
    VPREFETCH_L (*sp + 3*ul+1, ey + (i + core)     * NY     + j, core, STEP3_UNROLL_LEN    , VERTICAL_FETCH_TYPE);
  }
  #endif

  #ifndef MANYCORE_PREFETCH
  *sp += STEP3_REGION_SIZE;
  if (*sp == STEP3_POST_FRAME_WORD) *sp = 0;
  #endif  
}



#endif

// covariance specific value
#define FLOAT_N 3214212.01

// pthread argument for the kernel
typedef struct Kern_Args {
  DTYPE *fict, *ex, *ey, *hz;
  int NX, NY, tmax;
  int tid_x, tid_y;
  int dim_x, dim_y;
} Kern_Args;

// helper to pack args
Kern_Args *construct_args(
    DTYPE *fict, DTYPE *ex, DTYPE *ey, DTYPE *hz,
    int NX, int NY, int tmax,
    int tid_x, int tid_y, int dim_x, int dim_y
  );

// pthread call
void *pthread_kernel(void *args);

// kernel
void kernel(
    DTYPE *fict, DTYPE *ex, DTYPE *ey, DTYPE *hz,
    int NX, int NY, int tmax,
    int tid_x, int tid_y, int dim_x, int dim_y
  );

#endif
