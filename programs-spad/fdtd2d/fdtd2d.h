#ifndef __FDTD2D_H__
#define __FDTD2D_H__

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

// prefetch sizing
#if defined(USE_VEC) || defined(MANYCORE_PREFETCH)
// dedicate a quarter of scratchpad to frames
#define POST_FRAME_WORD 120

// number of frames to get ahead
#define INIT_FRAMES 1

// prefetch sizings for step1
#define STEP1_REGION_SIZE 3
#define STEP1_NUM_REGIONS (POST_FRAME_WORD / STEP1_REGION_SIZE)

// prefetch sizings for step2
#define STEP2_REGION_SIZE 3
#define STEP2_NUM_REGIONS (POST_FRAME_WORD / STEP2_REGION_SIZE)

// prefetch sizings for step3
#define STEP3_REGION_SIZE 5
#define STEP3_NUM_REGIONS (POST_FRAME_WORD / STEP3_REGION_SIZE)

inline void prefetch_step1_frame_i0(DTYPE *fict, int t, int *sp) {
  // pad out to region size (3). also only fetch one element
  for (int core = 0; core < VECTOR_LEN; core++) {
    VPREFETCH_L(*sp + 0, fict + t, core, 1, VERTICAL);
    VPREFETCH_L(*sp + 1, fict + t, core, 1, VERTICAL);
    VPREFETCH_L(*sp + 2, fict + t, core, 1, VERTICAL);
  }

  #ifndef MANYCORE_PREFETCH
  *sp += STEP1_REGION_SIZE;
  if (*sp == POST_FRAME_WORD) *sp = 0;
  #endif
}

inline void prefetch_step1_frame_in0(DTYPE *ey, DTYPE *hz, int i, int j, int NY, int *sp) {
  VPREFETCH_LR(*sp + 0, ey + i     * NY + j, 0, VECTOR_LEN, HORIZONTAL);
  VPREFETCH_LR(*sp + 1, hz + i     * NY + j, 0, VECTOR_LEN, HORIZONTAL);
  VPREFETCH_LR(*sp + 2, hz + (i-1) * NY + j, 0, VECTOR_LEN, HORIZONTAL);

  #ifndef MANYCORE_PREFETCH
  *sp += STEP1_REGION_SIZE;
  if (*sp == POST_FRAME_WORD) *sp = 0;
  #endif
}

inline void prefetch_step2_frame(DTYPE *ex, DTYPE *hz, int i, int j, int NY, int *sp) {
  VPREFETCH_LR(*sp + 0, ex + i * (NY+1) + j, 0, VECTOR_LEN, HORIZONTAL);
  VPREFETCH_LR(*sp + 1, hz + i * NY + j    , 0, VECTOR_LEN, HORIZONTAL);
  VPREFETCH_LR(*sp + 2, hz + i * NY + (j-1), 0, VECTOR_LEN, HORIZONTAL);

  #ifndef MANYCORE_PREFETCH
  *sp += STEP2_REGION_SIZE;
  if (*sp == POST_FRAME_WORD) *sp = 0;
  #endif
}

inline void prefetch_step3_frame(DTYPE *ex, DTYPE *ey, DTYPE *hz, 
      int i, int j, int NY, int *sp) {

  VPREFETCH_LR(*sp + 0, hz + i     * NY     + j    , 0, VECTOR_LEN, HORIZONTAL);
  VPREFETCH_LR(*sp + 1, ex + i     * (NY+1) + (j+1), 0, VECTOR_LEN, HORIZONTAL);
  VPREFETCH_LR(*sp + 2, ex + i     * (NY+1) + j    , 0, VECTOR_LEN, HORIZONTAL);
  VPREFETCH_LR(*sp + 3, ey + (i+1) * NY     + j    , 0, VECTOR_LEN, HORIZONTAL);
  VPREFETCH_LR(*sp + 4, ey + i     * NY     + j    , 0, VECTOR_LEN, HORIZONTAL);

  #ifndef MANYCORE_PREFETCH
  *sp += STEP3_REGION_SIZE;
  if (*sp == POST_FRAME_WORD) *sp = 0;
  #endif  
}



#endif

// covariance specific value
#define FLOAT_N 3214212.01

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
