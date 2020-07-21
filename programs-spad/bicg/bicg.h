#ifndef __BICG_H__
#define __BICG_H__

// data type to do computation with
#define DTYPE float

// one of these should be defined to dictate config
// #define NO_VEC 1
// #define VEC_4_SIMD 1
// #define VEC_16_SIMD 1

// vvadd_execute config directives
#if !defined(NO_VEC)
#define USE_VEC 1
#endif

// vector grouping directives
#if defined(VEC_4_SIMD)
#define VECTOR_LEN 4
#endif
#if defined(VEC_16_SIMD)
#define VECTOR_LEN 16
#endif

// prefetch sizing
#ifdef USE_VEC
// number of 32bits words per frame
#define FRAME_SIZE 2
// number of frames
#define NUM_FRAMES 16
// scratchpad offset after prefetch frames
#define POST_FRAME_WORD (FRAME_SIZE * NUM_FRAMES)
// number of frames to get ahead
#define INIT_FRAMES 2
#define INIT_SPM_OFFSET (INIT_FRAMES * FRAME_SIZE)
// lenght of a prefetch
#define Q_PREFETCH_LEN 1
#define S_PREFETCH_LEN VECTOR_LEN
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
  DTYPE *a, *r, *p, *s, *q;
  int NX, NY;
  int tid_x, tid_y;
  int dim_x, dim_y;
} Kern_Args;

// helper to pack args
Kern_Args *construct_args(
    DTYPE *a, DTYPE *r, DTYPE *p,
    DTYPE *s, DTYPE *q, 
    int NX, int NY,
    int tid_x, int tid_y, int dim_x, int dim_y
  );

// pthread call
void *pthread_kernel(void *args);

// kernel
void kernel(
    DTYPE *a, DTYPE *r, DTYPE *p,
    DTYPE *s, DTYPE *q, 
    int NX, int NY,
    int tid_x, int tid_y, int dim_x, int dim_y
  );

#endif
