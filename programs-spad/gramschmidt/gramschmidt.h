#ifndef __GRAMSCHMIDT_H__
#define __GRAMSCHMIDT_H__

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
#define INIT_FRAMES 1
#endif
#if defined(VEC_16_SIMD)
#define VECTOR_LEN 16
#define INIT_FRAMES 0
#endif

// prefetch sizings
#ifdef USE_VEC
#define POST_FRAME_WORD_NORM 128
#define FRAME_SIZE_NORM 1
#define NUM_FRAMES_NORM (POST_FRAME_WORD_NORM / FRAME_SIZE_NORM)
#define INIT_FRAMES_NORM INIT_FRAMES

#define POST_FRAME_WORD_SUB 128
#define FRAME_SIZE_SUB 2
#define NUM_FRAMES_SUB (POST_FRAME_WORD_SUB / FRAME_SIZE_SUB)
#define INIT_FRAMES_SUB INIT_FRAMES
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
  DTYPE *a, *r, *q;
  int numVectors, vectorLen;
  int tid_x, tid_y;
  int dim_x, dim_y;
} Kern_Args;

// helper to pack args
Kern_Args *construct_args(
    DTYPE *a, DTYPE *r, DTYPE *q, 
    int numVectors, int vectorLen,
    int tid_x, int tid_y, int dim_x, int dim_y
  );

// pthread call
void *pthread_kernel(void *args);

// kernel
void kernel(
    DTYPE *a, DTYPE *r, DTYPE *q, 
    int numVectors, int vectorLen,
    int tid_x, int tid_y, int dim_x, int dim_y
  );

#endif
