#ifndef __ATAX_H__
#define __ATAX_H__

// #define VECTOR_LEN 4
#ifdef VECTOR_LEN
#define _VEC
#endif

// #define MANYCORE_PREFETCH

// only v16 better this way so use it
#if VECTOR_LEN==16 || defined(LONGLINES)
#define POLYBENCH_VERSION
#else
// longlines derives from this, because reduction at the end might be terrible w/ longlines
#define REDUCE_VERSION
#endif

#ifndef INIT_FRAMES
#define INIT_FRAMES 2
#endif

#ifdef LONGLINES
  #define PREFETCH_LEN 16
  #define J_STRIDE (PREFETCH_LEN*VECTOR_LEN)
  #define I_STRIDE (1)
#else
  #define PREFETCH_LEN 16
  #define J_STRIDE (PREFETCH_LEN)
  #define I_STRIDE (VECTOR_LEN)
#endif
#define REGION_SIZE (PREFETCH_LEN * 2)
#define POST_FRAME_WORD (512)
#define NUM_REGIONS (POST_FRAME_WORD / REGION_SIZE)


// stuff for partial reduction
#ifndef ACCUM_GRANULARITY
#define ACCUM_GRANULARITY 4
#endif

// TODO hardcode this based on spipe
#if VECTOR_LEN == 4
#define NUM_GROUPS_PER_PIPE (3)
#else
#define NUM_GROUPS_PER_PIPE (1)
#endif

#define SUB_FRAME_SIZE (VECTOR_LEN * NUM_GROUPS_PER_PIPE)
#define MAILER_FRAME_SIZE (SUB_FRAME_SIZE * ACCUM_GRANULARITY)
#define MAILER_NUM_FRAMES (POST_FRAME_WORD / MAILER_FRAME_SIZE)
#define MAILER_POST_FRAME_WORD (MAILER_FRAME_SIZE * MAILER_NUM_FRAMES)

// needs to be maxed at number of frame counters
#if MAILER_NUM_FRAMES < 5
#define FRAMES_TO_SYNC_AFTER (MAILER_NUM_FRAMES)
#else
#define FRAMES_TO_SYNC_AFTER (5)
#endif

// how much vector core will write
#define PER_CORE_MAILER_FRAME (VECTOR_LEN)
// includes also if any base value to the sum
#define PER_CORE_FULL_MAILER_FRAME (PER_CORE_MAILER_FRAME)

typedef float DTYPE;

// pthread argument for the kernel
typedef struct Kern_Args
{
  DTYPE *a, *_x, *_y, *ax, *_y_partial;
  int nx, ny;
  int tid_x, tid_y;
  int dim_x, dim_y;
} Kern_Args;

// helper to pack vvadd args
Kern_Args *construct_args(
    DTYPE *a, DTYPE *_x, DTYPE *_y, DTYPE *ax, DTYPE* _y_partial, int nx, int ny,
    int tid_x, int tid_y, int dim_x, int dim_y);

// pthread call
void *pthread_kernel(void *args);

// vvadd kernel
void kernel(
    DTYPE *a, DTYPE *_x, DTYPE *_y, DTYPE *ax, DTYPE *_y_partial, int nx, int ny,
    int tid_x, int tid_y, int dim_x, int dim_y);

#endif
