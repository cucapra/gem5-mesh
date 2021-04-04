#ifndef __GESUMMV_H__
#define __GESUMMV_H__

// #define VEC_LEN 4
#ifdef VEC_LEN
#define _VEC
#endif

// #define MANYCORE_PREFETCH

#ifndef INIT_FRAMES
#define INIT_FRAMES 2
#endif

#define REGION_SIZE 24 //configure using LCM of required frame/region sizes, multiple of 3(loading 3 arrays) and 4(cache aligned acccess)
#define NUM_REGIONS 10
#define POST_FRAME_WORD (REGION_SIZE * NUM_REGIONS)

#define UNROLL_LEN (REGION_SIZE/3)

#ifdef LONGLINES
  #define J_STRIDE (UNROLL_LEN*VEC_LEN)
  #define I_STRIDE (1)
#else
  #define J_STRIDE (UNROLL_LEN)
  #define I_STRIDE (VEC_LEN)
#endif

// stuff for partial reduction
#ifndef ACCUM_GRANULARITY
#define ACCUM_GRANULARITY 8
#endif

// TODO hardcode this based on spipe
#if VEC_LEN == 4
#define NUM_GROUPS_PER_PIPE (3)
#else
#define NUM_GROUPS_PER_PIPE (1)
#endif

#define OFFSET_PER_CORE (2)
#define MAILER_OFFSET (OFFSET_PER_CORE*NUM_GROUPS_PER_PIPE)
#define SUB_FRAME_SIZE (MAILER_OFFSET + VEC_LEN * NUM_GROUPS_PER_PIPE)
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
#define PER_CORE_MAILER_FRAME (VEC_LEN)
// includes also if any base value to the sum
#define PER_CORE_FULL_MAILER_FRAME (PER_CORE_MAILER_FRAME + OFFSET_PER_CORE)


#define ALPHA 3
#define BETA 2

typedef float DTYPE;

// pthread argument for the kernel
typedef struct Kern_Args
{
  DTYPE *a, *b, *x, *tmp, *y;
  int n;
  int tid_x, tid_y;
  int dim_x, dim_y;
} Kern_Args;

// helper to pack vvadd args
Kern_Args *construct_args(
    DTYPE *a, DTYPE *b, DTYPE *x, DTYPE *tmp, DTYPE *y, int n,
    int tid_x, int tid_y, int dim_x, int dim_y);

// pthread call
void *pthread_kernel(void *args);

// vvadd kernel
void kernel(
    DTYPE *a, DTYPE *b, DTYPE *x, DTYPE *tmp, DTYPE *y, int n,
    int tid_x, int tid_y, int dim_x, int dim_y);

#endif
