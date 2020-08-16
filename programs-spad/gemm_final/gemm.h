#ifndef __VVADD_H__
#define __VVADD_H__

// #define VEC_LEN 4
#ifdef VEC_LEN
#define _VEC
#endif

#ifndef BLK_DIM
#define BLK_DIM 4 //tile size
#endif


#if VEC_LEN==4
#define DIM_X 2
#elif VEC_LEN==16
#define DIM_X 4
#endif

#define ALPHA 32412.0f
#define BETA 2123.0f



// #define SHARING
// #define C_PREFETCH 
// #define MANYCORE_PREFETCH

#ifndef INIT_FRAMES
#define INIT_FRAMES 2
#endif

#ifdef SHARING
#define REGION_SIZE (BLK_DIM*2)/DIM_X
#define NUM_REGIONS (512 / REGION_SIZE)
#else
#define REGION_SIZE (BLK_DIM * 2)
#define NUM_REGIONS (512 / REGION_SIZE)
#endif

typedef float DTYPE;


#if VEC_LEN==4 && _N_SPS==64
#define WORK_DIV(m,n) \
  int uid_x,uid_y; \
  int tg_x,tg_y; \
  tg_x = 4; \
  tg_y = 3; \
  uid_x = cinfo.unique_id%tg_x; \
  uid_y = cinfo.unique_id/tg_x; \
  if(cinfo.used) { \
    int alignment = BLK_DIM * cinfo.vdim_x; \
    m_start = roundUp((uid_y + 0) * m / tg_y, alignment); \
    m_end = roundUp((uid_y + 1) * m / tg_y, alignment); \
    n_start = roundUp((uid_x + 0) * n / tg_x, alignment); \
    n_end = roundUp((uid_x + 1) * n / tg_x, alignment); \
  }

#else

#define WORK_DIV(m,n) \
  int uid_x,uid_y; \
  int tg_x,tg_y; \
  tg_x = 3; \
  tg_y = 1; \
  uid_x = cinfo.unique_id%tg_x; \
  uid_y = cinfo.unique_id/tg_x; \
  if(cinfo.used) { \
    int alignment = BLK_DIM * cinfo.vdim_x; \
    m_start = roundUp((uid_y + 0) * m / tg_y, alignment); \
    m_end = roundUp((uid_y + 1) * m / tg_y, alignment); \
    n_start = roundUp((uid_x + 0) * n / tg_x, alignment); \
    n_end = roundUp((uid_x + 1) * n / tg_x, alignment); \
  }

#endif

// pthread argument for the kernel
typedef struct Kern_Args
{
  DTYPE *a, *aT, *b, *c;
  int m, n, t;
  int tid_x, tid_y;
  int dim_x, dim_y;
} Kern_Args;

// helper to pack vvadd args
Kern_Args *construct_args(
    DTYPE *a, DTYPE *aT, DTYPE *b, DTYPE *c, int m, int n, int t,
    int tid_x, int tid_y, int dim_x, int dim_y);

// pthread call
void *pthread_kernel(void *args);

// vvadd kernel
void kernel(
    DTYPE *a, DTYPE *aT, DTYPE *b, DTYPE *c, int m, int n, int t,
    int tid_x, int tid_y, int dim_x, int dim_y);

#endif
