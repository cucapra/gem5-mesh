#ifndef __3MM_H__
#define __3MM_H__

// #define VECTOR_LEN 4
#ifdef VECTOR_LEN
#define _VEC
#endif

//tile size
#ifndef BLK_DIM
#ifdef PER_CORE_SIMD
// match blk dim to simd length for ease of use
// 16 * 16 * 4 = 1kB space so fits in 4kB spad
#define BLK_DIM 16
#else
#define BLK_DIM 4
#endif
#endif

// #define MANYCORE_PREFETCH

#ifndef INIT_FRAMES
#define INIT_FRAMES 2
#endif

#define REGION_SIZE (BLK_DIM * 2)
#define NUM_REGIONS (512 / REGION_SIZE)


#define ALPHA 1
#define BETA 0

typedef float DTYPE;

#if VECTOR_LEN==4 && _N_SPS==64
#define WORK_DIV(m,n) \
  tg_x = 4; \
  tg_y = 3; \
  uid_x = unique_id%tg_x; \
  uid_y = unique_id/tg_x; \
  if(used) { \
    int alignment = BLK_DIM * vdim_x; \
    m_start = roundUp((uid_y + 0) * m / tg_y, alignment); \
    m_end = roundUp((uid_y + 1) * m / tg_y, alignment); \
    n_start = roundUp((uid_x + 0) * n / tg_x, alignment); \
    n_end = roundUp((uid_x + 1) * n / tg_x, alignment); \
  }

#else

#define WORK_DIV(m,n) \
  tg_x = 3; \
  tg_y = 1; \
  uid_x = unique_id%tg_x; \
  uid_y = unique_id/tg_x; \
  if(used) { \
    int alignment = BLK_DIM * vdim_x; \
    m_start = roundUp((uid_y + 0) * m / tg_y, alignment); \
    m_end = roundUp((uid_y + 1) * m / tg_y, alignment); \
    n_start = roundUp((uid_x + 0) * n / tg_x, alignment); \
    n_end = roundUp((uid_x + 1) * n / tg_x, alignment); \
  }

#endif


// pthread argument for the kernel
typedef struct Kern_Args
{
  DTYPE *a, *aT, *b, *e, *c, *cT, *d, *f, *eT, *g;
  int m, t1, k ,t2, n;
  int tid_x, tid_y;
  int dim_x, dim_y;
} Kern_Args;

// helper to pack vvadd args
Kern_Args *construct_args(
    DTYPE *a, DTYPE *aT, DTYPE *b, DTYPE *e, DTYPE *c, DTYPE *cT, DTYPE *d, DTYPE *f, DTYPE *eT, DTYPE *g, 
    int m, int t1, int k, int t2, int n, int tid_x, int tid_y, int dim_x, int dim_y);

// pthread call
void *pthread_kernel(void *args);

// vvadd kernel
void kernel(
    DTYPE *a, DTYPE *aT, DTYPE *b, DTYPE *e, DTYPE *c, DTYPE *cT, DTYPE *d, DTYPE *f, DTYPE *eT, DTYPE *g, 
    int m, int t1, int k, int t2, int n, int tid_x, int tid_y, int dim_x, int dim_y);


void transpose(DTYPE *a, int row, int col, DTYPE *aT);

#endif
