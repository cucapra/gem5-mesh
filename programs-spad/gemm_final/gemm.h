#ifndef __VVADD_H__
#define __VVADD_H__

// #define VEC_LEN 16
#ifdef VEC_LEN
#define _VEC
#endif

#ifndef BLK_DIM
#define BLK_DIM 4 //tile size
#endif

#ifndef _N_SPS
#define _N_SPS 64
#endif

#if VEC_LEN==4
#define DIM_X 2
#elif VEC_LEN==16
#define DIM_X 4
#endif

#define VEC_MANYCORE_OPT

#if defined VEC_MANYCORE_OPT && defined _VEC
#define MANYCORE_PREFETCH
#endif

#define ALPHA 32412.0f
#define BETA 2123.0f

// #define SHARING
// #define C_PREFETCH 
// #define MANYCORE_PREFETCH

#define INIT_FRAMES 2

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
  tg_x = 1; \
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

#endif

#define PTID_FINDER(ptid)\
  if(ptid==38) ptid_new=0;\
  else if(ptid==40) ptid_new=1;\
  else if(ptid==41) ptid_new=2;\
  else if(ptid==46) ptid_new=3;\
  else if(ptid==47) ptid_new=4;\
  else if(ptid==48) ptid_new=5;\
  else if(ptid==49) ptid_new=6;\
  else if(ptid==54) ptid_new=7;\
  else if(ptid==55) ptid_new=8;\
  else if(ptid==56) ptid_new=9;\
  else if(ptid==57) ptid_new=10;\
  else if(ptid==62) ptid_new=11;\
  else if(ptid==63) ptid_new=12;

#define WORK_DIV_OPT(m,n) \
  int total_groups = 3; \
  int unused = _N_SPS - (total_groups*(VEC_LEN+1)); \
  int total_compute_cores = (total_groups*VEC_LEN) + unused; \
  int alignment = BLK_DIM * DIM_X * total_groups; \
  int m_vec = roundUp((total_groups*VEC_LEN*m)/total_compute_cores, alignment);\
  if(m_vec>m) m_vec=m;\
  m_manycore = m-m_vec; \
  if(cinfo.used) { \
    alignment = BLK_DIM * DIM_X; \
    m_start = roundUp((cinfo.unique_id + 0) * m_vec / total_groups, alignment); \
    m_end = roundUp((cinfo.unique_id + 1) * m_vec / total_groups, alignment); \
    n_start = 0; \
    n_end = n; \
  }\
  else if(ptid_new>0){\
    ptid_new-=1;\
    int tg_x=6;\
    int tg_y=2;\
    int utid_x = ptid_new%tg_x;\
    int utid_y = ptid_new/tg_x;\
    alignment = BLK_DIM; \
    m_start = m_vec + roundUp((utid_y + 0) * m_manycore / tg_y, alignment); \
    m_end = m_vec+ roundUp((utid_y + 1) * m_manycore / tg_y, alignment); \
    n_start = roundUp((utid_x + 0) * n / tg_x, alignment); \
    n_end = roundUp((utid_x + 1) * n / tg_x, alignment); \
  }\
  else\
    m_start=m_end=n_start=n_end=0;

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
