#ifndef __TEMP_H__
#define __TEMP_H__

#include <math.h>

// #define VEC_LEN 4 //vec group size
#ifdef VEC_LEN
#define _VEC
#endif

// #define MANYCORE_PREFETCH


#define REGION_SIZE 16 //configure using LCM of required frame/region sizes
#define NUM_REGIONS (512 / REGION_SIZE) // (0,512) in this case is the hardware region area 

#define INIT_FRAMES 2
#define REGION_SIZE_K2 32 //region size for kernel 2
#define NUM_REGIONS_K2 (512 / REGION_SIZE_K2) // kernel 2

#define PF_BEGIN(pf_len) \
  for (int j = 0; j < n; j+=pf_len){ 

#define PF_END(num_region) \
  REMEM(); \
  spadRegion = (spadRegion + 1) % num_region;  \
  }

#define PF1(off,idx) \
  off = spadRegion * REGION_SIZE; \
  VPREFETCH_L(off, data + idx, 0, REGION_SIZE,1); \
  FRAME_START(); \
  for(int jj=0; jj<REGION_SIZE; jj++)

#define PF2(off1,off2,idx1,idx2) \
  off1 = spadRegion * REGION_SIZE_K2; \
  off2 = off1 + REGION_SIZE_K2/2; \
  VPREFETCH_L(off1, data + idx1, 0, REGION_SIZE_K2/2,1); \
  VPREFETCH_L(off2, data + idx2, 0, REGION_SIZE_K2/2,1); \
  FRAME_START(); \
  for(int jj=0; jj<REGION_SIZE_K2/2; jj++)


typedef float DTYPE;

// pthread argument for the kernel
typedef struct Kern_Args
{
  DTYPE *data;
  DTYPE *symmat, *mean, *stddev;
  int m,n;
  int tid_x, tid_y;
  int dim_x, dim_y;
} Kern_Args;

// helper to pack vvadd args
Kern_Args *construct_args(
    DTYPE *data, DTYPE *symmat, DTYPE *mean, DTYPE *stddev, int m, int n,
    int tid_x, int tid_y, int dim_x, int dim_y);

// pthread call
void *pthread_kernel(void *args);

// vvadd kernel
void kernel(
    DTYPE *data, DTYPE *symmat, DTYPE *mean, DTYPE *stddev, int m, int n,
    int tid_x, int tid_y, int dim_x, int dim_y);

#endif
