#ifndef __GEMM_COMMON_H__
#define __GEMM_COMMON_H__

typedef int DTYPE;

#define BLK_DIM 4
#define REGION_SIZE (BLK_DIM * 2)
#define NUM_REGIONS (512 / REGION_SIZE)

#define MANYCORE_PREFETCH

#ifdef GEMM
#define ALPHA 4
#define BETA 5
#elif defined MM
#define ALPHA 1
#define BETA 0
#endif

void gemm_manycore(DTYPE *aT, DTYPE *b, DTYPE *c, int m, int n, int t,
     int m_start, int n_start, int ptid, int pdim_x, int pdim_y);

#endif