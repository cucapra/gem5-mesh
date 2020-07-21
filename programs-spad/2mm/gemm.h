#ifndef __GEMM_COMMON_H__
#define __GEMM_COMMON_H__

#include "2mm.h"

void gemm_manycore(DTYPE *aT, DTYPE *b, DTYPE *c, int m, int n, int t,
     int m_start, int n_start, int ptid, int pdim_x, int pdim_y);

#endif