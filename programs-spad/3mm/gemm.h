#ifndef __GEMM_H__
#define __GEMM_H__

#include "3mm.h"

void gemm_manycore(DTYPE *aT, DTYPE *b, DTYPE *c, int m, int n, int t,
     int m_start, int n_start, int ptid, int pdim_x, int pdim_y);

#endif