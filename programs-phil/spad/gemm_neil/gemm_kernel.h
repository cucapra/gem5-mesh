#ifndef __GEMM_KERNEL_H__
#define __GEMM_KERNEL_H__

#include <stdlib.h>
#include <stdio.h>

#include "pthread_launch.h"
#include "gemm.h"
#include "spad.h"
#include "../../common/bind_defs.h"

void gemm_vec_simd(int mask, DTYPE *a, DTYPE *b, DTYPE *c, int m, int n, int t,
                   int m_start, int m_end, int n_start, int n_end, int tid_x, int tid_y, int tid, int ptid, int* ptid_group);

#endif
