#ifndef __GEMM_KERNEL_H__
#define __GEMM_KERNEL_H__

#include <stdlib.h>
#include <stdio.h>

#include "pthread_launch.h"
#include "gemm.h"
#include "spad.h"
#include "bind_defs.h"

void tril_gemm_vec(int mask, DTYPE *a, DTYPE *b, DTYPE *c, int m, int n, int t,
                   int m_start, int m_end, int tid_x, int tid_y, int tid, int ptid);

#endif
