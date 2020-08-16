#ifndef __GEMM_KERNEL_H__
#define __GEMM_KERNEL_H__

#include <stdlib.h>
#include <stdio.h>

#include "pthread_launch.h"
#include "3mm.h"
#include "spad.h"
#include "bind_defs.h"
#include "util.h"

void tril_gemm_vec(int mask, DTYPE *a, DTYPE *b, DTYPE *c, int m, int n, int t,
                   int m_start, int m_end, int n_start, int n_end, int vtid_x, int vtid_y, int vtid, int ptid);

#endif
