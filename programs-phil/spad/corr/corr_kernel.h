#ifndef __CORR_KERNEL_H__
#define __CORR_KERNEL_H__

#include <stdlib.h>
#include <stdio.h>

#include "pthread_launch.h"
#include "corr.h"
#include "spad.h"
#include "../../common/bind_defs.h"

void tril_corr_vec_1(int mask, DTYPE *data, DTYPE *symmat, DTYPE *mean, DTYPE *stddev, int m, int n,
              int start, int end, int vtid, int vdim, int ptid);

void tril_corr_vec_2(int mask, DTYPE *data, DTYPE *symmat, DTYPE *mean, DTYPE *stddev, int m, int n,
              int start, int end, int vtid, int vdim, int ptid);

#endif
