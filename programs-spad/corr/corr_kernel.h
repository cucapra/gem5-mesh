#ifndef __CORR_KERNEL_H__
#define __CORR_KERNEL_H__

#include <stdlib.h>
#include <stdio.h>

#include "pthread_launch.h"
#include "corr.h"
#include "spad.h"
#include "bind_defs.h"

#define PREFETCH_VISSUE(label) \
    for (int j = 0; j < startOffset; j+=prefetch_stride) { \
      prefetch_data_frame(data,i,j,n,vdim,&sp_data_offset); \
    } \
    for(int j=startOffset; j<n; j+=prefetch_stride){ \
      prefetch_data_frame(data,i,j,n,vdim,&sp_data_offset); \
      ISSUE_VINST(label); \
    } \
    for (int j = n - startOffset; j < n; j+=prefetch_stride) { \
      ISSUE_VINST(label); \
    }

void tril_corr_vec_1(int mask, DTYPE *data, DTYPE *symmat, DTYPE *mean, DTYPE *stddev, int m, int n,
              int start, int end, int vtid, int vdim, int ptid, float eps);

void tril_corr_vec_2(int mask, DTYPE *data, DTYPE *symmat, DTYPE *mean, DTYPE *stddev, int m, int n,
              int start, int end, int vtid, int vdim, int ptid);

#endif
