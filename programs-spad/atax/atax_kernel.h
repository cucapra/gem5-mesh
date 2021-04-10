#ifndef __ATAX_KERNEL_H__
#define __ATAX_KERNEL_H__

#include <stdlib.h>
#include <stdio.h>

#include "pthread_launch.h"
#include "atax.h"
#include "spad.h"
#include "bind_defs.h"
#include "util.h"

#ifdef PER_CORE_SIMD
#include <riscv_vector.h>
#endif

#ifdef REDUCE_VERSION
void tril_atax(int mask, DTYPE *a, DTYPE *_x, DTYPE *_y_partial, DTYPE *ax, int nx, int ny,
      int nx_start, int nx_end, int ptid, int vtid, int dim, int* ptid_group);

#elif defined(POLYBENCH_VERSION)
void tril_atax1(int mask, DTYPE *a, DTYPE *_x, DTYPE *ax, int nx, int ny,
      int nx_start, int nx_end, int ptid, int vtid, int ptidMailer, int linkId);

void tril_atax2(int mask, DTYPE *a, DTYPE *ax, DTYPE *_y, int nx, int ny,
      int nx_start, int nx_end, int ptid, int vtid, int dim);
#endif

#endif
