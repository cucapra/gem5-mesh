#ifndef __TEMP_KERNEL_H__
#define __TEMP_KERNEL_H__

#include <stdlib.h>
#include <stdio.h>

#include "pthread_launch.h"
#include "mvt.h"
#include "spad.h"
#include "bind_defs.h"
#include "util.h"

#ifdef PER_CORE_SIMD
#include <riscv_vector.h>
#endif

void tril_mvt_vec(int mask, DTYPE *a, DTYPE *y1, DTYPE *y2, DTYPE *x1, DTYPE *x2, int n, 
                  int start, int end, int ptid, int vtid);

#endif
