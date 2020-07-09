#ifndef __TEMP_KERNEL_H__
#define __TEMP_KERNEL_H__

#include <stdlib.h>
#include <stdio.h>

#include "pthread_launch.h"
#include "mvt.h"
#include "spad.h"
#include "../../common/bind_defs.h"

void tril_mvt_vec(int mask, DTYPE *a, DTYPE *y1, DTYPE *y2, DTYPE *x1, DTYPE *x2, int n, 
                  DTYPE *x2_partial, int start, int end, int ptid, int vtid, int dim, int* ptid_group);

#endif
