#ifndef __ATAX_KERNEL_H__
#define __ATAX_KERNEL_H__

#include <stdlib.h>
#include <stdio.h>

#include "pthread_launch.h"
#include "atax.h"
#include "spad.h"
#include "../../common/bind_defs.h"

void atax_vec(int mask, DTYPE *a, DTYPE *_x, DTYPE *_y_partial, DTYPE *ax, int nx, int ny,
      int nx_start, int nx_end, int ptid, int vtid, int dim, int* ptid_group);

#endif
