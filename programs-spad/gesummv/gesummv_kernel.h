#ifndef __GESUMMV_KERNEL_H__
#define __GESUMMV_KERNEL_H__

#include <stdlib.h>
#include <stdio.h>

#include "pthread_launch.h"
#include "gesummv.h"
#include "spad.h"
#include "bind_defs.h"

void tril_gesummv_vec(int mask, DTYPE *a, DTYPE *b, DTYPE *x, DTYPE *tmp, DTYPE *y,
    int n, int start, int end, int ptid, int vtid, int ptidMailer, int linkId);

#endif
