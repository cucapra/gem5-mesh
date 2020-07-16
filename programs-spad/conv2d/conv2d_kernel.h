#ifndef __CONV2D_KERNEL_H__
#define __CONV2D_KERNEL_H__

#include "conv2d.h"

void tril_conv2d(int mask,
    DTYPE *a, DTYPE *b, int start_row, int end_row, int ncols,
    int ptid, int vtid_x, int vtid_y, int vdim_x, int vdim_y, int effCols);


#endif