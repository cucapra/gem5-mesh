#ifndef __CONV2D_KERNEL_H__
#define __CONV2D_KERNEL_H__

#include "conv2d.h"

void tril_conv2d(int mask,
    DTYPE *a, DTYPE *b, int outer_start, int outer_end, int inner_dim, int eff_inner_dim,
    int ptid, int vtid, DTYPE *p_sp_ptr, DTYPE *n_sp_ptr);


#endif