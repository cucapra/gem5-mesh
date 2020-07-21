#ifndef __CONV3D_KERNEL_H__
#define __CONV3D_KERNEL_H__

#include "conv3d.h"

void tril_conv3d(int mask,
    DTYPE *a, DTYPE *b, int outer_start, int outer_end, int inner_dim, int eff_inner_dim,
    int ptid, int vtid_x, int vtid_y, int vdim_x, int vdim_y, DTYPE *p_sp_ptr, DTYPE *n_sp_ptr);


#endif