#ifndef __CONV3D_KERNEL_H__
#define __CONV3D_KERNEL_H__

#include "conv3d.h"

void tril_conv3d(int mask,
    DTYPE *a, DTYPE *b, int outer_start, int outer_end, int NJ, int NK, //int eff_NK,
    int ptid, int vtid_x, int vtid_y, int vdim_x, int vdim_y);


#endif