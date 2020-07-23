#ifndef __FDTD2D_KERNEL_H__
#define __FDTD2D_KERNEL_H__

#include "fdtd2d.h"

void tril_fdtd_step1(int mask,
  DTYPE *fict, DTYPE *ex, DTYPE *ey, DTYPE *hz, int t, int NX, int NY,
  int ptid, int groupId, int numGroups, int vtid);

void tril_fdtd_step2(int mask,
  DTYPE *ex, DTYPE *ey, DTYPE *hz, int t, int NX, int NY, int eff_NY,
  int ptid, int groupId, int numGroups, int vtid);

void tril_fdtd_step3(int mask,
  DTYPE *ex, DTYPE *ey, DTYPE *hz, int t, int NX, int NY, 
  int ptid, int groupId, int numGroups, int vtid);


#endif