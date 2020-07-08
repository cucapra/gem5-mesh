#ifndef __BICGK_H__
#define __BICGK_H__

#include "bicg.h"

void tril_bicg_s(int mask, DTYPE *a, DTYPE *r, DTYPE *s, int NX, int NY, int ptid, int groupId, int numGroups, int vtid);
void tril_bicg_q(int mask, DTYPE *a, DTYPE *p, DTYPE *q, int NX, int NY, int ptid, int groupId, int numGroups, int vtid);

#endif
