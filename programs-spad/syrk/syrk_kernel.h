#ifndef __SYRKK_H__
#define __SYRKK_H__

#include "syrk.h"

void tril_syrk(int mask, DTYPE *a, DTYPE *c, int N, int M, 
                  int ptid, int groupId, int numGroups, int vtid);

#endif