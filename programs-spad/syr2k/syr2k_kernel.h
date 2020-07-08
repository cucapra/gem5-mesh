#ifndef __SYR2K_KERNEL_H__
#define __SYR2K_KERNEL_H__

#include "syr2k.h"

void tril_syr2k(int mask, DTYPE *a, DTYPE *c, int N, int M, 
                  int ptid, int groupId, int numGroups, int vtid);

#endif
