#ifndef __SYR2K_KERNEL_H__
#define __SYR2K_KERNEL_H__

#include "syr2k.h"

#ifdef USE_VEC

void tril_syr2k(int mask, DTYPE *a, DTYPE *b, DTYPE *c, int N, int M, 
                  int ptid, int groupId, int numGroups, int vtid,
                  int ptidMailer, int linkId);

#endif

#endif
