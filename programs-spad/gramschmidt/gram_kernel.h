#ifndef __GRAM_KERNEL_H__
#define __GRAM_KERNEL_H__

#include "gramschmidt.h"

#ifdef USE_VEC
void tril_u_normalize(int mask, DTYPE *a, DTYPE *r, DTYPE *q, 
    int numVectors, int vectorLen, int k, int ptid, int groupId, int numGroups, int vtid);
void tril_u_dot_subtract(int mask, DTYPE *a, DTYPE *r, DTYPE *q, 
    int numVectors, int vectorLen, int k, int ptid, int groupId, int numGroups, int vtid);
#endif

#endif