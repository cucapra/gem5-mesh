#ifndef __COVAR_KERNEL_H__
#define __COVAR_KERNEL_H__

#include "covar.h"

void tril_mean(int mask, DTYPE *mean, DTYPE *data, int N, int M, 
    int ptid, int groupId, int numGroups, int vtid);

void tril_covar(int mask, DTYPE *symmat, DTYPE *data, int N, int M, 
    int ptid, int groupId, int numGroups, int vtid, int ptidMailer, int linkId);


#endif