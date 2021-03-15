#ifndef __SYRK_KERNEL_H__
#define __SYRK_KERNEL_H__

#include "syrk.h"

void tril_syrk(int mask, DTYPE *a, DTYPE *c, int N, int M, 
                  int ptid, int groupId, int numGroups, int vtid,
                  int ptidMailer, int linkId, int numGroupsPerMailer);

#endif