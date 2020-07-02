#ifndef __VVADDK_H__
#define __VVADDK_H__

#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

#include "pthread_launch.h"
#include "vvadd.h"
#include "spad.h"
#include "../../common/bind_defs.h"

void tril_vvadd(int mask, DTYPE *a, DTYPE *b, DTYPE *c, int start, int end, int ptid, int vtid, int dim, int is_master);

#endif
