#ifndef __SPAD_H__
#define __SPAD_H__

#include <stddef.h>

void initScratchpads();
void *getSpAddr(int pad, size_t offset);

#endif

