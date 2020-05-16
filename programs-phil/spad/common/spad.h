#ifndef __SPAD_H__
#define __SPAD_H__

#include <stddef.h>

void initScratchpads();
// get word addr in spad
void *getSpAddr(int pad, size_t wordOffset);
// get the top of the spad
void *getSpTop(int pad);

// get single spad size in bytes
size_t getSpadNumBytes();

#endif

