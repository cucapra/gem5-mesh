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


#define MOVE_STACK_ONTO_SCRATCHPAD()          \
  unsigned long long *spTop = getSpTop(ptid); \
  spTop -= 30;                                \
  unsigned long long stackLoc;                \
  unsigned long long temp;                    \
  _Pragma("GCC unroll(30)")                   \
  for(int i=0;i<30;i++){                      \
    asm volatile("ld t0, %[id](sp)\n\t"       \
                "sd t0, %[id](%[spad])\n\t"   \
                : "=r"(temp)                  \
                : [id] "i"(i*8), [spad] "r"(spTop)); \
  }                                                   \
  asm volatile (                                      \
      "addi %[dest], sp, 0\n\t"                       \
      "addi sp, %[spad], 0\n\t"                       \
      : [ dest ] "=r"(stackLoc)                       \
      : [ spad ] "r"(spTop))

#define RECOVER_DRAM_STACK() \
  asm volatile(              \
      "addi sp, %[stackTop], 0\n\t" ::[stackTop] "r"(stackLoc))

#endif

