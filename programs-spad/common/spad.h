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


// moves top of stack onto scratchpad and adjust $sp to that location on spad
// NOTE GCC10 produces literally incorrect code if do
// asm volatile ( \
// "addi %[dest], sp, 0\n\t" \
// "addi sp, %[spad], 0\n\t" \
// : [ dest ] "=r"(stackLoc) \
// : [ spad ] "r"(spTop))
// 
// ->
//  11948:	00010993          	mv	s3,sp
//  1194c:	00098113          	mv	sp,s3
//
// should be more like
//  11958:	00010b13          	mv	s6,sp
//  1195c:	00050113          	mv	sp,a0
// do weird sequence to resolve

#define MOVE_STACK_ONTO_SCRATCHPAD()          \
  unsigned long long *spTop = getSpTop(ptid); \
  spTop -= 35;                                \
  unsigned long long stackLoc;                \
  unsigned long long temp;                    \
  _Pragma("GCC unroll(35)")                   \
  for(int i=0;i<35;i++){                      \
    asm volatile("ld t0, %[id](sp)\n\t"       \
                "sd t0, %[id](%[spad])\n\t"   \
                : "=r"(temp)                  \
                : [id] "i"(i*8), [spad] "r"(spTop) : "memory"); \
  }                                                   \
  asm volatile (                                      \
      "addi t0, sp, 0\n\t"                            \
      "addi sp, %[spad], 0\n\t"                       \
      "addi %[dest], t0, 0\n\t"                       \
      : [ dest ] "=r"(stackLoc)                       \
      : [ spad ] "r"(spTop))

#define RECOVER_DRAM_STACK() \
  asm volatile(              \
      "addi sp, %[stackTop], 0\n\t" ::[stackTop] "r"(stackLoc) : "memory")

#endif

