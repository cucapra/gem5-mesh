#include "template_kernel.h"

// #define SCALAR_CORE
// #define VECTOR_CORE

void template_vec(int mask)
{

  VECTOR_EPOCH(mask);

  #ifdef SCALAR_CORE
  //scalar core code

  //issue stack end portions of vector cores
  ISSUE_VINST(stack_end);
  // devec with unique tag
  DEVEC(devec_0);
  #elif defined VECTOR_CORE

  //vector core code
  #endif

  //fence for all cores to ensure memory operations have completed
  asm volatile("fence\n\t");

  return;

  #ifdef SCALAR_CORE
  //labels for vissue
  stack_end:
    asm("nop");

  return;
  #endif

}