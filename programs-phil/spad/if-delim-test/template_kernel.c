#include "template_kernel.h"

void tril_template_vec(int mask) {
#ifdef SCALAR_CORE
  VECTOR_EPOCH(mask);
#endif

  // The vector block for initialization.
#ifdef SCALAR_CORE
  ISSUE_VINST(init_label);
#elif defined VECTOR_CORE
  asm("trillium vissue_delim until_next vector_init");
  volatile int myResult = 0;
#endif

  // A loop that issues vector blocks. We use a "black hole" loop to replace
  // the proper loop on the vector cores.
#ifdef SCALAR_CORE
  for (int i = 0; i < 100; ++i) {
#elif defined VECTOR_CORE
  volatile int BH;
  while (BH) {
#endif
#ifdef SCALAR_CORE
    ISSUE_VINST(if_block_label);
#elif defined VECTOR_CORE
    asm("trillium vissue_delim if_begin if_block");
    if (BH) {
      myResult += 42;
    } else {
      myResult += 24;
    }
    asm("trillium vissue_delim if_end");
#endif
  }

  // Clean up on the vector cores.
#ifdef SCALAR_CORE
  ISSUE_VINST(vector_return_label);
#elif defined VECTOR_CORE
  asm("trillium vissue_delim return vector_return");
  return;
#endif

  // Disband the vector group.
#ifdef SCALAR_CORE
  DEVEC(devec_0);
  asm volatile("fence\n\t");
  asm("trillium vissue_delim return scalar_return");  // XXX is this real???
  return;
#endif

  // Glue points!
#ifdef SCALAR_CORE
init_label:
  asm("trillium glue_point vector_init");
if_block_label:
  asm("trillium glue_point if_block");
vector_return_label:
  asm("trillium glue_point vector_return");
#endif
}
