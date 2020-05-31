#include "atax_kernel.h"

// #define SCALAR_CORE
// #define VECTOR_CORE

void atax_vec(int mask, DTYPE *a, DTYPE *_x, DTYPE *_y_partial, DTYPE *ax, int nx, int ny,
      int nx_start, int nx_end, int vtid)
{

  VECTOR_EPOCH(mask);


}