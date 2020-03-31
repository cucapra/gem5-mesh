#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>

#include "spad.h"
#include "pthread_launch.h"
#include "stencil.h"
// #include "../../common/bind_defs.h"

int main(int argc, char *argv[]) {
  
  /*--------------------------------------------------------------------
   * Setup scratchpads
   *------------------------------------------------------------------*/ 
  
  initScratchpads();

  /*--------------------------------------------------------------------
  * Get info about manycore
  *-------------------------------------------------------------------*/  
  
  int cores_x, cores_y;
  int num_cores = get_dimensions(&cores_x, &cores_y);

  /*--------------------------------------------------------------------
  * Put the command line arguments into variables
  *-------------------------------------------------------------------*/
  
  // default values
  int nrows = 3 + (FILTER_DIM - 1); // single row
  int ncols = 16 /*+ (FILTER_DIM - 1)*/;
  
  // parse positional arguments (X Y)
  if (argc > 1) {
    ncols = atoi(argv[1]);
  }
  if (argc > 2) {
    nrows = atoi(argv[2]);
  }
  
  printf("Stencil %dx%d on %dx%d image. Num cores is %d\n", FILTER_DIM, FILTER_DIM, ncols, nrows, num_cores);

  /*--------------------------------------------------------------------
  * Data initialization
  *-------------------------------------------------------------------*/

  int boundOffset = (FILTER_DIM - 1);
  DTYPE *a_ptr, *b_ptr, *c_ptr;
  DTYPE *a = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), nrows * ncols, (void**)&a_ptr);
  DTYPE *b = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), FILTER_DIM * FILTER_DIM, (void**)&b_ptr);
  DTYPE *c = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), (nrows - boundOffset) * (ncols /*- boundOffset*/), (void**)&c_ptr);

  // image
  for (int i = 0; i < nrows * ncols; i++) {
    a[i] = i + 1;
  }

  // filter
  // 1 2 3
  // 4 5 6
  // 7 8 9
  for (int i = 0; i < FILTER_DIM * FILTER_DIM; i++) {
    b[i] = i + 1;
  }

  // result
  for (int i = 0; i < (nrows - boundOffset) * (ncols /*- boundOffset*/); i++) {
    c[i] = 0;
  }
  
  /*--------------------------------------------------------------------
  * Pack argument for kernel
  *-------------------------------------------------------------------*/  

  // initialize the arguments to send to each device core
  Kern_Args **kern_args = (Kern_Args**)malloc(sizeof(Kern_Args*) * num_cores);

  for (int y = 0; y < cores_y; y++) {
    for (int x = 0; x < cores_x; x++){
      int i = x + y * cores_x; 
      kern_args[i] = construct_args(a, b, c, nrows, ncols, x, y, cores_x, cores_y);
    }  
  }

  /*--------------------------------------------------------------------
  * Run the kernel
  *-------------------------------------------------------------------*/
  
  printf("Begin kernel on %d cores\n", num_cores);
  launch_kernel(pthread_kernel, (void**)kern_args, cores_x, cores_y);
  
  /*--------------------------------------------------------------------
  * Check result and cleanup data
  *-------------------------------------------------------------------*/
  
  for (int row = 0; row < nrows - boundOffset; row++) {
    for (int col = 0; col < ncols - boundOffset; col++) {
      int cexp = 0;
      for (int k1 = 0; k1 < FILTER_DIM; k1++) {
        for (int k2 = 0; k2 < FILTER_DIM; k2++) {
          int aIdx = (row + k1) * ncols + (col + k2);
          int bIdx = k1 * FILTER_DIM + k2;
          cexp += b[bIdx] * a[aIdx];
        }
      }
      if (c[row * (ncols /*- boundOffset*/) + col] != cexp) {
        printf("%d != %d @ row %d cold %d\n", c[row * (ncols /*- boundOffset*/) + col], cexp, row, col);
        printf("[[FAIL]]\n");
        return 1;
      }
      // printf("%d == %d @ row %d cold %d\n", c[row * (ncols /*- boundOffset*/) + col], cexp, row, col);
    }
  }
  
  free(a_ptr);
  free(b_ptr);
  free(c_ptr);
  
  printf("[[SUCCESS]]\n");
  
  
  return 0;
}
