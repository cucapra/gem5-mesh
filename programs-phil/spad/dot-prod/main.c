#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>

#include "spad.h"
#include "pthread_launch.h"
#include "dot.h"

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
  
  #ifdef USE_VEC
  int len = 96;
  #else
  int len = 64;
  #endif

  if (argc > 1) {
    len = atoi(argv[1]);
  }
  
  printf("Dot product on len %d vectors. Num cores is %d\n", len, num_cores);

  /*--------------------------------------------------------------------
  * Data initialization
  *-------------------------------------------------------------------*/

  DTYPE *a_ptr, *b_ptr, *c_ptr;
  DTYPE *a = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), len, (void**)&a_ptr);
  DTYPE *b = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), len, (void**)&b_ptr);
  DTYPE *c = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), 1, (void**)&c_ptr);

  // vectors
  for (int i = 0; i < len; i++) {
    a[i] = i + 1;
    b[i] = i + 1;
  }

  // result
  c[0] = 0;
  
  /*--------------------------------------------------------------------
  * Pack argument for kernel
  *-------------------------------------------------------------------*/  

  // initialize the arguments to send to each device core
  Kern_Args **kern_args = (Kern_Args**)malloc(sizeof(Kern_Args*) * num_cores);

  for (int y = 0; y < cores_y; y++) {
    for (int x = 0; x < cores_x; x++){
      int i = x + y * cores_x;
      kern_args[i] = construct_args(a, b, c, len, x, y, cores_x, cores_y);
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
  
  // just one value at the end to check
  // doing the sum of n squares, which has closed form solution
  // sum{1_n}(k^2) = n * ( n + 1 ) * ( 2n + 1 ) / 6
  int n = len;
  int calc = c[0]; 
  int exp = ((n * (n + 1) * (2*n + 1)) / 6) * 6; // (done some comp 6 times)
  if (calc != exp) {
    printf("%d != %d\n", calc, exp);
    printf("[[FAIL]]\n");
    return 1;
  }
  
  free(a_ptr);
  free(b_ptr);
  free(c_ptr);
  
  printf("[[SUCCESS]]\n");
  
  return 0;
}
