#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>

#include "spad.h"
#include "pthread_launch.h"
#include "vvadd.h"
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
  int size = 1024;
  
  // parse positional arguments
  if (argc > 1) {
    size = atoi(argv[1]);
  }
  
  printf("Vector size is %d. Num cores is %d\n", size, num_cores);

  /*--------------------------------------------------------------------
  * Data initialization
  *-------------------------------------------------------------------*/

  DTYPE *a_ptr, *b_ptr, *c_ptr;
  DTYPE *a = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), size, (void**)&a_ptr);
  DTYPE *b = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), size, (void**)&b_ptr);
  DTYPE *c = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), size, (void**)&c_ptr);

  for (int i = 0; i < size; i++) {
    a[i] = i + 1;
    b[i] = i + 1;
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
      kern_args[i] = construct_args(a, b, c, size, x, y, cores_x, cores_y);
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
  // printf("Checking results\n");
  for (int i = 0; i < size; i++) {
    // printf("%d\n", c[i]);
    if (c[i] != 2 * ( i + 1 )) {
      printf("%d %d\n", i, c[i]);
      printf("[[FAIL]]\n");
      return 1;
    }
  }
  
  free(a_ptr);
  free(b_ptr);
  free(c_ptr);
  
  printf("[[SUCCESS]]\n");
  
  
  return 0;
}
