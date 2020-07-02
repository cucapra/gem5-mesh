#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <math.h>

#include "spad.h"
#include "pthread_launch.h"
#include "if_test.h"


int main(int argc, char *argv[])
{

  /*--------------------------------------------------------------------
   * Setup scratchpads
   *------------------------------------------------------------------*/

  printf("starting\n");

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
  int n = 1000;

  // parse positional arguments
  if (argc > 1)
    n = atoi(argv[1]);

  printf("Problem size is %d\n", n);

  /*--------------------------------------------------------------------
  * Data initialization
  *-------------------------------------------------------------------*/

  DTYPE *a_ptr;
  DTYPE *a = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), n, (void**)&a_ptr);
  // DTYPE *a = (DTYPE *)malloc(sizeof(DTYPE) * n);

  // initilaize arrays
  for(int i=0; i<n; i++){
    a[i] = i+1;
  }

  /*--------------------------------------------------------------------
  * Pack argument for kernel
  *-------------------------------------------------------------------*/

  // initialize the arguments to send to each device core
  Kern_Args **kern_args = (Kern_Args **)malloc(sizeof(Kern_Args *) * num_cores);

  for (int y = 0; y < cores_y; y++)
  {
    for (int x = 0; x < cores_x; x++)
    {
      int i = x + y * cores_x;
      kern_args[i] = construct_args(a, n, x, y, cores_x, cores_y);
    }
  }

  /*--------------------------------------------------------------------
  * Run the kernel
  *-------------------------------------------------------------------*/

  printf("Begin kernel on %d cores\n", num_cores);
  printf("Cores x:%d Cores y:%d\n", cores_x, cores_y);
  launch_kernel(pthread_kernel, (void **)kern_args, cores_x, cores_y);

/*--------------------------------------------------------------------
  * Check result and cleanup data
  *-------------------------------------------------------------------*/

  int incorrect = 0;
  for (int i = 0; i < 100; ++i) {
    int expected = i == 6 ? 42 : i + 1;
    if (a[i] != expected) {
      printf("a[%i] == %i; expected %i\n", i, a[i], expected);
      incorrect = 1;
    }
  }

  if (incorrect){
    printf("[[FAIL]]\n");
    return 1;
  }

  free(a_ptr);
  printf("[[SUCCESS]]\n");
  return 0;
}
