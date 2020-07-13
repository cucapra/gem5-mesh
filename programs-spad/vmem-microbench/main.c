#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <math.h>

#include "spad.h"
#include "pthread_launch.h"
#include "vmem.h"

//#define RANDOM_DIST 1

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
  int n = 16;
  
  // parse positional arguments
  if (argc > 1)
    n = atoi(argv[1]);
  
  printf("Problem size is %d\n", n);

  /*--------------------------------------------------------------------
  * Data initialization
  *-------------------------------------------------------------------*/
 
  size_t sizeA = n;
  size_t sizeB = n;
  size_t sizeC = n;
  size_t sizeD = n;
  int *a = (int*)malloc(sizeof(int) * sizeA);
  int *b = (int*)malloc(sizeof(int) * sizeB);
  int *c = (int*)malloc(sizeof(int) * sizeC);
  int *d = (int*)malloc(sizeof(int) * sizeD);
  
  // generate a synthetic distribution to branch based on
  #ifdef RANDOM_DIST
  float fraction = 0.8f;
  srand(140129302);
  #endif
  
  for (int i = 0; i < sizeA; i++)
    a[i] = i + 1;
  for (int i = 0; i < sizeB; i++)
    b[i] = 0;
  for (int i = 0; i < sizeC; i++)
    c[i] = 0;
  for (int i = 0; i < sizeD; i++)
    d[i] = 0;
  
  /*--------------------------------------------------------------------
  * Pack argument for kernel
  *-------------------------------------------------------------------*/  

  // initialize the arguments to send to each device core
  Kern_Args **kern_args = (Kern_Args**)malloc(sizeof(Kern_Args*) * num_cores);

  for (int y = 0; y < cores_y; y++) {
    for (int x = 0; x < cores_x; x++){
      int i = x + y * cores_x;
      
      kern_args[i] = construct_args(a, b, c, d, n, x, y, cores_x, cores_y);
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

  for (int i = 0; i < 4; i++) {
    printf("%d %d %d\n", b[i], c[i], d[i]);
    
    int expD = 4 + 4 * i + 6 * num_cores;
    int expC = i != 1 ? i + 1 + num_cores : i + 1;
    int expB = i < 2 ? (i + 1) : -1;
    if ((d[i] != expD) ||
        (c[i] != expC) ||
        (b[i] != expB)
    ) {
      
      printf("[[FAIL]]\n");
      //return 1;
    }
  }
  
  free(a);
  free(b);
  free(c);
  free(d);
  
  printf("[[SUCCESS]]\n");
  
  
  return 0;
}
