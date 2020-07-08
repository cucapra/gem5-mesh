#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <math.h>

#include "spad.h"
#include "pthread_launch.h"
#include "gemm.h"

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
  int m = 4;
  int n = 4;
  int t = 4;
  
  // parse positional arguments
  if (argc > 1)
    m = atoi(argv[1]);
  if (argc > 2)
    n = atoi(argv[2]);
  if (argc > 3)
    t = atoi(argv[3]);
  
  printf("Problem size is (x,y) A: %d,%d x B: %d,%d -> C: %d,%d\n", t, m, n, t, n, m);

  /*--------------------------------------------------------------------
  * Data initialization
  *-------------------------------------------------------------------*/
 
  size_t sizeA = t * m;
  size_t sizeB = n * t;
  size_t sizeC = n * m;
  float *a = (float*)malloc(sizeof(float) * sizeA);
  float *b = (float*)malloc(sizeof(float) * sizeB);
  float *c = (float*)malloc(sizeof(float) * sizeC);
  
  for (int i = 0; i < sizeA; i++)
    a[i] = 3;
  for (int i = 0; i < sizeB; i++)
    b[i] = 2;
  for (int i = 0; i < sizeC; i++)
    c[i] = 0;
  
  // figure out good tile size for the architecture
  // i.e. the 2d tiles for the three matrices should fit into scratchpad
  const int num_mat = 3;
  const int blk_dim = sqrt((float)(getSpadNumBytes() / sizeof(float)) / (float)num_mat);

  /*--------------------------------------------------------------------
  * Pack argument for kernel
  *-------------------------------------------------------------------*/  

  // initialize the arguments to send to each device core
  Kern_Args **kern_args = (Kern_Args**)malloc(sizeof(Kern_Args*) * num_cores);

  for (int y = 0; y < cores_y; y++) {
    for (int x = 0; x < cores_x; x++){
      int i = x + y * cores_x;
      
      #ifdef _USE_SCRATCHPAD
      //kern_args[i] = construct_args(sp_a[i], sp_b[i], sp_c[i], size, x, y, cores_x, cores_y);
      #else
      kern_args[i] = construct_args(a, b, c, m, n, t, blk_dim, x, y, cores_x, cores_y);
      #endif
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
  
  for (int i = 0; i < sizeC; i++) {
    //printf("%f\n", c[i]);
    if (c[i] != 6 * t) {
      printf("[[FAIL]]\n");
      return 1;
    }
  }
  
  free(a);
  free(b);
  free(c);
  
  printf("[[SUCCESS]]\n");
  
  
  return 0;
}
