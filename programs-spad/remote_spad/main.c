#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <math.h>

#include "spad.h"
#include "pthread_launch.h"
#include "rem_sp.h"


void fill_array(DTYPE* a, int n){
  srand(0);
  for (int i = 0; i < n; i++){
    a[i] = rand()%10;
  }
}

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
  int n = 512;
  
  // parse positional arguments
  if (argc > 1)
    n = atoi(argv[1]);
  
  printf("Problem size is %d \n", n);

  /*--------------------------------------------------------------------
  * Data initialization
  *-------------------------------------------------------------------*/
 
  DTYPE *a = (DTYPE*)malloc(sizeof(DTYPE) * n);
  DTYPE *c = (DTYPE*)malloc(sizeof(DTYPE) * n);
  
  fill_array(a,n);
  
  for (int i = 0; i < n; i++)
    c[i] = 0;

  
  // figure out good tile size for the architecture
  // i.e. the 2d tiles for the three matrices should fit into scratchpad
  const int num_mat = 3;
  //const int blk_dim = sqrt((DTYPE)(getSpadNumBytes() / sizeof(DTYPE)) / (DTYPE)num_mat);

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
      kern_args[i] = construct_args(a, c,  n, x, y, cores_x, cores_y);
      #endif
    }  
  }

  /*--------------------------------------------------------------------
  * Run the kernel
  *-------------------------------------------------------------------*/
  
  printf("Begin kernel on %d cores\n", num_cores);
  printf("Cores x:%d Cores y:%d\n",cores_x,cores_y);
  launch_kernel(pthread_kernel, (void**)kern_args, cores_x, cores_y);
  
  /*--------------------------------------------------------------------
  * Check result and cleanup data
  *-------------------------------------------------------------------*/
  
  
  /*
  for (int i = 0; i < n; i++) {
    //printf("%f\n", c[i]);
    if (c[i] != a[i]* 5) {
      printf("[[FAIL]]\n");
      return 1;
    }
  }
  */
  
  free(a);
  free(c);
  printf("[[SUCCESS]]\n");
  return 0;
}
