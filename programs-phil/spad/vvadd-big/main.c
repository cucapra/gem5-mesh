#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>

#include "spad.h"
#include "pthread_launch.h"
#include "vvadd.h"

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
  int size = 1;
  
  // parse positional arguments
  if (argc > 1) {
    size = atoi(argv[1]);
  }
  
  printf("Vector size is %d\n", size);

  /*--------------------------------------------------------------------
  * Data initialization
  *-------------------------------------------------------------------*/
 
  size_t arrSize = sizeof(float) * size;
  float *a = (float*)malloc(arrSize);
  float *b = (float*)malloc(arrSize);
  float *c = (float*)malloc(arrSize);
  
  for (int i = 0; i < size; i++) {
    a[i] = i;
    b[i] = i;
    c[i] = 0;
  }
  
  // if we're going to be using scratchpads, simply copy the data from
  // global to scratchpads before we start running the kernel
  #ifdef _USE_SCRATCHPAD
  float **sp_a; float **sp_b; float **sp_c;
  sp_a = (float**)malloc(sizeof(float*) * num_cores);
  sp_b = (float**)malloc(sizeof(float*) * num_cores);
  sp_c = (float**)malloc(sizeof(float*) * num_cores);
  for (int i = 0; i < num_cores; i++) {
    float *sp = (float*)getSpAddr(i, 0);
    
    sp_a[i] = &(sp[0]);
    sp_b[i] = &(sp[size]);
    sp_c[i] = &(sp[2 * size]);
    
    // copy same data to each scratchpad
    for (int j = 0; j < size; j++) {
        sp_a[i][j] = a[j];
        sp_b[i][j] = b[j];
        sp_c[i][j] = c[j];
    }
  }
  #endif

  /*--------------------------------------------------------------------
  * Pack argument for kernel
  *-------------------------------------------------------------------*/  

  // initialize the arguments to send to each device core
  Kern_Args **kern_args = (Kern_Args**)malloc(sizeof(Kern_Args*) * num_cores);

  for (int y = 0; y < cores_y; y++) {
    for (int x = 0; x < cores_x; x++){
      int i = x + y * cores_x;
      
      #ifdef _USE_SCRATCHPAD
      kern_args[i] = construct_args(sp_a[i], sp_b[i], sp_c[i], size, x, y, cores_x, cores_y);
      #else
      kern_args[i] = construct_args(a, b, c, size, x, y, cores_x, cores_y);
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
  
  #ifdef _USE_SCRATCHPAD
  for (int i = 0; i < num_cores; i++) {
    for (int j = 0; j < size; j++) {
      //printf("%f\n", sp_c[i][j]);
      if (sp_c[i][j] != 2 * j) {
        printf("[[FAIL]]\n");
        return 1;
      }
    }
  }
  free(sp_a);
  free(sp_b);
  free(sp_c);
  #else
  for (int i = 0; i < size; i++) {
    //printf("%f\n", c[i]);
    if (c[i] != 2 * i) {
      printf("[[FAIL]]\n");
      return 1;
    }
  }
  #endif
  
  free(a);
  free(b);
  free(c);
  
  printf("[[SUCCESS]]\n");
  
  
  return 0;
}
