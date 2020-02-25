#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <math.h>

#include "spad.h"
#include "pthread_launch.h"
#include "synth.h"

#define RANDOM_DIST 1

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
    
  // info for rng if using
  #ifdef RANDOM_DIST
  float fraction = 0.8f;
  if (argc > 2)
    fraction = atof(argv[2]);
  if (argc > 3)
    srand(atoi(argv[3]));
  else
    srand(140129302);
  #endif
  
  #ifdef RANDOM_DIST
  printf("Problem size is %d and shared branch fraction is %f\n", n, fraction);
  #endif

  /*--------------------------------------------------------------------
  * Data initialization
  *-------------------------------------------------------------------*/
 
  size_t sizeA = n;
  size_t sizeB = n;
  size_t sizeC = n;
  size_t sizeD = n;
  int *a = (int*)malloc(sizeof(int) * (sizeA + 64));
  int *b = (int*)malloc(sizeof(int) * (sizeB + 64));
  int *c = (int*)malloc(sizeof(int) * (sizeC + 64));
  int *d = (int*)malloc(sizeof(int) * (sizeD + 64));
  
  int *_a = a;
  int *_b = b;
  int *_c = c;
  int *_d = d;
  
  // align data to cache lines
  a = (int*)((unsigned long long)(a + 64) & ~((1ULL << 6) - 1));
  b = (int*)((unsigned long long)(b + 64) & ~((1ULL << 6) - 1));
  c = (int*)((unsigned long long)(c + 64) & ~((1ULL << 6) - 1));
  d = (int*)((unsigned long long)(d + 64) & ~((1ULL << 6) - 1));
  
  // generate a synthetic distribution to branch based on
  for (int i = 0; i < sizeA; i++) {
    #ifdef RANDOM_DIST
    float num = (float)rand() / (float)RAND_MAX;
    int val;
    if (num > fraction) val = 1;
    else val = 0;
    a[i] = val;
    #else
    if (i == 0 || i == 9)
      a[i] = 0;
    else
      a[i] = 1;
    #endif
    //printf("%d ", a[i]);
  }
  //printf("\n");
  
  for (int i = 0; i < sizeB; i++)
    b[i] = 2;
  for (int i = 0; i < sizeC; i++)
    c[i] = 0;
  for (int i = 0; i < sizeD; i++)
    d[i] = 3;
  
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
  
  /*for (int y = 0; y < cores_y; y++) {
    for (int x = 0; x < cores_x; x++){
      int i = x + y * cores_x;
      printf("kern_args[%d] = [", i);
      printf("%p, ", kern_args[i]->a);
      printf("%p, ", kern_args[i]->b);
      printf("%p, ", kern_args[i]->c);
      printf("%p, ", kern_args[i]->d);
      printf("%d, ", kern_args[i]->n);
      printf("%d, ", kern_args[i]->tid_x);
      printf("%d, ", kern_args[i]->tid_y);
      printf("%d, ", kern_args[i]->dim_x);
      printf("%d"  , kern_args[i]->dim_y);
      printf("]\n");
    }  
  }*/


  /*--------------------------------------------------------------------
  * Run the kernel
  *-------------------------------------------------------------------*/
  
  printf("Begin kernel on %d cores\n", num_cores);
  launch_kernel(pthread_kernel, (void**)kern_args, cores_x, cores_y);
  
  /*--------------------------------------------------------------------
  * Check result and cleanup data
  *-------------------------------------------------------------------*/
  for (int i = 0; i < sizeC; i++) {
    if (a[i] == 0) {
      if (c[i] != pow(2, 7)) {
        printf("[[FAIL]]\n");
        printf("i=%d c=%d exp=%d\n", i, c[i], (int)pow(2, 3));
        return 1;
      }
    }
    else if (a[i] == 1) {
      if (c[i] != pow(2, 9)) {
        printf("[[FAIL]]\n");
        return 1;
      }
    }
    else {
      printf("[[FAIL]]\n");
      return 1;
    }
  }
  
  free(_a);
  free(_b);
  free(_c);
  free(_d);

  printf("[[SUCCESS]]\n");
  
  
  return 0;
}
