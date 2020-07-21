#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>

#include "spad.h"
#include "pthread_launch.h"
#include "conv2d.h"
#include "util.h"

// serial version for verification
void conv2D(DTYPE* A, DTYPE* B, int NI, int NJ)
{
	int i, j;
  DEF_WEIGHTS();

	for (i = 1; i < NI - 1; ++i) // 0
	{
		for (j = 1; j < NJ - 1; ++j) // 1
		{
			B[i*NJ + j] = 
          c11 * A[(i - 1)*NJ + (j - 1)]  +  c12 * A[(i + 0)*NJ + (j - 1)]  +  c13 * A[(i + 1)*NJ + (j - 1)]
				+ c21 * A[(i - 1)*NJ + (j + 0)]  +  c22 * A[(i + 0)*NJ + (j + 0)]  +  c23 * A[(i + 1)*NJ + (j + 0)] 
				+ c31 * A[(i - 1)*NJ + (j + 1)]  +  c32 * A[(i + 0)*NJ + (j + 1)]  +  c33 * A[(i + 1)*NJ + (j + 1)];
		}
	}
}

void init(DTYPE* A, int NI, int NJ)
{
	int i, j;

	for (i = 0; i < NI; ++i)
  {
		for (j = 0; j < NJ; ++j)
		{
		  // A[i*NJ + j] = (float)rand()/RAND_MAX;
      A[i*NJ + j] = ((float)(i + 1) / 4.0f) * ((float)(j + 1) / 3.0f);
      // printf("%f\n", A[i*NJ+j]);
    }
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
  int nrows = 3;
  int ncols = 15;

  int skip_check = 0;
  
  // parse positional arguments (X Y)
  if (argc > 1) {
    ncols = atoi(argv[1]);
  }
  if (argc > 2) {
    nrows = atoi(argv[2]);
  }
  if (argc > 3) {
    skip_check = atoi(argv[3]);
  }

  printf("Conv2D %dx%d on %dx%d image. Num cores is %d\n", FILTER_DIM, FILTER_DIM, ncols, nrows, num_cores);

  /*--------------------------------------------------------------------
  * Data initialization
  *-------------------------------------------------------------------*/

  DTYPE *a_ptr, *b_ptr;
  DTYPE *a = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), nrows * ncols, (void**)&a_ptr);
  DTYPE *b = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), nrows * ncols, (void**)&b_ptr);

  init(a, nrows, ncols);
  
  /*--------------------------------------------------------------------
  * Pack argument for kernel
  *-------------------------------------------------------------------*/  

  // initialize the arguments to send to each device core
  Kern_Args **kern_args = (Kern_Args**)malloc(sizeof(Kern_Args*) * num_cores);

  for (int y = 0; y < cores_y; y++) {
    for (int x = 0; x < cores_x; x++){
      int i = x + y * cores_x;
      kern_args[i] = construct_args(a, b, nrows, ncols, x, y, cores_x, cores_y);
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
  
  if (skip_check) {
    printf("Skipping verification\n");
    printf("[[SUCCESS]]\n");
    free(a_ptr);
    free(b_ptr);
    return 0;
  }

  // verify
  DTYPE *a_exp = (DTYPE*)malloc(sizeof(DTYPE) * nrows * ncols);
  DTYPE *b_exp = (DTYPE*)malloc(sizeof(DTYPE) * nrows * ncols);
  init(a_exp, nrows, ncols);
  conv2D(a_exp, b_exp, nrows, ncols);


  for (int row = 1; row < nrows - 1; row++) {
    for (int col = 1; col < ncols - 1; col++) {
      int idx = row * ncols + col;
      // if (b[idx] != b_exp[idx]) {
      if (!float_compare(b[idx], b_exp[idx], 0.00001f)) {
        printf("%f != %f @ row %d cold %d\n", b[idx], b_exp[idx], row, col);
        printf("[[FAIL]]\n");
        return 1;
      }
      // else {
      //   printf("%f == %f @ row %d cold %d\n", b[idx], b_exp[idx], row, col);
      // }
    }
  }
  
  free(a_ptr);
  free(b_ptr);
  free(a_exp);
  free(b_exp);
  
  printf("[[SUCCESS]]\n");
  
  
  return 0;
}
