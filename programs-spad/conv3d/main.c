#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>

#include "spad.h"
#include "pthread_launch.h"
#include "conv3d.h"
#include "util.h"

// serial version for verification
void conv3D(DTYPE* A, DTYPE* B, int NI, int NJ, int NK)
{
	int i, j, k;
  DEF_WEIGHTS();

	for (i = 1; i < NI - 1; ++i) // 0
	{
		for (j = 1; j < NJ - 1; ++j) // 1
		{
			for (k = 1; k < NK -1; ++k) // 2
			{
				//printf("i:%d\nj:%d\nk:%d\n", i, j, k);
				B[i*(NK * NJ) + j*NK + k] = c11 * A[(i - 1)*(NK * NJ) + (j - 1)*NK + (k - 1)]  +  c13 * A[(i + 1)*(NK * NJ) + (j - 1)*NK + (k - 1)]
					     +   c21 * A[(i - 1)*(NK * NJ) + (j - 1)*NK + (k - 1)]  +  c23 * A[(i + 1)*(NK * NJ) + (j - 1)*NK + (k - 1)]
					     +   c31 * A[(i - 1)*(NK * NJ) + (j - 1)*NK + (k - 1)]  +  c33 * A[(i + 1)*(NK * NJ) + (j - 1)*NK + (k - 1)]
					     +   c12 * A[(i + 0)*(NK * NJ) + (j - 1)*NK + (k + 0)]  +  c22 * A[(i + 0)*(NK * NJ) + (j + 0)*NK + (k + 0)]   
					     +   c32 * A[(i + 0)*(NK * NJ) + (j + 1)*NK + (k + 0)]  +  c11 * A[(i - 1)*(NK * NJ) + (j - 1)*NK + (k + 1)]  
					     +   c13 * A[(i + 1)*(NK * NJ) + (j - 1)*NK + (k + 1)]  +  c21 * A[(i - 1)*(NK * NJ) + (j + 0)*NK + (k + 1)]  
					     +   c23 * A[(i + 1)*(NK * NJ) + (j + 0)*NK + (k + 1)]  +  c31 * A[(i - 1)*(NK * NJ) + (j + 1)*NK + (k + 1)]  
					     +   c33 * A[(i + 1)*(NK * NJ) + (j + 1)*NK + (k + 1)];
			}
		}
	}
}

void init(DTYPE* A, int NI, int NJ, int NK)
{
	int i, j, k;

	for (i = 0; i < NI; ++i)
    	{
		for (j = 0; j < NJ; ++j)
		{
			for (k = 0; k < NK; ++k)
			{
				A[i*(NK * NJ) + j*NK + k] = i % 12 + 2 * (j % 7) + 3 * (k % 13);
			}
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
  int NI = 6;
  int NJ = 6;
  int NK = 6;

  int skip_check = 0;
  
  // parse positional arguments (X Y)
  if (argc > 1) {
    NI = atoi(argv[1]);
  }
  if (argc > 2) {
    NJ = atoi(argv[2]);
  }
  if (argc > 3) {
    NK = atoi(argv[3]);
  }
  if (argc > 4) {
    skip_check = atoi(argv[4]);
  }

  printf("Conv3D %dx%d on %dx%dx%d image. Num cores is %d\n", FILTER_DIM, FILTER_DIM, NI, NJ, NK, num_cores);

  /*--------------------------------------------------------------------
  * Data initialization
  *-------------------------------------------------------------------*/

  DTYPE *a_ptr, *b_ptr;
  DTYPE *a = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), NI * NJ * NK, (void**)&a_ptr);
  DTYPE *b = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), NI * NJ * NK, (void**)&b_ptr);

  init(a, NI, NJ, NK);
  
  /*--------------------------------------------------------------------
  * Pack argument for kernel
  *-------------------------------------------------------------------*/  

  // initialize the arguments to send to each device core
  Kern_Args **kern_args = (Kern_Args**)malloc(sizeof(Kern_Args*) * num_cores);

  for (int y = 0; y < cores_y; y++) {
    for (int x = 0; x < cores_x; x++){
      int i = x + y * cores_x;
      kern_args[i] = construct_args(a, b, NI, NJ, NK, x, y, cores_x, cores_y);
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
  DTYPE *a_exp = (DTYPE*)malloc(sizeof(DTYPE) * NI * NJ * NK);
  DTYPE *b_exp = (DTYPE*)malloc(sizeof(DTYPE) * NI * NJ * NK);
  init(a_exp, NI, NJ, NK);
  conv3D(a_exp, b_exp, NI, NJ, NK);

  for (int i = 1; i < NI - 1; i++) {
    for (int j = 1; j < NJ - 1; j++) {
      for (int k = 1; k < NK - 1; k++) {
        int idx = IDX(i, j, k, NJ, NK);
        if (!float_compare(b[idx], b_exp[idx], 0.0001f)) {
          printf("%f != %f @ %d-%d-%d\n", b[idx], b_exp[idx], i, j, k);
          printf("[[FAIL]]\n");
          return 1;
        }
      }
    }
  }

  free(a_ptr);
  free(b_ptr);
  free(a_exp);
  free(b_exp);
  
  printf("[[SUCCESS]]\n");
  
  
  return 0;
}
