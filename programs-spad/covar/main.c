#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>

#include "spad.h"
#include "pthread_launch.h"
#include "covar.h"
#include "util.h"

// checker from polybench. single core implementation
void covariance(DTYPE* data, DTYPE* symmat, DTYPE* mean, int N, int M)
{
	int i, j, j1,j2;

  	/* Determine mean of column vectors of input data matrix */
	for (j = 1; j < (M+1); j++)
	{
		mean[j] = 0.0;
		for (i = 1; i < (N+1); i++)
		{
        		mean[j] += data[i*(M+1) + j];
		}
		mean[j] /= FLOAT_N;
	}

  	/* Center the column vectors. */
	for (i = 1; i < (N+1); i++)
	{
		for (j = 1; j < (M+1); j++)
		{
			data[i*(M+1) + j] -= mean[j];
		}
	}

  	/* Calculate the m * m covariance matrix. */
	for (j1 = 1; j1 < (M+1); j1++)
	{
		for (j2 = j1; j2 < (M+1); j2++)
     		{
       		symmat[j1*(M+1) + j2] = 0.0;
			for (i = 1; i < N+1; i++)
			{
				symmat[j1*(M+1) + j2] += data[i*(M+1) + j1] * data[i*(M+1) + j2];
			}
        		symmat[j2*(M+1) + j1] = symmat[j1*(M+1) + j2];
      		}
	}
}

void init_arrays(DTYPE* data, int N, int M)
{
	int i, j;

	for (i = 1; i < (M+1); i++)
	{
		for (j = 1; j < (N+1); j++)
		{
			data[i*(N+1) + j] = ((DTYPE) i*j) / M;
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
  
  int N = 12;
  int M = N;

  // whether to skip verification or not
  int skip_check = 0;

  if (argc > 1) {
    N = atoi(argv[1]);
  }
  if (argc > 2) {
    M = atoi(argv[2]);
  }
  if (argc > 3) {
    skip_check = atoi(argv[3]);
  }
  
  printf("Covariance on %d x %d matrix. Num cores is %d\n", 
    N, M, num_cores);

  /*--------------------------------------------------------------------
  * Data initialization
  *-------------------------------------------------------------------*/

  DTYPE *mean_ptr, *data_ptr, *symmat_ptr;
  DTYPE *data   = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), (M+1)*(N+1), (void**)&data_ptr);
  DTYPE *symmat = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), (M+1)*(N+1), (void**)&symmat_ptr);
  DTYPE *mean   = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), (M+1),       (void**)&mean_ptr);

  // initial data (other arrays initialized to zero within the kernel)
  init_arrays(data, N, M);
  
  /*--------------------------------------------------------------------
  * Pack argument for kernel
  *-------------------------------------------------------------------*/  

  // initialize the arguments to send to each device core
  Kern_Args **kern_args = (Kern_Args**)malloc(sizeof(Kern_Args*) * num_cores);

  for (int y = 0; y < cores_y; y++) {
    for (int x = 0; x < cores_x; x++){
      int i = x + y * cores_x;
      kern_args[i] = construct_args(data, mean, symmat, N, M, x, y, cores_x, cores_y);
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
    free(mean_ptr);
    free(symmat_ptr);
    free(data_ptr);
    return 0;
  }

  printf("Checking results\n");

  // compare with results from a sequential version
  DTYPE *data_exp   = (DTYPE*)malloc(sizeof(DTYPE) * (M+1)*(N+1));
  DTYPE *symmat_exp = (DTYPE*)malloc(sizeof(DTYPE) * (M+1)*(N+1));
  DTYPE *mean_exp   = (DTYPE*)malloc(sizeof(DTYPE) * (M+1));

  init_arrays(data_exp, N, M);

  covariance(data_exp, symmat_exp, mean_exp, N, M);

  for (int i = 1; i < (M+1); i++) {
    for (int j = 1; j < (N+1); j++) {
      int idx = i*(N+1) + j;
      // if (symmat[idx] != symmat_exp[idx]) {
      if (!float_compare(symmat[idx], symmat_exp[idx], symmat_exp[idx] * 0.000001f)) {
        printf("i %d j %d idx %d | %f != %f\n", i, j, idx, symmat[idx], symmat_exp[idx]);
        printf("[[FAIL]]\n");
        return 1;      
      }
      // else {
      //   printf("i %d j %d idx %d | %f == %f\n", i, j, idx, symmat[idx], symmat_exp[idx]);
      // }
    }
  }

  free(data_exp);
  free(symmat_exp);
  free(mean_exp);
  
  free(data_ptr);
  free(symmat_ptr);
  free(mean_ptr);
  
  printf("[[SUCCESS]]\n");
  
  return 0;
}
