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
	for (j = 0; j < M; j++)
	{
		mean[j] = 0.0;
		for (i = 0; i < N; i++)
		{
        		mean[j] += data[i*M + j];
		}
		mean[j] /= FLOAT_N;
	}

  	/* Center the column vectors. */
	for (i = 0; i < N; i++)
	{
		for (j = 0; j < M; j++)
		{
			data[i*M + j] -= mean[j];
		}
	}

  	/* Calculate the m * m covariance matrix. */
	for (j1 = 0; j1 < M; j1++)
	{
		for (j2 = j1; j2 < M; j2++)
     		{
       		symmat[j1*M + j2] = 0.0;
			for (i = 0; i < N; i++)
			{
				symmat[j1*M + j2] += data[i*M + j1] * data[i*M + j2];
			}
        		symmat[j2*M + j1] = symmat[j1*M + j2];
      		}
	}
}

void init_arrays(DTYPE* data, int N, int M)
{
	int i, j;

	for (i = 0; i < M; i++)
	{
		for (j = 0; j < N; j++)
		{
			data[i*N + j] = ((DTYPE) (i+2)*(j+1)) / M;
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

  DTYPE *mean_ptr, *data_ptr, *symmat_ptr, *dataT_ptr;
  DTYPE *data   = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), M*N, (void**)&data_ptr);
  DTYPE *dataT  = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), M*N, (void**)&dataT_ptr);
  DTYPE *symmat = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), M*N, (void**)&symmat_ptr);
  DTYPE *mean   = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), M  ,       (void**)&mean_ptr);

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
      kern_args[i] = construct_args(data, dataT, mean, symmat, N, M, x, y, cores_x, cores_y);
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
    free(dataT_ptr);
    return 0;
  }

  printf("Checking results\n");

  // compare with results from a sequential version
  DTYPE *data_exp   = (DTYPE*)malloc(sizeof(DTYPE) * M*N);
  DTYPE *symmat_exp = (DTYPE*)malloc(sizeof(DTYPE) * M*N);
  DTYPE *mean_exp   = (DTYPE*)malloc(sizeof(DTYPE) * M  );

  init_arrays(data_exp, N, M);

  covariance(data_exp, symmat_exp, mean_exp, N, M);

  // for (int i = 1; i < (M+1); i++) {
  //   for (int j = 1; j < (N+1); j++) {
  //     int idx = i*(N+1) + j;
  //     printf("%f ", dataT[idx]);
  //   }
  //   printf("\n");
  // }

  // printf("-------\n");

  // for (int i = 1; i < (M+1); i++) {
  //   for (int j = 1; j < (N+1); j++) {
  //     int idx = i*(N+1) + j;
  //     printf("%f ", data_exp[idx]);
  //   }
  //   printf("\n");
  // }

  for (int i = 0; i < M; i++) {
    for (int j = 0; j < N; j++) {
      int idx = i*N + j;
      // if (!float_compare(data[idx], data_exp[idx], 0.001f)) {
      if (!float_compare(symmat[idx], symmat_exp[idx], 0.01f)) {
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
  free(dataT_ptr);
  free(symmat_ptr);
  free(mean_ptr);
  
  printf("[[SUCCESS]]\n");
  
  return 0;
}
