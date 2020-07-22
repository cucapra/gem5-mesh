#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>

#include "spad.h"
#include "pthread_launch.h"
#include "fdtd2d.h"
#include "util.h"

// checker from polybench. single core implementation
void fdtd(DTYPE* _fict_, DTYPE* ex, DTYPE* ey, DTYPE* hz, int NY, int NX, int tmax)
{
	int t, i, j;
	
	for (t=0; t < tmax; t++)  
	{
		for (j=0; j < NY; j++)
		{
			ey[0*NY + j] = _fict_[t];
		}
	
		for (i = 1; i < NX; i++)
		{
       		for (j = 0; j < NY; j++)
			{
       			ey[i*NY + j] = ey[i*NY + j] - 0.5*(hz[i*NY + j] - hz[(i-1)*NY + j]);
        		}
		}

		for (i = 0; i < NX; i++)
		{
       		for (j = 1; j < NY; j++)
			{
				ex[i*(NY+1) + j] = ex[i*(NY+1) + j] - 0.5*(hz[i*NY + j] - hz[i*NY + (j-1)]);
			}
		}

		for (i = 0; i < NX; i++)
		{
			for (j = 0; j < NY; j++)
			{
				hz[i*NY + j] = hz[i*NY + j] - 0.7*(ex[i*(NY+1) + (j+1)] - ex[i*(NY+1) + j] + ey[(i+1)*NY + j] - ey[i*NY + j]);
			}
		}
	}
}


void init_arrays(DTYPE* _fict_, DTYPE* ex, DTYPE* ey, DTYPE* hz, int NY, int NX, int tmax)
{
	int i, j;

  	for (i = 0; i < tmax; i++)
	{
		_fict_[i] = (DTYPE) i;
	}
	
	for (i = 0; i < NX; i++)
	{
		for (j = 0; j < NY; j++)
		{
			ex[i*NY + j] = ((DTYPE) i*(j+1) + 1) / NX;
			ey[i*NY + j] = ((DTYPE) (i-1)*(j+2) + 2) / NX;
			hz[i*NY + j] = ((DTYPE) (i-9)*(j+4) + 3) / NX;
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
