#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>

#include "spad.h"
#include "pthread_launch.h"
#include "syr2k.h"

// checker from polybench. single core implementation
#include <math.h>

void init_arrays(DTYPE *A, DTYPE *B, DTYPE *C, int N, int M) {
  int i, j;
  
  for (i = 0; i < N; i++)
      {
        for (j = 0; j < N; j++)
    {
      C[i*N + j] = ((DTYPE) i*j + 2) / N;
    }
        
    for (j = 0; j < M; j++)
    {
        A[i*N + j] = ((DTYPE) i*j) / N;
        B[i*N + j] = ((DTYPE) i*j + 1) / N;
    }
      }
}


void syrk_seq(DTYPE* A, DTYPE* B, DTYPE* C, int N, int M)
{
	int i, j, k;
	
	/*  C := alpha*A*A' + beta*C */
	for (i = 0; i < N; i++)
	{
		for (j = 0; j < N; j++)
		{
			C[i*N + j] *= beta;
		}
	}
	
	for (i = 0; i < N; i++)
	{
		for (j = 0; j < N; j++)
		{
			for (k = 0; k < M; k++)
			{
			  C[i*N + j] += ALPHA * A[i*M + k] * B[j*M + k];
        C[i*N + j] += ALPHA * B[i*M + k] * A[j*M + k];	
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
  
  int N = 8;
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
  
  printf("SYR2K on %d x %d. Num cores is %d\n", 
    N, M, num_cores);

  /*--------------------------------------------------------------------
  * Data initialization
  *-------------------------------------------------------------------*/

  DTYPE *a_ptr, *b_ptr, *c_ptr;
  DTYPE *a = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), N*M, (void**)&a_ptr);
  DTYPE *b = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), N*M, (void**)&b_ptr);
  DTYPE *c = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), N*M, (void**)&c_ptr);

  // initial data
  init_arrays(a, b, c, N, M);
  
  /*--------------------------------------------------------------------
  * Pack argument for kernel
  *-------------------------------------------------------------------*/  

  // initialize the arguments to send to each device core
  Kern_Args **kern_args = (Kern_Args**)malloc(sizeof(Kern_Args*) * num_cores);

  for (int y = 0; y < cores_y; y++) {
    for (int x = 0; x < cores_x; x++){
      int i = x + y * cores_x;
      kern_args[i] = construct_args(a, b, c, N, M, x, y, cores_x, cores_y);
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
    free(a_ptr);
    free(b_ptr);
    free(c_ptr);
    return 0;
  }

  printf("Checking results\n");

  // compare with results from a sequential version
	DTYPE *a_exp = (DTYPE*)malloc(N*M*sizeof(DTYPE));
	DTYPE *b_exp = (DTYPE*)malloc(N*M*sizeof(DTYPE));
  DTYPE *c_exp = (DTYPE*)malloc(N*M*sizeof(DTYPE));

  init_arrays(a_exp, b_exp, c_exp, N, M);

  syrk_seq(a_exp, b_exp, c_exp, N, M);

  for (int i = 0; i < N; i++) {
    for (int j = 0; j < M; j++) {
      int idx = i * M + j;
      if (c[idx] != c_exp[idx]) {
        printf("i %d j %d idx %d | %f != %f\n", i, j, idx, c[idx], c_exp[idx]);
        printf("[[FAIL]]\n");
        return 1;
      }
    }
  }

  free(a_exp);
  free(b_exp);
  free(c_exp);
  
  free(a_ptr);
  free(b_ptr);
  free(c_ptr);
  
  printf("[[SUCCESS]]\n");
  
  return 0;
}
