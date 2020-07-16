#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>

#include "spad.h"
#include "pthread_launch.h"
#include "gramschmidt.h"
#include "util.h"

// checker from polybench. single core implementation
#include <math.h>

void gramschmidt(DTYPE* A, DTYPE* R, DTYPE* Q, int N, int M)
{
	int i,j,k;
	DTYPE nrm;
	for (k = 0; k < N; k++) // dont need to process the last one
	{
		nrm = 0.0f;
		for (i = 0; i < M; i++)
		{
      // printf("%f %f %f idx %d\n", nrm, A[i*N + k], A[i*N + k] * A[i*N + k], i*N+k);
			nrm += A[i*N + k] * A[i*N + k];
		}
		
		R[k*N + k] = sqrtf(nrm);
    // printf("mag sqrt(%f) = %f idx %d\n", nrm, R[k*N + k], k*N+k);

		for (i = 0; i < M; i++)
		{
			Q[i*N + k] = A[i*N + k] / R[k*N + k];
      // printf("q %f idx %d\n", Q[i*N + k], i*N + k);
		}
		
		for (j = k + 1; j < N; j++)
		{
			R[k*N + j] = 0;
			for (i = 0; i < M; i++)
			{
				R[k*N + j] += Q[i*N + k] * A[i*N + j];
			}
      // printf("dot %f idx %d\n", R[k*N + j], k*N + j);

			for (i = 0; i < M; i++)
			{
        // printf("a before %f\n", A[i*N+j]);
				A[i*N + j] -= Q[i*N + k] * R[k*N + j];
        // printf("%f -= %f * %f idx %d %d\n", A[i*N + j], Q[i*N + k], R[k*N + j], i*N + j, k*N + j);
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
  
  // TODO default polybench doesn't work when these differ
  int num_vectors = 256;
  int vector_len = num_vectors;

  // whether to skip verification or not
  int skip_check = 0;

  if (argc > 1) {
    num_vectors = atoi(argv[1]);
  }
  if (argc > 2) {
    vector_len = atoi(argv[2]);
  }
  if (argc > 3) {
    skip_check = atoi(argv[3]);
  }
  
  printf("Gram-Schmidt Orthonormalization on %d vectors of length %d. Num cores is %d\n", 
    num_vectors, vector_len, num_cores);

  /*--------------------------------------------------------------------
  * Data initialization
  *-------------------------------------------------------------------*/

  // vectors will be stored as a flat array
  // store WHICH_VECTOR_IDX - WHICH_VECTOR
  int flat_len = vector_len * num_vectors;
  DTYPE *a_ptr, *r_ptr, *q_ptr;
  // holds the non-orthogonal input vectors and will be modified inplace to store the orthogonal output vectors
  DTYPE *a = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), flat_len, (void**)&a_ptr);
  // holds intermediate data, first the magnitude of u_k, then final projection
  DTYPE *r = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), flat_len, (void**)&r_ptr);
  // holds the normalized u_k and is reused over the course of the computation
  DTYPE *q = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), flat_len, (void**)&q_ptr);

  // initial vectors
  for (int i = 0; i < vector_len; i++) {
    for (int j = 0; j < num_vectors; j++) {
      // a[i * num_vectors + j] = ((DTYPE) (( i + 1 ) * ( j + 1 ))) / (DTYPE)( vector_len + 1 );
      a[i * num_vectors + j] = ((DTYPE)(( i + 1 ) / (j + 1)) + (DTYPE)((j + 1) * (j + 1))) / (DTYPE)( vector_len + 1 );
      // printf("%f\n", a[i * num_vectors + j]);
    }
  }
  
  /*--------------------------------------------------------------------
  * Pack argument for kernel
  *-------------------------------------------------------------------*/  

  // initialize the arguments to send to each device core
  Kern_Args **kern_args = (Kern_Args**)malloc(sizeof(Kern_Args*) * num_cores);

  for (int y = 0; y < cores_y; y++) {
    for (int x = 0; x < cores_x; x++){
      int i = x + y * cores_x;
      kern_args[i] = construct_args(a, r, q, num_vectors, vector_len, x, y, cores_x, cores_y);
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
    free(r_ptr);
    free(q_ptr);
    return 0;
  }

  printf("Checking results\n");
  
  //  srand(103103);

  // compare with results from a sequential version
  // TODO maybe worth writing to file and comparing natively?
  DTYPE *A_exp = (DTYPE*)malloc(sizeof(DTYPE)*flat_len);
  DTYPE *R_exp = (DTYPE*)malloc(sizeof(DTYPE)*flat_len);
  DTYPE *Q_exp = (DTYPE*)malloc(sizeof(DTYPE)*flat_len);
  for (int i = 0; i < vector_len; i++) {
    for (int j = 0; j < num_vectors; j++) {
      // A_exp[i * num_vectors + j] = ((DTYPE) (( i + 1 ) * ( j + 1 ))) / (DTYPE)( vector_len + 1 );
      // A_exp[i * num_vectors + j] = (float)rand() / (float)RAND_MAX; // works but probably slow to generate
      A_exp[i * num_vectors + j] = ((DTYPE)(( i + 1 ) / (j + 1)) + (DTYPE)((j + 1) * (j + 1))) / (DTYPE)( vector_len + 1 );
    }
  }
  gramschmidt(A_exp, R_exp, Q_exp, num_vectors, vector_len);
  for (int j = 0; j < vector_len; j++) {
    for (int i = 0; i < num_vectors; i++) {
      int idx = j * num_vectors + i;
      // if (!float_compare(a[idx], A_exp[idx], A_exp[idx] * 0.001f)) {
      if (a[idx] != A_exp[idx]) {
        printf("vec %d element %d | %f != %f\n", i, j, a[idx], A_exp[idx]);
        printf("[[FAIL]]\n");
        return 1;
      }
      // else {
      //   printf("%f\n", a[idx]);
      // }
    }
  }

  free(A_exp);
  free(R_exp);
  free(Q_exp);
  
  free(a_ptr);
  free(r_ptr);
  free(q_ptr);
  
  printf("[[SUCCESS]]\n");
  
  return 0;
}
