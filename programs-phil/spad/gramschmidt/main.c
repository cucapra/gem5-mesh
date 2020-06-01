#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>

#include "spad.h"
#include "pthread_launch.h"
#include "gramschmidt.h"

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
  
  int num_vectors = 8;
  int vector_len = 8;

  if (argc > 1) {
    num_vectors = atoi(argv[1]);
  }
  if (argc > 2) {
    vector_len = atoi(argv[2]);
  }
  
  printf("Gram-Schmidt Orthornormalization on %d vectors of length %d. Num cores is %d\n", 
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
      a[i * num_vectors + j] = ((DTYPE) ( i + 1 ) * ( j + 1 )) / (DTYPE)( vector_len + 1 );
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
  
  // could just do calc sequentially one cpu if want to trust that
  // int n = len;
  // int calc = c[0]; 
  // int exp = ((n * (n + 1) * (2*n + 1)) / 6) * 6; // (done some comp 6 times)
  // if (calc != exp) {
  //   printf("%d != %d\n", calc, exp);
  //   printf("[[FAIL]]\n");
  //   return 1;
  // }
  
  free(a_ptr);
  free(r_ptr);
  free(q_ptr);
  
  printf("[[SUCCESS]]\n");
  
  return 0;
}
