#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>

#include "spad.h"
#include "pthread_launch.h"
#include "bicg.h"

// checker from polybench. single core implementation
#include <math.h>

void bigc(DTYPE* A, DTYPE* r, DTYPE* s, DTYPE *p, DTYPE *q, int NX, int NY)
{
	int i,j;
	
  	for (i = 0; i < NY; i++)
	{
		s[i] = 0.0;
	}

    for (i = 0; i < NX; i++)
    {
		q[i] = 0.0;
		for (j = 0; j < NY; j++)
	  	{
	    		s[j] = s[j] + r[i] * A[i*NY + j];
	    		q[i] = q[i] + A[i*NY + j] * p[j];
	  	}
	}
}

#ifndef M_PI
#define M_PI 3.14159
#endif

void init_data(DTYPE *A, DTYPE *p, DTYPE *r, int NX, int NY) {
  	int i, j;

  	for (i = 0; i < NX; i++)
	{
    		r[i] = i * M_PI;

    		for (j = 0; j < NY; j++)
		{
      			A[i*NY + j] = ((DTYPE) i*j) / NX;
		}
 	}
	
	for (i = 0; i < NY; i++)
	{
    		p[i] = i * M_PI;
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
  
  int NX = 960;
  int NY = NX;

  // whether to skip verification or not
  int skip_check = 1;

  if (argc > 1) {
    NX = atoi(argv[1]);
  }
  if (argc > 2) {
    NY = atoi(argv[2]);
  }
  if (argc > 3) {
    skip_check = atoi(argv[3]);
  }
  
  printf("The BIG C on %d x %d. Num cores is %d\n", 
    NX, NY, num_cores);

  /*--------------------------------------------------------------------
  * Data initialization
  *-------------------------------------------------------------------*/

  DTYPE *a_ptr, *r_ptr, *p_ptr, *s_ptr, *q_ptr;
  DTYPE *a = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), NX*NY, (void**)&a_ptr);
  DTYPE *r = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), NX,    (void**)&r_ptr);
  DTYPE *s = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), NY,    (void**)&s_ptr);
  DTYPE *p = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), NY,    (void**)&p_ptr);
  DTYPE *q = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), NX,    (void**)&q_ptr);

  // initial data
  init_data(a, p, r, NX, NY);
  
  /*--------------------------------------------------------------------
  * Pack argument for kernel
  *-------------------------------------------------------------------*/  

  // initialize the arguments to send to each device core
  Kern_Args **kern_args = (Kern_Args**)malloc(sizeof(Kern_Args*) * num_cores);

  for (int y = 0; y < cores_y; y++) {
    for (int x = 0; x < cores_x; x++){
      int i = x + y * cores_x;
      kern_args[i] = construct_args(a, r, p, s, q, NX, NY, x, y, cores_x, cores_y);
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
    free(r_ptr);
    free(q_ptr);
    free(s_ptr);
    free(p_ptr);
    return 0;
  }

  printf("Checking results\n");

  // compare with results from a sequential version
  // DTYPE *a_exp = (DTYPE*)malloc(NX*NY*sizeof(DTYPE));
	// DTYPE *r_exp = (DTYPE*)malloc(NX*sizeof(DTYPE));
	// DTYPE *s_exp = (DTYPE*)malloc(NY*sizeof(DTYPE));
	DTYPE *s_exp = (DTYPE*)malloc(NY*sizeof(DTYPE));
	DTYPE *q_exp = (DTYPE*)malloc(NX*sizeof(DTYPE));

  // init_data(a_exp, p_exp, r_exp, NX, NY);

  bigc(a, r, s_exp, p, q_exp, NX, NY);

  for (int j = 0; j < NY; j++) {
    if (s[j] != s_exp[j]) {
      printf("j %d | %f != %f\n", j, s[j], s_exp[j]);
      printf("[[FAIL]]\n");
      return 1;      
    }
    // else {
    //   printf("j %d | %f == %f\n", j, s[j], s_exp[j]);
    // }
  }
  for (int i = 0; i < NX; i++) {
    if (q[i] != q_exp[i]) {
      printf("i %d | %f != %f\n", i, q[i], q_exp[i]);
      printf("[[FAIL]]\n");
      return 1;      
    }
  }

  free(s_exp);
  free(q_exp);
  
  free(a_ptr);
  free(r_ptr);
  free(q_ptr);
  free(s_ptr);
  free(p_ptr);
  
  printf("[[SUCCESS]]\n");
  
  return 0;
}
