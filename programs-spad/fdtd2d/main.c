#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>

#include "spad.h"
#include "pthread_launch.h"
#include "fdtd2d.h"
#include "util.h"

// checker from polybench. single core implementation
void fdtd_serial(DTYPE* _fict_, DTYPE* ex, DTYPE* ey, DTYPE* hz, int NY, int NX, int tmax)
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
  
  int NX   = 8;
  int NY   = 8;
  int tmax = 4;

  // whether to skip verification or not
  int skip_check = 0;

  if (argc > 1) {
    NX = atoi(argv[1]);
  }
  if (argc > 2) {
    NY = atoi(argv[2]);
  }
  if (argc > 3) {
    tmax = atoi(argv[3]);
  }
  if (argc > 4) {
    skip_check = atoi(argv[4]);
  }
  
  printf("FDTD-2D on %dx%d image w/ %d steps. Num cores is %d\n", 
    NX, NY, tmax, num_cores);

  /*--------------------------------------------------------------------
  * Data initialization
  *-------------------------------------------------------------------*/

  DTYPE *fict_ptr, *ex_ptr, *ey_ptr, *hz_ptr;
  DTYPE *fict = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), tmax         , (void**)&fict_ptr);
  DTYPE *ex   = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), NX * (NY + 1), (void**)&ex_ptr);
  DTYPE *ey   = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), (NX + 1) * NY, (void**)&ey_ptr);
  DTYPE *hz   = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), NX * NY      , (void**)&hz_ptr);

  // initial data
  init_arrays(fict, ex, ey, hz, NY, NX, tmax);
  
  /*--------------------------------------------------------------------
  * Pack argument for kernel
  *-------------------------------------------------------------------*/  

  // initialize the arguments to send to each device core
  Kern_Args **kern_args = (Kern_Args**)malloc(sizeof(Kern_Args*) * num_cores);

  for (int y = 0; y < cores_y; y++) {
    for (int x = 0; x < cores_x; x++){
      int i = x + y * cores_x;
      kern_args[i] = construct_args(fict, ex, ey, hz, NX, NY, tmax, x, y, cores_x, cores_y);
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
    free(fict_ptr);
    free(ex_ptr);
    free(ey_ptr);
    free(hz_ptr);
    return 0;
  }

  printf("Checking results\n");

  // compare with results from a sequential version
  DTYPE *fict_exp = (DTYPE*)malloc(sizeof(DTYPE) * tmax);
  DTYPE *ex_exp   = (DTYPE*)malloc(sizeof(DTYPE) * NX * (NY + 1));
  DTYPE *ey_exp   = (DTYPE*)malloc(sizeof(DTYPE) * (NX + 1) * NY);
  DTYPE *hz_exp   = (DTYPE*)malloc(sizeof(DTYPE) * NX * NY);

  init_arrays(fict_exp, ex_exp, ey_exp, hz_exp, NY, NX, tmax);

  fdtd_serial(fict_exp, ex_exp, ey_exp, hz_exp, NY, NX, tmax);

  // check hz
  for (int i = 0; i < NX; i++) {
    for (int j = 0; j < NY; j++) {
      int idx = i*NY + j;
      if (hz[idx] != hz_exp[idx]) {
        printf("%f != %f | i %d j %d (%d)\n", hz[idx], hz_exp[idx], i, j, idx);
        printf("[[FAIL]]\n");
        return 1;
      }
    }
  }

  free(fict_exp);
  free(ex_exp);
  free(ey_exp);
  free(hz_exp);
  
  free(fict_ptr);
  free(ex_ptr);
  free(ey_ptr);
  free(hz_ptr);
  
  printf("[[SUCCESS]]\n");
  
  return 0;
}
