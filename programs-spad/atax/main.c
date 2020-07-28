#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <math.h>

#include "spad.h"
#include "pthread_launch.h"
#include "atax.h"

void fill_array(DTYPE *m, int n)
{
  int rand_temp = rand()%10;
  for (int i = 0; i < n; i++)
  {
    m[i] = (rand_temp + i)%10;
  }
}

int check_ax (DTYPE* a, DTYPE* _x, DTYPE* ax, int nx, int ny){

  DTYPE* tmp = (DTYPE*)malloc(nx*sizeof(DTYPE));
  for (int i = 0; i < nx; i++){
    tmp[i] = 0;
    for (int j = 0; j < ny; j++)
      tmp[i] = tmp[i] + a[i*ny+j] * _x[j];
    if(tmp[i]!=ax[i]){
      printf("[[FAIL]] for matrix vector product Ax\n");
      return 1;
    }
  }
  return 0;
}

int check_atax (DTYPE* a, DTYPE* _x, DTYPE* _y, int nx, int ny){

  DTYPE* y_temp = (DTYPE*)malloc(ny*sizeof(DTYPE));
  DTYPE* tmp = (DTYPE*)malloc(nx*sizeof(DTYPE));

  
  for (int i = 0; i < ny; i++)
    y_temp[i] = 0;
  for (int i = 0; i < nx; i++){
    tmp[i] = 0;
    for (int j = 0; j < ny; j++)
      tmp[i] = tmp[i] + a[i*ny+j] * _x[j];
    for (int j = 0; j < ny; j++)
      y_temp[j] = y_temp[j] + a[i*ny+j] * tmp[i];
  }

  // for (int i = 0; i < ny; i++)
  // printf("Kernel val:%d Actual val:%d\n",y_temp[i],_y[i]);
  for (int i = 0; i < ny; i++){
    if (y_temp[i]!=_y[i]){
      printf("[[FAIL]] for At(Ax)\n");
      return 1;
    }
  }

  return 0;
}

int main(int argc, char *argv[])
{

  /*--------------------------------------------------------------------
   * Setup scratchpads
   *------------------------------------------------------------------*/

  printf("starting\n");

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
  int nx = 1000;
  int ny = 1000;

  // parse positional arguments
  if (argc > 1)
    nx = atoi(argv[1]);
  if (argc > 2)
    ny = atoi(argv[2]);

  printf("Problem size is A: %d,%d, x: %d and y: %d\n", nx,ny,ny,ny);

  /*--------------------------------------------------------------------
  * Data initialization
  *-------------------------------------------------------------------*/

  size_t sizeA = nx*ny;

  DTYPE *a_ptr, *x_ptr, *y_ptr, *ax_ptr, *y_partial_ptr;
  DTYPE *a = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), sizeA, (void**)&a_ptr);
  DTYPE *_x = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), ny, (void**)&x_ptr);
  DTYPE *_y = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), ny, (void**)&y_ptr);
  DTYPE *ax = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), nx, (void**)&ax_ptr);

  DTYPE* _y_partial = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), ny*num_cores, (void**)&y_partial_ptr);
  // DTYPE *a = (DTYPE *)malloc(sizeof(DTYPE) * n);

  // initilaize arrays

  srand(0);
  // printf("matrix a\n");
  fill_array(a, nx*ny);
  // printf("matrix _x\n");
  fill_array(_x, ny);

  // printf("matrix _y\n");
  for (int i = 0; i < ny; i++)
    _y[i] = 0;
  // printf("matrix ax\n");
  for (int i = 0; i < nx; i++)
    ax[i] = 0;

  for (int i = 0; i < ny*num_cores; i++)
    _y_partial[i] = 0;
  /*--------------------------------------------------------------------
  * Pack argument for kernel
  *-------------------------------------------------------------------*/

  // initialize the arguments to send to each device core
  Kern_Args **kern_args = (Kern_Args **)malloc(sizeof(Kern_Args *) * num_cores);

  for (int y = 0; y < cores_y; y++)
  {
    for (int x = 0; x < cores_x; x++)
    {
      int i = x + y * cores_x;
      kern_args[i] = construct_args(a, _x, _y, ax, _y_partial, nx, ny , x, y, cores_x, cores_y);
      // for (int ii = 0; ii < ny; ii++){
      //   kern_args[i]->_y_partial[ii]=0;
      // }
    }
  }

  /*--------------------------------------------------------------------
  * Run the kernel
  *-------------------------------------------------------------------*/

  printf("Begin kernel on %d cores\n", num_cores);
  printf("Cores x:%d Cores y:%d\n", cores_x, cores_y);
  launch_kernel(pthread_kernel, (void **)kern_args, cores_x, cores_y);

/*--------------------------------------------------------------------
  * Check result and cleanup data
  *-------------------------------------------------------------------*/

  int fail = check_ax(a,_x,ax,nx,ny);
  if (fail)
    return 1;
  
  printf("[[mini SUCCESS]] for Ax\n");
  
  fail = check_atax(a, _x, _y, nx, ny);
  if (fail)
    return 1;
  
  printf("[[SUCCESS]] for At(Ax)\n");

  free(a_ptr);
  free(x_ptr);
  free(y_ptr);
  free(ax_ptr);
  free(y_partial_ptr);
  return 0;
}
