#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <math.h>

#include "spad.h"
#include "pthread_launch.h"
#include "atax.h"

void fill_matrix(DTYPE *m, int row, int col)
{
  
  for (int i = 0; i < row; i++)
  {
    for (int j = 0; j < col; j++)
    {
      m[i * col + j] = rand() % 10;
      // printf("%d ", (int)m[i * col + j]);
    }
    // printf("\n");
  }
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

  for (int i = 0; i < ny; i++){
    if (y_temp[i]!=_y[i]){
      printf("[[FAIL]]\n");
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

  DTYPE *a_ptr, *b_ptr, *c_ptr;
  DTYPE *a = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), sizeA, (void**)&a_ptr);
  DTYPE *_x = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), ny, (void**)&a_ptr);
  DTYPE *_y = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), ny, (void**)&a_ptr);

  // DTYPE *a = (DTYPE *)malloc(sizeof(DTYPE) * n);

  // initilaize arrays

  srand(0);
  fill_matrix(a, nx, ny);
  fill_matrix(_x, ny, 1);

  for (int i = 0; i < ny; i++)
    _y[i] = 0;

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
      kern_args[i] = construct_args(a, _x, _y, nx, ny , x, y, cores_x, cores_y);
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

  int fail = check_atax(a, _x, _y, nx, ny);
  if (fail)
    return 1;

  free(a);
  printf("[[SUCCESS]]\n");
  return 0;
}
