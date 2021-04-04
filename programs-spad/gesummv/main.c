#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <math.h>

#include "spad.h"
#include "pthread_launch.h"
#include "gesummv.h"
#include "util.h"

#define CONST 5

void fill_array(DTYPE *m, int n)
{
  int rand_temp = rand()%8;
  for (int i = 0; i < n; i++)
  {
    m[i] = (rand_temp+i)%(n/4); //(rand_temp + i)%8;
  }
}

// int check_ax (DTYPE* a, DTYPE* _x, DTYPE* ax, int n){

//   DTYPE* tmp_ = (DTYPE*)malloc(n*sizeof(DTYPE));
//   for (int i = 0; i < n; i++){
//     tmp_[i] = CONST;
//     for (int j = 0; j < n; j++)
//       tmp_[i] = tmp_[i] + a[i*n+j] * _x[j];
//     // if(tmp_[i]!=ax[i]){
//     if (!float_compare(tmp_[i], ax[i], 0.0001f)){
//       printf("[[FAIL]] for matrix vector product Ax\n");
//       return 1;
//     }
//   }
//   return 0;
// }

int check_gesummv(DTYPE *a, DTYPE *b, DTYPE *x, DTYPE *y, int n){

  DTYPE* ax = (DTYPE*)malloc(n*sizeof(DTYPE));
  for (int i = 0; i < n; i++){
    ax[i] = CONST;
    for (int j = 0; j < n; j++)
      ax[i] = ax[i] + a[i*n+j] * x[j];
  }

  DTYPE* tmp_ = (DTYPE*)malloc(n*sizeof(DTYPE));
  for (int i = 0; i < n; i++){
    tmp_[i] = CONST;
    
    for (int j = 0; j < n; j++)
      tmp_[i] = tmp_[i] + b[i*n+j] * x[j];
    tmp_[i] = ALPHA* ax[i] + BETA* tmp_[i];

    // if(tmp_[i]!=y[i]){
    if (!float_compare(tmp_[i], y[i], 0.0001f)){
      printf("[[FAIL]] for GESUMMV %f != %f @ %d\n", tmp_[i], y[i], i);
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
  int n = 1000;

  // parse positional arguments
  if (argc > 1)
    n = atoi(argv[1]);

  printf("Problem size is %d\n", n);

  /*--------------------------------------------------------------------
  * Data initialization
  *-------------------------------------------------------------------*/

  DTYPE *a_ptr, *b_ptr, *x_ptr, *tmp_ptr, *y_ptr;
  DTYPE *a = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), n*n, (void**)&a_ptr);
  DTYPE *b = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), n*n, (void**)&b_ptr);
  DTYPE *x = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), n, (void**)&x_ptr);
  DTYPE *tmp = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), n, (void**)&tmp_ptr);
  DTYPE *y = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), n, (void**)&y_ptr);
  
 // initilaize arrays
  srand(0);
  fill_array(a, n*n);
  fill_array(b, n*n);
  fill_array(x, n);

  for (int i = 0; i < n; i++){
    tmp[i] = CONST;
    y[i] = CONST;
  }
  printf("------------\n \n");

  /*--------------------------------------------------------------------
  * Pack argument for kernel
  *-------------------------------------------------------------------*/

  // initialize the arguments to send to each device core
  Kern_Args **kern_args = (Kern_Args **)malloc(sizeof(Kern_Args *) * num_cores);

  for (int yy = 0; yy < cores_y; yy++)
  {
    for (int xx = 0; xx < cores_x; xx++)
    {
      int i = xx + yy * cores_x;
      kern_args[i] = construct_args(a, b, x, tmp, y, n, xx, yy, cores_x, cores_y);
    }
  }

  printf("Run kernel \n");

  /*--------------------------------------------------------------------
  * Run the kernel
  *-------------------------------------------------------------------*/

  printf("Begin kernel on %d cores\n", num_cores);
  printf("Cores x:%d Cores y:%d\n", cores_x, cores_y);
  launch_kernel(pthread_kernel, (void **)kern_args, cores_x, cores_y);

/*--------------------------------------------------------------------
  * Check result and cleanup data
  *-------------------------------------------------------------------*/

  // int fail = check_ax(a,x,tmp,n);
  // if (fail)
  //   return 1;
  
  // printf("[[mini SUCCESS]] for Ax\n");

  int fail = check_gesummv(a,b,x,y,n);
  if (fail)
    return 1;

  printf("[[SUCCESS]]\n");
  free(a_ptr);
  free(b_ptr);
  free(x_ptr);
  free(y_ptr);
  free(tmp_ptr);
  return 0;
}
