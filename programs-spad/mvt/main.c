#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <math.h>

#include "spad.h"
#include "pthread_launch.h"
#include "mvt.h"
#include "util.h"

#define CONST 5

void fill_array(DTYPE *m, int n)
{
  int rand_temp = rand()%10;
  for (int i = 0; i < n; i++)
  {
    m[i] = (rand_temp + i)%10;
  }
}

int check_ay1(DTYPE* a, DTYPE* y1, DTYPE* x1, int n){

  DTYPE* tmp = (DTYPE*)malloc(n*sizeof(DTYPE));
  for (int i = 0; i < n; i++){
    tmp[i] = CONST;
    for (int j = 0; j < n; j++)
      tmp[i] = tmp[i] + a[i*n+j] * y1[j];
    // if(tmp[i]!=x1[i]){
    if (!float_compare(tmp[i], x1[i], 0.0001f)){
      printf("[[FAIL]] for matrix vector product Ay1\n");
      return 1;
    }
  }
  return 0;
}

int check_aty2(DTYPE* a, DTYPE* y2, DTYPE* x2, int n){

  DTYPE* tmp = (DTYPE*)malloc(n*sizeof(DTYPE));
  for (int i = 0; i < n; i++){
    tmp[i] = CONST;
    for (int j = 0; j < n; j++)
      tmp[i] = tmp[i] + a[j*n+i] * y2[j];
    // if(tmp[i]!=x2[i]){
    if (!float_compare(tmp[i], x2[i], 0.0001f)){
      printf("[[FAIL]] for matrix vector product Aty2\n");
      printf("actual = %f calculated = %f at i=%d\n",tmp[i],x2[i],i);
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

  DTYPE *a_ptr, *y1_ptr, *y2_ptr, *x1_ptr, *x2_ptr;
  DTYPE *a = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), n*n, (void**)&a_ptr);
  DTYPE *y1 = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), n, (void**)&y1_ptr);
  DTYPE *y2 = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), n, (void**)&y2_ptr);
  DTYPE *x1 = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), n, (void**)&x1_ptr);
  DTYPE *x2 = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), n, (void**)&x2_ptr);

  // DTYPE *a = (DTYPE *)malloc(sizeof(DTYPE) * n);

  // initilaize arrays
  srand(0);
  // printf("matrix a\n");
  fill_array(a, n*n);
  fill_array(y1, n);
  fill_array(y2, n);


  //Tests are designed to assume the matrix is intialized by constant
  // fill_array(x1, n);
  // fill_array(x2, n);

  for (int i = 0; i < n; i++)
    x1[i] = CONST;
  for (int i = 0; i < n; i++)
    x2[i] = CONST;


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
      kern_args[i] = construct_args(a, y1, y2, x1, x2, n, x, y, cores_x, cores_y);
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

  int fail = check_ay1(a,y1,x1,n);
  if (fail)
    return 1;
  
  printf("[[mini SUCCESS]] for Ay1\n");

  fail = check_aty2(a, y2, x2, n);
  if (fail)
    return 1;
  
  printf("[[SUCCESS]] for MVT\n");

  free(a_ptr);
  free(y1_ptr);
  free(y2_ptr);
  free(x1_ptr);
  free(x2_ptr);
  return 0;
}
