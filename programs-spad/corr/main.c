#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <math.h>

#include "spad.h"
#include "pthread_launch.h"
#include "corr.h"
#include "util.h"

void fill_array(DTYPE *m, int n)
{
  int rand_temp = rand()%10;
  for (int i = 0; i < n; i++)
  {
    m[i] = (rand_temp + i)%10;
  }
}

int check_mean (DTYPE* mean, DTYPE* data, int m, int n){

  DTYPE* tmp = (DTYPE*)malloc(m*sizeof(DTYPE));
  for (int i = 0; i < m; i++){
    tmp[i] = 0;
    for (int j = 0; j < n; j++){
      tmp[i] += data[i*n+j];
    }
    tmp[i] = tmp[i]/n;
    // if(tmp[i]!=mean[i]){
    if (!float_compare(tmp[i], mean[i], 0.0001f)){
      printf("[[FAIL]] for mean of A\n");
      printf("kernel out:%f, actual out:%f\n",mean[i],tmp[i]);
      return 1;
    }
  }
  return 0;
}

int check_stddev(DTYPE* stddev, DTYPE* data, DTYPE* mean, int m, int n){

  double eps = 0.1f;
  DTYPE* tmp = (DTYPE*)malloc(m*sizeof(DTYPE));
  for (int i = 0; i < m; i++){
    tmp[i] = 0;
    for (int j = 0; j < n; j++)
      tmp[i] += (data[i*n+j]-mean[i])*(data[i*n+j]-mean[i]);
    tmp[i] = tmp[i]/n;
    tmp[i] = sqrt(tmp[i]);
    tmp[i] = tmp[i] <= eps ? 1.0 : tmp[i];
    // if(tmp[i]!=stddev[i]){
    if (!float_compare(tmp[i], stddev[i], 0.0001f)){
      printf("[[FAIL]] for stddev of A\n");
      printf("kernel out:%f, actual out:%f\n",stddev[i],tmp[i]);
      return 1;
    }
  }
  return 0;
}

int check_center (DTYPE* kernel_data, DTYPE* orig_data, DTYPE* stddev, DTYPE* mean, int m, int n){

  DTYPE* tmp = (DTYPE*)malloc(m*n*sizeof(DTYPE));
  for (int i = 0; i < m; i++){
    for (int j = 0; j < n; j++){
      tmp[i*n+j] = orig_data[i*n+j] - mean[i];
      tmp[i*n+j] /= sqrt(n)*stddev[i];
      // if(tmp[i*n+j]!=kernel_data[i*n+j]){
      if (!float_compare(tmp[i*n+j], kernel_data[i*n+j], 0.0001f)){
        printf("[[FAIL]] for centering of A\n");
        printf("kernel out:%f, actual out:%f\n",kernel_data[i*n+j],tmp[i*n+j]);
        return 1;
      }
    }
  }
  return 0;
}

int check_corr (DTYPE* symmat, DTYPE* data, int m, int n){

  DTYPE* tmp = (DTYPE*)malloc(m*m*sizeof(DTYPE));
  for (int i1 = 0; i1 < m-1; i1++){
    for (int i2 = i1+1; i2 < m; i2++){
      tmp[i1*m+i2]=0;
      for(int j=0; j<n; j++){
        tmp[i1*m+i2]+=data[i1*n+j]*data[i2*n+j];
      }
      tmp[i2*m+i1]=tmp[i1*m+i2];
      // if(tmp[i1*m+i2]!=symmat[i1*m+i2]){
      if (!float_compare(tmp[i1*m+i2], symmat[i1*m+i2], 0.0001f)){
        printf("[[FAIL]] for corr\n");
        printf("kernel out:%f, actual out:%f\n",symmat[i1*m+i2],tmp[i1*m+i2]);
        return 1;
      }
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
  int m = 1000;
  int n = 1000;

  // parse positional arguments
  if (argc > 1)
    m = atoi(argv[1]);
  if (argc > 2)
    n = atoi(argv[2]);

  printf("Problem size is A: %d,%d\n", m,n);

  /*--------------------------------------------------------------------
  * Data initialization
  *-------------------------------------------------------------------*/

  DTYPE *data_ptr, *symmat_ptr, *mean_ptr, *stddev_ptr;
  DTYPE *data = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), m*n, (void**)&data_ptr);
  DTYPE *symmat = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), m*m, (void**)&symmat_ptr);
  DTYPE *mean = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), m, (void**)&mean_ptr);
  DTYPE *stddev = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), m, (void**)&stddev_ptr);

  

  srand(0);
  // printf("matrix a\n");
  fill_array(data, m*n);

  DTYPE *data_copy = (DTYPE*)malloc(sizeof(DTYPE)* m*n);
  for (int i = 0; i < m*n; i++) data_copy[i]=data[i];


  for (int i = 0; i < m*m; i++)
    symmat[i] = 0;
  for (int i = 0; i < m; i++){
    mean[i] = 0;
    stddev[i] = 0;
  }

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
      kern_args[i] = construct_args(data, symmat, mean, stddev, m, n, x, y, cores_x, cores_y);
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

  int fail;
  // fail = check_mean(mean,data_copy,m,n);
  // if (fail)
  //   return 1;
  // printf("[[mini SUCCESS]] for mean\n");

  // fail = check_stddev(stddev, data_copy, mean,  m, n);
  // if (fail)
  //   return 1;

  // printf("[[mini SUCCESS]] for std dev\n");

  fail = check_center(data, data_copy, stddev, mean, m, n);
  if (fail)
    return 1;

  printf("[[mini SUCCESS]] for centering data\n");  

  fail = check_corr(symmat, data, m, n);
  if (fail)
    return 1;

  printf("[[SUCCESS]]\n");

  free(data_ptr);
  free(symmat_ptr);
  free(mean_ptr);
  free(stddev_ptr);
  return 0;
}
