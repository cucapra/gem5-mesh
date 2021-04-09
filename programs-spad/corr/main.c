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

void transpose(DTYPE *a, int row, int col, DTYPE *aT){

  for(int i=0; i<row; i++){
    for(int j=i; j<col; j++){
      aT[i*col+j] = a[j*row+i];
      aT[j*row+i] = a[i*col+j];
    }
  }
}

int check_corr(DTYPE* orig_data, DTYPE* kernel_dataT, DTYPE* kernel_mean, DTYPE* kernel_stddev, DTYPE* symmat_out, int m, int n){

  /* Determine mean of column vectors of input data matrix */
  DTYPE* mean = (DTYPE*)malloc(m*sizeof(DTYPE));
  for (int j = 0; j < m; j++)
    {
      mean[j] = 0.0;
      for (int i = 0; i < n; i++)
	      mean[j] += orig_data[i*m+j];
      mean[j] /= n;
    }
  
  // for (int j = 0; j < m; j++)
  //   {
  //     if (!float_compare(mean[j], kernel_mean[j], 0.0001f)){
  //         printf("[[FAIL]] for corr mean\n");
  //         printf("kernel out:%f, actual out:%f, j:%d\n",kernel_mean[j],mean[j],j);
  //         return 1;
  //       }
  //   }

  

  /* Determine standard deviations of column vectors of data matrix. */
  double eps = 0.1f;
  DTYPE* stddev = (DTYPE*)malloc(m*sizeof(DTYPE));
  for (int j = 0; j < m; j++)
    {
      stddev[j] = 0.0;
      for (int i = 0; i < n; i++)
	      stddev[j] += (orig_data[i*m+j] - mean[j]) * (orig_data[i*m+j] - mean[j]);
      stddev[j] /= n;
      stddev[j] = sqrt(stddev[j]);
      /* The following in an inelegant but usual way to handle
	 near-zero std. dev. values, which below would cause a zero-
	 divide. */
      stddev[j] = stddev[j] <= eps ? 1.0 : stddev[j];
    }
  
  // for (int j = 0; j < m; j++)
  //   {
  //     if (!float_compare(stddev[j], kernel_stddev[j], 0.0001f)){
  //         printf("[[FAIL]] for corr stddev\n");
  //         printf("kernel out:%f, actual out:%f, j:%d\n",kernel_stddev[j],stddev[j],j);
  //         return 1;
  //       }
  //   }

  /* Center and reduce the column vectors. */
  for (int i = 0; i < n; i++)
    for (int j = 0; j < m; j++)
      {
	orig_data[i*m+j] -= mean[j];
	orig_data[i*m+j] /= sqrt(n) * stddev[j];
      }

  // for (int i = 0; i < n; i++){
  //   for (int j = 0; j < m; j++)
  //     {
  //       if (!float_compare(orig_data[i*m+j], kernel_dataT[j*n+i], 0.0001f)){
  //         printf("[[FAIL]] for corr data\n");
  //         printf("kernel out:%f, actual out:%f, i:%d, j:%d\n",kernel_dataT[j*n+i],orig_data[i*m+j],i,j);
  //         return 1;
  //       }
  //     }
  // }
  

  /* Calculate the m * m correlation matrix. */
  DTYPE* symmat_actual = (DTYPE*)malloc(m*m*sizeof(DTYPE));
  for (int j1 = 0; j1 < m-1; j1++)
  {
    symmat_actual[j1*m+j1] = 1.0;
    for (int j2 = j1+1; j2 < m; j2++)
	  {
	    symmat_actual[j1*m+j2] = 0.0;
	    for (int i = 0; i < n; i++)
	      symmat_actual[j1*m+j2] += (orig_data[i*m+j1] * orig_data[i*m+j2]);
	    symmat_actual[j2*m+j1] = symmat_actual[j1*m+j2];
	  }
  }
  symmat_actual[m*m-1] = 1.0;



  //check
  for(int i=0;i<m*m;i++){
    if (!float_compare(symmat_actual[i], symmat_out[i], 0.0001f)){
        printf("[[FAIL]] for corr\n");
        printf("kernel out:%f, actual out:%f, id:%d\n",symmat_out[i],symmat_actual[i],i);
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

  
  DTYPE *dataT_ptr;
  DTYPE *dataT = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), m*n, (void**)&dataT_ptr);
  
  

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
      kern_args[i] = construct_args(data, dataT, symmat, mean, stddev, m, n, x, y, cores_x, cores_y);
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

  fail = check_corr(data_copy, dataT, mean, stddev, symmat,m,n);
  if (fail)
    return 1;

  printf("[[SUCCESS]]\n");

  free(data_ptr);
  free(symmat_ptr);
  free(mean_ptr);
  free(stddev_ptr);
  return 0;
}
