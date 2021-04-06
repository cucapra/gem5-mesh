#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <math.h>

#include "spad.h"
#include "pthread_launch.h"
#include "gemm.h"
#include "util.h"

#define CONST 5
// #define PRINT_OUT

// #define TRANSPOSE


void fill_array(DTYPE *m, int n)
{
  int rand_temp = rand()%10;
  for (int i = 0; i < n; i++)
  {
    m[i] = (rand_temp + i)%10;
  }
}


float absVal(float a)
{
	if(a < 0)
	{
		return (a * -1);
	}
   	else
	{ 
		return a;
	}
}


#define SMALL_FLOAT_VAL 0.00000001f
float percentDiff(float val1, float val2)
{
	if ((absVal(val1) < 0.01) && (absVal(val2) < 0.01))
	{
		return 0.0f;
	}

	else
	{
    		return 100.0f * (absVal(absVal(val1 - val2) / absVal(val1 + SMALL_FLOAT_VAL)));
	}
} 

//define the error threshold for the results "not matching"
#define PERCENT_DIFF_ERROR_THRESHOLD 0.05
// ret 1 if fail
// ret 0 if sucess
int polybenchCompare(float val1, float val2) {
  return (percentDiff(val1, val2) > PERCENT_DIFF_ERROR_THRESHOLD);
}

int check_matmul(DTYPE *a, DTYPE *b, DTYPE *c, int m, int n, int t)
{

#ifdef PRINT_OUT
  for (int i = 0; i < m; i++)
  {
    for (int j = 0; j < n; j++)
    {
      printf("%f ", (int)c[i * n + j]);
    }
    printf("\n");
  }

  printf("------------\n \n");

  for (int i = 0; i < m; i++)
  {
    for (int j = 0; j < n; j++)
    {
      DTYPE c_temp = 0;
      for (int k = 0; k < t; k++)
      {
        c_temp += a[i * t + k] * b[k * n + j];
      }
      printf("%f ", (int)c_temp);
    }
    printf("\n");
  }
  #endif

  for (int i = 0; i < m; i++)
  {
    for (int j = 0; j < n; j++)
    {
      DTYPE c_temp = CONST*BETA;
      for (int k = 0; k < t; k++)
      {
        c_temp += ALPHA* a[i * t + k] * b[k * n + j];
      }
      // if (c[i * n + j] != c_temp)
      // printf("%f %f at i:%d, j:%d\n",c[i * n + j],c_temp, i,j);
      if (polybenchCompare(c[i * n + j], c_temp))
      {
        printf("%f %f at i:%d, j:%d\n",c[i * n + j],c_temp, i,j);
        printf("[[FAIL]]\n");
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
  int m = 16;
  int n = 16;
  int t = 16;

  // parse positional arguments
  if (argc > 1)
    m = atoi(argv[1]);
  if (argc > 2)
    n = atoi(argv[2]);
  if (argc > 3)
    t = atoi(argv[3]);

  printf("Problem size is (x,y) A: %d,%d x B: %d,%d -> C: %d,%d\n", t, m, n, t, n, m);

  /*--------------------------------------------------------------------
  * Data initialization
  *-------------------------------------------------------------------*/

  size_t sizeA = t * m;
  size_t sizeB = n * t;
  size_t sizeC = n * m;

  DTYPE *a_ptr, *b_ptr, *c_ptr;
  DTYPE *a = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), sizeA, (void**)&a_ptr);
  DTYPE *b = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), sizeB, (void**)&b_ptr);
  DTYPE *c = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), sizeC, (void**)&c_ptr);

  DTYPE *aT_ptr;
  DTYPE *aT = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), sizeA, (void**)&aT_ptr);

  // initilaize arrays
  srand(0);
  printf("matrix a\n");
  fill_array(a, sizeA);
  printf("------------\n \n");
  printf("matrix b\n");
  fill_array(b, sizeB);


  for (int i = 0; i < sizeC; i++)
    c[i] = CONST;

#ifdef TRANSPOSE
  DTYPE *at_ptr;
  //do transpose of a for contiguous access
  DTYPE *a_ = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), sizeA, (void**)&at_ptr);
  DTYPE *_temp;
  for (int k = 0; k < t; k++)
  {
    for (int i = 0; i < m; i++)
    {
      a_[k * m + i] = a[i * t + k];
    }
  }
  _temp = a;
  a = a_;
  a_ = _temp;
#endif

  
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
      kern_args[i] = construct_args(a, aT, b, c, m, n, t, x, y, cores_x, cores_y);
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
#ifdef TRANSPOSE
  a = a_;
#endif

  int fail = check_matmul(a, b, c, m, n, t);
  if (fail)
    return 1;

  printf("[[SUCCESS]]\n");

  free(a_ptr);
  free(b_ptr);
  free(c_ptr);
  return 0;
}
