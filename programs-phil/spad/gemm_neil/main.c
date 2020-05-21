#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <math.h>

#include "spad.h"
#include "pthread_launch.h"
#include "gemm.h"

// #define PRINT_OUT
#define RAND_MAT

#if !defined NO_VEC
#define TRANSPOSE
#endif

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

int check_matmul(DTYPE *a, DTYPE *b, DTYPE *c, int m, int n, int t)
{

#ifdef PRINT_OUT
  for (int i = 0; i < m; i++)
  {
    for (int j = 0; j < n; j++)
    {
      printf("%d ", (int)c[i * n + j]);
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
      printf("%d ", (int)c_temp);
    }
    printf("\n");
  }
  #endif

  for (int i = 0; i < m; i++)
  {
    for (int j = 0; j < n; j++)
    {
      DTYPE c_temp = 0;
      for (int k = 0; k < t; k++)
      {
        c_temp += a[i * t + k] * b[k * n + j];
      }
      if (c[i * n + j] != c_temp)
      {
        printf("%f %f\n",c[i * n + j],c_temp);
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
  int m = 4;
  int n = 4;
  int t = 4;

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

  // DTYPE *a_ptr, *b_ptr, *c_ptr;
  // DTYPE *a = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), sizeA, (void**)&a_ptr);
  // DTYPE *b = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), sizeB, (void**)&b_ptr);
  // DTYPE *c = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), sizeC, (void**)&c_ptr);

  DTYPE *a = (DTYPE *)malloc(sizeof(DTYPE) * sizeA);
  DTYPE *b = (DTYPE *)malloc(sizeof(DTYPE) * sizeB);
  DTYPE *c = (DTYPE *)malloc(sizeof(DTYPE) * sizeC);

#ifdef RAND_MAT
  srand(0);
  printf("matrix a\n");
  fill_matrix(a, m, t);
  printf("------------\n \n");
  printf("matrix b\n");
  fill_matrix(b, t, n);
#else
  for (int i = 0; i < sizeA; i++)
    a[i] = 3;
  for (int i = 0; i < sizeB; i++)
    b[i] = 2;
#endif

  for (int i = 0; i < sizeC; i++)
    c[i] = 0;

#ifdef TRANSPOSE
  //do transpose of a for contiguous access
  DTYPE *a_ = (DTYPE *)malloc(sizeof(DTYPE) * sizeA);
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

  // figure out good tile size for the architecture
  // i.e. the 2d tiles for the three matrices should fit into scratchpad
  const int num_mat = 3;
  //const int blk_dim = sqrt((DTYPE)(getSpadNumBytes() / sizeof(DTYPE)) / (DTYPE)num_mat);

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

#ifdef _USE_SCRATCHPAD
//kern_args[i] = construct_args(sp_a[i], sp_b[i], sp_c[i], size, x, y, cores_x, cores_y);
#else
      kern_args[i] = construct_args(a, b, c, m, n, t, x, y, cores_x, cores_y);
#endif
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

#ifdef RAND_MAT
  int fail = check_matmul(a, b, c, m, n, t);
  if (fail)
    return 1;
#else
  for (int i = 0; i < sizeC; i++)
  {
    //printf("%f\n", c[i]);
    if (c[i] != 6 * t)
    {
      printf("[[FAIL]]\n");
      return 1;
    }
  }
#endif

  free(a);
  free(b);
  free(c);
  printf("[[SUCCESS]]\n");
  return 0;
}
