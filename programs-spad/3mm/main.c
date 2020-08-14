#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <math.h>

#include "spad.h"
#include "pthread_launch.h"
#include "3mm.h"
#include "util.h"

// #define TRANSPOSE


void fill_array(DTYPE *m, int n)
{
  int rand_temp = rand()%10;
  for (int i = 0; i < n; i++)
  {
    m[i] = (rand_temp + i)%10;
  }
}

int check_1mm(DTYPE *a, DTYPE *b, DTYPE *c, int m, int n, int t)
{
  for (int i = 0; i < m; i++)
  {
    for (int j = 0; j < n; j++)
    {
      DTYPE c_temp = 0;
      for (int k = 0; k < t; k++)
      {
        c_temp += a[i * t + k] * b[k * n + j];
      }
      // if (c[i * n + j] != c_temp)
      if (!float_compare(c[i * n + j], c_temp, 0.0001f))
      {
        printf("%d %d at i:%d, j:%d\n",c[i * n + j],c_temp, i,j);
        printf("[[FAIL]]\n");
        return 1;
      }
    }
  }
  return 0;
}

int check_transpose(DTYPE* a, DTYPE* aT, int r, int c){
  for(int i=0; i<r; i++){
    for(int j=0; j<c; j++){
      if(a[i*c+j]!=aT[j*r+i]) {
        printf("[[FAIL at transpose]]\n");
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
  int m = 32;
  int n = 32;
  int t1 = 32;
  int t2 = 32;
  int k = 32;

  // parse positional arguments
  if (argc > 1)
    m = atoi(argv[1]);
  if (argc > 2)
    n = atoi(argv[2]);
  if (argc > 3)
    t1 = atoi(argv[3]);
  if (argc > 4)
    t2 = atoi(argv[4]);
  if (argc >5)
    k = atoi(argv[5]);

  printf("Problem size is (x,y) A: %d,%d x B: %d,%d -> E: %d,%d\n C: %d,%d x D: %d,%d -> F: %d,%d\n ExF -> G: %d,%d\n", 
          m, t1, t1, k, m, k, k, t2,t2, n, k, n, m,n);

  /*--------------------------------------------------------------------
  * Data initialization
  *-------------------------------------------------------------------*/

  size_t sizeA = m * t1;
  size_t sizeB = t1 * k;
  size_t sizeE = m * k;
  size_t sizeC = k * t2;
  size_t sizeD = t2 * n;
  size_t sizeF = k * n;
  size_t sizeG = m * n;

  DTYPE *a_ptr, *b_ptr, *e_ptr, *eT_ptr, *c_ptr, *d_ptr, *f_ptr, *g_ptr;
  DTYPE *a = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), sizeA, (void**)&a_ptr);
  DTYPE *b = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), sizeB, (void**)&b_ptr);
  DTYPE *e = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), sizeE, (void**)&e_ptr);
  DTYPE *eT = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), sizeE, (void**)&eT_ptr);
  DTYPE *c = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), sizeC, (void**)&c_ptr);
  DTYPE *d = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), sizeD, (void**)&d_ptr);
  DTYPE *f = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), sizeF, (void**)&f_ptr);
  DTYPE *g = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), sizeG, (void**)&g_ptr);

  DTYPE *aT_ptr;
  DTYPE *aT = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), sizeA, (void**)&aT_ptr);
  DTYPE *cT_ptr;
  DTYPE *cT = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), sizeC, (void**)&cT_ptr);

  // initilaize arrays
  srand(0);
  printf("matrix a\n");
  fill_array(a, sizeA);
  printf("------------\n \n");
  printf("matrix b\n");
  fill_array(b, sizeB);
  printf("------------\n \n");
  printf("matrix c\n");
  fill_array(c, sizeC);
  printf("------------\n \n");
  printf("matrix d\n");
  fill_array(d, sizeD);

  printf("------------\n \n");
  printf("matrix c,cT,e\n");
  for (int i = 0; i < sizeE; i++){
    e[i] = 0;
    eT[i] = 0;
  }
  for (int i = 0; i < sizeF; i++)
    f[i] = 0;
  for (int i = 0; i < sizeG; i++)
    g[i] = 0;

  #ifdef TRANSPOSE
  DTYPE *at_ptr, *ct_ptr;
  //do transpose of a for contiguous access
  DTYPE *a_ = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), sizeA, (void**)&at_ptr);
  DTYPE *c_ = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), sizeC, (void**)&ct_ptr);
  DTYPE *_temp;
  transpose(a,m,t1,a_);
  transpose(c,k,t2,c_);

  _temp = a;
  a = a_;
  a_ = _temp;

  _temp = c;
  c = c_;
  c_ = _temp;
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
      kern_args[i] = construct_args(a,aT,b,e,c,cT,d,f,eT,g,m,t1,k,t2,n, x, y, cores_x, cores_y);
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
  c = c_;
#endif

  int fail;
  // fail = check_1mm(a, b, e, m, k, t1);
  // if (fail)
  //   return 1;

  // printf("[[mini SUCCESS for 1st mm]]\n");

  // fail = check_1mm(c, d, f, k, n, t2);
  // if (fail)
  //   return 1;

  // printf("[[mini SUCCESS for 2nd mm]]\n");

  // fail = check_transpose(e, eT, m,k);
  // if (fail)
  //   return 1;

  // printf("[[mini SUCCESS E transpose]]\n");

  fail = check_1mm(e, f, g, m, n, k);
  if (fail)
    return 1;
  printf("[[SUCCESS]]\n");
  free(a_ptr);
 
  return 0;
}
