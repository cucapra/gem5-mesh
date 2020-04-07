#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>

#include "spad.h"
#include "pthread_launch.h"
#include "stencil.h"
// #include "../../common/bind_defs.h"

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
  
  // default values
  int nrows = 1 + (FILTER_DIM - 1); // single row
  int ncols = 12; //32; // + (FILTER_DIM - 1);
  
  // parse positional arguments (X Y)
  if (argc > 1) {
    ncols = atoi(argv[1]);
  }
  if (argc > 2) {
    nrows = atoi(argv[2]);
  }

  // if (ncols < 32) {
  //   printf("[[TODO]] size too small for good prefetching. exiting\n");
  //   return 1;
  // }
  
  printf("Stencil %dx%d on %dx%d image. Num cores is %d\n", FILTER_DIM, FILTER_DIM, ncols, nrows, num_cores);

  /*--------------------------------------------------------------------
  * Data initialization
  *-------------------------------------------------------------------*/

  int boundOffset = (FILTER_DIM - 1);
  DTYPE *a_ptr, *b_ptr, *c_ptr;
  DTYPE *a = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), nrows * ncols, (void**)&a_ptr);
  DTYPE *b = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), FILTER_DIM * FILTER_DIM, (void**)&b_ptr);
  DTYPE *c = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), (nrows - boundOffset) * (ncols /*- boundOffset*/), (void**)&c_ptr);

  // image
  for (int i = 0; i < nrows * ncols; i++) {
    a[i] = i + 1;
  }
    for (int i = 0; i < ncols; i++) {
    printf("%d ", a[i]);
  }
  printf("\n");
  for (int i = ncols; i < 2*ncols; i++) {
    printf("%d ", a[i]);
  }
  printf("\n");
  for (int i = 2*ncols; i < 3*ncols; i++) {
    printf("%d ", a[i]);
  }
  printf("\n");
  #ifndef REUSE
  int group_len = 4;
  DTYPE *a_re_ptr;
  DTYPE *a_re = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), nrows * ncols, (void**)&a_re_ptr); 
  // 0 1 2 3 | 4 5 6 7  | 8 9 10 11 || 12 13 14 15
  // 0 3 6 9 | 1 4 7 10 | 2 5 8  11 || 12 15 18 21 
  for (int r = 0; r < nrows; r++) { // each row is independent
    for (int g = 0; g < ncols; g+=group_len*FILTER_DIM) { // reuse groups ( see || above )
      for (int f = 0; f < FILTER_DIM; f++) { // number of elements per core ( see | above )
        for (int c = 0; c < group_len; c++) { // vector fetch
          int thisCol = f * group_len + c;
          int replCol = f + c * FILTER_DIM;
          int thisIdx = r * ncols + thisCol + g;
          int replIdx = r * ncols + replCol + g;
          a_re[thisIdx] = a[replIdx];
        }
      }
    }
  }
  for (int i = 0; i < ncols; i++) {
    printf("%d ", a_re[i]);
  }
  printf("\n");
  for (int i = ncols; i < 2*ncols; i++) {
    printf("%d ", a_re[i]);
  }
  printf("\n");
  for (int i = 2*ncols; i < 3*ncols; i++) {
    printf("%d ", a_re[i]);
  }
  printf("\n");
  #endif

  // filter
  // 1 2 3
  // 4 5 6
  // 7 8 9
  for (int i = 0; i < FILTER_DIM * FILTER_DIM; i++) {
    b[i] = i + 1;
  }

  // result
  for (int i = 0; i < (nrows - boundOffset) * (ncols /*- boundOffset*/); i++) {
    c[i] = 0;
  }
  
  /*--------------------------------------------------------------------
  * Pack argument for kernel
  *-------------------------------------------------------------------*/  

  // initialize the arguments to send to each device core
  Kern_Args **kern_args = (Kern_Args**)malloc(sizeof(Kern_Args*) * num_cores);

  for (int y = 0; y < cores_y; y++) {
    for (int x = 0; x < cores_x; x++){
      int i = x + y * cores_x; 
      #ifndef REUSE
      kern_args[i] = construct_args(a_re, b, c, nrows, ncols, x, y, cores_x, cores_y);
      #else
      kern_args[i] = construct_args(a, b, c, nrows, ncols, x, y, cores_x, cores_y);
      #endif
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
  
  for (int row = 0; row < nrows - boundOffset; row++) {
    for (int col = 0; col < ncols - boundOffset; col++) {
      int cexp = 0;
      for (int k1 = 0; k1 < FILTER_DIM; k1++) {
        for (int k2 = 0; k2 < FILTER_DIM; k2++) {
          int aIdx = (row + k1) * ncols + (col + k2);
          int bIdx = k1 * FILTER_DIM + k2;
          cexp += b[bIdx] * a[aIdx];
        }
      }
      if (c[row * (ncols /*- boundOffset*/) + col] != cexp) {
        printf("%d != %d @ row %d cold %d\n", c[row * (ncols /*- boundOffset*/) + col], cexp, row, col);
        printf("[[FAIL]]\n");
        // return 1;
      }
      else {
        printf("%d == %d @ row %d cold %d\n", c[row * (ncols /*- boundOffset*/) + col], cexp, row, col);
      }
    }
  }
  
  free(a_ptr);
  free(b_ptr);
  free(c_ptr);

  #ifndef REUSE
  free(a_re_ptr);
  #endif
  
  printf("[[SUCCESS]]\n");
  
  
  return 0;
}
