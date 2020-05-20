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
  #ifdef VECTOR_LEN
  int nrows = 2 * ( _N_SPS / ( VECTOR_LEN + 1 ) ) + (FILTER_DIM - 1);
  #else
  int nrows = _N_SPS + ( FILTER_DIM - 1);
  #endif
  // reuse has very stingy requirements on what sizes are allowed
  #ifdef REUSE
  int ncols = 3 * ( VECTOR_LEN * FILTER_DIM - (FILTER_DIM - 1)) + (FILTER_DIM - 1) + 23; // factor of (DIM * FILTER_DIM) - ( FILTER_DIM + 1 ), + edge case
  #elif defined(VECTOR_LEN)
  int ncols = 10 * ( VECTOR_LEN * FILTER_DIM ) + (FILTER_DIM - 1) + 23; // factor of DIM * FILTER_DIM (12) + 2... wow i.e. 1214 // vertical needs to be factor of 24 + 2... i.e. 1224 + 2 = 1226
  #else
  int ncols = 122;
  #endif
  
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

  // TODO why can't non reuse version trim the edge??
  int rowOffset = (FILTER_DIM - 1);
  int colOffset = (FILTER_DIM - 1);
  DTYPE *a_ptr, *b_ptr, *c_ptr;
  DTYPE *a = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), nrows * ncols, (void**)&a_ptr);
  DTYPE *b = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), FILTER_DIM * FILTER_DIM, (void**)&b_ptr);
  DTYPE *c = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), (nrows - rowOffset) * (ncols - colOffset), (void**)&c_ptr);

  // image
  for (int i = 0; i < nrows * ncols; i++) {
    a[i] = i + 1;
  }
  // ridicuolous reorder required to do reuse with spatial
  // #ifdef REUSE
  // int group_len = 4;
  // int first_group_size = FILTER_DIM * group_len;
  // int normal_group_size = 1 + (FILTER_DIM * (group_len - 1));
  // int without_initial = ncols - first_group_size;
  // int num_sections = 1 + (without_initial / normal_group_size);
  // DTYPE *a_re_ptr;
  // DTYPE *a_re = (DTYPE*)malloc_cache_aligned(sizeof(DTYPE), nrows * ncols, (void**)&a_re_ptr); 
  // // 0 1 2 3 | 4 5 6 7  | 8 9 10 11 || 12 13 14 15
  // // 0 3 6 9 | 1 4 7 10 | 2 5 8  11 || 12 15 18 21 
  // // if want to scale requires a different reorder. because first core needs to tie into last core
  // // 0 1 2 | 3 6 9 | 4 7 10 | 5 8 11 || 12 | 13 16 19 | 14 17 20 | 15 18 21 || 22 | 
  // for (int r = 0; r < nrows; r++) { // each row is independent
  //   int colIdx = 0;
  //   // sharing sections
  //   for (int section = 0; section < num_sections; section++) {
  //     // always 4 groups per sharing section
  //     // the first group in a section gets special behavior
  //     // and the first group in the first sharing section gets very special behavior
  //     int offset = 0;
  //     int rowStart = r * ncols;
  //     for (int g = 0; g < group_len; g++) {
  //       if (g == 0) {
  //         offset = rowStart + colIdx; // base
  //         if (section == 0) {
  //           for (int i = 0; i < FILTER_DIM; i++) {
  //             int idx = rowStart + colIdx;
  //             a_re[idx] = a[idx];
  //             colIdx++;
  //           }
  //           offset += FILTER_DIM;
  //         }
  //         else {
  //           int idx = rowStart + colIdx;
  //           a_re[idx] = a[idx];
  //           colIdx++;
  //           offset++;
  //         }
  //       }
  //       else {
  //         for (int i = 0; i < (group_len - 1); i++) {
  //           int replIdx = offset + (g - 1) + i * FILTER_DIM;
  //           a_re[rowStart + colIdx] = a[replIdx];
  //           colIdx++;
  //         }
  //       }
  //     }
  //   }
  // }
  // // for (int r = 0; r < nrows; r++) {
  // // for (int i = 0; i < ncols; i++) {
  // //   printf("%d ", a[r * ncols + i]);
  // // }
  // // printf("\n");
  // // }
  // // for (int r = 0; r < nrows; r++) {
  // // for (int i = 0; i < ncols; i++) {
  // //   printf("%d ", a_re[r * ncols + i]);
  // // }
  // // printf("\n");
  // // }
  // #endif

  // filter
  // 1 2 3
  // 4 5 6
  // 7 8 9
  for (int i = 0; i < FILTER_DIM * FILTER_DIM; i++) {
    b[i] = i + 1;
  }

  // result
  for (int i = 0; i < (nrows - rowOffset) * (ncols - colOffset); i++) {
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
      // #ifdef REUSE
      // kern_args[i] = construct_args(a_re, b, c, nrows, ncols, x, y, cores_x, cores_y);
      // #else
      kern_args[i] = construct_args(a, b, c, nrows, ncols, x, y, cores_x, cores_y);
      // #endif
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
  
  for (int row = 0; row < nrows - rowOffset; row++) {
    for (int col = 0; col < ncols - colOffset; col++) {
      int cexp = 0;
      for (int k1 = 0; k1 < FILTER_DIM; k1++) {
        for (int k2 = 0; k2 < FILTER_DIM; k2++) {
          int aIdx = (row + k1) * ncols + (col + k2);
          int bIdx = k1 * FILTER_DIM + k2;
          cexp += b[bIdx] * a[aIdx];
        }
      }
      if (c[row * (ncols - colOffset) + col] != cexp) {
        printf("%d != %d @ row %d cold %d\n", c[row * (ncols - colOffset) + col], cexp, row, col);
        printf("[[FAIL]]\n");
        return 1;
      }
      // else {
        // printf("%d == %d @ row %d cold %d\n", c[row * (ncols - colOffset) + col], cexp, row, col);
      // }
    }
  }
  
  free(a_ptr);
  free(b_ptr);
  free(c_ptr);

  // #ifdef REUSE
  // free(a_re_ptr);
  // #endif
  
  printf("[[SUCCESS]]\n");
  
  
  return 0;
}
