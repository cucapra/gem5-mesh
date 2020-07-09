#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>

#include "spad.h"
#include "pthread_launch.h"
#include "fused-sddmm.h"
#include "sparse_mat.h"
#include "sddmm_data.h"

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
  * Matrix initialization
  *-------------------------------------------------------------------*/
 
  // dense matrix dimensions
  // m is the y dim, n is the x dim
  int m = sddmm_m;
  int n = sddmm_n;
  int t = sddmm_t;
  
  // two dense input matrices
  float *a = mat_a;
  float *b_t = mat_b;
  
  sparse_mat_t *mask = (sparse_mat_t*)malloc(sizeof(sparse_mat_t));
  
  // TODO if want bigger then may have issues with binary sizes
  // need to assemble the sparse matrices out of the header arrays and params
  mask->m = m;
  mask->n = n;
  mask->nnz = sddmm_nnz;
  mask->nz_data = mask_nz_data;
  mask->idx_ptrs = mask_nz_rows;
  mask->nz_idxs = mask_nz_cols;

  // c matrix will resemble the output matrix in terms of where the non-zero values will be
  // so actually just set them as the same matrix
  sparse_mat_t *c = mask;


  /*--------------------------------------------------------------------
  * Pack argument for kernel
  *-------------------------------------------------------------------*/  

  // initialize the arguments to send to each device core
  Kern_Args **kern_args = (Kern_Args**)malloc(sizeof(Kern_Args*) * num_cores);

  for (int y = 0; y < cores_y; y++) {
    for (int x = 0; x < cores_x; x++){
      int tid = x + y * cores_x;
      
      // create the arguments to pass to each thread
      // TODO should prob move into kernel
      int nnz_per_core = mask->nnz / num_cores;
      int remainder = mask->nnz % num_cores;
      int start_nnz = nnz_per_core * tid;
      int end_nnz = nnz_per_core * (tid + 1);
      if (tid == num_cores - 1) end_nnz += remainder;
      
      kern_args[tid] = construct_args(a, b_t, c, mask, m, n, t, x, y, 
        cores_x, cores_y, start_nnz, end_nnz);
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

  // check the output
  sparse_mat_t exp;
  exp.m = m;
  exp.n = n;
  exp.nnz = c->nnz;
  exp.nz_data = exp_nz_data;
  exp.idx_ptrs = exp_nz_rows;
  exp.nz_idxs = exp_nz_cols;

  if (sparse_mat_equal(c, &exp)) printf("[[SUCCESS]]\n");
  else printf("[[FAIL]]\n");
  
  
  return 0;
}
