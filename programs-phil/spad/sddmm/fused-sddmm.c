#include <stdlib.h>
#include <stdio.h>

#include "fused-sddmm.h"

inline int sparse_mat_get_x(sparse_mat_t *mat, int nnz) {
  return mat->nz_idxs[nnz];
}

inline int sparse_mat_get_y(sparse_mat_t *mat, int nnz) {
  return mat->idx_ptrs[nnz];
}


// t x m @ n x t ( x -- y )
// b is the transpose (not to be confused with the inverse!)
void kernel(data_t *a, data_t *b_t, sparse_mat_t *c,
    sparse_mat_t *mask, int m, int n, int t, int tid_x, int tid_y, 
    int threads_x, int threads_y, int start_row, int end_row) {
  
  for (int i = start_row; i < end_row; i++) {
      
    /*----------------------------------------------------------------
    * Dot product
    *---------------------------------------------------------------*/
    
    // get the row of mat a and col of mat b
    int row_num = sparse_mat_get_y(mask, i);
    int col_num = sparse_mat_get_x(mask, i);
    
    // do the dot product between the columns and rows
    data_t dot = 0.0f;
    int a_y_idx = row_num * t;
    int b_y_idx = col_num * t;
    for (int k = 0; k < t; k++) {
      #ifdef FIXED_POINT
      dot += fixed_mul(a[a_y_idx + k], b_t[b_y_idx + k]);
      #else
      dot += a[a_y_idx + k] * b_t[b_y_idx + k];
      #endif
    }
    
    /*----------------------------------------------------------------
    * Fused element wise divide
    *---------------------------------------------------------------*/
    
    data_t val   = sparse_mat_get_val(mask, i);
      
    #ifdef FIXED_POINT
    data_t fused = fixed_div(val, dot);
    #else
    data_t fused = val / dot;
    #endif    

    //if (temp == 0) printf("fused: %u\n", fused);
    
    /*----------------------------------------------------------------
    * Store the sparse output
    *---------------------------------------------------------------*/
    
    // write the value of the old matrix
    sparse_mat_set_nz_val(c, i, fused);
      
  }
  
}


Kern_Args *construct_args(data_t *a, data_t *b_t, sparse_mat_t *c,
    sparse_mat_t *mask, int m, int n, int t, int tid_x, int tid_y, 
    int threads_x, int threads_y, int start_row, int end_row) {
      
  Kern_Args *args = (Kern_Args*)malloc(sizeof(Kern_Args));
  
  args->a         = a;
  args->b_t       = b_t;
  args->c         = c;
  args->mask      = mask;
  args->m         = m;
  args->n         = n;
  args->t         = t;
  args->tid_x     = tid_x;
  args->tid_y     = tid_y;
  args->threads_x = threads_x;
  args->threads_y = threads_y;
  args->start_row = start_row;
  args->end_row   = end_row;
  
  return args;
      
}

void *pthread_kernel(void *args) {
  Kern_Args *a = (Kern_Args*)args;
  
  kernel(a->a, a->b_t, a->c, a->mask, a->m, a->n, a->t,
      a->tid_x, a->tid_y, a->threads_x, a->threads_y, a->start_row, a->end_row);
      
  return NULL;
}
