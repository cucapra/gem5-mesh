#ifndef __SPARSE_MAT_H__
#define __SPARSE_MAT_H__

#include <stdint.h>
#include <stdbool.h>

#ifdef FIXED_POINT
#include "fixed_point.h"
typedef fixed_t data_t;
#else
typedef float data_t;
#endif
 
typedef struct sparse_mat_t {
  // the non-zero values in the matrix
  data_t *nz_data;
  // number of non-zero
  int nnz;
  
  // the dimensions of the matrix
  // m is x, n is y
  int m, n;
  
  // m + 1 size, the number of non-zero elements in the ALL of previous rows
  // this is used for lookup into into a row
  int *idx_ptrs;
  
  // nnz size, the column of the correspnding non-zero value
  // or a row in the case of csc
  int *nz_idxs;
  
} sparse_mat_t;


struct Args {
  // ptrs to the global input matrices
  uint64_t mat_a_addr;
  uint64_t mat_b_trans_addr;
  sparse_mat_t mat_mask;
  sparse_mat_t mat_c;
  
  // distribution
  uint32_t start_nnz;
  uint32_t end_nnz;
  
  // dot product length
  uint32_t t;
};



// 'methods'
// https://stackoverflow.com/questions/16850992/call-a-c-function-from-c-code
// any method that's called from a cpp file needs to be denoted as c

#ifdef __cplusplus
extern "C" {
#endif

// constructor
sparse_mat_t *create_from_sparse(double *nz_data, int *idx_ptrs, 
    int *nz_cols, int m, int n, int nnz);

sparse_mat_t *create_from_sparsef(float *nz_data, int *idx_ptrs, 
    int *nz_cols, int m, int n, int nnz);

#ifdef __cplusplus    
}
#endif

// destructor
void destroy_sparse_mat(sparse_mat_t *mat);

bool sparse_mat_equal(sparse_mat_t *a, sparse_mat_t *b);

// setters
void sparse_mat_set_nz_val(sparse_mat_t *mat, int idx, data_t val);
// getters
int sparse_mat_get_idx(sparse_mat_t *mat, int idx);
int sparse_mat_get_idx_ptr(sparse_mat_t *mat, int row);
data_t sparse_mat_get_val(sparse_mat_t *mat, int idx);

void convert_to_coo(sparse_mat_t *mat);

/*// sets value in the nz_data, don't change anything about where it is in the matrix
inline void sparse_mat_set_nz_val(sparse_mat_t *mat, int nnz, data_t val) {
  (mat->nz_data)[nnz] = val;
}

inline int sparse_mat_get_idx(sparse_mat_t *mat, int nnz) {
  return mat->nz_idxs[nnz];
}

inline int sparse_mat_get_idx_ptr(sparse_mat_t *mat, int row) {
  return mat->idx_ptrs[row];
}

inline data_t sparse_mat_get_val(sparse_mat_t *mat, int nnz) {
  return mat->nz_data[nnz];
}*/

#endif
