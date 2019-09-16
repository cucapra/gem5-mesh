/*
 * Functions for working with a compressed matrix
 * 
 * Currently using CSR (as in original python)
 */ 
 

#include <stdlib.h>
#include <stdio.h>

#include "sparse_mat.h"

//sparse_mat_t *create_from_dense(float *dense_matrix) {}

sparse_mat_t *create_from_sparse(double *nz_data, int *idx_ptrs, 
    int *nz_cols, int m, int n, int nnz) {
  sparse_mat_t *mat = (sparse_mat_t*)malloc(sizeof(sparse_mat_t));
  
  // x
  mat->m = m;
  // y
  mat->n = n;
  mat->nnz = nnz;
  
  // hardcopy all of the arrays
  mat->idx_ptrs = (int*)malloc(sizeof(int) * n + 1);
  for (int i = 0; i < n + 1; i++) {
    mat->idx_ptrs[i] = idx_ptrs[i];
  }
  
  mat->nz_idxs = (int*)malloc(sizeof(int) * nnz);
  mat->nz_data = (data_t*)malloc(sizeof(data_t) * nnz);
  for (int i = 0; i < nnz; i++) {
    mat->nz_idxs[i] = nz_cols[i];
    
    #ifdef FIXED_POINT
    mat->nz_data[i] = float_to_fixed_pt(nz_data[i]);
    //if (i < 100) printf("%f -> %u\n", nz_data[i], mat->nz_data[i]);
    #else
    mat->nz_data[i] = (data_t)nz_data[i];
    #endif
  }
  
  return mat;
}

sparse_mat_t *create_from_sparsef(float *nz_data, int *idx_ptrs, 
    int *nz_cols, int m, int n, int nnz) {
  sparse_mat_t *mat = (sparse_mat_t*)malloc(sizeof(sparse_mat_t));
  
  // x
  mat->m = m;
  // y
  mat->n = n;
  mat->nnz = nnz;
  
  // hardcopy all of the arrays
  mat->idx_ptrs = (int*)malloc(sizeof(int) * n + 1);
  for (int i = 0; i < n + 1; i++) {
    mat->idx_ptrs[i] = idx_ptrs[i];
  }
  
  mat->nz_idxs = (int*)malloc(sizeof(int) * nnz);
  mat->nz_data = (data_t*)malloc(sizeof(data_t) * nnz);
  for (int i = 0; i < nnz; i++) {
    mat->nz_idxs[i] = nz_cols[i];
    
    #ifdef FIXED_POINT
    mat->nz_data[i] = float_to_fixed_pt(nz_data[i]);
    //if (i < 100) printf("%f -> %u\n", nz_data[i], mat->nz_data[i]);
    #else
    mat->nz_data[i] = (data_t)nz_data[i];
    #endif
  }
  
  return mat;
}

void destroy_sparse_mat(sparse_mat_t *mat) {
  free(mat->idx_ptrs);
  free(mat->nz_idxs);
  free(mat->nz_data);
  free(mat);
}

// sets value in the nz_data, don't change anything about where it is in the matrix
void sparse_mat_set_nz_val(sparse_mat_t *mat, int nnz, data_t val) {
  (mat->nz_data)[nnz] = val;
}

int sparse_mat_get_idx(sparse_mat_t *mat, int nnz) {
  return mat->nz_idxs[nnz];
}

int sparse_mat_get_idx_ptr(sparse_mat_t *mat, int row) {
  return mat->idx_ptrs[row];
}

data_t sparse_mat_get_val(sparse_mat_t *mat, int nnz) {
  return mat->nz_data[nnz];
}


bool float_eq(float a, float b) {
  #ifndef FIXED_POINT
  // should be within 0.0001% of each other
  float float_eps = a * 0.001f; // 0.000001f;
  #else
  // fixed point won't be as accurate
  float float_eps = a * 0.001f;
  #endif
  float diff = a - b;
  if ((diff < float_eps) && (diff > -1.0f * float_eps)) {
    return true;
  }
  else {
    return false;
  }
}

bool sparse_mat_equal(sparse_mat_t *a, sparse_mat_t *b) {
  
  if (a->nnz != b->nnz) return false;
  if (a->n != b->n) return false;
  if (a->m != b->m) return false;
  
  for (int i = 0; i < a->nnz; i++) {
    if (a->idx_ptrs[i] != b->idx_ptrs[i]) return false;
  }
  
  for (int i = 0; i < a->nnz; i++) {
    if (a->nz_idxs[i] != b->nz_idxs[i]) return false;
  }
  
  // check values
  for (int i = 0; i < a->nnz; i++) {
    #ifdef FIXED_POINT
    float anz = fixed_pt_to_float(a->nz_data[i]);
    float bnz = fixed_pt_to_float(b->nz_data[i]);
    //printf("%u %u\n", a->nz_data[i], b->nz_data[i]);
    #else
    float anz = a->nz_data[i];
    float bnz = b->nz_data[i];
    #endif
    
    //if (i < 100) printf("%.10f %.10f\n", anz, bnz);
    if (!float_eq(anz, bnz)) { 
      printf("fail on idx: %d ... %.10f!=%.10f\n", i, anz, bnz);
      return false;
    }
  }
  
  return true;
}

void convert_to_coo(sparse_mat_t *mat) {
 
    // save the pointers for the old data
    int *idxs = mat->idx_ptrs;
    
    // names are bad but reuse the old ptrs for x,y list
    // y list
    mat->idx_ptrs = (int*)malloc(sizeof(int) * mat->nnz);
    // x list
    //mat->nz_cols  = (int*)malloc(sizeof(int) * mat->nnz);

    for (int j = 0; j < mat->n; j++){
      for (int i = idxs[j]; i < idxs[j + 1]; i++) {
        int y = j;

        mat->idx_ptrs[i] = y;
      }
    }

    //printf("idx %d\n", idxs[mat->n]);


    free(idxs);
  
}
