#include <stdio.h>
#include <stdlib.h>
#include <iostream>

// include the relevant kernels
#include "weirdo-kernel.h"
#include "sparse_mat.h"

#include "sddmm_data.h"

/*#if defined(_PARALLEL_PREPROC) || defined(_PARALLEL_CHUNKED) || defined(_PARALLEL_COO)
#define PARALLEL 1
#endif*/

/*#ifdef PARALLEL
#include <pthread.h>
#define NUM_THREADS_X 4
#define NUM_THREADS_Y 4
#endif*/
#include <pthread.h>

// TODO only supported in BRG flow
inline void toggle_stats(bool on)
{
 /*__asm__ volatile ("csrw 0x7C1, %0;"
                    :
                    : "r" (on)
                    :);
   */                 
}
// 4 kB
#define SP_SIZE 4096
#define HOST_CPUS 1
#define N_SPS (HOST_CPUS + _N_CPUS)

// SPM space mapped to scratchpads
uint8_t spm[SP_SIZE * N_SPS] __attribute__ ((section(".spm")));
uint8_t*  spm_base_ptr_arr     [N_SPS];
uint8_t*  spm_next_ptr_arr     [N_SPS];
uint64_t* spm_base_addr_ptr_arr[N_SPS];
uint32_t* spm_go_flag_ptr_arr  [N_SPS];
uint32_t* spm_done_flag_ptr_arr[N_SPS];

template <class T>
T* spm_alloc(uint8_t** base_pp, size_t size = 1) {
  T* p = (T*) (*base_pp);
  (*base_pp) += sizeof(T) * size;
  return p;
}

int main(int argc, char *argv[]) {
  
  #ifdef FIXED_POINT
  set_range(0.0f, 20.0f);
  //return 0;
  #endif
  
  std::cout << "N_CPUS = " << _N_CPUS << ". N_XCELS = " << 0 << "\n";
  
  /*--------------------------------------------------------------------
   * Init scratchpad xcels
   *------------------------------------------------------------------*/ 
  
  for (size_t i = 0; i < N_SPS; ++i) {
    // get important spad pointers to interface with the respective xcel
    spm_base_ptr_arr[i]       = spm + i * SP_SIZE;
    spm_next_ptr_arr[i]       = spm_base_ptr_arr[i];
    spm_base_addr_ptr_arr[i]  = spm_alloc<uint64_t>(&(spm_next_ptr_arr[i]));
    spm_go_flag_ptr_arr[i]    = spm_alloc<uint32_t>(&(spm_next_ptr_arr[i]));
    spm_done_flag_ptr_arr[i]  = spm_alloc<uint32_t>(&(spm_next_ptr_arr[i]));
    
    // send the base addr of the scratchpad to the scratchpad
    // do this so that xcel sees this message and knows where its scratchpad is
    *(spm_base_addr_ptr_arr[i]) = (uint64_t)(spm_base_ptr_arr[i]);
  }
 
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
  
  //sparse_mat_t *mask = (sparse_mat_t*)malloc(sizeof(sparse_mat_t));
  sparse_mat_t *mask = new sparse_mat_t();
  
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
  * Thread init
  *-------------------------------------------------------------------*/
  
  //int num_threads   = num_threads_x * num_threads_y;
  int num_threads     = _N_CPUS;
  pthread_t **threads = 
    (pthread_t**)malloc(sizeof(pthread_t*) * num_threads);
  
  sddmm_args_t **args =
    (sddmm_args_t**)malloc(sizeof(sddmm_args_t*) * num_threads);
  
  for (int tid = 0; tid < _N_CPUS; tid++) {
    // create the arguments to pass to each thread
    int nnz_per_core = mask->nnz / num_threads;
    int remainder = mask->nnz % num_threads;
    int start_nnz = nnz_per_core * tid;
    int end_nnz = nnz_per_core * (tid + 1);
    if (tid == num_threads - 1) end_nnz += remainder;
    //printf("%d %d -> %d\n", mask->nnz, start_nnz, end_nnz);
    sddmm_args_t *t_args = construct_sddmm_args(a, b_t, c, mask, m, n, 
      t, tid, 1, _N_CPUS, 1, start_nnz, end_nnz);
      
    args[tid] = t_args;
      
    // initialize a thread
    threads[tid] = (pthread_t*)malloc(sizeof(pthread_t));
  }

  /*--------------------------------------------------------------------
  * Run the kernel
  *-------------------------------------------------------------------*/
  
  printf("Begin kernel\n");
  
  toggle_stats(true);
  
  for (int i = 0; i < num_threads; i++) {
    pthread_create(threads[i], NULL, pthread_sddmm_fused_div, (void*)(args[i]));
  }
  
  // wait for all of the threads to finish
  for (int i = 0; i < num_threads; i++) {
    pthread_join(*(threads[i]), NULL);
  }
  
  toggle_stats(false);
  
  /*--------------------------------------------------------------------
  * Check result and cleanup
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
