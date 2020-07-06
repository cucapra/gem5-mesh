#include <stdlib.h>
#include <stdio.h>

#include "pthread_launch.h"
#include "atax.h"
#include "spad.h"
#include "bind_defs.h"
#include "token_queue.h"
#include "group_templates.h"
#include "util.h"

#include "atax_kernel.h"

#define TOKEN_LEN 8

// #define SERIAL_OUTPUT_REDUCE
#define PARALLEL_OUTPUT_REDUCE

void __attribute__((optimize("-fno-inline")))
atax_manycore(DTYPE *a, DTYPE *_x, DTYPE *_y_partial, DTYPE *ax, int nx, int ny,
      int nx_start, int nx_end, int tid)
{
    DTYPE temp;
    DTYPE *partial_prod = _y_partial + tid*ny;

    // for (int i = 0; i < ny; i++)
    //   _y_partial[i] = 0;
    for (int i = nx_start; i < nx_end; i++) {
      temp=0;
      for(int j=0; j<ny; j++){
        temp += a[i*ny+j] * _x[j];
      }
      STORE_NOACK(temp, ax + i, 0);
      for(int j=0; j<ny; j++){
        partial_prod[j] += a[i*ny+j] * temp;
        // _y_partial[j] += a[i*ny+j] * temp;
      }
    }
}

//---------ASSUMES INT?? ------------//
// parallel reduction in a dataflow like manner using token queues
void reduce_scalar_manycore(int partialSum, DTYPE *c, int ptid, int activeTid, int dim, token_queue_t *cons0, token_queue_t *cons1, token_queue_t *prod) {

  int sum = partialSum;

  // one core is responsible for writing back the final value at the end
  if (activeTid == 0) {
    // printf("tid %d atid %d dim %d special wait for %p\n", ptid, activeTid, dim, get_pair_base_ptr(cons1, ptid));
    int t = wait_tokens_consumer(cons1, 1, ptid);
    int *data = (int*)get_token(cons1, t, ptid);
    sum += data[0];
    *c += sum;
    consume_tokens(cons1, 1, ptid);
  }
  // in general cores recv two values in input token queues and writes a value to the next core
  else {
    // only lower half consumes data
    if (activeTid < dim / 2) {
      // printf("tid %d atid %d dim %d wait for %p %p\n", ptid, activeTid, dim, get_pair_base_ptr(cons0, ptid), get_pair_base_ptr(cons1, ptid));
      int t0 = wait_tokens_consumer(cons0, 1, ptid);
      int t1 = wait_tokens_consumer(cons1, 1, ptid);
      int *data0 = (int*)get_token(cons0, t0, ptid);
      int *data1 = (int*)get_token(cons1, t1, ptid);
      sum += data0[0] + data1[0];
      consume_tokens(cons0, 1, ptid);
      consume_tokens(cons1, 1, ptid);
    }

    // everyone produces, except for tid0 who does the writeback
    // printf("tid %d atid %d dim %d produce for %p\n", ptid, activeTid, dim, get_pair_base_ptr(prod, ptid));
    int tokenOffset = wait_tokens_producer(prod, 1, ptid);
    set_token(prod, sum, tokenOffset, ptid);
    produce_tokens(prod, 1, ptid);
  }

}

void reduce_vector_manycore(DTYPE* partialVec, DTYPE *c, int ptid, int activeTid, int dim, token_queue_t *cons0, 
    token_queue_t *cons1, token_queue_t *prod, int numElements) {

  // one core is responsible for writing back the final value at the end
  if (activeTid == 0) {
    // printf("tid %d atid %d dim %d special wait for %p\n", ptid, activeTid, dim, get_pair_base_ptr(cons1, ptid));
    for(int i=0; i<numElements; i+=TOKEN_LEN){
      int t = wait_tokens_consumer(cons1, TOKEN_LEN, ptid);
      for(int j=0; j<TOKEN_LEN; j++){
        int *data = (int*)get_token(cons1, t+j, ptid);
        partialVec[i+j] += data[0];
        c[i+j] += partialVec[i+j];
      }
      consume_tokens(cons1, TOKEN_LEN, ptid);
    }
  }
  // in general cores recv two values in input token queues and writes a value to the next core
  else {
    // only lower half consumes data
    if (activeTid < dim / 2) {
      // printf("tid %d atid %d dim %d wait for %p %p\n", ptid, activeTid, dim, get_pair_base_ptr(cons0, ptid), get_pair_base_ptr(cons1, ptid));
      for(int i=0; i<numElements; i+=TOKEN_LEN){
        int t0 = wait_tokens_consumer(cons0, TOKEN_LEN, ptid);
        int t1 = wait_tokens_consumer(cons1, TOKEN_LEN, ptid);
        for(int j=0; j<TOKEN_LEN; j++){
          asm volatile("nop");
          int *data0 = (int*)get_token(cons0, t0+j, ptid); //use offset here to get data in circ buffer
          int *data1 = (int*)get_token(cons1, t1+j, ptid);
          partialVec[i+j] += data0[0] + data1[0]; //data[0] and not to be used as data[i] due to circ buffer on Spad
        }
        consume_tokens(cons0, TOKEN_LEN, ptid);
        consume_tokens(cons1, TOKEN_LEN, ptid);

        // everyone produces, except for tid0 who does the writeback
        int tokenOffset = wait_tokens_producer(prod, TOKEN_LEN, ptid); // TOKEN_LEN< buffer size, hence safe
        for(int j=0; j<TOKEN_LEN; j++) set_token(prod, partialVec[i+j], tokenOffset+j, ptid);
        produce_tokens(prod, TOKEN_LEN, ptid);
      }
    }
    else{

      // everyone produces, except for tid0 who does the writeback
      // printf("tid %d atid %d dim %d produce for %p\n", ptid, activeTid, dim, get_pair_base_ptr(prod, ptid));
      for(int i=0; i<numElements; i+=TOKEN_LEN){ //go in steps of tokens < buffer size
        int tokenOffset = wait_tokens_producer(prod, TOKEN_LEN, ptid); // TOKEN_LEN< buffer size, hence safe
        for(int j=0; j<TOKEN_LEN; j++) set_token(prod, partialVec[i+j], tokenOffset+j, ptid);
        produce_tokens(prod, TOKEN_LEN, ptid);
      }
    }
  }

}

void reduce_parallel(DTYPE* partial, DTYPE *out, int n, int ptid, int pdim, int unique_id, int total_groups, int vtid,
                      int vdim_x, int vdim_y, int phys_dim_x, template_info_t *tinfo){

  #ifndef _VEC
  //cores are used
  int start = (ptid + 0) * n / pdim; 
  int end = (ptid + 1) * n / pdim;

  DTYPE temp;
  for(int i=start; i<end; i++){
    temp=0;
    for(int j=0; j<pdim; j++){
      temp+=partial[j*n+i];
    }
    out[i]+=temp;
  }

  #else
  int start = (unique_id + 0) * n / total_groups;
  int end = (unique_id + 1) * n / total_groups;
  start+=vtid;

  DTYPE temp;
  for(int i=start; i<end; i+=VEC_LEN){
    temp=0;
    for(int j=0; j<pdim; j++){
      temp+=partial[j*n+i];
    }
    // for(int j=0; j<total_groups; j++){
    //   for(int x=0; x<vdim_x; x++){
    //     for(int y=0; y<vdim_y; y++){
    //       int p = get_ptid_from_group(tinfo, j, x, y, phys_dim_x);
    //       temp+=partial[p*n+i];
    //     }
    //   }
    // }
    out[i]+=temp;
  }
  #endif
  return;

}

// based on id (in manycore its the ptid, in vector its the group id + vtid)
// figure out where to send your data to be reduced by another core
#ifndef _VEC
int get_reduction_dest(int src_id) {
  // pattern is to half your id and send to that id
  return src_id / 2;
}
#else
// a little more complicated to get when there's vector groups
int get_reduction_dest(template_info_t *tinfo, int group_id, int vid_x, int vid_y, int virt_dim_x, int phys_dim_x, int *active_tid) {
  // get a flat id from group and vid
  int vid = vid_y * virt_dim_x + vid_x;
  int src = group_id * VEC_LEN + vid;
  // divide by two as in manycore case
  int dest = src / 2;
  // get the ptid corresponding to this group
  int dest_group_id = dest / VEC_LEN;
  int dest_vid = dest % VEC_LEN;
  int dest_vid_x = dest_vid % virt_dim_x;
  int dest_vid_y = dest_vid / virt_dim_x;
  int ptid = get_ptid_from_group(tinfo, dest_group_id, dest_vid_x, dest_vid_y, phys_dim_x);
  
  *active_tid = src;

  return ptid;
}
#endif




void __attribute__((optimize("-freorder-blocks-algorithm=simple"))) kernel(
    DTYPE *a, DTYPE *_x, DTYPE *_y, DTYPE *ax, DTYPE *_y_partial, int nx, int ny,
    int tid_x, int tid_y, int dim_x, int dim_y)
{

  // start recording all stats (all cores)
  if (tid_x == 0 && tid_y == 0)
  {
    stats_on();
  }


  // linearize tid and dim
  int tid = tid_x + tid_y * dim_x;
  int dim = dim_x * dim_y;

  // split into physical and virtual tids + dim
  int ptid_x = tid_x;
  int ptid_y = tid_y;
  int ptid = tid;
  int pdim_x = dim_x;
  int pdim_y = dim_y;
  int pdim = dim;
  int vtid_x = 0;
  int vtid_y = 0;
  int vtid = 0;
  int vdim_x = 0;
  int vdim_y = 0;
  int vdim = 0;
  int start = 0;
  int end = 0;
  int orig_x = 0;
  int orig_y = 0;
  int is_da = 0;
  int master_x = 0;
  int master_y = 0;
  int unique_id = 0;
  int total_groups = 0;
  int used = 0;
  template_info_t tinfo;

  #ifdef _VEC
  #if VEC_LEN==4
  tinfo = init_template_4x4_2x2();
  int ptid_group[4];
  #elif VEC_LEN==16
  tinfo = init_template_8x8_4x4();
  int ptid_group[16];
  #endif
  core_config_info_t cinfo = vector_group_template(ptid_x, ptid_y, pdim_x, pdim_y, &tinfo);

  vtid = cinfo.vtid;
  vtid_x = cinfo.vtid_x;
  vtid_y = cinfo.vtid_y;
  vdim_x = cinfo.vdim_x;
  vdim_y = cinfo.vdim_y;
  orig_x = cinfo.orig_x;
  orig_y = cinfo.orig_y;
  is_da  = cinfo.is_scalar;
  master_x = cinfo.master_x;
  master_y = cinfo.master_y;
  unique_id = cinfo.unique_id;
  total_groups = cinfo.total_groups;
  used = cinfo.used;

  if(used){
    int alignment = VEC_LEN;
    start = roundUp((unique_id + 0) * nx / total_groups, alignment); 
    end = roundUp((unique_id + 1) * nx / total_groups, alignment); 

    if(is_da==1){
    for(int i=0; i<vdim_y;i++){
      for(int j=0; j<vdim_x; j++){
        ptid_group[i*vdim_x+j] = get_ptid_from_group(&tinfo, unique_id,j,i,dim_x);
        // if (ptid==0) printf("Ptid: %d\n", ptid_group_[i*vdim_x+j]);
      }
    }
  }
  }

  #else
  vdim_x = 1;
  vdim_y = 1;
  vtid_x = 0;
  vtid_y = 0;
  vtid   = 0;
  start  = ( ( ptid + 0 ) * nx ) / pdim;
  end    = ( ( ptid + 1 ) * nx ) / pdim;
  used = 1;
  #endif


  // linearize some fields
  vdim = vdim_x * vdim_y;

  // get behavior of each core
  #ifdef _VEC
  int mask = getSIMDMask(&cinfo);
  #else
  int mask = 0;
  #endif




  // setup token queues
  // TODO lightweight scratchpad memory allocator?
  #ifndef _VEC
  int spmOffset = 100;
  #else
  int spmOffset = REGION_SIZE*NUM_REGIONS;
  #endif
  int bufSize = 10;
  int tqWords = bufSize + 4 + 2 + 2; // +2 extra just to be safe

  // each spm gets two consumer queues and one producer queue for a reduction
  token_queue_t consumer0;
  token_queue_t consumer1;
  token_queue_t producer;

  #ifndef _VEC
  int activeTid = ptid;
  int pairTid = get_reduction_dest(ptid); 
  int active_dim = pdim;
  #else
  int activeTid;
  int pairTid = get_reduction_dest(&tinfo, unique_id, vtid_x, vtid_y, vdim_x, pdim_x, &activeTid);
  int active_dim = total_groups * VEC_LEN;
  #endif

  init_token_queue_consumer(spmOffset + tqWords * 0, bufSize, ptid, &consumer0);
  init_token_queue_consumer(spmOffset + tqWords * 1, bufSize, ptid, &consumer1);

  // important to skip this if the core won't be used b/c might overwrite the link ptr
  if (used && !is_da) {
    // figure out which token queue you're going to be sending to
    int pairOffset;
    if (activeTid % 2 == 0) {
      pairOffset = spmOffset + tqWords * 0;
    }
    else {
      pairOffset = spmOffset + tqWords * 1;
    }
    init_token_queue_producer(spmOffset + tqWords * 2, pairOffset, bufSize, ptid, pairTid, &producer);
  }

// do after tokens to avoid stalling due to region not ready
// region based mask for scratchpad
#ifdef _VEC
  int prefetchMask = (NUM_REGIONS << PREFETCH_NUM_REGION_SHAMT) | (REGION_SIZE << PREFETCH_REGION_SIZE_SHAMT);
  PREFETCH_EPOCH(prefetchMask);
#endif

    // single barrier before kernel start
  pthread_barrier_wait(&start_barrier);

  // only let certain tids continue
  // if (used == 0) return; moved this part later

  // save the stack pointer to top of spad and change the stack pointer to point into the scratchpad
  // reset after the kernel is done
  // do before the function call so the arg stack frame is on the spad
  // store the the current spAddr to restore later

  unsigned long long *spTop = getSpTop(ptid);
  // // // guess the remaining of the part of the frame (n) that might be needed?? here n = 30
  spTop -= 60;

  unsigned long long stackLoc;
  unsigned long long temp;
  #pragma GCC unroll(60)
  for(int i=0;i<60;i++){
    asm volatile("ld t0, %[id](sp)\n\t"
                "sd t0, %[id](%[spad])\n\t"
                : "=r"(temp)
                : [id] "i"(i*8), [spad] "r"(spTop));
  }
  asm volatile (// save the stack ptr
      "addi %[dest], sp, 0\n\t"
      // overwrite stack ptr
      "addi sp, %[spad], 0\n\t"
      : [ dest ] "=r"(stackLoc)
      : [ spad ] "r"(spTop));


  


  if(used!=0){
    #if defined _VEC
      tril_atax(mask,a,_x,_y_partial,ax,nx,ny,start,end,ptid,vtid,vdim,ptid_group);
    #else
      atax_manycore(a,_x,_y_partial,ax,nx,ny,start,end,ptid);
    #endif

  }

  // asm volatile(
  //     "addi sp, %[stackTop], 0\n\t" ::[stackTop] "r"(stackLoc));

  // if(ptid==0) stats_on();

  //requires barrier since each core needs values from all other cores
  #ifdef PARALLEL_OUTPUT_REDUCE
  pthread_barrier_wait(&start_barrier);
  #endif

  if (used == 0) goto stack_end; //return;
  #ifdef _VEC
  if (is_da) goto stack_end; //return; // scalar cores don't have data to accumulate so should not partcipate
  #endif
  
  
  #ifdef SERIAL_OUTPUT_REDUCE
  DTYPE *partialVec = _y_partial + ptid*ny;
  #if TOKEN_LEN == 1
  for(int i=0; i<ny; i++){
      int partial_sum=partialVec[i];
      reduce_scalar_manycore(partial_sum, _y+i, ptid, activeTid, activeDim, consumer0, consumer1, producer);
  }
  #else
  reduce_vector_manycore(partialVec, _y, ptid, activeTid, active_dim, &consumer0, &consumer1, &producer,ny);
  #endif
  #elif defined PARALLEL_OUTPUT_REDUCE
  reduce_parallel(_y_partial, _y, ny, ptid, pdim, unique_id, total_groups, vtid,
                      vdim_x, vdim_y, pdim_x, &tinfo);
  #endif


  stack_end:
  // restore stack pointer
  asm volatile(
      "addi sp, %[stackTop], 0\n\t" ::[stackTop] "r"(stackLoc));
  return;

  
  
}

// helper functions
Kern_Args *construct_args(DTYPE *a, DTYPE *_x, DTYPE *_y, DTYPE *ax, DTYPE *_y_partial, 
                          int nx, int ny, int tid_x, int tid_y, int dim_x, int dim_y)
{

  Kern_Args *args = (Kern_Args *)malloc(sizeof(Kern_Args));

  args->a = a;
  args->_x = _x;
  args->_y = _y;
  args->ax = ax;
  args->_y_partial = _y_partial;
  args->nx = nx;
  args->ny = ny;
  args->tid_x = tid_x;
  args->tid_y = tid_y;
  args->dim_x = dim_x;
  args->dim_y = dim_y;

  return args;
}

void *pthread_kernel(void *args)
{
  // guarentee one thread goes to each core, by preventing any threads
  // from finishing early

  pthread_barrier_wait(&start_barrier);

  // call the spmd kernel
  Kern_Args *a = (Kern_Args *)args;

  kernel(a->a, a->_x, a->_y, a->ax, a->_y_partial, a->nx, a->ny,
         a->tid_x, a->tid_y, a->dim_x, a->dim_y);


  // pthread_barrier_wait(&start_barrier);

  if (a->tid_x == 1 && a->tid_y == 0)
  {
    stats_off();
  }

  return NULL;
}
