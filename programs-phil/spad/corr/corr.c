#include <stdlib.h>
#include <stdio.h>

#include "pthread_launch.h"
#include "corr.h"
#include "spad.h"
#include "../../common/bind_defs.h"
#include "token_queue.h"
#include "group_templates.h"

#include "corr_kernel.h"

inline int min(int a, int b) {
  if (a > b) {
    return b;
  }
  else {
    return a;
  }
}

// use this to chunk data among vector groups
// https://stackoverflow.com/questions/3407012/c-rounding-up-to-the-nearest-multiple-of-a-number
int roundUp(int numToRound, int multiple) {
  if (multiple == 0) {
    return numToRound;
  }

  int remainder = abs(numToRound) % multiple;
  if (remainder == 0) {
    return numToRound;
  }

  if (numToRound < 0) {
    return -(abs(numToRound) - remainder);
  }
  else {
    return numToRound + multiple - remainder;
  }
}

void __attribute__((optimize("-fno-inline")))
corr_manycore_1(DTYPE *data, DTYPE *symmat, DTYPE *mean, DTYPE *stddev, int m, int n,
              int start, int end, int ptid)
{
  double eps = 0.1f;

  DTYPE mean_temp=0;
  DTYPE stddev_temp=0;
  DTYPE data_temp;
  for (int i = start; i < end; i++){
    //mean
    mean_temp = 0;
    for (int j = 0; j < n; j++)
      mean_temp += data[i*n+j];
    mean_temp /= n;
    mean[i]=mean_temp;

    //stddev
    stddev_temp = 0;
    for (int j = 0; j < n; j++)
      stddev_temp += (data[i*n+j]-mean_temp)*(data[i*n+j]-mean[i]);
    stddev_temp = stddev_temp/n;
    stddev_temp = sqrt(stddev_temp);
    stddev_temp = stddev_temp <= eps ? 1.0 : stddev_temp;
    stddev[i] = stddev_temp;

    //center
    for (int j = 0; j < n; j++){
      data_temp = data[i*n+j]-mean_temp;
      data[i*n+j] = data_temp/(sqrt(n)*stddev_temp);
    }

    symmat[i*m+i]=1; //make diagonal 1 for the vectors it is assigned
  }
    
}

void __attribute__((optimize("-fno-inline")))
corr_manycore_2(DTYPE *data, DTYPE *symmat, DTYPE *mean, DTYPE *stddev, int m, int n,
              int start, int stride, int ptid)
{

  DTYPE sym_temp=0;
  for (int i1 = start; i1 < m-1; i1+=stride){
    for (int i2 = i1+1; i2 < m; i2++){
      sym_temp=0;
      for(int j=0; j<n; j++){
        sym_temp+=data[i1*n+j]*data[i2*n+j];
      }
      symmat[i1*m+i2]=sym_temp;
      symmat[i2*m+i1]=sym_temp;
    }
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


void kernel(DTYPE *data, DTYPE *symmat, DTYPE *mean, DTYPE *stddev, int m, int n,
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
  int used=0;


  #ifdef _VEC
  #if VEC_LEN==4
  template_info_t tinfo = init_template_4x4_2x2();
  #elif VEC_LEN==16
  template_info_t tinfo = init_template_8x8_4x4();
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
    //do work division here
    int alignment = VEC_LEN; //each group should have elements of multiple of this number
    start = roundUp((unique_id + 0) * m / total_groups, alignment); 
    end = roundUp((unique_id + 1) * m / total_groups, alignment); 
  }

  #else
  vdim_x = 1;
  vdim_y = 1;
  vtid_x = 0;
  vtid_y = 0;
  vtid   = 0;
  //do work division here
  start  = ( ( ptid + 0 ) * m ) / pdim;
  end    = ( ( ptid + 1 ) * m ) / pdim;
  used = 1;
  #endif


  // linearize some fields
  vdim = vdim_x * vdim_y;
  int orig = orig_x + orig_y * dim_x;

  // get behavior of each core
  #ifdef _VEC
  int mask = getSIMDMask(master_x, master_y, orig_x, orig_y, vtid_x, vtid_y, vdim_x, vdim_y, is_da);
  #else
  int mask = 0;
  #endif


  // setup token queues
  // TODO lightweight scratchpad memory allocator?
  #ifndef _VEC
  int spmOffset = 100; //Sp offset for tokens
  #else
  int spmOffset = REGION_SIZE*NUM_REGIONS; //keep it in non region area
  #endif
  int bufSize = TOKEN_LEN;
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



  // region based mask for scratchpad
#ifdef _VEC
  int prefetchMask = (NUM_REGIONS << PREFETCH_NUM_REGION_SHAMT) | (REGION_SIZE << PREFETCH_REGION_SIZE_SHAMT);
  PREFETCH_EPOCH(prefetchMask);
#endif

// only let certain tids continue
  // if (used == 0) return;


  // save the stack pointer to top of spad and change the stack pointer to point into the scratchpad
  // reset after the kernel is done
  // do before the function call so the arg stack frame is on the spad
  // store the the current spAddr to restore later

  unsigned long long *spTop = getSpTop(ptid);
  // // guess the remaining of the part of the frame (n) that might be needed?? here n = 30
  spTop -= 30;

  unsigned long long stackLoc;
  unsigned long long temp;
  #pragma GCC unroll(30)
  for(int i=0;i<30;i++){
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
      tril_corr_vec_1(mask, data, symmat, mean, stddev, m, n, start, end, vtid, vdim, ptid);
    #else
      corr_manycore_1(data, symmat, mean, stddev, m, n, start, end, ptid);
    #endif
  }


  pthread_barrier_wait(&start_barrier);
  if (used == 0) goto stack_end;
  //redistribute work for 2nd kernel
  #ifdef _VEC
  start = unique_id*VEC_LEN;
  int stride = total_groups*VEC_LEN;
  #else
  start  = ptid;
  int stride = pdim;
  #endif

  #if defined _VEC
    tril_corr_vec_2(mask,data, symmat, mean, stddev, m, n, start, stride, vtid, vdim, ptid);
  #else
    corr_manycore_2(data, symmat, mean, stddev, m, n, start, stride, ptid);
  #endif

  stack_end:
  // restore stack pointer
  asm volatile(
      "addi sp, %[stackTop], 0\n\t" ::[stackTop] "r"(stackLoc));
}

// helper functions
Kern_Args *construct_args(DTYPE *data, DTYPE *symmat, DTYPE *mean, DTYPE *stddev, int m, int n,
                          int tid_x, int tid_y, int dim_x, int dim_y)
{

  Kern_Args *args = (Kern_Args *)malloc(sizeof(Kern_Args));

  args->data = data;
  args->symmat = symmat;
  args->mean = mean; 
  args->stddev = stddev;
  args->m = m;
  args->n = n;
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

  kernel(a->data, a->symmat, a->mean, a->stddev, a->m, a->n,
         a->tid_x, a->tid_y, a->dim_x, a->dim_y);


  if (a->tid_x == 0 && a->tid_y == 0)
  {
    stats_off();
  }

  return NULL;
}
