#include "reduction.h"

// each spm gets two consumer queues and one producer queue for a reduction
void setup_reduction(token_queue_t *cons0, token_queue_t *cons1, token_queue_t *prod,
    int spmOffset, int bufSize, int ptid, int pdim_x, int pdim_y, int is_vec, core_config_info_t *cinfo, template_info_t *tinfo) {
  // setup token queues
  int tqWords = bufSize + 4 + 2 + 2; // +2 extra just to be safe

  int activeTid;
  int pairTid;
  if (!is_vec) {
    activeTid = ptid;
    pairTid = get_reduction_dest_manycore(ptid); 
    // int active_dim = pdim;
  }
  else {
    int pairTid = get_reduction_dest_vector(tinfo, cinfo->unique_id, cinfo->vtid.x, cinfo->vtid.y, 
      cinfo->vdim.x, pdim_x, &activeTid);
    // int active_dim = total_groups * VEC_LEN;
  }

  init_token_queue_consumer(spmOffset + tqWords * 0, bufSize, ptid, cons0);
  init_token_queue_consumer(spmOffset + tqWords * 1, bufSize, ptid, cons1);

  // important to skip this if the core won't be used b/c might overwrite the link ptr
  if (cinfo->used && !cinfo->is_scalar) {
    // figure out which token queue you're going to be sending to
    int pairOffset;
    if (activeTid % 2 == 0) {
      pairOffset = spmOffset + tqWords * 0;
    }
    else {
      pairOffset = spmOffset + tqWords * 1;
    }
    init_token_queue_producer(spmOffset + tqWords * 2, pairOffset, bufSize, ptid, pairTid, prod);
  }

}

// dim is the total number of active cores
void reduce_vector_on_manycore(int* partialVec, int *c, int ptid, int activeTid, int dim, token_queue_t *cons0, 
    token_queue_t *cons1, token_queue_t *prod, int numElements, int token_len) {

  // one core is responsible for writing back the final value at the end
  if (activeTid == 0) {
    // printf("tid %d atid %d dim %d special wait for %p\n", ptid, activeTid, dim, get_pair_base_ptr(cons1, ptid));
    for(int i=0; i<numElements; i+=token_len){
      int t = wait_tokens_consumer(cons1, token_len, ptid);
      for(int j=0; j<token_len; j++){
        int *data = (int*)get_token(cons1, t+j, ptid);
        partialVec[i+j] += data[0];
        c[i+j] += partialVec[i+j];
      }
      consume_tokens(cons1, token_len, ptid);
    }
  }
  // in general cores recv two values in input token queues and writes a value to the next core
  else {
    // only lower half consumes data
    if (activeTid < dim / 2) {
      // printf("tid %d atid %d dim %d wait for %p %p\n", ptid, activeTid, dim, get_pair_base_ptr(cons0, ptid), get_pair_base_ptr(cons1, ptid));
      for(int i=0; i<numElements; i+=token_len){
        int t0 = wait_tokens_consumer(cons0, token_len, ptid);
        int t1 = wait_tokens_consumer(cons1, token_len, ptid);
        for(int j=0; j<token_len; j++){
          asm volatile("nop");
          int *data0 = (int*)get_token(cons0, t0+j, ptid); //use offset here to get data in circ buffer
          int *data1 = (int*)get_token(cons1, t1+j, ptid);
          partialVec[i+j] += data0[0] + data1[0]; //data[0] and not to be used as data[i] due to circ buffer on Spad
        }
        consume_tokens(cons0, token_len, ptid);
        consume_tokens(cons1, token_len, ptid);

        // everyone produces, except for tid0 who does the writeback
        int tokenOffset = wait_tokens_producer(prod, token_len, ptid); // TOKEN_LEN< buffer size, hence safe
        for(int j=0; j<token_len; j++) set_token(prod, partialVec[i+j], tokenOffset+j, ptid);
        produce_tokens(prod, token_len, ptid);
      }
    }
    else{

      // everyone produces, except for tid0 who does the writeback
      // printf("tid %d atid %d dim %d produce for %p\n", ptid, activeTid, dim, get_pair_base_ptr(prod, ptid));
      for(int i=0; i<numElements; i+=token_len){ //go in steps of tokens < buffer size
        int tokenOffset = wait_tokens_producer(prod, token_len, ptid); // TOKEN_LEN< buffer size, hence safe
        for(int j=0; j<token_len; j++) set_token(prod, partialVec[i+j], tokenOffset+j, ptid);
        produce_tokens(prod, token_len, ptid);
      }
    }
  }

}

// based on id (in manycore its the ptid, in vector its the group id + vtid)
// figure out where to send your data to be reduced by another core

int get_reduction_dest_manycore(int src_id) {
  // pattern is to half your id and send to that id
  return src_id / 2;
}

// a little more complicated to get when there's vector groups
int get_reduction_dest_vector(template_info_t *tinfo, int group_id, int vid_x, int vid_y, int virt_dim_x, int phys_dim_x, int *active_tid) {
  // get a flat id from group and vid
  int vid = vid_y * virt_dim_x + vid_x;
  int vlen = tinfo->groups[0].vector_dim.x * tinfo->groups[0].vector_dim.y;
  int src = group_id * vlen + vid;
  // divide by two as in manycore case
  int dest = src / 2;
  // get the ptid corresponding to this group
  int dest_group_id = dest / vlen;
  int dest_vid = dest % vlen;
  int dest_vid_x = dest_vid % virt_dim_x;
  int dest_vid_y = dest_vid / virt_dim_x;
  int ptid = get_ptid_from_group(tinfo, dest_group_id, dest_vid_x, dest_vid_y, phys_dim_x);
  
  *active_tid = src;

  return ptid;
}