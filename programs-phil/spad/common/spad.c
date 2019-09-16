#include <stdint.h>
#include "spad.h"

// 4 kB
#define SP_SIZE 4096
#define N_SPS _N_SPS

// SPM space mapped to scratchpads
uint8_t spm[SP_SIZE * N_SPS] __attribute__ ((section(".spm")));
uint8_t*  spm_base_ptr_arr     [N_SPS];
uint8_t*  spm_next_ptr_arr     [N_SPS];
uint64_t* spm_base_addr_ptr_arr[N_SPS];
uint32_t* spm_go_flag_ptr_arr  [N_SPS];
uint32_t* spm_done_flag_ptr_arr[N_SPS];

uint64_t* spm_alloc64(uint8_t** base_pp, size_t size) {
  uint64_t* p = (uint64_t*) (*base_pp);
  (*base_pp) += sizeof(uint64_t) * size;
  return p;
}

uint32_t* spm_alloc32(uint8_t** base_pp, size_t size) {
  uint32_t* p = (uint32_t*) (*base_pp);
  (*base_pp) += sizeof(uint32_t) * size;
  return p;
}

void initScratchpads() {
  for (size_t i = 0; i < N_SPS; ++i) {
    // get important spad pointers to interface with the respective xcel
    spm_base_ptr_arr[i]       = spm + i * SP_SIZE;
    spm_next_ptr_arr[i]       = spm_base_ptr_arr[i];
    spm_base_addr_ptr_arr[i]  = spm_alloc64(&(spm_next_ptr_arr[i]), 1);
    
    // this is xcel specific stuff
    spm_go_flag_ptr_arr[i]    = spm_alloc32(&(spm_next_ptr_arr[i]), 1);
    spm_done_flag_ptr_arr[i]  = spm_alloc32(&(spm_next_ptr_arr[i]), 1);
    
    // send the base addr of the scratchpad to the scratchpad
    // do this so that xcel sees this message and knows where its scratchpad is
    *(spm_base_addr_ptr_arr[i]) = (uint64_t)(spm_base_ptr_arr[i]);
  }
  
}


void *getSpAddr(int pad, size_t offset) {
  if (pad >= N_SPS) return NULL;
  
  // the spad treats the addr, go, done flag as special, don't write to them
  // TODO we probably want the stack to exist in the scratchpad... how to do?
  size_t afterFlags = sizeof(uint64_t) + 2 * sizeof(uint32_t);
  void *basePtr = (void*)(spm_base_ptr_arr[pad] + afterFlags);
  return basePtr + offset;
}
