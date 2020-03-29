#ifndef __SPAD_H__
#define __SPAD_H__

#include <stddef.h>
#include <stdint.h>

// 4 kB
#define SP_SIZE 4096
#define N_SPS _N_SPS



#if !defined(__x86_64__) && !defined(__i386__)
// SPM space mapped to scratchpads
uint8_t spm[SP_SIZE * N_SPS] __attribute__ ((section(".spm")));
uint8_t*  spm_base_ptr_arr     [N_SPS];
uint8_t*  spm_next_ptr_arr     [N_SPS];
uint64_t* spm_base_addr_ptr_arr[N_SPS];
uint32_t* spm_go_flag_ptr_arr  [N_SPS];
uint32_t* spm_done_flag_ptr_arr[N_SPS];


#endif

void initScratchpads();
// get word addr in spad
inline void *getSpAddr(int pad, size_t wordOffset) {
#if !defined(__x86_64__) && !defined(__i386__)
  if (pad >= N_SPS) return NULL;
  
  // the spad treats the addr, go, done flag as special, don't write to them
  // TODO we probably want the stack to exist in the scratchpad... how to do?
  size_t afterFlags = sizeof(uint64_t) + 2 * sizeof(uint32_t);
  void *ptr = (void*)(spm_base_ptr_arr[pad] + afterFlags + sizeof(uint32_t) * wordOffset);
  return ptr;
#else
  return NULL;
#endif
}

// get the top of the spad
void *getSpTop(int pad);

// get single spad size in bytes
size_t getSpadNumBytes();

#endif

