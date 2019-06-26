#include <stdio.h>

// https://forums.sifive.com/t/confusion-regarding-freedom-e-sdk-inline-asm/383
// # is stringify
#define WRITE_CSR(reg, val) \
  asm volatile ("csrrwi x0, " #reg ", %[x]\n\t" :: [x] "i" (val))

int main(int argc, char* argv[])
{
  printf("Hello world!\n");

  // needs to agree with CSR value specified in src/arch/riscv/register.hh
  #define csr_idx 0x400
  int rd = 0;
  const int rs1 = 0x2;
  
  /*asm volatile
  (
    "csrrwi x0, %[x], %[y]\n\t"
    :
    : [x] "r" (rs1), [y] "i" (csr_idx)
  );*/

  //asm volatile ("csrwi " 
  WRITE_CSR(0x400, rs1);

  return 0;
}

