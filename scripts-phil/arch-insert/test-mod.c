#include <stdio.h>

// https://stackoverflow.com/questions/51825942/adding-new-instruction-to-riscv-32ima-bad-riscv-opcode
// .insn <inst_format=r,i,s,u> <opcode> ...
// .insn r <opcode>, <funct3>, <funct7>, <rd>, <rs1>, <rs2>
// .insn i <opcode>, <funct3>, <rd>, <imm>(<rs1>)
#define MOD(c, a, b)                        \
  asm volatile                              \
  (                                         \
    ".insn r 0x6b, 0, 0, %[z], %[x], %[y]\n\t" \
    : [z] "=r" (c)                          \
    : [x] "r" (a), [y] "r" (b)              \
  )


int main(){
  int a,b,c;
  a = 5;
  b = 2;
  /*asm volatile
  (
    ".insn r mod   %[z], %[x], %[y]\n\t"
    : [z] "=r" (c)
    : [x] "r" (a), [y] "r" (b)
  );*/

  MOD(c, a, b);  
 
  if ( c != 1 ){
     printf("\n[[FAILED]]\n");
     return -1;
  }
  
  printf("\n[[PASSED]]\n");

  return 0;
}
