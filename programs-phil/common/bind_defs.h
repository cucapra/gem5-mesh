#ifndef __BIND_DEFS_H__
#define __BIND_DEFS_H__

// 20 bit / 5 hex
#define ALL_NORM  0x00000

// 3 / 20
#define BITS_RD   3
#define RD_NORM   0x0
#define RD_RIGHT  0x1
#define RD_DOWN   0x2
#define RD_LEFT   0x3
#define RD_UP     0x4

// 6 / 20
#define BITS_RS1  3
#define SHAMT_RS1 BITS_RD
#define RS1_NORM  0x0 << SHAMT_RS1
#define RS1_RIGHT 0x1 << SHAMT_RS1
#define RS1_DOWN  0x2 << SHAMT_RS1
#define RS1_LEFT  0x3 << SHAMT_RS1
#define RS1_UP    0x4 << SHAMT_RS1

// 9 / 20
#define BITS_RS2  3
#define SHAMT_RS2 SHAMT_RS1 + BITS_RS1
#define RS2_NORM  0x0 << SHAMT_RS2
#define RS2_RIGHT 0x1 << SHAMT_RS2
#define RS2_DOWN  0x2 << SHAMT_RS2
#define RS2_LEFT  0x3 << SHAMT_RS2
#define RS2_UP    0x4 << SHAMT_RS2



#define COMMA ,

// https://forums.sifive.com/t/confusion-regarding-freedom-e-sdk-inline-asm/383
// # is stringify, 'reg' must be explictliy written out
// 'val' must be defined at compile time
// in c this means it MUST BE a define or value
// in c++ it can be define, value, or const int
#define WRITE_CSR(reg, val) \
  asm volatile ("csrrwi x0, " #reg ", %[x]\n\t" :: [x] "i" (val))

// 0x400 is the csr specified in gem5 in src/arch/riscv/register.hh
#define WRITE_MESH_CSR(val) \
  WRITE_CSR(0x400, val)

#define BIND_EXE(val) \
  asm volatile (".insn u 0x6b, x0, %[x]\n\t" :: [x] "i" (val))

// to ensure that the compiler doesn't place unwanted instructions
// within the binds we enforce with a single asm volatile
#define BINDED_SECTION(sbind, ebind, code, wr, rd)  \
  asm volatile (                                    \
    ".insn u 0x6b, x0, %[bind0]\n\t"                 \
    code                                            \
    ".insn u 0x6b, x0, %[bind1]\n\t"                 \
    : wr                                            \
    : [bind0] "i" (sbind), [bind1] "i" (ebind) rd        \
  )

#endif
