#ifndef __BIND_DEFS_H__
#define __BIND_DEFS_H__

// 20 bit / 5 hex
#define ALL_NORM  0x00000

// define possible sources for output directions
// 8 / 20 bits
// might need to go up to 32 bit configs, but then would need at least
// 2-3 instructions (i.e. li pseudo inst, which apparenlty broken)

// up 
#define BITS_U        2
#define SHAMT_U       0
#define O_UP_NORM     0x0 << SHAMT_U
#define O_UP_RD       0x1 << SHAMT_U
#define O_UP_RS1      0x2 << SHAMT_U
#define O_UP_RS2      0x3 << SHAMT_U

// right
#define BITS_R        SHAMT_U + BITS_U
#define SHAMT_R       BITS_U
#define O_RIGHT_NORM  0x0 << SHAMT_R
#define O_RIGHT_RD    0x1 << SHAMT_R
#define O_RIGHT_RS1   0x2 << SHAMT_R
#define O_RIGHT_RS2   0x3 << SHAMT_R

// down
#define BITS_D        2
#define SHAMT_D       SHAMT_R + BITS_R
#define O_DOWN_NORM   0x0 << SHAMT_D
#define O_DOWN_RD     0x1 << SHAMT_D
#define O_DOWN_RS1    0x2 << SHAMT_D
#define O_DOWN_RS2    0x3 << SHAMT_D

// left
#define BITS_L        2
#define SHAMT_L       SHAMT_D + BITS_D
#define O_LEFT_NORM   0x0 << SHAMT_L
#define O_LEFT_RD     0x1 << SHAMT_L
#define O_LEFT_RS1    0x2 << SHAMT_L
#define O_LEFT_RS2    0x3 << SHAMT_L

// 11 / 20
#define BITS_RS1      3
#define SHAMT_RS1     SHAMT_L + BITS_L
#define I_RS1_NORM    0x0 << SHAMT_RS1
#define I_RS1_RIGHT   0x1 << SHAMT_RS1
#define I_RS1_DOWN    0x2 << SHAMT_RS1
#define I_RS1_LEFT    0x3 << SHAMT_RS1
#define I_RS1_UP      0x4 << SHAMT_RS1

// 14 / 20
#define BITS_RS2      3
#define SHAMT_RS2     SHAMT_RS1 + BITS_RS1
#define I_RS2_NORM    0x0 << SHAMT_RS2
#define I_RS2_RIGHT   0x1 << SHAMT_RS2
#define I_RS2_DOWN    0x2 << SHAMT_RS2
#define I_RS2_LEFT    0x3 << SHAMT_RS2
#define I_RS2_UP      0x4 << SHAMT_RS2


// if you want to include a comma in a macro, need to indirectly do like
// so, otherwise the pre-processor will assume its a delimiter for the
// macro args
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
