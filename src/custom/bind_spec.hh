#ifndef __CUSTOM_BIND_SPEC_HH__
#define __CUSTOM_BIND_SPEC_HH__

/*
 * Define encoding for different bindings
 */


// order from mesh.py reproduced here
typedef enum Mesh_Dir {
  RIGHT = 0,
  DOWN = 1,
  LEFT = 2,
  UP = 3,
  NUM_DIR
} Mesh_Dir;

typedef enum Mesh_Out_Src {
  RD = 0,
  RS1 = 1,
  RS2 = 2,
  INST = 3
} Mesh_Out_Src;

typedef struct Mesh_DS_t {
  Mesh_Dir outDir;
  Mesh_Out_Src src;
} Mesh_DS_t;

// define possible sources for output directions
// 8 / 20 bits
// might need to go up to 32 bit configs, but then would need at least
// 2-3 instructions (i.e. li pseudo inst, which apparenlty broken)

/*----------------------------------------------------------------------
 * Execute stage encodings
 *--------------------------------------------------------------------*/ 

// up 
#define EXE_BITS_U        (2)
#define EXE_SHAMT_U       (0)
#define EXE_O_UP_NORM     (0x0       << EXE_SHAMT_U)
#define EXE_O_UP_RD       ((RD + 1)  << EXE_SHAMT_U)
#define EXE_O_UP_RS1      (RS1 + 1) << EXE_SHAMT_U
#define EXE_O_UP_RS2      (RS2 + 1) << EXE_SHAMT_U

// right
#define EXE_BITS_R        (EXE_SHAMT_U + EXE_BITS_U)
#define EXE_SHAMT_R       (EXE_BITS_U)
#define EXE_O_RIGHT_NORM  (0x0       << EXE_SHAMT_R)
#define EXE_O_RIGHT_RD    ((RD + 1)  << EXE_SHAMT_R)
#define EXE_O_RIGHT_RS1   (RS1 + 1) << EXE_SHAMT_R
#define EXE_O_RIGHT_RS2   (RS2 + 1) << EXE_SHAMT_R

// down
#define EXE_BITS_D        (2)
#define EXE_SHAMT_D       (EXE_SHAMT_R + EXE_BITS_R)
#define EXE_O_DOWN_NORM   (0x0       << EXE_SHAMT_D)
#define EXE_O_DOWN_RD     ((RD + 1)  << EXE_SHAMT_D)
#define EXE_O_DOWN_RS1    (RS1 + 1) << EXE_SHAMT_D
#define EXE_O_DOWN_RS2    (RS2 + 1) << EXE_SHAMT_D

// left
#define EXE_BITS_L        (2)
#define EXE_SHAMT_L       (EXE_SHAMT_D + EXE_BITS_D)
#define EXE_O_LEFT_NORM   (0x0       << EXE_SHAMT_L)
#define EXE_O_LEFT_RD     ((RD + 1)  << EXE_SHAMT_L)
#define EXE_O_LEFT_RS1    (RS1 + 1) << EXE_SHAMT_L
#define EXE_O_LEFT_RS2    (RS2 + 1) << EXE_SHAMT_L

// 11 / 20
#define EXE_BITS_RS1      3
#define EXE_SHAMT_RS1     EXE_SHAMT_L + EXE_BITS_L
#define EXE_I_RS1_NORM    0x0         << EXE_SHAMT_RS1
#define EXE_I_RS1_RIGHT   (RIGHT + 1) << EXE_SHAMT_RS1
#define EXE_I_RS1_DOWN    (DOWN + 1)  << EXE_SHAMT_RS1
#define EXE_I_RS1_LEFT    (LEFT + 1)  << EXE_SHAMT_RS1
#define EXE_I_RS1_UP      (UP + 1)    << EXE_SHAMT_RS1

// 14 / 20
#define EXE_BITS_RS2      3
#define EXE_SHAMT_RS2     EXE_SHAMT_RS1 + EXE_BITS_RS1
#define EXE_I_RS2_NORM    0x0         << EXE_SHAMT_RS2
#define EXE_I_RS2_RIGHT   (RIGHT + 1) << EXE_SHAMT_RS2
#define EXE_I_RS2_DOWN    (DOWN + 1)  << EXE_SHAMT_RS2
#define EXE_I_RS2_LEFT    (LEFT + 1)  << EXE_SHAMT_RS2
#define EXE_I_RS2_UP      (UP + 1)    << EXE_SHAMT_RS2

// explicilty determine bitranges for decoding

#define EXE_UP_HI (EXE_SHAMT_U + EXE_BITS_U - 1)
#define EXE_UP_LO (EXE_SHAMT_U)

#define EXE_RIGHT_HI (EXE_SHAMT_R + EXE_BITS_R - 1)
#define EXE_RIGHT_LO (EXE_SHAMT_R)

#define EXE_DOWN_HI (EXE_SHAMT_D + EXE_BITS_D - 1)
#define EXE_DOWN_LO (EXE_SHAMT_D)

#define EXE_LEFT_HI (EXE_SHAMT_L + EXE_BITS_L - 1)
#define EXE_LEFT_LO (EXE_SHAMT_L)

#define EXE_OP1_HI EXE_SHAMT_RS1 + EXE_BITS_RS1 - 1
#define EXE_OP1_LO EXE_SHAMT_RS1

#define EXE_OP2_HI EXE_SHAMT_RS2 + EXE_BITS_RS2 - 1
#define EXE_OP2_LO EXE_SHAMT_RS2

/*----------------------------------------------------------------------
 * Fetch stage encodings
 *--------------------------------------------------------------------*/

typedef enum Locked_Insts {
  ADD = 0,
  ADDI,
  SUB,
  MUL,
  SLL,
  SLLI,
  SRL,
  SRLI,
  SRA,
  SRAI,
  AND,
  ANDI,
  OR,
  ORI,
  XOR,
  XORI,
  LW,
  SW,
  
  NUM_LOCK_INST
  
} Locked_Insts;

// don't need to encode every instruction, just a subset that's useful
// these don't include operands --> need to do operand binds beforehand

#define FET_LOCKED_INST_BITS  (6)
#define FET_LOCKED_SHAMT (0)
#define FET_LOCK_ADD    (ADD)
#define FET_LOCK_SUB    (SUB)
#define FET_LOCK_MUL    (MUL)
#define FET_LOCK_SLL    (SLL)
#define FET_LOCK_SLLI   (SLLI)
#define FET_LOCK_SRL    (SRL)
#define FET_LOCK_SRLI   (SRLI)
#define FET_LOCK_SRA    (SRA)
#define FET_LOCK_SRAI   (SRAI)
#define FET_LOCK_ADDI   (ADDI)
#define FET_LOCK_LW     (LW)
#define FET_LOCK_SW     (SW)
#define FET_LOCK_AND    (AND)
#define FET_LOCK_ANDI   (ANDI)
#define FET_LOCK_OR     (OR)
#define FET_LOCK_ORI    (ORI)
#define FET_LOCK_XOR    (XOR)
#define FET_LOCK_XORI   (XORI)

// in dir --> 3 bits
#define FET_I_INST_BITS     (3)
#define FET_I_INST_SHAMT    ((FET_LOCKED_SHAMT + FET_LOCKED_INST_BITS))
#define FET_I_INST_NORM     (0x0)
#define FET_I_INST_RIGHT    (((RIGHT + 1) << FET_I_INST_SHAMT))
#define FET_I_INST_DOWN     (((DOWN + 1)  << FET_I_INST_SHAMT))
#define FET_I_INST_LEFT     (((LEFT + 1)  << FET_I_INST_SHAMT))
#define FET_I_INST_UP       (((UP + 1)    << FET_I_INST_SHAMT))
#define FET_I_INST_LOCK     (((NUM_DIR + 1) << FET_I_INST_SHAMT))

// out dir --> 4 bits
// these should be consecutive and ordered the same as mesh_dir enum! 
// to make decoding easier

#define FET_O_INST_RIGHT_BITS (1)
#define FET_O_INST_RIGHT_SHAMT (FET_I_INST_SHAMT + FET_I_INST_BITS)
#define FET_O_INST_RIGHT_NORM (0 << FET_O_INST_RIGHT_SHAMT)
#define FET_O_INST_RIGHT_SEND (1 << FET_O_INST_RIGHT_SHAMT)

#define FET_O_INST_DOWN_BITS (1)
#define FET_O_INST_DOWN_SHAMT (FET_O_INST_RIGHT_SHAMT + FET_O_INST_RIGHT_BITS)
#define FET_O_INST_DOWN_NORM (0 << FET_O_INST_DOWN_SHAMT)
#define FET_O_INST_DOWN_SEND (1 << FET_O_INST_DOWN_SHAMT)

#define FET_O_INST_LEFT_BITS (1)
#define FET_O_INST_LEFT_SHAMT (FET_O_INST_DOWN_SHAMT + FET_O_INST_DOWN_BITS)
#define FET_O_INST_LEFT_NORM (0 << FET_O_INST_LEFT_SHAMT)
#define FET_O_INST_LEFT_SEND (1 << FET_O_INST_LEFT_SHAMT)

#define FET_O_INST_UP_BITS (1)
#define FET_O_INST_UP_SHAMT (FET_O_INST_LEFT_SHAMT + FET_O_INST_LEFT_BITS)
#define FET_O_INST_UP_NORM (0 << FET_O_INST_UP_SHAMT)
#define FET_O_INST_UP_SEND (1 << FET_O_INST_UP_SHAMT)

// encode 2d vector length (5 bits each for x and y, 10 bits total)
#define FET_XLEN_BITS (5)
#define FET_XLEN_SHAMT (FET_O_INST_UP_SHAMT + FET_O_INST_UP_BITS)

#define FET_YLEN_BITS (5)
#define FET_YLEN_SHAMT (FET_XLEN_SHAMT + FET_XLEN_BITS)


// remaining bits goes to the count amt (7 bits)
#define FET_COUNT_BITS     (7)
#define FET_COUNT_SHAMT    (FET_YLEN_SHAMT + FET_YLEN_BITS)


// explicilty determine bitranges for decoding

#define FET_IN_SRC_HI (FET_I_INST_SHAMT + FET_I_INST_BITS - 1)
#define FET_IN_SRC_LO (FET_I_INST_SHAMT)

#define FET_OUT_HI (FET_O_INST_RIGHT_SHAMT + FET_O_INST_RIGHT_BITS - 1)
#define FET_OUT_LO (FET_O_INST_RIGHT_SHAMT)

#define FET_COUNT_HI (FET_COUNT_SHAMT + FET_COUNT_BITS - 1)
#define FET_COUNT_LO (FET_COUNT_SHAMT)

#define FET_INST_HI (FET_LOCKED_SHAMT + FET_LOCKED_INST_BITS - 1)
#define FET_INST_LO (FET_LOCKED_SHAMT)

#define FET_XLEN_HI (FET_XLEN_SHAMT + FET_XLEN_BITS - 1)
#define FET_XLEN_LO (FET_XLEN_SHAMT)

#define FET_YLEN_HI (FET_YLEN_SHAMT + FET_YLEN_BITS - 1)
#define FET_YLEN_LO (FET_YLEN_SHAMT)


#endif
