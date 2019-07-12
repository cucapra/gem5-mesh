#ifndef __CUSTOM_BIND_SPEC_HH__
#define __CUSTOM_BIND_SPEC_HH__

/*
 * Define encoding for different bindings
 */


// order from mesh.py reproduced here (and should agree with software bind_defs.h
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
  RS2 = 2
} Mesh_Out_Src;

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



// explicilty determine bitranges for decoding

#define UP_HI SHAMT_U + BITS_U - 1
#define UP_LO SHAMT_U

#define RIGHT_HI SHAMT_R + BITS_R - 1
#define RIGHT_LO SHAMT_R

#define DOWN_HI SHAMT_D + BITS_D - 1
#define DOWN_LO SHAMT_D

#define LEFT_HI SHAMT_L + BITS_L - 1
#define LEFT_LO SHAMT_L

#define OP1_HI SHAMT_RS1 + BITS_RS1 - 1
#define OP1_LO SHAMT_RS1

#define OP2_HI SHAMT_RS2 + BITS_RS2 - 1
#define OP2_LO SHAMT_RS2


#endif
