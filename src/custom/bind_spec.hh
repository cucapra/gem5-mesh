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

// in dir --> 3 bits
#define FET_I_INST_BITS     (3)
#define FET_I_INST_SHAMT    (0)
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


// encode whether this is a decoupled access core (1 bit)
#define FET_DAE_BITS     (1)
#define FET_DAE_SHAMT    (FET_YLEN_SHAMT + FET_YLEN_BITS)

// encode whether this is a decoupled execute core (1bit)
#define FET_EXPANDER_BITS (1)
#define FET_EXPANDER_SHAMT (FET_DAE_SHAMT + FET_DAE_BITS)

// encode where the origin of the vector group is (7 * 2 = 14 bits, up to 16k cores)
// 6bits is 4096 cores, maybe fine?
#define FET_XORIGIN_BITS (6)
#define FET_XORIGIN_SHAMT (FET_EXPANDER_SHAMT + FET_EXPANDER_BITS)

#define FET_YORIGIN_BITS (6)
#define FET_YORIGIN_SHAMT (FET_XORIGIN_SHAMT + FET_XORIGIN_BITS)


// explicilty determine bitranges for decoding

#define FET_IN_SRC_HI (FET_I_INST_SHAMT + FET_I_INST_BITS - 1)
#define FET_IN_SRC_LO (FET_I_INST_SHAMT)

#define FET_OUT_HI (FET_O_INST_RIGHT_SHAMT + FET_O_INST_RIGHT_BITS - 1)
#define FET_OUT_LO (FET_O_INST_RIGHT_SHAMT)

// #define FET_INST_HI (FET_LOCKED_SHAMT + FET_LOCKED_INST_BITS - 1)
// #define FET_INST_LO (FET_LOCKED_SHAMT)

#define FET_XLEN_HI (FET_XLEN_SHAMT + FET_XLEN_BITS - 1)
#define FET_XLEN_LO (FET_XLEN_SHAMT)

#define FET_YLEN_HI (FET_YLEN_SHAMT + FET_YLEN_BITS - 1)
#define FET_YLEN_LO (FET_YLEN_SHAMT)

#define FET_DAE_HI (FET_DAE_SHAMT + FET_DAE_BITS - 1)
#define FET_DAE_LO (FET_DAE_SHAMT)

#define FET_EXPANDER_HI (FET_EXPANDER_SHAMT + FET_EXPANDER_BITS - 1)
#define FET_EXPANDER_LO (FET_EXPANDER_SHAMT)

#define FET_XORIGIN_HI (FET_XORIGIN_SHAMT + FET_XORIGIN_BITS - 1)
#define FET_XORIGIN_LO (FET_XORIGIN_SHAMT)

#define FET_YORIGIN_HI (FET_YORIGIN_SHAMT + FET_YORIGIN_BITS - 1)
#define FET_YORIGIN_LO (FET_YORIGIN_SHAMT)


/*----------------------------------------------------------------------
 * Prefetch encodings
 *--------------------------------------------------------------------*/

#define PREFETCH_NUM_REGION_BITS (10)
#define PREFETCH_NUM_REGION_SHAMT (0)

#define PREFETCH_REGION_SIZE_BITS (10)
#define PREFETCH_REGION_SIZE_SHAMT (PREFETCH_NUM_REGION_SHAMT + PREFETCH_NUM_REGION_BITS)

#define PREFETCH_NUM_REGION_HI (PREFETCH_NUM_REGION_SHAMT + PREFETCH_NUM_REGION_BITS - 1)
#define PREFETCH_NUM_REGION_LO (PREFETCH_NUM_REGION_SHAMT)

#define PREFETCH_REGION_SIZE_HI (PREFETCH_REGION_SIZE_SHAMT + PREFETCH_REGION_SIZE_BITS - 1)
#define PREFETCH_REGION_SIZE_LO (PREFETCH_REGION_SIZE_SHAMT)

#endif
