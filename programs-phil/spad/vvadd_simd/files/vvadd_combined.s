    .file   "vvadd_kernel.c"
    .option nopic
    .text
    .align  2
    .globl  vvadd_execute_simd
    .type   vvadd_execute_simd, @function
vvadd_execute_simd:
#APP
# 25 "vvadd_kernel.c" 1
    .insn i 0x77, 0, x0, a0, 0x401
    
# 0 "" 2
#NO_APP
    lw  t1,0(sp)

    subw    a5,a5,a4
    divw    a6,a5,t1
    li  a5,15
    mv  a0,a6
    bgt a6,a5,.L15
    mv  a7,a6
    blez    a6,.L3
.L2:
    slli    a5,a4,2
    add t3,a1,a5
    li  a3,0
#APP
# 38 "vvadd_kernel.c" 1
    .insn sb 0x23, 0x6, a3, 4(t3)
    
# 0 "" 2
# 39 "vvadd_kernel.c" 1
    .insn sb 0x23, 0x7, a3, 4(t3)
    
# 0 "" 2
#NO_APP
    li  a3,1
    add a5,a2,a5
#APP
# 40 "vvadd_kernel.c" 1
    .insn sb 0x23, 0x6, a3, 4(a5)
    
# 0 "" 2
# 41 "vvadd_kernel.c" 1
    .insn sb 0x23, 0x7, a3, 4(a5)
    
# 0 "" 2
#NO_APP
    li  a5,1
    beq a7,a5,.L3
    add a5,t1,a4
    slli    a5,a5,2
    add t3,a1,a5
    li  a3,2
#APP
# 38 "vvadd_kernel.c" 1
    .insn sb 0x23, 0x6, a3, 4(t3)
    
# 0 "" 2
# 39 "vvadd_kernel.c" 1
    .insn sb 0x23, 0x7, a3, 4(t3)
    
# 0 "" 2
#NO_APP
    li  a3,3
    add a5,a2,a5
#APP
# 40 "vvadd_kernel.c" 1
    .insn sb 0x23, 0x6, a3, 4(a5)
    
# 0 "" 2
# 41 "vvadd_kernel.c" 1
    .insn sb 0x23, 0x7, a3, 4(a5)
    
# 0 "" 2
#NO_APP
    li  a5,2
    beq a7,a5,.L3
    slliw   a3,t1,1
    mv  a5,a3
    add a3,a3,a4
    slli    a3,a3,2
    add t4,a1,a3
    li  t3,4
#APP
# 38 "vvadd_kernel.c" 1
    .insn sb 0x23, 0x6, t3, 4(t4)
    
# 0 "" 2
# 39 "vvadd_kernel.c" 1
    .insn sb 0x23, 0x7, t3, 4(t4)
    
# 0 "" 2
#NO_APP
    li  t3,5
    add a3,a2,a3
#APP
# 40 "vvadd_kernel.c" 1
    .insn sb 0x23, 0x6, t3, 4(a3)
    
# 0 "" 2
# 41 "vvadd_kernel.c" 1
    .insn sb 0x23, 0x7, t3, 4(a3)
    
# 0 "" 2
#NO_APP
    li  a3,3
    beq a7,a3,.L3
    addw    a3,a5,t1
    mv  a5,a3
    add a3,a3,a4
    slli    a3,a3,2
    add t4,a1,a3
    li  t3,6
#APP
# 38 "vvadd_kernel.c" 1
    .insn sb 0x23, 0x6, t3, 4(t4)
    
# 0 "" 2
# 39 "vvadd_kernel.c" 1
    .insn sb 0x23, 0x7, t3, 4(t4)
    
# 0 "" 2
#NO_APP
    li  t3,7
    add a3,a2,a3
#APP
# 40 "vvadd_kernel.c" 1
    .insn sb 0x23, 0x6, t3, 4(a3)
    
# 0 "" 2
# 41 "vvadd_kernel.c" 1
    .insn sb 0x23, 0x7, t3, 4(a3)
    
# 0 "" 2
#NO_APP
    li  a3,4
    beq a7,a3,.L3
    addw    a3,a5,t1
    mv  a5,a3
    add a3,a3,a4
    slli    a3,a3,2
    add t4,a1,a3
    li  t3,8
#APP
# 38 "vvadd_kernel.c" 1
    .insn sb 0x23, 0x6, t3, 4(t4)
    
# 0 "" 2
# 39 "vvadd_kernel.c" 1
    .insn sb 0x23, 0x7, t3, 4(t4)
    
# 0 "" 2
#NO_APP
    li  t3,9
    add a3,a2,a3
#APP
# 40 "vvadd_kernel.c" 1
    .insn sb 0x23, 0x6, t3, 4(a3)
    
# 0 "" 2
# 41 "vvadd_kernel.c" 1
    .insn sb 0x23, 0x7, t3, 4(a3)
    
# 0 "" 2
#NO_APP
    li  a3,5
    beq a7,a3,.L3
    addw    a3,t1,a5
    mv  a5,a3
    add a3,a3,a4
    slli    a3,a3,2
    add t4,a1,a3
    li  t3,10
#APP
# 38 "vvadd_kernel.c" 1
    .insn sb 0x23, 0x6, t3, 4(t4)
    
# 0 "" 2
# 39 "vvadd_kernel.c" 1
    .insn sb 0x23, 0x7, t3, 4(t4)
    
# 0 "" 2
#NO_APP
    li  t3,11
    add a3,a2,a3
#APP
# 40 "vvadd_kernel.c" 1
    .insn sb 0x23, 0x6, t3, 4(a3)
    
# 0 "" 2
# 41 "vvadd_kernel.c" 1
    .insn sb 0x23, 0x7, t3, 4(a3)
    
# 0 "" 2
#NO_APP
    li  a3,6
    beq a7,a3,.L3
    addw    a3,t1,a5
    mv  a5,a3
    add a3,a3,a4
    slli    a3,a3,2
    add t4,a1,a3
    li  t3,12
#APP
# 38 "vvadd_kernel.c" 1
    .insn sb 0x23, 0x6, t3, 4(t4)
    
# 0 "" 2
# 39 "vvadd_kernel.c" 1
    .insn sb 0x23, 0x7, t3, 4(t4)
    
# 0 "" 2
#NO_APP
    li  t3,13
    add a3,a2,a3
#APP
# 40 "vvadd_kernel.c" 1
    .insn sb 0x23, 0x6, t3, 4(a3)
    
# 0 "" 2
# 41 "vvadd_kernel.c" 1
    .insn sb 0x23, 0x7, t3, 4(a3)
    
# 0 "" 2
#NO_APP
    li  a3,7
    beq a7,a3,.L3
    addw    a3,t1,a5
    mv  a5,a3
    add a3,a3,a4
    slli    a3,a3,2
    add t4,a1,a3
    li  t3,14
#APP
# 38 "vvadd_kernel.c" 1
    .insn sb 0x23, 0x6, t3, 4(t4)
    
# 0 "" 2
# 39 "vvadd_kernel.c" 1
    .insn sb 0x23, 0x7, t3, 4(t4)
    
# 0 "" 2
#NO_APP
    li  t3,15
    add a3,a2,a3
#APP
# 40 "vvadd_kernel.c" 1
    .insn sb 0x23, 0x6, t3, 4(a3)
    
# 0 "" 2
# 41 "vvadd_kernel.c" 1
    .insn sb 0x23, 0x7, t3, 4(a3)
    
# 0 "" 2
#NO_APP
    li  a3,8
    beq a7,a3,.L3
    addw    a3,t1,a5
    mv  a5,a3
    add a3,a3,a4
    slli    a3,a3,2
    add t4,a1,a3
    li  t3,16
#APP
# 38 "vvadd_kernel.c" 1
    .insn sb 0x23, 0x6, t3, 4(t4)
    
# 0 "" 2
# 39 "vvadd_kernel.c" 1
    .insn sb 0x23, 0x7, t3, 4(t4)
    
# 0 "" 2
#NO_APP
    li  t3,17
    add a3,a2,a3
#APP
# 40 "vvadd_kernel.c" 1
    .insn sb 0x23, 0x6, t3, 4(a3)
    
# 0 "" 2
# 41 "vvadd_kernel.c" 1
    .insn sb 0x23, 0x7, t3, 4(a3)
    
# 0 "" 2
#NO_APP
    li  a3,9
    beq a7,a3,.L3
    addw    a3,t1,a5
    mv  a5,a3
    add a3,a3,a4
    slli    a3,a3,2
    add t4,a1,a3
    li  t3,18
#APP
# 38 "vvadd_kernel.c" 1
    .insn sb 0x23, 0x6, t3, 4(t4)
    
# 0 "" 2
# 39 "vvadd_kernel.c" 1
    .insn sb 0x23, 0x7, t3, 4(t4)
    
# 0 "" 2
#NO_APP
    li  t3,19
    add a3,a2,a3
#APP
# 40 "vvadd_kernel.c" 1
    .insn sb 0x23, 0x6, t3, 4(a3)
    
# 0 "" 2
# 41 "vvadd_kernel.c" 1
    .insn sb 0x23, 0x7, t3, 4(a3)
    
# 0 "" 2
#NO_APP
    li  a3,10
    beq a7,a3,.L3
    addw    a3,t1,a5
    mv  a5,a3
    add a3,a3,a4
    slli    a3,a3,2
    add t4,a1,a3
    li  t3,20
#APP
# 38 "vvadd_kernel.c" 1
    .insn sb 0x23, 0x6, t3, 4(t4)
    
# 0 "" 2
# 39 "vvadd_kernel.c" 1
    .insn sb 0x23, 0x7, t3, 4(t4)
    
# 0 "" 2
#NO_APP
    li  t3,21
    add a3,a2,a3
#APP
# 40 "vvadd_kernel.c" 1
    .insn sb 0x23, 0x6, t3, 4(a3)
    
# 0 "" 2
# 41 "vvadd_kernel.c" 1
    .insn sb 0x23, 0x7, t3, 4(a3)
    
# 0 "" 2
#NO_APP
    li  a3,11
    beq a7,a3,.L3
    addw    a3,t1,a5
    mv  a5,a3
    add a3,a3,a4
    slli    a3,a3,2
    add t4,a1,a3
    li  t3,22
#APP
# 38 "vvadd_kernel.c" 1
    .insn sb 0x23, 0x6, t3, 4(t4)
    
# 0 "" 2
# 39 "vvadd_kernel.c" 1
    .insn sb 0x23, 0x7, t3, 4(t4)
    
# 0 "" 2
#NO_APP
    li  t3,23
    add a3,a2,a3
#APP
# 40 "vvadd_kernel.c" 1
    .insn sb 0x23, 0x6, t3, 4(a3)
    
# 0 "" 2
# 41 "vvadd_kernel.c" 1
    .insn sb 0x23, 0x7, t3, 4(a3)
    
# 0 "" 2
#NO_APP
    li  a3,12
    beq a7,a3,.L3
    addw    a3,t1,a5
    mv  a5,a3
    add a3,a3,a4
    slli    a3,a3,2
    add t4,a1,a3
    li  t3,24
#APP
# 38 "vvadd_kernel.c" 1
    .insn sb 0x23, 0x6, t3, 4(t4)
    
# 0 "" 2
# 39 "vvadd_kernel.c" 1
    .insn sb 0x23, 0x7, t3, 4(t4)
    
# 0 "" 2
#NO_APP
    li  t3,25
    add a3,a2,a3
#APP
# 40 "vvadd_kernel.c" 1
    .insn sb 0x23, 0x6, t3, 4(a3)
    
# 0 "" 2
# 41 "vvadd_kernel.c" 1
    .insn sb 0x23, 0x7, t3, 4(a3)
    
# 0 "" 2
#NO_APP
    li  a3,13
    beq a7,a3,.L3
    addw    a3,t1,a5
    mv  a5,a3
    add a3,a3,a4
    slli    a3,a3,2
    add t4,a1,a3
    li  t3,26
#APP
# 38 "vvadd_kernel.c" 1
    .insn sb 0x23, 0x6, t3, 4(t4)
    
# 0 "" 2
# 39 "vvadd_kernel.c" 1
    .insn sb 0x23, 0x7, t3, 4(t4)
    
# 0 "" 2
#NO_APP
    li  t3,27
    add a3,a2,a3
#APP
# 40 "vvadd_kernel.c" 1
    .insn sb 0x23, 0x6, t3, 4(a3)
    
# 0 "" 2
# 41 "vvadd_kernel.c" 1
    .insn sb 0x23, 0x7, t3, 4(a3)
    
# 0 "" 2
#NO_APP
    li  a3,14
    beq a7,a3,.L3
    addw    a3,t1,a5
    mv  a5,a3
    add a3,a3,a4
    slli    a3,a3,2
    add t4,a1,a3
    li  t3,28
#APP
# 38 "vvadd_kernel.c" 1
    .insn sb 0x23, 0x6, t3, 4(t4)
    
# 0 "" 2
# 39 "vvadd_kernel.c" 1
    .insn sb 0x23, 0x7, t3, 4(t4)
    
# 0 "" 2
#NO_APP
    li  t3,29
    add a3,a2,a3
#APP
# 40 "vvadd_kernel.c" 1
    .insn sb 0x23, 0x6, t3, 4(a3)
    
# 0 "" 2
# 41 "vvadd_kernel.c" 1
    .insn sb 0x23, 0x7, t3, 4(a3)
    
# 0 "" 2
#NO_APP
    li  a3,16
    bne a7,a3,.L17
    addw    a5,a5,t1
    add a5,a5,a4
    slli    a5,a5,2
    add t3,a1,a5
    li  a3,30
#APP
# 38 "vvadd_kernel.c" 1
    .insn sb 0x23, 0x6, a3, 4(t3)
    
# 0 "" 2
# 39 "vvadd_kernel.c" 1
    .insn sb 0x23, 0x7, a3, 4(t3)
    
# 0 "" 2
#NO_APP
    li  a3,31
    add a5,a2,a5
#APP
# 40 "vvadd_kernel.c" 1
    .insn sb 0x23, 0x6, a3, 4(a5)
    
# 0 "" 2
# 41 "vvadd_kernel.c" 1
    .insn sb 0x23, 0x7, a3, 4(a5)
    
# 0 "" 2
#NO_APP
.L3:
#APP
# 45 "vvadd_kernel.c" 1
    .insn uj 0x6b, x0, .L4
    
# 0 "" 2
#NO_APP
    slliw   a5,a7,1
    bge a7,a6,.L5
    mulw    a3,a7,t1
    mv  t3,a7
    slli    t1,t1,2
    li  t5,512
    add a4,a3,a4
    slli    a4,a4,2
.L6:
#APP
# 78 "vvadd_kernel.c" 1
    .insn uj 0x6b, x0, .L7
    
# 0 "" 2
#NO_APP
    add a3,a1,a4
#APP
# 83 "vvadd_kernel.c" 1
    .insn sb 0x23, 0x6, a5, 4(a3)
    
# 0 "" 2
# 84 "vvadd_kernel.c" 1
    .insn sb 0x23, 0x7, a5, 4(a3)
    
# 0 "" 2
#NO_APP
    addiw   a3,a5,1
    add t4,a2,a4
#APP
# 85 "vvadd_kernel.c" 1
    .insn sb 0x23, 0x6, a3, 4(t4)
    
# 0 "" 2
# 86 "vvadd_kernel.c" 1
    .insn sb 0x23, 0x7, a3, 4(t4)
    
# 0 "" 2
#NO_APP
    addiw   a5,a5,2
    addiw   t3,t3,1
    add a4,a4,t1
    beq a5,t5,.L22
    bne a6,t3,.L6
.L5:
    subw    a0,a0,a7
    ble a6,a0,.L10
.L11:
#APP
# 144 "vvadd_kernel.c" 1
    .insn uj 0x6b, x0, .L7
    
# 0 "" 2
#NO_APP
    addiw   a0,a0,1
    bne a6,a0,.L11
.L10:
#APP
# 182 "vvadd_kernel.c" 1
    .insn uj 0x6b, x0, .L12
    
# 0 "" 2
#NO_APP
.L13:
#APP
# 184 "vvadd_kernel.c" 1
    .insn uj 0x2b, x0, .L13
    
# 0 "" 2
# 188 "vvadd_kernel.c" 1
    fence
    
# 0 "" 2
#NO_APP
    ret
.L15:
    li  a7,16
    j   .L2
.L22:
    beq a6,t3,.L5
    li  a5,0
    j   .L6
.L4:
    addi    sp,sp,-16
    lw  t1,16(sp)

    add a4,a4,a7
    slli    a4,a4,2
    li  a5,15
    add a3,a3,a4

    lui a5,%hi(spm_base_ptr_arr)
    addi    a5,a5,%lo(spm_base_ptr_arr)
    slli    a6,a6,3
    add a6,a6,a5
    lw  a5,8(sp)
    ld  a1,0(a6)
    sext.w  a5,a5
    addi    a1,a1,16

    slli    a0,t1,2
    li  a5,0

.L7:
slli    a4,a5,2
    add a2,a1,a4
#APP
# 112 "vvadd_kernel.c" 1
    .insn s 0x03, 0x7, a2, 0(a2)
    
# 0 "" 2
#NO_APP
    addi    a4,a4,4
    add a4,a1,a4
#APP
# 113 "vvadd_kernel.c" 1
    .insn s 0x03, 0x7, a4, 0(a4)
    
# 0 "" 2
# 117 "vvadd_kernel.c" 1
    .insn u 0x0b, x0, 0
    
# 0 "" 2
#NO_APP
    addw    a4,a2,a4
#APP
# 121 "vvadd_kernel.c" 1
    .insn sb 0x23, 0x5, a4, 0(a3)
    
# 0 "" 2
#NO_APP
    lw  a4,8(sp)
    addi    a5,a5,2
    add a3,a3,a0
    sext.w  a4,a4
    andi    a5,a5,511
    
.L12:
addi    sp,sp,16

.L17:
    li  a7,15
    j   .L3
    .size   vvadd_execute_simd, .-vvadd_execute_simd
    .comm   start_barrier,32,8
    .ident  "GCC: (GNU) 8.3.0"
    .section    .note.GNU-stack,"",@progbits

