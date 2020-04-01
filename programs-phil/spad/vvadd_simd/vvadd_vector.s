	.file	"vvadd_kernel.c"
	.option nopic
	.text
	.align	2
	.globl	vvadd_execute_simd
	.type	vvadd_execute_simd, @function
vvadd_execute_simd:
	li	a2,15
	bgt	a5,a2,.L14
	lui	a2,%hi(spm_base_ptr_arr)
	addi	a2,a2,%lo(spm_base_ptr_arr)
	slli	a5,a5,3
	add	a5,a5,a2
	ld	a2,0(a5)
	addi	a2,a2,16
.L2:
	lw	a5,0(sp)
#APP
# 24 "vvadd_kernel.c" 1
	.insn i 0x77, 0, x0, a5, 0x401
	
# 0 "" 2
#NO_APP
	subw	a4,a4,a3
	divw	t1,a4,a7
	li	a5,15
	mv	a6,t1
	bgt	t1,a5,.L15
	mv	t3,t1
	blez	t1,.L4
.L3:
	slli	a5,a3,2
	add	a4,a0,a5
#APP
# 33 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x4, a2, 0(a4)
	
# 0 "" 2
#NO_APP
	addi	a4,a2,4
	add	a5,a1,a5
#APP
# 34 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x4, a4, 0(a5)
	
# 0 "" 2
#NO_APP
	li	a5,1
	beq	t3,a5,.L4
	add	a5,a7,a3
	slli	a5,a5,2
	addi	a4,a2,8
	add	t4,a0,a5
#APP
# 33 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x4, a4, 0(t4)
	
# 0 "" 2
#NO_APP
	addi	a4,a2,12
	add	a5,a1,a5
#APP
# 34 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x4, a4, 0(a5)
	
# 0 "" 2
#NO_APP
	li	a5,2
	beq	t3,a5,.L4
	slliw	a5,a7,1
	mv	a4,a5
	add	a5,a5,a3
	slli	a5,a5,2
	addi	t4,a2,16
	add	t5,a0,a5
#APP
# 33 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x4, t4, 0(t5)
	
# 0 "" 2
#NO_APP
	addi	t4,a2,20
	add	a5,a1,a5
#APP
# 34 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x4, t4, 0(a5)
	
# 0 "" 2
#NO_APP
	li	a5,3
	beq	t3,a5,.L4
	addw	a5,a7,a4
	mv	a4,a5
	add	a5,a5,a3
	slli	a5,a5,2
	addi	t4,a2,24
	add	t5,a0,a5
#APP
# 33 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x4, t4, 0(t5)
	
# 0 "" 2
#NO_APP
	addi	t4,a2,28
	add	a5,a1,a5
#APP
# 34 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x4, t4, 0(a5)
	
# 0 "" 2
#NO_APP
	li	a5,4
	beq	t3,a5,.L4
	addw	a5,a7,a4
	mv	a4,a5
	add	a5,a5,a3
	slli	a5,a5,2
	addi	t4,a2,32
	add	t5,a0,a5
#APP
# 33 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x4, t4, 0(t5)
	
# 0 "" 2
#NO_APP
	addi	t4,a2,36
	add	a5,a1,a5
#APP
# 34 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x4, t4, 0(a5)
	
# 0 "" 2
#NO_APP
	li	a5,5
	beq	t3,a5,.L4
	addw	a5,a7,a4
	mv	a4,a5
	add	a5,a5,a3
	slli	a5,a5,2
	addi	t4,a2,40
	add	t5,a0,a5
#APP
# 33 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x4, t4, 0(t5)
	
# 0 "" 2
#NO_APP
	addi	t4,a2,44
	add	a5,a1,a5
#APP
# 34 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x4, t4, 0(a5)
	
# 0 "" 2
#NO_APP
	li	a5,6
	beq	t3,a5,.L4
	addw	a5,a7,a4
	mv	a4,a5
	add	a5,a5,a3
	slli	a5,a5,2
	addi	t4,a2,48
	add	t5,a0,a5
#APP
# 33 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x4, t4, 0(t5)
	
# 0 "" 2
#NO_APP
	addi	t4,a2,52
	add	a5,a1,a5
#APP
# 34 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x4, t4, 0(a5)
	
# 0 "" 2
#NO_APP
	li	a5,7
	beq	t3,a5,.L4
	addw	a5,a7,a4
	mv	a4,a5
	add	a5,a5,a3
	slli	a5,a5,2
	addi	t4,a2,56
	add	t5,a0,a5
#APP
# 33 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x4, t4, 0(t5)
	
# 0 "" 2
#NO_APP
	addi	t4,a2,60
	add	a5,a1,a5
#APP
# 34 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x4, t4, 0(a5)
	
# 0 "" 2
#NO_APP
	li	a5,8
	beq	t3,a5,.L4
	addw	a5,a7,a4
	mv	a4,a5
	add	a5,a5,a3
	slli	a5,a5,2
	addi	t4,a2,64
	add	t5,a0,a5
#APP
# 33 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x4, t4, 0(t5)
	
# 0 "" 2
#NO_APP
	addi	t4,a2,68
	add	a5,a1,a5
#APP
# 34 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x4, t4, 0(a5)
	
# 0 "" 2
#NO_APP
	li	a5,9
	beq	t3,a5,.L4
	addw	a5,a7,a4
	mv	a4,a5
	add	a5,a5,a3
	slli	a5,a5,2
	addi	t4,a2,72
	add	t5,a0,a5
#APP
# 33 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x4, t4, 0(t5)
	
# 0 "" 2
#NO_APP
	addi	t4,a2,76
	add	a5,a1,a5
#APP
# 34 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x4, t4, 0(a5)
	
# 0 "" 2
#NO_APP
	li	a5,10
	beq	t3,a5,.L4
	addw	a5,a7,a4
	mv	a4,a5
	add	a5,a5,a3
	slli	a5,a5,2
	addi	t4,a2,80
	add	t5,a0,a5
#APP
# 33 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x4, t4, 0(t5)
	
# 0 "" 2
#NO_APP
	addi	t4,a2,84
	add	a5,a1,a5
#APP
# 34 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x4, t4, 0(a5)
	
# 0 "" 2
#NO_APP
	li	a5,11
	beq	t3,a5,.L4
	addw	a5,a7,a4
	mv	a4,a5
	add	a5,a5,a3
	slli	a5,a5,2
	addi	t4,a2,88
	add	t5,a0,a5
#APP
# 33 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x4, t4, 0(t5)
	
# 0 "" 2
#NO_APP
	addi	t4,a2,92
	add	a5,a1,a5
#APP
# 34 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x4, t4, 0(a5)
	
# 0 "" 2
#NO_APP
	li	a5,12
	beq	t3,a5,.L4
	addw	a5,a7,a4
	mv	a4,a5
	add	a5,a5,a3
	slli	a5,a5,2
	addi	t4,a2,96
	add	t5,a0,a5
#APP
# 33 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x4, t4, 0(t5)
	
# 0 "" 2
#NO_APP
	addi	t4,a2,100
	add	a5,a1,a5
#APP
# 34 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x4, t4, 0(a5)
	
# 0 "" 2
#NO_APP
	li	a5,13
	beq	t3,a5,.L4
	addw	a5,a7,a4
	mv	a4,a5
	add	a5,a5,a3
	slli	a5,a5,2
	addi	t4,a2,104
	add	t5,a0,a5
#APP
# 33 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x4, t4, 0(t5)
	
# 0 "" 2
#NO_APP
	addi	t4,a2,108
	add	a5,a1,a5
#APP
# 34 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x4, t4, 0(a5)
	
# 0 "" 2
#NO_APP
	li	a5,14
	beq	t3,a5,.L4
	addw	a5,a7,a4
	mv	a4,a5
	add	a5,a5,a3
	slli	a5,a5,2
	addi	t4,a2,112
	add	t5,a0,a5
#APP
# 33 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x4, t4, 0(t5)
	
# 0 "" 2
#NO_APP
	addi	t4,a2,116
	add	a5,a1,a5
#APP
# 34 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x4, t4, 0(a5)
	
# 0 "" 2
#NO_APP
	li	a5,16
	bne	t3,a5,.L17
	addw	a4,a4,a7
	add	a4,a4,a3
	slli	a4,a4,2
	addi	a5,a2,120
	add	t4,a0,a4
#APP
# 33 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x4, a5, 0(t4)
	
# 0 "" 2
#NO_APP
	addi	a5,a2,124
	add	a4,a1,a4
#APP
# 34 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x4, a5, 0(a4)
	
# 0 "" 2
#NO_APP
.L4:
#APP
# 38 "vvadd_kernel.c" 1
	.insn uj 0x6b, x0, .L5
	
# 0 "" 2
#NO_APP
	slliw	a4,t3,1
	bge	t3,t1,.L6
	mulw	a5,t3,a7
	mv	t4,t3
	slli	a7,a7,2
	li	t0,512
	add	a3,a5,a3
	slli	a3,a3,2
.L7:
#APP
# 69 "vvadd_kernel.c" 1
	.insn uj 0x6b, x0, .L8
	
# 0 "" 2
#NO_APP
	slli	a5,a4,2
	add	t5,a2,a5
	add	t6,a0,a3
#APP
# 72 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x4, t5, 0(t6)
	
# 0 "" 2
#NO_APP
	addi	a5,a5,4
	add	a5,a2,a5
	add	t5,a1,a3
#APP
# 73 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x4, a5, 0(t5)
	
# 0 "" 2
#NO_APP
	addiw	a4,a4,2
	addiw	t4,t4,1
	add	a3,a3,a7
	beq	a4,t0,.L23
	bne	t1,t4,.L7
.L6:
	subw	a6,a6,t3
	ble	t1,a6,.L19
.L12:
#APP
# 127 "vvadd_kernel.c" 1
	.insn uj 0x6b, x0, .L8
	
# 0 "" 2
#NO_APP
	addiw	a6,a6,1
	bne	t1,a6,.L12
.L11:
.L19:
#APP
# 166 "vvadd_kernel.c" 1
	.insn uj 0x2b, x0, .L19
	
# 0 "" 2
# 170 "vvadd_kernel.c" 1
	fence
	
# 0 "" 2
#NO_APP
	ret
.L15:
	li	t3,16
	j	.L3
.L23:
	beq	t1,t4,.L6
	li	a4,0
	j	.L7
.L5:
#APP
# 176 "vvadd_kernel.c" 1
	nop
# 0 "" 2
#NO_APP
.L8:
#APP
# 179 "vvadd_kernel.c" 1
	nop
# 0 "" 2
#NO_APP
	ret
.L14:
	li	a2,0
	j	.L2
.L17:
	li	t3,15
	j	.L4
	.size	vvadd_execute_simd, .-vvadd_execute_simd
	.comm	start_barrier,32,8
	.ident	"GCC: (GNU) 8.3.0"
	.section	.note.GNU-stack,"",@progbits
