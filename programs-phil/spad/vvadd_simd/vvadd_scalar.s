	.file	"vvadd_kernel.c"
	.option nopic
	.text
	.align	2
	.globl	vvadd_execute_simd
	.type	vvadd_execute_simd, @function
vvadd_execute_simd:
	addi	sp,sp,-48
	sd	s0,32(sp)
	sd	s1,24(sp)
	mv	s0,a0
	mv	s1,a1
	mv	a0,a5
	li	a1,0
	sd	s2,16(sp)
	sd	s3,8(sp)
	sd	s4,0(sp)
	sd	ra,40(sp)
	mv	s2,a3
	mv	s4,a4
	mv	s3,a7
	call	getSpAddr
	lw	a5,48(sp)
#APP
# 24 "vvadd_kernel.c" 1
	.insn i 0x77, 0, x0, a5, 0x401
	
# 0 "" 2
#NO_APP
	subw	a2,s4,s2
	divw	a1,a2,s3
	li	a5,15
	mv	a2,a1
	bgt	a1,a5,.L13
	mv	a6,a1
	blez	a1,.L3
.L2:
	slli	a5,s2,2
	add	a4,s0,a5
#APP
# 32 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x4, a0, 0(a4)
	
# 0 "" 2
#NO_APP
	addi	a4,a0,4
	add	a5,s1,a5
#APP
# 33 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x4, a4, 0(a5)
	
# 0 "" 2
#NO_APP
	li	a5,1
	beq	a6,a5,.L3
	add	a5,s3,s2
	slli	a5,a5,2
	addi	a4,a0,8
	add	a3,s0,a5
#APP
# 32 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x4, a4, 0(a3)
	
# 0 "" 2
#NO_APP
	addi	a4,a0,12
	add	a5,s1,a5
#APP
# 33 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x4, a4, 0(a5)
	
# 0 "" 2
#NO_APP
	li	a5,2
	beq	a6,a5,.L3
	slliw	a5,s3,1
	mv	a4,a5
	add	a5,a5,s2
	slli	a5,a5,2
	addi	a3,a0,16
	add	a7,s0,a5
#APP
# 32 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x4, a3, 0(a7)
	
# 0 "" 2
#NO_APP
	addi	a3,a0,20
	add	a5,s1,a5
#APP
# 33 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x4, a3, 0(a5)
	
# 0 "" 2
#NO_APP
	li	a5,3
	beq	a6,a5,.L3
	addw	a5,s3,a4
	mv	a4,a5
	add	a5,a5,s2
	slli	a5,a5,2
	addi	a3,a0,24
	add	a7,s0,a5
#APP
# 32 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x4, a3, 0(a7)
	
# 0 "" 2
#NO_APP
	addi	a3,a0,28
	add	a5,s1,a5
#APP
# 33 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x4, a3, 0(a5)
	
# 0 "" 2
#NO_APP
	li	a5,4
	beq	a6,a5,.L3
	addw	a5,s3,a4
	mv	a4,a5
	add	a5,a5,s2
	slli	a5,a5,2
	addi	a3,a0,32
	add	a7,s0,a5
#APP
# 32 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x4, a3, 0(a7)
	
# 0 "" 2
#NO_APP
	addi	a3,a0,36
	add	a5,s1,a5
#APP
# 33 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x4, a3, 0(a5)
	
# 0 "" 2
#NO_APP
	li	a5,5
	beq	a6,a5,.L3
	addw	a5,s3,a4
	mv	a4,a5
	add	a5,a5,s2
	slli	a5,a5,2
	addi	a3,a0,40
	add	a7,s0,a5
#APP
# 32 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x4, a3, 0(a7)
	
# 0 "" 2
#NO_APP
	addi	a3,a0,44
	add	a5,s1,a5
#APP
# 33 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x4, a3, 0(a5)
	
# 0 "" 2
#NO_APP
	li	a5,6
	beq	a6,a5,.L3
	addw	a5,s3,a4
	mv	a4,a5
	add	a5,a5,s2
	slli	a5,a5,2
	addi	a3,a0,48
	add	a7,s0,a5
#APP
# 32 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x4, a3, 0(a7)
	
# 0 "" 2
#NO_APP
	addi	a3,a0,52
	add	a5,s1,a5
#APP
# 33 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x4, a3, 0(a5)
	
# 0 "" 2
#NO_APP
	li	a5,7
	beq	a6,a5,.L3
	addw	a5,s3,a4
	mv	a4,a5
	add	a5,a5,s2
	slli	a5,a5,2
	addi	a3,a0,56
	add	a7,s0,a5
#APP
# 32 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x4, a3, 0(a7)
	
# 0 "" 2
#NO_APP
	addi	a3,a0,60
	add	a5,s1,a5
#APP
# 33 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x4, a3, 0(a5)
	
# 0 "" 2
#NO_APP
	li	a5,8
	beq	a6,a5,.L3
	addw	a5,s3,a4
	mv	a4,a5
	add	a5,a5,s2
	slli	a5,a5,2
	addi	a3,a0,64
	add	a7,s0,a5
#APP
# 32 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x4, a3, 0(a7)
	
# 0 "" 2
#NO_APP
	addi	a3,a0,68
	add	a5,s1,a5
#APP
# 33 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x4, a3, 0(a5)
	
# 0 "" 2
#NO_APP
	li	a5,9
	beq	a6,a5,.L3
	addw	a5,s3,a4
	mv	a4,a5
	add	a5,a5,s2
	slli	a5,a5,2
	addi	a3,a0,72
	add	a7,s0,a5
#APP
# 32 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x4, a3, 0(a7)
	
# 0 "" 2
#NO_APP
	addi	a3,a0,76
	add	a5,s1,a5
#APP
# 33 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x4, a3, 0(a5)
	
# 0 "" 2
#NO_APP
	li	a5,10
	beq	a6,a5,.L3
	addw	a5,s3,a4
	mv	a4,a5
	add	a5,a5,s2
	slli	a5,a5,2
	addi	a3,a0,80
	add	a7,s0,a5
#APP
# 32 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x4, a3, 0(a7)
	
# 0 "" 2
#NO_APP
	addi	a3,a0,84
	add	a5,s1,a5
#APP
# 33 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x4, a3, 0(a5)
	
# 0 "" 2
#NO_APP
	li	a5,11
	beq	a6,a5,.L3
	addw	a5,s3,a4
	mv	a4,a5
	add	a5,a5,s2
	slli	a5,a5,2
	addi	a3,a0,88
	add	a7,s0,a5
#APP
# 32 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x4, a3, 0(a7)
	
# 0 "" 2
#NO_APP
	addi	a3,a0,92
	add	a5,s1,a5
#APP
# 33 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x4, a3, 0(a5)
	
# 0 "" 2
#NO_APP
	li	a5,12
	beq	a6,a5,.L3
	addw	a5,s3,a4
	mv	a4,a5
	add	a5,a5,s2
	slli	a5,a5,2
	addi	a3,a0,96
	add	a7,s0,a5
#APP
# 32 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x4, a3, 0(a7)
	
# 0 "" 2
#NO_APP
	addi	a3,a0,100
	add	a5,s1,a5
#APP
# 33 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x4, a3, 0(a5)
	
# 0 "" 2
#NO_APP
	li	a5,13
	beq	a6,a5,.L3
	addw	a5,s3,a4
	mv	a4,a5
	add	a5,a5,s2
	slli	a5,a5,2
	addi	a3,a0,104
	add	a7,s0,a5
#APP
# 32 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x4, a3, 0(a7)
	
# 0 "" 2
#NO_APP
	addi	a3,a0,108
	add	a5,s1,a5
#APP
# 33 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x4, a3, 0(a5)
	
# 0 "" 2
#NO_APP
	li	a5,14
	beq	a6,a5,.L3
	addw	a5,s3,a4
	mv	a4,a5
	add	a5,a5,s2
	slli	a5,a5,2
	addi	a3,a0,112
	add	a7,s0,a5
#APP
# 32 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x4, a3, 0(a7)
	
# 0 "" 2
#NO_APP
	addi	a3,a0,116
	add	a5,s1,a5
#APP
# 33 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x4, a3, 0(a5)
	
# 0 "" 2
#NO_APP
	li	a5,16
	bne	a6,a5,.L15
	addw	a4,a4,s3
	add	a5,a4,s2
	slli	a5,a5,2
	addi	a4,a0,120
	add	a3,s0,a5
#APP
# 32 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x4, a4, 0(a3)
	
# 0 "" 2
#NO_APP
	addi	a4,a0,124
	add	a5,s1,a5
#APP
# 33 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x4, a4, 0(a5)
	
# 0 "" 2
#NO_APP
.L3:
#APP
# 37 "vvadd_kernel.c" 1
	.insn uj 0x6b, x0, .L4
	
# 0 "" 2
#NO_APP
	slliw	a4,a6,1
	bge	a6,a1,.L5
	mulw	a5,a6,s3
	mv	a7,a6
	slli	s3,s3,2
	li	t4,512
	add	a3,a5,s2
	slli	a3,a3,2
.L6:
#APP
# 68 "vvadd_kernel.c" 1
	.insn uj 0x6b, x0, .L7
	
# 0 "" 2
#NO_APP
	slli	a5,a4,2
	add	t1,a0,a5
	add	t3,s0,a3
#APP
# 71 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x4, t1, 0(t3)
	
# 0 "" 2
#NO_APP
	addi	a5,a5,4
	add	a5,a0,a5
	add	t1,s1,a3
#APP
# 72 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x4, a5, 0(t1)
	
# 0 "" 2
#NO_APP
	addiw	a4,a4,2
	addiw	a7,a7,1
	add	a3,a3,s3
	beq	a4,t4,.L22
	bne	a1,a7,.L6
.L5:
	subw	a2,a2,a6
	ble	a1,a2,.L17
.L11:
#APP
# 126 "vvadd_kernel.c" 1
	.insn uj 0x6b, x0, .L7
	
# 0 "" 2
#NO_APP
	addiw	a2,a2,1
	bne	a1,a2,.L11
.L10:
.L17:
#APP
# 165 "vvadd_kernel.c" 1
	.insn uj 0x2b, x0, .L17
	
# 0 "" 2
# 169 "vvadd_kernel.c" 1
	fence
	
# 0 "" 2
#NO_APP
	ld	ra,40(sp)
	ld	s0,32(sp)
	ld	s1,24(sp)
	ld	s2,16(sp)
	ld	s3,8(sp)
	ld	s4,0(sp)
	addi	sp,sp,48
	jr	ra
.L13:
	li	a6,16
	j	.L2
.L22:
	beq	a1,a7,.L5
	li	a4,0
	j	.L6
.L4:
#APP
# 175 "vvadd_kernel.c" 1
	nop
# 0 "" 2
#NO_APP
.L7:
#APP
# 178 "vvadd_kernel.c" 1
	nop
# 0 "" 2
#NO_APP
	ld	ra,40(sp)
	ld	s0,32(sp)
	ld	s1,24(sp)
	ld	s2,16(sp)
	ld	s3,8(sp)
	ld	s4,0(sp)
	addi	sp,sp,48
	jr	ra
.L15:
	li	a6,15
	j	.L3
	.size	vvadd_execute_simd, .-vvadd_execute_simd
	.comm	start_barrier,32,8
	.ident	"GCC: (GNU) 8.3.0"
	.section	.note.GNU-stack,"",@progbits
