	.file	"vvadd_kernel.c"
	.option nopic
	.text
	.align	2
	.globl	vvadd_execute_simd
	.type	vvadd_execute_simd, @function
vvadd_execute_simd:
	lw	t1,0(sp)
#APP
# 132 "vvadd_kernel.c" 1
	.insn i 0x77, 0, x0, a0, 0x401
	
# 0 "" 2
#NO_APP
	subw	a5,a5,a4
	divw	a6,a5,t1
	li	a5,15
	mv	a0,a6
	bgt	a6,a5,.L15
	mv	a7,a6
	blez	a6,.L3
.L2:
	slli	a5,a4,2
	add	t3,a1,a5
	li	a3,0
#APP
# 143 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x6, a3, 4(t3)
	
# 0 "" 2
# 144 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x7, a3, 4(t3)
	
# 0 "" 2
#NO_APP
	li	a3,1
	add	a5,a2,a5
#APP
# 145 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x6, a3, 4(a5)
	
# 0 "" 2
# 146 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x7, a3, 4(a5)
	
# 0 "" 2
#NO_APP
	li	a5,1
	beq	a7,a5,.L3
	add	a5,t1,a4
	slli	a5,a5,2
	add	t3,a1,a5
	li	a3,2
#APP
# 143 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x6, a3, 4(t3)
	
# 0 "" 2
# 144 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x7, a3, 4(t3)
	
# 0 "" 2
#NO_APP
	li	a3,3
	add	a5,a2,a5
#APP
# 145 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x6, a3, 4(a5)
	
# 0 "" 2
# 146 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x7, a3, 4(a5)
	
# 0 "" 2
#NO_APP
	li	a5,2
	beq	a7,a5,.L3
	slliw	a3,t1,1
	mv	a5,a3
	add	a3,a3,a4
	slli	a3,a3,2
	add	t4,a1,a3
	li	t3,4
#APP
# 143 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x6, t3, 4(t4)
	
# 0 "" 2
# 144 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x7, t3, 4(t4)
	
# 0 "" 2
#NO_APP
	li	t3,5
	add	a3,a2,a3
#APP
# 145 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x6, t3, 4(a3)
	
# 0 "" 2
# 146 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x7, t3, 4(a3)
	
# 0 "" 2
#NO_APP
	li	a3,3
	beq	a7,a3,.L3
	addw	a3,a5,t1
	mv	a5,a3
	add	a3,a3,a4
	slli	a3,a3,2
	add	t4,a1,a3
	li	t3,6
#APP
# 143 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x6, t3, 4(t4)
	
# 0 "" 2
# 144 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x7, t3, 4(t4)
	
# 0 "" 2
#NO_APP
	li	t3,7
	add	a3,a2,a3
#APP
# 145 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x6, t3, 4(a3)
	
# 0 "" 2
# 146 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x7, t3, 4(a3)
	
# 0 "" 2
#NO_APP
	li	a3,4
	beq	a7,a3,.L3
	addw	a3,a5,t1
	mv	a5,a3
	add	a3,a3,a4
	slli	a3,a3,2
	add	t4,a1,a3
	li	t3,8
#APP
# 143 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x6, t3, 4(t4)
	
# 0 "" 2
# 144 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x7, t3, 4(t4)
	
# 0 "" 2
#NO_APP
	li	t3,9
	add	a3,a2,a3
#APP
# 145 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x6, t3, 4(a3)
	
# 0 "" 2
# 146 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x7, t3, 4(a3)
	
# 0 "" 2
#NO_APP
	li	a3,5
	beq	a7,a3,.L3
	addw	a3,t1,a5
	mv	a5,a3
	add	a3,a3,a4
	slli	a3,a3,2
	add	t4,a1,a3
	li	t3,10
#APP
# 143 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x6, t3, 4(t4)
	
# 0 "" 2
# 144 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x7, t3, 4(t4)
	
# 0 "" 2
#NO_APP
	li	t3,11
	add	a3,a2,a3
#APP
# 145 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x6, t3, 4(a3)
	
# 0 "" 2
# 146 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x7, t3, 4(a3)
	
# 0 "" 2
#NO_APP
	li	a3,6
	beq	a7,a3,.L3
	addw	a3,t1,a5
	mv	a5,a3
	add	a3,a3,a4
	slli	a3,a3,2
	add	t4,a1,a3
	li	t3,12
#APP
# 143 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x6, t3, 4(t4)
	
# 0 "" 2
# 144 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x7, t3, 4(t4)
	
# 0 "" 2
#NO_APP
	li	t3,13
	add	a3,a2,a3
#APP
# 145 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x6, t3, 4(a3)
	
# 0 "" 2
# 146 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x7, t3, 4(a3)
	
# 0 "" 2
#NO_APP
	li	a3,7
	beq	a7,a3,.L3
	addw	a3,t1,a5
	mv	a5,a3
	add	a3,a3,a4
	slli	a3,a3,2
	add	t4,a1,a3
	li	t3,14
#APP
# 143 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x6, t3, 4(t4)
	
# 0 "" 2
# 144 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x7, t3, 4(t4)
	
# 0 "" 2
#NO_APP
	li	t3,15
	add	a3,a2,a3
#APP
# 145 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x6, t3, 4(a3)
	
# 0 "" 2
# 146 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x7, t3, 4(a3)
	
# 0 "" 2
#NO_APP
	li	a3,8
	beq	a7,a3,.L3
	addw	a3,t1,a5
	mv	a5,a3
	add	a3,a3,a4
	slli	a3,a3,2
	add	t4,a1,a3
	li	t3,16
#APP
# 143 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x6, t3, 4(t4)
	
# 0 "" 2
# 144 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x7, t3, 4(t4)
	
# 0 "" 2
#NO_APP
	li	t3,17
	add	a3,a2,a3
#APP
# 145 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x6, t3, 4(a3)
	
# 0 "" 2
# 146 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x7, t3, 4(a3)
	
# 0 "" 2
#NO_APP
	li	a3,9
	beq	a7,a3,.L3
	addw	a3,t1,a5
	mv	a5,a3
	add	a3,a3,a4
	slli	a3,a3,2
	add	t4,a1,a3
	li	t3,18
#APP
# 143 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x6, t3, 4(t4)
	
# 0 "" 2
# 144 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x7, t3, 4(t4)
	
# 0 "" 2
#NO_APP
	li	t3,19
	add	a3,a2,a3
#APP
# 145 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x6, t3, 4(a3)
	
# 0 "" 2
# 146 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x7, t3, 4(a3)
	
# 0 "" 2
#NO_APP
	li	a3,10
	beq	a7,a3,.L3
	addw	a3,t1,a5
	mv	a5,a3
	add	a3,a3,a4
	slli	a3,a3,2
	add	t4,a1,a3
	li	t3,20
#APP
# 143 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x6, t3, 4(t4)
	
# 0 "" 2
# 144 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x7, t3, 4(t4)
	
# 0 "" 2
#NO_APP
	li	t3,21
	add	a3,a2,a3
#APP
# 145 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x6, t3, 4(a3)
	
# 0 "" 2
# 146 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x7, t3, 4(a3)
	
# 0 "" 2
#NO_APP
	li	a3,11
	beq	a7,a3,.L3
	addw	a3,t1,a5
	mv	a5,a3
	add	a3,a3,a4
	slli	a3,a3,2
	add	t4,a1,a3
	li	t3,22
#APP
# 143 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x6, t3, 4(t4)
	
# 0 "" 2
# 144 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x7, t3, 4(t4)
	
# 0 "" 2
#NO_APP
	li	t3,23
	add	a3,a2,a3
#APP
# 145 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x6, t3, 4(a3)
	
# 0 "" 2
# 146 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x7, t3, 4(a3)
	
# 0 "" 2
#NO_APP
	li	a3,12
	beq	a7,a3,.L3
	addw	a3,t1,a5
	mv	a5,a3
	add	a3,a3,a4
	slli	a3,a3,2
	add	t4,a1,a3
	li	t3,24
#APP
# 143 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x6, t3, 4(t4)
	
# 0 "" 2
# 144 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x7, t3, 4(t4)
	
# 0 "" 2
#NO_APP
	li	t3,25
	add	a3,a2,a3
#APP
# 145 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x6, t3, 4(a3)
	
# 0 "" 2
# 146 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x7, t3, 4(a3)
	
# 0 "" 2
#NO_APP
	li	a3,13
	beq	a7,a3,.L3
	addw	a3,t1,a5
	mv	a5,a3
	add	a3,a3,a4
	slli	a3,a3,2
	add	t4,a1,a3
	li	t3,26
#APP
# 143 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x6, t3, 4(t4)
	
# 0 "" 2
# 144 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x7, t3, 4(t4)
	
# 0 "" 2
#NO_APP
	li	t3,27
	add	a3,a2,a3
#APP
# 145 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x6, t3, 4(a3)
	
# 0 "" 2
# 146 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x7, t3, 4(a3)
	
# 0 "" 2
#NO_APP
	li	a3,14
	beq	a7,a3,.L3
	addw	a3,t1,a5
	mv	a5,a3
	add	a3,a3,a4
	slli	a3,a3,2
	add	t4,a1,a3
	li	t3,28
#APP
# 143 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x6, t3, 4(t4)
	
# 0 "" 2
# 144 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x7, t3, 4(t4)
	
# 0 "" 2
#NO_APP
	li	t3,29
	add	a3,a2,a3
#APP
# 145 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x6, t3, 4(a3)
	
# 0 "" 2
# 146 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x7, t3, 4(a3)
	
# 0 "" 2
#NO_APP
	li	a3,16
	bne	a7,a3,.L17
	addw	a5,a5,t1
	add	a5,a5,a4
	slli	a5,a5,2
	add	t3,a1,a5
	li	a3,30
#APP
# 143 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x6, a3, 4(t3)
	
# 0 "" 2
# 144 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x7, a3, 4(t3)
	
# 0 "" 2
#NO_APP
	li	a3,31
	add	a5,a2,a5
#APP
# 145 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x6, a3, 4(a5)
	
# 0 "" 2
# 146 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x7, a3, 4(a5)
	
# 0 "" 2
#NO_APP
.L3:
#APP
# 150 "vvadd_kernel.c" 1
	.insn uj 0x6b, x0, .L4
	
# 0 "" 2
#NO_APP
	slliw	a5,a7,1
	bge	a7,a6,.L5
	mulw	a3,a7,t1
	mv	t3,a7
	slli	t1,t1,2
	li	t5,512
	add	a4,a3,a4
	slli	a4,a4,2
.L6:
#APP
# 167 "vvadd_kernel.c" 1
	.insn uj 0x6b, x0, .L7
	
# 0 "" 2
#NO_APP
	add	a3,a1,a4
#APP
# 172 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x6, a5, 4(a3)
	
# 0 "" 2
# 173 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x7, a5, 4(a3)
	
# 0 "" 2
#NO_APP
	addiw	a3,a5,1
	add	t4,a2,a4
#APP
# 174 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x6, a3, 4(t4)
	
# 0 "" 2
# 175 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x7, a3, 4(t4)
	
# 0 "" 2
#NO_APP
	addiw	a5,a5,2
	addiw	t3,t3,1
	add	a4,a4,t1
	beq	a5,t5,.L22
	bne	a6,t3,.L6
.L5:
	subw	a0,a0,a7
	ble	a6,a0,.L10
.L11:
#APP
# 199 "vvadd_kernel.c" 1
	.insn uj 0x6b, x0, .L7
	
# 0 "" 2
#NO_APP
	addiw	a0,a0,1
	bne	a6,a0,.L11
.L10:
#APP
# 210 "vvadd_kernel.c" 1
	.insn uj 0x6b, x0, .L12
	
# 0 "" 2
#NO_APP
.L13:
#APP
# 212 "vvadd_kernel.c" 1
	.insn uj 0x2b, x0, .L13
	
# 0 "" 2
# 214 "vvadd_kernel.c" 1
	fence
	
# 0 "" 2
# 215 "vvadd_kernel.c" 1
	scalar_return
# 0 "" 2
#NO_APP
	ret
.L15:
	li	a7,16
	j	.L2
.L22:
	beq	a6,t3,.L5
	li	a5,0
	j	.L6
.L4:
#APP
# 219 "vvadd_kernel.c" 1
	vector_init
# 0 "" 2
#NO_APP
.L7:
#APP
# 222 "vvadd_kernel.c" 1
	vector_body
# 0 "" 2
#NO_APP
.L12:
#APP
# 225 "vvadd_kernel.c" 1
	vector_return
# 0 "" 2
#NO_APP
	ret
.L17:
	li	a7,15
	j	.L3
	.size	vvadd_execute_simd, .-vvadd_execute_simd
	.comm	start_barrier,32,8
	.ident	"GCC: (GNU) 8.3.0"
	.section	.note.GNU-stack,"",@progbits
