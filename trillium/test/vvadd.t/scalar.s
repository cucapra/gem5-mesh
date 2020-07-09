; ARGS: vvadd_vector.s

	.file	"vvadd_kernel_be.c"
	.option nopic
	.text
	.align	2
	.globl	tril_vvadd
	.type	tril_vvadd, @function
tril_vvadd:
	lw	a3,0(sp)
#APP
# 26 "vvadd_kernel_be.c" 1
	.insn i 0x77, 0, x0, a0, 0x401
	
# 0 "" 2
#NO_APP
	subw	a5,a5,a4
	divw	a7,a5,a3
	li	a5,15
	mv	a0,a7
	bgt	a7,a5,.L16
	mv	a6,a7
	blez	a7,.L3
.L2:
	slli	a5,a4,2
	add	t3,a1,a5
	li	t1,0
#APP
# 36 "vvadd_kernel_be.c" 1
	.insn sb 0x23, 0x6, t1, 16(t3)
	
# 0 "" 2
# 37 "vvadd_kernel_be.c" 1
	.insn sb 0x23, 0x7, t1, 16(t3)
	
# 0 "" 2
#NO_APP
	li	t1,1
	add	a5,a2,a5
#APP
# 38 "vvadd_kernel_be.c" 1
	.insn sb 0x23, 0x6, t1, 16(a5)
	
# 0 "" 2
# 39 "vvadd_kernel_be.c" 1
	.insn sb 0x23, 0x7, t1, 16(a5)
	
# 0 "" 2
#NO_APP
	li	a5,1
	beq	a6,a5,.L3
	add	a5,a3,a4
	slli	a5,a5,2
	add	t3,a1,a5
	li	t1,2
#APP
# 36 "vvadd_kernel_be.c" 1
	.insn sb 0x23, 0x6, t1, 16(t3)
	
# 0 "" 2
# 37 "vvadd_kernel_be.c" 1
	.insn sb 0x23, 0x7, t1, 16(t3)
	
# 0 "" 2
#NO_APP
	li	t1,3
	add	a5,a2,a5
#APP
# 38 "vvadd_kernel_be.c" 1
	.insn sb 0x23, 0x6, t1, 16(a5)
	
# 0 "" 2
# 39 "vvadd_kernel_be.c" 1
	.insn sb 0x23, 0x7, t1, 16(a5)
	
# 0 "" 2
#NO_APP
	li	a5,2
	beq	a6,a5,.L3
	slliw	t1,a3,1
	mv	a5,t1
	add	t1,t1,a4
	slli	t1,t1,2
	add	t4,a1,t1
	li	t3,4
#APP
# 36 "vvadd_kernel_be.c" 1
	.insn sb 0x23, 0x6, t3, 16(t4)
	
# 0 "" 2
# 37 "vvadd_kernel_be.c" 1
	.insn sb 0x23, 0x7, t3, 16(t4)
	
# 0 "" 2
#NO_APP
	li	t3,5
	add	t1,a2,t1
#APP
# 38 "vvadd_kernel_be.c" 1
	.insn sb 0x23, 0x6, t3, 16(t1)
	
# 0 "" 2
# 39 "vvadd_kernel_be.c" 1
	.insn sb 0x23, 0x7, t3, 16(t1)
	
# 0 "" 2
#NO_APP
	li	t1,3
	beq	a6,t1,.L3
	addw	t1,a5,a3
	mv	a5,t1
	add	t1,t1,a4
	slli	t1,t1,2
	add	t4,a1,t1
	li	t3,6
#APP
# 36 "vvadd_kernel_be.c" 1
	.insn sb 0x23, 0x6, t3, 16(t4)
	
# 0 "" 2
# 37 "vvadd_kernel_be.c" 1
	.insn sb 0x23, 0x7, t3, 16(t4)
	
# 0 "" 2
#NO_APP
	li	t3,7
	add	t1,a2,t1
#APP
# 38 "vvadd_kernel_be.c" 1
	.insn sb 0x23, 0x6, t3, 16(t1)
	
# 0 "" 2
# 39 "vvadd_kernel_be.c" 1
	.insn sb 0x23, 0x7, t3, 16(t1)
	
# 0 "" 2
#NO_APP
	li	t1,4
	beq	a6,t1,.L3
	addw	t1,a5,a3
	mv	a5,t1
	add	t1,t1,a4
	slli	t1,t1,2
	add	t4,a1,t1
	li	t3,8
#APP
# 36 "vvadd_kernel_be.c" 1
	.insn sb 0x23, 0x6, t3, 16(t4)
	
# 0 "" 2
# 37 "vvadd_kernel_be.c" 1
	.insn sb 0x23, 0x7, t3, 16(t4)
	
# 0 "" 2
#NO_APP
	li	t3,9
	add	t1,a2,t1
#APP
# 38 "vvadd_kernel_be.c" 1
	.insn sb 0x23, 0x6, t3, 16(t1)
	
# 0 "" 2
# 39 "vvadd_kernel_be.c" 1
	.insn sb 0x23, 0x7, t3, 16(t1)
	
# 0 "" 2
#NO_APP
	li	t1,5
	beq	a6,t1,.L3
	addw	t1,a3,a5
	mv	a5,t1
	add	t1,t1,a4
	slli	t1,t1,2
	add	t4,a1,t1
	li	t3,10
#APP
# 36 "vvadd_kernel_be.c" 1
	.insn sb 0x23, 0x6, t3, 16(t4)
	
# 0 "" 2
# 37 "vvadd_kernel_be.c" 1
	.insn sb 0x23, 0x7, t3, 16(t4)
	
# 0 "" 2
#NO_APP
	li	t3,11
	add	t1,a2,t1
#APP
# 38 "vvadd_kernel_be.c" 1
	.insn sb 0x23, 0x6, t3, 16(t1)
	
# 0 "" 2
# 39 "vvadd_kernel_be.c" 1
	.insn sb 0x23, 0x7, t3, 16(t1)
	
# 0 "" 2
#NO_APP
	li	t1,6
	beq	a6,t1,.L3
	addw	t1,a3,a5
	mv	a5,t1
	add	t1,t1,a4
	slli	t1,t1,2
	add	t4,a1,t1
	li	t3,12
#APP
# 36 "vvadd_kernel_be.c" 1
	.insn sb 0x23, 0x6, t3, 16(t4)
	
# 0 "" 2
# 37 "vvadd_kernel_be.c" 1
	.insn sb 0x23, 0x7, t3, 16(t4)
	
# 0 "" 2
#NO_APP
	li	t3,13
	add	t1,a2,t1
#APP
# 38 "vvadd_kernel_be.c" 1
	.insn sb 0x23, 0x6, t3, 16(t1)
	
# 0 "" 2
# 39 "vvadd_kernel_be.c" 1
	.insn sb 0x23, 0x7, t3, 16(t1)
	
# 0 "" 2
#NO_APP
	li	t1,7
	beq	a6,t1,.L3
	addw	t1,a3,a5
	mv	a5,t1
	add	t1,t1,a4
	slli	t1,t1,2
	add	t4,a1,t1
	li	t3,14
#APP
# 36 "vvadd_kernel_be.c" 1
	.insn sb 0x23, 0x6, t3, 16(t4)
	
# 0 "" 2
# 37 "vvadd_kernel_be.c" 1
	.insn sb 0x23, 0x7, t3, 16(t4)
	
# 0 "" 2
#NO_APP
	li	t3,15
	add	t1,a2,t1
#APP
# 38 "vvadd_kernel_be.c" 1
	.insn sb 0x23, 0x6, t3, 16(t1)
	
# 0 "" 2
# 39 "vvadd_kernel_be.c" 1
	.insn sb 0x23, 0x7, t3, 16(t1)
	
# 0 "" 2
#NO_APP
	li	t1,8
	beq	a6,t1,.L3
	addw	t1,a3,a5
	mv	a5,t1
	add	t1,t1,a4
	slli	t1,t1,2
	add	t4,a1,t1
	li	t3,16
#APP
# 36 "vvadd_kernel_be.c" 1
	.insn sb 0x23, 0x6, t3, 16(t4)
	
# 0 "" 2
# 37 "vvadd_kernel_be.c" 1
	.insn sb 0x23, 0x7, t3, 16(t4)
	
# 0 "" 2
#NO_APP
	li	t3,17
	add	t1,a2,t1
#APP
# 38 "vvadd_kernel_be.c" 1
	.insn sb 0x23, 0x6, t3, 16(t1)
	
# 0 "" 2
# 39 "vvadd_kernel_be.c" 1
	.insn sb 0x23, 0x7, t3, 16(t1)
	
# 0 "" 2
#NO_APP
	li	t1,9
	beq	a6,t1,.L3
	addw	t1,a3,a5
	mv	a5,t1
	add	t1,t1,a4
	slli	t1,t1,2
	add	t4,a1,t1
	li	t3,18
#APP
# 36 "vvadd_kernel_be.c" 1
	.insn sb 0x23, 0x6, t3, 16(t4)
	
# 0 "" 2
# 37 "vvadd_kernel_be.c" 1
	.insn sb 0x23, 0x7, t3, 16(t4)
	
# 0 "" 2
#NO_APP
	li	t3,19
	add	t1,a2,t1
#APP
# 38 "vvadd_kernel_be.c" 1
	.insn sb 0x23, 0x6, t3, 16(t1)
	
# 0 "" 2
# 39 "vvadd_kernel_be.c" 1
	.insn sb 0x23, 0x7, t3, 16(t1)
	
# 0 "" 2
#NO_APP
	li	t1,10
	beq	a6,t1,.L3
	addw	t1,a3,a5
	mv	a5,t1
	add	t1,t1,a4
	slli	t1,t1,2
	add	t4,a1,t1
	li	t3,20
#APP
# 36 "vvadd_kernel_be.c" 1
	.insn sb 0x23, 0x6, t3, 16(t4)
	
# 0 "" 2
# 37 "vvadd_kernel_be.c" 1
	.insn sb 0x23, 0x7, t3, 16(t4)
	
# 0 "" 2
#NO_APP
	li	t3,21
	add	t1,a2,t1
#APP
# 38 "vvadd_kernel_be.c" 1
	.insn sb 0x23, 0x6, t3, 16(t1)
	
# 0 "" 2
# 39 "vvadd_kernel_be.c" 1
	.insn sb 0x23, 0x7, t3, 16(t1)
	
# 0 "" 2
#NO_APP
	li	t1,11
	beq	a6,t1,.L3
	addw	t1,a3,a5
	mv	a5,t1
	add	t1,t1,a4
	slli	t1,t1,2
	add	t4,a1,t1
	li	t3,22
#APP
# 36 "vvadd_kernel_be.c" 1
	.insn sb 0x23, 0x6, t3, 16(t4)
	
# 0 "" 2
# 37 "vvadd_kernel_be.c" 1
	.insn sb 0x23, 0x7, t3, 16(t4)
	
# 0 "" 2
#NO_APP
	li	t3,23
	add	t1,a2,t1
#APP
# 38 "vvadd_kernel_be.c" 1
	.insn sb 0x23, 0x6, t3, 16(t1)
	
# 0 "" 2
# 39 "vvadd_kernel_be.c" 1
	.insn sb 0x23, 0x7, t3, 16(t1)
	
# 0 "" 2
#NO_APP
	li	t1,12
	beq	a6,t1,.L3
	addw	t1,a3,a5
	mv	a5,t1
	add	t1,t1,a4
	slli	t1,t1,2
	add	t4,a1,t1
	li	t3,24
#APP
# 36 "vvadd_kernel_be.c" 1
	.insn sb 0x23, 0x6, t3, 16(t4)
	
# 0 "" 2
# 37 "vvadd_kernel_be.c" 1
	.insn sb 0x23, 0x7, t3, 16(t4)
	
# 0 "" 2
#NO_APP
	li	t3,25
	add	t1,a2,t1
#APP
# 38 "vvadd_kernel_be.c" 1
	.insn sb 0x23, 0x6, t3, 16(t1)
	
# 0 "" 2
# 39 "vvadd_kernel_be.c" 1
	.insn sb 0x23, 0x7, t3, 16(t1)
	
# 0 "" 2
#NO_APP
	li	t1,13
	beq	a6,t1,.L3
	addw	t1,a3,a5
	mv	a5,t1
	add	t1,t1,a4
	slli	t1,t1,2
	add	t4,a1,t1
	li	t3,26
#APP
# 36 "vvadd_kernel_be.c" 1
	.insn sb 0x23, 0x6, t3, 16(t4)
	
# 0 "" 2
# 37 "vvadd_kernel_be.c" 1
	.insn sb 0x23, 0x7, t3, 16(t4)
	
# 0 "" 2
#NO_APP
	li	t3,27
	add	t1,a2,t1
#APP
# 38 "vvadd_kernel_be.c" 1
	.insn sb 0x23, 0x6, t3, 16(t1)
	
# 0 "" 2
# 39 "vvadd_kernel_be.c" 1
	.insn sb 0x23, 0x7, t3, 16(t1)
	
# 0 "" 2
#NO_APP
	li	t1,14
	beq	a6,t1,.L3
	addw	t1,a3,a5
	mv	a5,t1
	add	t1,t1,a4
	slli	t1,t1,2
	add	t4,a1,t1
	li	t3,28
#APP
# 36 "vvadd_kernel_be.c" 1
	.insn sb 0x23, 0x6, t3, 16(t4)
	
# 0 "" 2
# 37 "vvadd_kernel_be.c" 1
	.insn sb 0x23, 0x7, t3, 16(t4)
	
# 0 "" 2
#NO_APP
	li	t3,29
	add	t1,a2,t1
#APP
# 38 "vvadd_kernel_be.c" 1
	.insn sb 0x23, 0x6, t3, 16(t1)
	
# 0 "" 2
# 39 "vvadd_kernel_be.c" 1
	.insn sb 0x23, 0x7, t3, 16(t1)
	
# 0 "" 2
#NO_APP
	li	t1,16
	bne	a6,t1,.L18
	addw	a5,a5,a3
	add	a5,a5,a4
	slli	a5,a5,2
	add	t3,a1,a5
	li	t1,30
#APP
# 36 "vvadd_kernel_be.c" 1
	.insn sb 0x23, 0x6, t1, 16(t3)
	
# 0 "" 2
# 37 "vvadd_kernel_be.c" 1
	.insn sb 0x23, 0x7, t1, 16(t3)
	
# 0 "" 2
#NO_APP
	li	t1,31
	add	a5,a2,a5
#APP
# 38 "vvadd_kernel_be.c" 1
	.insn sb 0x23, 0x6, t1, 16(a5)
	
# 0 "" 2
# 39 "vvadd_kernel_be.c" 1
	.insn sb 0x23, 0x7, t1, 16(a5)
	
# 0 "" 2
#NO_APP
.L3:
#APP
# 43 "vvadd_kernel_be.c" 1
	.insn uj 0x6b, x0, .L4
	
# 0 "" 2
# 54 "vvadd_kernel_be.c" 1
	.insn uj 0x6b, x0, .L5
	
# 0 "" 2
#NO_APP
	slliw	a5,a6,1
	bge	a6,a7,.L6
	mulw	t1,a6,a3
	mv	t3,a6
	slli	a3,a3,2
	li	t5,512
	add	a4,t1,a4
	slli	a4,a4,2
.L7:
#APP
# 78 "vvadd_kernel_be.c" 1
	.insn uj 0x6b, x0, .L8
	
# 0 "" 2
#NO_APP
	add	t1,a1,a4
#APP
# 83 "vvadd_kernel_be.c" 1
	.insn sb 0x23, 0x6, a5, 16(t1)
	
# 0 "" 2
# 84 "vvadd_kernel_be.c" 1
	.insn sb 0x23, 0x7, a5, 16(t1)
	
# 0 "" 2
#NO_APP
	addiw	t1,a5,1
	add	t4,a2,a4
#APP
# 85 "vvadd_kernel_be.c" 1
	.insn sb 0x23, 0x6, t1, 16(t4)
	
# 0 "" 2
# 86 "vvadd_kernel_be.c" 1
	.insn sb 0x23, 0x7, t1, 16(t4)
	
# 0 "" 2
#NO_APP
	addiw	a5,a5,2
	addiw	t3,t3,1
	add	a4,a4,a3
	beq	a5,t5,.L23
	bne	a7,t3,.L7
.L6:
	subw	a0,a0,a6
	ble	a7,a0,.L11
.L12:
#APP
# 146 "vvadd_kernel_be.c" 1
	.insn uj 0x6b, x0, .L8
	
# 0 "" 2
#NO_APP
	addiw	a0,a0,1
	bne	a7,a0,.L12
.L11:
#APP
# 157 "vvadd_kernel_be.c" 1
	.insn uj 0x6b, x0, .L13
	
# 0 "" 2
#NO_APP
.L14:
#APP
# 159 "vvadd_kernel_be.c" 1
	.insn uj 0x2b, x0, .L14
	
# 0 "" 2
# 161 "vvadd_kernel_be.c" 1
	fence
	
# 0 "" 2
# 162 "vvadd_kernel_be.c" 1
	trillium vissue_delim return scalar_return
# 0 "" 2
#NO_APP
	ret
.L4:
#APP
# 165 "vvadd_kernel_be.c" 1
	trillium glue_point vector_init
# 0 "" 2
#NO_APP
.L8:
#APP
# 168 "vvadd_kernel_be.c" 1
	trillium glue_point vector_body
# 0 "" 2
#NO_APP
.L5:
#APP
# 171 "vvadd_kernel_be.c" 1
	trillium glue_point trillium_junk0
# 0 "" 2
#NO_APP
.L13:
#APP
# 177 "vvadd_kernel_be.c" 1
	trillium glue_point vector_return
# 0 "" 2
#NO_APP
	ret
.L16:
	li	a6,16
	j	.L2
.L23:
	beq	a7,t3,.L6
	li	a5,0
	j	.L7
.L18:
	li	a6,15
	j	.L3
	.size	tril_vvadd, .-tril_vvadd
	.comm	start_barrier,32,8
	.ident	"GCC: (GNU) 8.3.0"
	.section	.note.GNU-stack,"",@progbits
