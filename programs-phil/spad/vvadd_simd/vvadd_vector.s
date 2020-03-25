	.file	"vvadd_kernel.c"
	.option nopic
	.text
	.align	2
	.globl	vvadd_execute_simd
	.type	vvadd_execute_simd, @function
vvadd_execute_simd:
	addi	sp,sp,-64
	li	a1,0
	mv	a0,a5
	sd	s0,48(sp)
	sd	s1,40(sp)
	sd	s2,32(sp)
	sd	s3,24(sp)
	sd	ra,56(sp)
	mv	s2,a2
	mv	s1,a3
	mv	s3,a6
	mv	s0,a7
	call	getSpAddr
	lw	a5,64(sp)
#APP
# 24 "vvadd_kernel.c" 1
	.insn i 0x77, 0, x0, a5, 0x401
	
# 0 "" 2
#NO_APP
	lw	a5,8(sp)
	add	s1,s1,s3
	slli	s1,s1,2
	sext.w	a5,a5
	add	a2,s2,s1
	beqz	a5,.L6
	slli	a1,s0,2
	li	a5,0
.L3:
	slli	a4,a5,2
	add	a3,a0,a4
#APP
# 96 "vvadd_kernel.c" 1
	.insn s 0x03, 0x7, a3, 0(a3)
	
# 0 "" 2
#NO_APP
	addi	a4,a4,4
	add	a4,a0,a4
#APP
# 97 "vvadd_kernel.c" 1
	.insn s 0x03, 0x7, a4, 0(a4)
	
# 0 "" 2
# 101 "vvadd_kernel.c" 1
	.insn u 0x0b, x0, 0
	
# 0 "" 2
#NO_APP
	addw	a4,a3,a4
#APP
# 105 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x5, a4, 0(a2)
	
# 0 "" 2
#NO_APP
	lw	a4,8(sp)
	addi	a5,a5,2
	add	a2,a2,a1
	sext.w	a4,a4
	andi	a5,a5,511
	bnez	a4,.L3
.L2:
	lw	a4,12(sp)
	sext.w	a4,a4
	beqz	a4,.L4
	slli	a7,s0,2
.L5:
	slli	a4,a5,2
	add	a3,a0,a4
#APP
# 145 "vvadd_kernel.c" 1
	.insn s 0x03, 0x7, a3, 0(a3)
	
# 0 "" 2
#NO_APP
	addi	a4,a4,4
	add	a4,a0,a4
#APP
# 146 "vvadd_kernel.c" 1
	.insn s 0x03, 0x7, a4, 0(a4)
	
# 0 "" 2
# 150 "vvadd_kernel.c" 1
	.insn u 0x0b, x0, 0
	
# 0 "" 2
#NO_APP
	addw	a4,a3,a4
#APP
# 154 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x5, a4, 0(a2)
	
# 0 "" 2
#NO_APP
	lw	a4,12(sp)
	addi	a5,a5,2
	add	a2,a2,a7
	sext.w	a4,a4
	andi	a5,a5,511
	bnez	a4,.L5
.L4:
#APP
# 169 "vvadd_kernel.c" 1
	fence
	
# 0 "" 2
#NO_APP
	ld	ra,56(sp)
	ld	s0,48(sp)
	ld	s1,40(sp)
	ld	s2,32(sp)
	ld	s3,24(sp)
	addi	sp,sp,64
	jr	ra
.L6:
	li	a5,0
	j	.L2
	.size	vvadd_execute_simd, .-vvadd_execute_simd
	.comm	start_barrier,32,8
	.ident	"GCC: (GNU) 8.3.0"
	.section	.note.GNU-stack,"",@progbits
