	.file	"vvadd_kernel.c"
	.option nopic
	.text
	.align	2
	.globl	vvadd_execute_simd
	.type	vvadd_execute_simd, @function
vvadd_execute_simd:
	addi	sp,sp,-48
	sd	s1,24(sp)
	sd	ra,40(sp)
	sd	s0,32(sp)
	lw	s1,48(sp)
#APP
# 25 "vvadd_kernel.c" 1
	.insn i 0x77, 0, x0, a0, 0x401
	
# 0 "" 2
#NO_APP
	add	a4,a4,a7
	slli	a4,a4,2
	li	a1,0
	mv	a0,a6
	add	s0,a3,a4
	call	getSpAddr
	lw	a5,8(sp)
	sext.w	a5,a5
	beqz	a5,.L6
	slli	a1,s1,2
	li	a5,0
	li	a2,2
.L3:
#APP
# 109 "vvadd_kernel.c" 1
	.insn i 0x1b, 0x3, x0, a2, 0
	
# 0 "" 2
#NO_APP
	slli	a4,a5,2
	add	a4,a0,a4
	lw	a3,0(a4)
	lw	a4,4(a4)
#APP
# 119 "vvadd_kernel.c" 1
	.insn i 0x1b, 0x2, x0, a2, 0
	
# 0 "" 2
#NO_APP
	addw	a4,a3,a4
#APP
# 123 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x5, a4, 0(s0)
	
# 0 "" 2
#NO_APP
	lw	a4,8(sp)
	addi	a5,a5,2
	add	s0,s0,a1
	sext.w	a4,a4
	andi	a5,a5,511
	bnez	a4,.L3
.L2:
	lw	a4,12(sp)
	sext.w	a4,a4
	beqz	a4,.L4
	slli	a1,s1,2
	li	a2,2
.L5:
#APP
# 164 "vvadd_kernel.c" 1
	.insn i 0x1b, 0x3, x0, a2, 0
	
# 0 "" 2
#NO_APP
	slli	a4,a5,2
	add	a4,a0,a4
	lw	a3,0(a4)
	lw	a4,4(a4)
#APP
# 174 "vvadd_kernel.c" 1
	.insn i 0x1b, 0x2, x0, a2, 0
	
# 0 "" 2
#NO_APP
	addw	a4,a3,a4
#APP
# 178 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x5, a4, 0(s0)
	
# 0 "" 2
#NO_APP
	lw	a4,12(sp)
	addi	a5,a5,2
	add	s0,s0,a1
	sext.w	a4,a4
	andi	a5,a5,511
	bnez	a4,.L5
.L4:
#APP
# 194 "vvadd_kernel.c" 1
	fence
	
# 0 "" 2
#NO_APP
	ld	ra,40(sp)
	ld	s0,32(sp)
	ld	s1,24(sp)
	addi	sp,sp,48
	jr	ra
.L6:
	li	a5,0
	j	.L2
	.size	vvadd_execute_simd, .-vvadd_execute_simd
	.comm	start_barrier,32,8
	.ident	"GCC: (GNU) 8.3.0"
	.section	.note.GNU-stack,"",@progbits
