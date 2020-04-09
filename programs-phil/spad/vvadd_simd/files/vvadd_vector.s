	.file	"vvadd_kernel.c"
	.option nopic
	.text
	.align	2
	.globl	vvadd_execute_simd
	.type	vvadd_execute_simd, @function
vvadd_execute_simd:
	addi	sp,sp,-16
	lw	t1,16(sp)
#APP
# 25 "vvadd_kernel.c" 1
	.insn i 0x77, 0, x0, a0, 0x401
	
# 0 "" 2
#NO_APP
	add	a4,a4,a7
	slli	a4,a4,2
	li	a5,15
	add	a3,a3,a4
	bgt	a6,a5,.L7
	lui	a5,%hi(spm_base_ptr_arr)
	addi	a5,a5,%lo(spm_base_ptr_arr)
	slli	a6,a6,3
	add	a6,a6,a5
	lw	a5,8(sp)
	ld	a1,0(a6)
	sext.w	a5,a5
	addi	a1,a1,16
	beqz	a5,.L8
.L16:
	slli	a0,t1,2
	li	a5,0
.L4:
	slli	a4,a5,2
	add	a2,a1,a4
#APP
# 112 "vvadd_kernel.c" 1
	.insn s 0x03, 0x7, a2, 0(a2)
	
# 0 "" 2
#NO_APP
	addi	a4,a4,4
	add	a4,a1,a4
#APP
# 113 "vvadd_kernel.c" 1
	.insn s 0x03, 0x7, a4, 0(a4)
	
# 0 "" 2
# 117 "vvadd_kernel.c" 1
	.insn u 0x0b, x0, 0
	
# 0 "" 2
#NO_APP
	addw	a4,a2,a4
#APP
# 121 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x5, a4, 0(a3)
	
# 0 "" 2
#NO_APP
	lw	a4,8(sp)
	addi	a5,a5,2
	add	a3,a3,a0
	sext.w	a4,a4
	andi	a5,a5,511
	bnez	a4,.L4
.L3:
	lw	a4,12(sp)
	sext.w	a4,a4
	beqz	a4,.L5
	slli	a0,t1,2
.L6:
	slli	a4,a5,2
	add	a2,a1,a4
#APP
# 163 "vvadd_kernel.c" 1
	.insn s 0x03, 0x7, a2, 0(a2)
	
# 0 "" 2
#NO_APP
	addi	a4,a4,4
	add	a4,a1,a4
#APP
# 164 "vvadd_kernel.c" 1
	.insn s 0x03, 0x7, a4, 0(a4)
	
# 0 "" 2
# 168 "vvadd_kernel.c" 1
	.insn u 0x0b, x0, 0
	
# 0 "" 2
#NO_APP
	addw	a4,a2,a4
#APP
# 172 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x5, a4, 0(a3)
	
# 0 "" 2
#NO_APP
	lw	a4,12(sp)
	addi	a5,a5,2
	add	a3,a3,a0
	sext.w	a4,a4
	andi	a5,a5,511
	bnez	a4,.L6
.L5:
#APP
# 188 "vvadd_kernel.c" 1
	fence
	
# 0 "" 2
#NO_APP
	addi	sp,sp,16
	jr	ra
.L7:
	lw	a5,8(sp)
	li	a1,0
	sext.w	a5,a5
	bnez	a5,.L16
.L8:
	li	a5,0
	j	.L3
	.size	vvadd_execute_simd, .-vvadd_execute_simd
	.comm	start_barrier,32,8
	.ident	"GCC: (GNU) 8.3.0"
	.section	.note.GNU-stack,"",@progbits
