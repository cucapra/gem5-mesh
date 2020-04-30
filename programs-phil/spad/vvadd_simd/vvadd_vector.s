	.file	"vvadd_kernel.c"
	.option nopic
	.text
	.align	2
	.globl	vvadd_execute_simd
	.type	vvadd_execute_simd, @function
vvadd_execute_simd:
	addi	sp,sp,-16
#APP
# 229 "vvadd_kernel.c" 1
	init_block_start
# 0 "" 2
#NO_APP
	add	a4,a4,a7
	slli	a4,a4,2
	li	a5,15
	add	a3,a3,a4
	bgt	a6,a5,.L5
	lui	a5,%hi(spm_base_ptr_arr)
	slli	a6,a6,3
	addi	a5,a5,%lo(spm_base_ptr_arr)
	add	a6,a6,a5
	ld	a1,0(a6)
	addi	a1,a1,16
.L2:
#APP
# 235 "vvadd_kernel.c" 1
	init_block_end
# 0 "" 2
#NO_APP
	lw	a5,12(sp)
	sext.w	a5,a5
	beqz	a5,.L3
	lw	a0,16(sp)
	li	a2,0
	slli	a0,a0,2
.L4:
#APP
# 239 "vvadd_kernel.c" 1
	vector_body_start
# 0 "" 2
#NO_APP
	slli	a5,a2,2
	add	a4,a1,a5
#APP
# 249 "vvadd_kernel.c" 1
	.insn s 0x03, 0x7, a4, 0(a4)
	
# 0 "" 2
#NO_APP
	addi	a5,a5,4
	add	a5,a1,a5
#APP
# 250 "vvadd_kernel.c" 1
	.insn s 0x03, 0x7, a5, 0(a5)
	
# 0 "" 2
# 254 "vvadd_kernel.c" 1
	.insn u 0x0b, x0, 0
	
# 0 "" 2
#NO_APP
	addw	a5,a4,a5
#APP
# 258 "vvadd_kernel.c" 1
	.insn sb 0x23, 0x5, a5, 0(a3)
	
# 0 "" 2
#NO_APP
	addi	a2,a2,2
	add	a3,a3,a0
	andi	a2,a2,511
#APP
# 264 "vvadd_kernel.c" 1
	vector_body_end
# 0 "" 2
#NO_APP
	lw	a5,12(sp)
	sext.w	a5,a5
	bnez	a5,.L4
.L3:
#APP
# 266 "vvadd_kernel.c" 1
	vector_return
# 0 "" 2
#NO_APP
	addi	sp,sp,16
	jr	ra
.L5:
	li	a1,0
	j	.L2
	.size	vvadd_execute_simd, .-vvadd_execute_simd
	.comm	start_barrier,32,8
	.ident	"GCC: (GNU) 8.3.0"
	.section	.note.GNU-stack,"",@progbits
