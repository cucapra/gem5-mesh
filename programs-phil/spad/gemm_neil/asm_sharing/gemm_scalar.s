	.file	"gemm_kernel.c"
	.option nopic
	.text
	.align	2
	.globl	gemm_vec_simd
	.type	gemm_vec_simd, @function
gemm_vec_simd:
	addi	sp,sp,-16
	sd	s0,8(sp)
	sd	s1,0(sp)
	lw	a5,16(sp)
	lw	t6,24(sp)
	lw	t0,32(sp)
	lw	a3,64(sp)
#APP
# 20 "gemm_kernel.c" 1
	.insn i 0x77, 0, x0, a0, 0x401
	
# 0 "" 2
#NO_APP
	li	a0,15
	bgt	a3,a0,.L19
	lui	a0,%hi(spm_base_ptr_arr)
	addi	a0,a0,%lo(spm_base_ptr_arr)
	slli	a3,a3,3
	add	a3,a3,a0
	ld	a0,0(a3)
	addi	a0,a0,16
.L2:
#APP
# 36 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L3
	
# 0 "" 2
#NO_APP
	bge	a7,a5,.L17
	addiw	a5,a5,-1
	addiw	a3,t0,-1
	subw	a5,a5,a7
	subw	a3,a3,t6
	srliw	a5,a5,3
	srliw	a3,a3,3
	slli	a5,a5,3
	slli	a3,a3,3
	add	a5,a5,a7
	add	a3,a3,t6
	addi	t2,a1,32
	slli	a3,a3,2
	addi	s0,a2,32
	slli	a7,a7,2
	slli	a5,a5,2
	slli	t1,t6,2
	add	s0,a3,s0
	add	a1,a1,a7
	add	t2,a5,t2
	add	a2,a2,t1
	slli	a4,a4,2
	li	a3,0
.L16:
#APP
# 42 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L7
	
# 0 "" 2
#NO_APP
	bge	t6,t0,.L8
	mv	s1,a2
.L14:
#APP
# 46 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L9
	
# 0 "" 2
#NO_APP
	blez	a6,.L10
	mv	t3,s1
	mv	t1,a1
	li	t4,0
.L12:
	slli	a5,a3,2
	slli	a7,a5,2
	add	a7,a0,a7
#APP
# 55 "gemm_kernel.c" 1
	.insn sb 0x23, 0x4, a7, 0(t1)
	
# 0 "" 2
#NO_APP
	addi	a5,a5,1
	slli	a5,a5,2
	add	a7,a0,a5
	addi	t5,t1,16
#APP
# 55 "gemm_kernel.c" 1
	.insn sb 0x23, 0x4, a7, 0(t5)
	
# 0 "" 2
#NO_APP
	addi	a7,a5,4
	add	a7,a0,a7
#APP
# 61 "gemm_kernel.c" 1
	.insn sb 0x23, 0x4, a7, 0(t3)
	
# 0 "" 2
#NO_APP
	addi	a5,a5,8
	add	a5,a0,a5
	addi	a7,t3,16
#APP
# 61 "gemm_kernel.c" 1
	.insn sb 0x23, 0x4, a5, 0(a7)
	
# 0 "" 2
# 63 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L11
	
# 0 "" 2
#NO_APP
	addi	a3,a3,1
	addiw	t4,t4,1
	andi	a3,a3,127
	add	t1,t1,a4
	add	t3,t3,a4
	bne	a6,t4,.L12
.L10:
#APP
# 66 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L13
	
# 0 "" 2
#NO_APP
	addi	s1,s1,32
	bne	s0,s1,.L14
.L8:
#APP
# 68 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L15
	
# 0 "" 2
#NO_APP
	addi	a1,a1,32
	bne	t2,a1,.L16
.L17:
#APP
# 71 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L5
	
# 0 "" 2
#NO_APP
.L6:
#APP
# 73 "gemm_kernel.c" 1
	.insn uj 0x2b, x0, .L6
	
# 0 "" 2
# 145 "gemm_kernel.c" 1
	fence
	
# 0 "" 2
#NO_APP
	j	.L1
.L3:
#APP
# 151 "gemm_kernel.c" 1
	nop
# 0 "" 2
#NO_APP
.L7:
#APP
# 153 "gemm_kernel.c" 1
	nop
# 0 "" 2
#NO_APP
.L9:
#APP
# 155 "gemm_kernel.c" 1
	nop
# 0 "" 2
#NO_APP
.L11:
#APP
# 157 "gemm_kernel.c" 1
	nop
# 0 "" 2
#NO_APP
.L13:
#APP
# 159 "gemm_kernel.c" 1
	nop
# 0 "" 2
#NO_APP
.L15:
#APP
# 161 "gemm_kernel.c" 1
	nop
# 0 "" 2
#NO_APP
.L5:
#APP
# 163 "gemm_kernel.c" 1
	nop
# 0 "" 2
#NO_APP
.L1:
	ld	s0,8(sp)
	ld	s1,0(sp)
	addi	sp,sp,16
	jr	ra
.L19:
	li	a0,0
	j	.L2
	.size	gemm_vec_simd, .-gemm_vec_simd
	.comm	start_barrier,32,8
	.ident	"GCC: (GNU) 8.3.0"
	.section	.note.GNU-stack,"",@progbits
