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
	lw	t0,24(sp)
	lw	t2,32(sp)
#APP
# 32 "gemm_kernel.c" 1
	.insn i 0x77, 0, x0, a0, 0x401
	
# 0 "" 2
# 51 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L2
	
# 0 "" 2
#NO_APP
	bge	a7,a5,.L16
	addiw	a5,a5,-1
	addiw	a3,t2,-1
	subw	a5,a5,a7
	subw	a3,a3,t0
	srliw	a5,a5,3
	srliw	a3,a3,3
	slli	a5,a5,3
	slli	a3,a3,3
	add	a5,a5,a7
	add	a3,a3,t0
	slli	a7,a7,2
	addi	t6,a1,32
	slli	a5,a5,2
	slli	s0,t0,2
	slli	a3,a3,2
	addi	a0,a2,32
	add	a1,a1,a7
	add	s0,a2,s0
	add	t6,a5,t6
	add	a2,a3,a0
	slli	a4,a4,2
	li	a7,0
.L15:
#APP
# 57 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L6
	
# 0 "" 2
#NO_APP
	bge	t0,t2,.L7
	mv	s1,s0
.L13:
#APP
# 61 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L8
	
# 0 "" 2
#NO_APP
	blez	a6,.L9
	mv	a0,s1
	mv	a3,a1
	li	t5,0
.L11:
	slliw	a5,a7,2
	addiw	t1,a5,2
#APP
# 75 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a5, 4(a3)
	
# 0 "" 2
# 76 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a5, 4(a3)
	
# 0 "" 2
#NO_APP
	addiw	t3,a5,1
	addi	t4,a3,16
#APP
# 75 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t3, 4(t4)
	
# 0 "" 2
# 76 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, t3, 4(t4)
	
# 0 "" 2
# 83 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t1, 4(a0)
	
# 0 "" 2
# 84 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, t1, 4(a0)
	
# 0 "" 2
#NO_APP
	addiw	a5,a5,3
	addi	t1,a0,16
#APP
# 83 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a5, 4(t1)
	
# 0 "" 2
# 84 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a5, 4(t1)
	
# 0 "" 2
# 111 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L10
	
# 0 "" 2
#NO_APP
	addi	a7,a7,1
	addiw	t5,t5,1
	andi	a7,a7,127
	add	a3,a3,a4
	add	a0,a0,a4
	bne	a6,t5,.L11
.L9:
#APP
# 114 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L12
	
# 0 "" 2
#NO_APP
	addi	s1,s1,32
	bne	a2,s1,.L13
.L7:
#APP
# 116 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L14
	
# 0 "" 2
#NO_APP
	addi	a1,a1,32
	bne	a1,t6,.L15
.L16:
#APP
# 119 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L4
	
# 0 "" 2
#NO_APP
.L5:
#APP
# 121 "gemm_kernel.c" 1
	.insn uj 0x2b, x0, .L5
	
# 0 "" 2
# 200 "gemm_kernel.c" 1
	fence
	
# 0 "" 2
#NO_APP
	j	.L1
.L2:
#APP
# 206 "gemm_kernel.c" 1
	nop
# 0 "" 2
#NO_APP
.L6:
#APP
# 208 "gemm_kernel.c" 1
	nop
# 0 "" 2
#NO_APP
.L8:
#APP
# 210 "gemm_kernel.c" 1
	nop
# 0 "" 2
#NO_APP
.L10:
#APP
# 212 "gemm_kernel.c" 1
	nop
# 0 "" 2
#NO_APP
.L12:
#APP
# 214 "gemm_kernel.c" 1
	nop
# 0 "" 2
#NO_APP
.L14:
#APP
# 216 "gemm_kernel.c" 1
	nop
# 0 "" 2
#NO_APP
.L4:
#APP
# 218 "gemm_kernel.c" 1
	nop
# 0 "" 2
#NO_APP
.L1:
	ld	s0,8(sp)
	ld	s1,0(sp)
	addi	sp,sp,16
	jr	ra
	.size	gemm_vec_simd, .-gemm_vec_simd
	.comm	start_barrier,32,8
	.ident	"GCC: (GNU) 8.3.0"
	.section	.note.GNU-stack,"",@progbits
