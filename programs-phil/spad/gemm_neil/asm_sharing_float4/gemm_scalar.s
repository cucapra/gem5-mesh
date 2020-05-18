	.file	"gemm_kernel.c"
	.option nopic
	.text
	.align	2
	.globl	gemm_vec_simd
	.type	gemm_vec_simd, @function
gemm_vec_simd:
	addi	sp,sp,-96
	sd	s0,88(sp)
	sd	s2,72(sp)
	sd	s3,64(sp)
	sd	s1,80(sp)
	sd	s4,56(sp)
	sd	s5,48(sp)
	sd	s6,40(sp)
	sd	s7,32(sp)
	sd	s8,24(sp)
	sd	s9,16(sp)
	sd	s10,8(sp)
	sd	s11,0(sp)
	lw	s3,96(sp)
	lw	s0,104(sp)
	lw	s2,112(sp)
#APP
# 37 "gemm_kernel.c" 1
	.insn i 0x77, 0, x0, a0, 0x401
	
# 0 "" 2
# 40 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L2
	
# 0 "" 2
#NO_APP
	subw	a5,s2,s0
	sraiw	t3,a5,31
	srliw	t3,t3,29
	addw	t3,t3,a5
	sraiw	t3,t3,3
	sext.w	s4,a6
	blez	a6,.L3
	li	s4,1
.L3:
	bgtz	a6,.L42
	li	t1,0
	slli	s1,s0,2
	bge	a7,s3,.L29
.L5:
	sext.w	s6,a6
	mulw	t3,t3,s6
	mv	s5,a7
	add	s1,a2,s1
	slli	t5,a4,2
	mv	s7,a7
	li	a0,0
	addi	t2,a1,16
.L23:
	ble	s2,s0,.L26
	slli	t6,s5,2
	sub	t6,s1,t6
	mv	s8,s0
.L22:
	li	a5,0
	beq	s7,a7,.L43
.L7:
	ble	a6,a5,.L8
	mulw	a2,a4,a5
	addw	t4,a0,s6
	subw	t4,t4,a5
	addi	t0,t6,16
	add	a2,a2,s5
	slli	a2,a2,2
.L21:
	remw	a5,a0,t3
	bnez	a5,.L9
#APP
# 139 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L10
	
# 0 "" 2
#NO_APP
.L9:
	remw	a5,a0,a6
	bnez	a5,.L11
#APP
# 140 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L12
	
# 0 "" 2
#NO_APP
.L11:
#APP
# 141 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L15
	
# 0 "" 2
#NO_APP
	addiw	a0,a0,1
	slliw	a5,t1,2
	mv	a3,a0
	addiw	s9,a5,2
	add	s10,a1,a2
#APP
# 154 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a5, 16(s10)
	
# 0 "" 2
# 155 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a5, 16(s10)
	
# 0 "" 2
#NO_APP
	addiw	s10,a5,1
	add	s11,t2,a2
#APP
# 154 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, s10, 16(s11)
	
# 0 "" 2
# 155 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, s10, 16(s11)
	
# 0 "" 2
#NO_APP
	add	s10,t6,a2
#APP
# 162 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, s9, 16(s10)
	
# 0 "" 2
# 163 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, s9, 16(s10)
	
# 0 "" 2
#NO_APP
	addiw	a5,a5,3
	add	s9,t0,a2
#APP
# 162 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a5, 16(s9)
	
# 0 "" 2
# 163 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a5, 16(s9)
	
# 0 "" 2
#NO_APP
	remw	a5,a0,a6
	addi	t1,t1,1
	andi	t1,t1,127
	bnez	a5,.L16
#APP
# 210 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L18
	
# 0 "" 2
#NO_APP
.L16:
	remw	a3,a3,t3
	bnez	a3,.L19
#APP
# 211 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L20
	
# 0 "" 2
#NO_APP
.L19:
	add	a2,a2,t5
	bne	t4,a0,.L21
.L8:
	addiw	s8,s8,8
	addi	t6,t6,32
	bgt	s2,s8,.L22
.L26:
	addiw	s7,s7,8
	addi	s5,s5,8
	bgt	s3,s7,.L23
	blez	a6,.L29
.L6:
#APP
# 220 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L15
	
# 0 "" 2
#NO_APP
	addiw	a0,a0,1
	remw	a0,a0,a6
	bnez	a0,.L29
#APP
# 222 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L18
	
# 0 "" 2
#NO_APP
.L29:
#APP
# 226 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L27
	
# 0 "" 2
#NO_APP
.L28:
#APP
# 228 "gemm_kernel.c" 1
	.insn uj 0x2b, x0, .L28
	
# 0 "" 2
# 317 "gemm_kernel.c" 1
	fence
	
# 0 "" 2
#NO_APP
	j	.L1
.L2:
#APP
# 323 "gemm_kernel.c" 1
	nop
# 0 "" 2
#NO_APP
.L10:
#APP
# 325 "gemm_kernel.c" 1
	nop
# 0 "" 2
#NO_APP
.L12:
#APP
# 327 "gemm_kernel.c" 1
	nop
# 0 "" 2
#NO_APP
.L15:
#APP
# 329 "gemm_kernel.c" 1
	nop
# 0 "" 2
#NO_APP
.L18:
#APP
# 331 "gemm_kernel.c" 1
	nop
# 0 "" 2
#NO_APP
.L20:
#APP
# 333 "gemm_kernel.c" 1
	nop
# 0 "" 2
#NO_APP
.L27:
#APP
# 335 "gemm_kernel.c" 1
	nop
# 0 "" 2
#NO_APP
.L1:
	ld	s0,88(sp)
	ld	s1,80(sp)
	ld	s2,72(sp)
	ld	s3,64(sp)
	ld	s4,56(sp)
	ld	s5,48(sp)
	ld	s6,40(sp)
	ld	s7,32(sp)
	ld	s8,24(sp)
	ld	s9,16(sp)
	ld	s10,8(sp)
	ld	s11,0(sp)
	addi	sp,sp,96
	jr	ra
.L42:
	slli	a5,a7,2
	add	a0,a1,a5
	li	a3,0
#APP
# 68 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a3, 16(a0)
	
# 0 "" 2
# 69 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a3, 16(a0)
	
# 0 "" 2
#NO_APP
	addi	a5,a5,16
	add	a5,a1,a5
	li	a3,1
#APP
# 68 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a3, 16(a5)
	
# 0 "" 2
# 69 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a3, 16(a5)
	
# 0 "" 2
#NO_APP
	slli	s1,s0,2
	add	a3,a2,s1
	li	a5,2
#APP
# 76 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a5, 16(a3)
	
# 0 "" 2
# 77 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a5, 16(a3)
	
# 0 "" 2
#NO_APP
	addi	a5,s1,16
	add	a5,a2,a5
	li	a3,3
#APP
# 76 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a3, 16(a5)
	
# 0 "" 2
# 77 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a3, 16(a5)
	
# 0 "" 2
#NO_APP
	li	t1,1
	li	a0,0
	blt	a7,s3,.L5
	j	.L6
.L43:
	mv	a5,s4
	beq	s0,s8,.L7
	li	a5,0
	j	.L7
	.size	gemm_vec_simd, .-gemm_vec_simd
	.comm	start_barrier,32,8
	.ident	"GCC: (GNU) 8.3.0"
	.section	.note.GNU-stack,"",@progbits
