	.file	"gemm_kernel.c"
	.option nopic
	.text
	.align	2
	.globl	gemm_vec_simd
	.type	gemm_vec_simd, @function
gemm_vec_simd:
	addi	sp,sp,-80
	sd	s1,64(sp)
	sd	s2,56(sp)
	sd	s3,48(sp)
	sd	s0,72(sp)
	sd	s4,40(sp)
	sd	s5,32(sp)
	sd	s6,24(sp)
	sd	s7,16(sp)
	sd	s8,8(sp)
	sd	s9,0(sp)
	lw	s2,80(sp)
	lw	s1,88(sp)
	lw	s3,96(sp)
#APP
# 55 "gemm_kernel.c" 1
	.insn i 0x77, 0, x0, a0, 0x401
	
# 0 "" 2
# 58 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L2
	
# 0 "" 2
#NO_APP
	subw	a3,s3,s1
	sraiw	t6,a3,31
	srliw	t6,t6,28
	addw	t6,t6,a3
	sraiw	t6,t6,4
	sext.w	s4,a6
	blez	a6,.L3
	li	s4,1
.L3:
	blez	a6,.L4
	slli	a3,a7,2
	add	a3,a1,a3
	li	a0,0
#APP
# 92 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a0, 64(a3)
	
# 0 "" 2
# 93 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a0, 64(a3)
	
# 0 "" 2
#NO_APP
	slli	a3,s1,2
	add	a3,a2,a3
	li	a0,1
#APP
# 100 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a0, 64(a3)
	
# 0 "" 2
# 101 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a0, 64(a3)
	
# 0 "" 2
#NO_APP
	li	a3,1
	li	t1,0
	bge	a7,s2,.L6
.L5:
	sext.w	s6,a6
	mulw	t6,t6,s6
	mv	s8,a7
	slli	s0,a4,2
	slli	t2,a5,2
	mv	s7,a7
	li	t1,0
.L23:
	mv	s5,s1
	ble	s3,s1,.L26
.L22:
	li	a0,0
	beq	s7,a7,.L43
.L7:
	ble	a6,a0,.L8
	mulw	t4,a4,a0
	addw	t0,t1,s6
	subw	t0,t0,a0
	mulw	t3,a5,a0
	add	t4,t4,s8
	slli	t4,t4,2
	add	t4,a1,t4
	add	t3,t3,s5
	slli	t3,t3,2
	add	t3,a2,t3
.L21:
	remw	a0,t1,t6
	bnez	a0,.L9
#APP
# 163 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L10
	
# 0 "" 2
#NO_APP
.L9:
	remw	a0,t1,a6
	bnez	a0,.L11
#APP
# 164 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L12
	
# 0 "" 2
#NO_APP
.L11:
#APP
# 165 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L15
	
# 0 "" 2
#NO_APP
	addiw	t1,t1,1
	slliw	t5,a3,1
	mv	a0,t1
	addiw	s9,t5,1
#APP
# 178 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t5, 64(t4)
	
# 0 "" 2
# 179 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, t5, 64(t4)
	
# 0 "" 2
# 186 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, s9, 64(t3)
	
# 0 "" 2
# 187 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, s9, 64(t3)
	
# 0 "" 2
#NO_APP
	remw	t5,t1,a6
	bnez	t5,.L16
#APP
# 234 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L18
	
# 0 "" 2
#NO_APP
.L16:
	remw	a0,a0,t6
	bnez	a0,.L19
#APP
# 235 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L20
	
# 0 "" 2
#NO_APP
.L19:
	addi	a3,a3,1
	andi	a3,a3,15
	add	t4,t4,s0
	add	t3,t3,t2
	bne	t0,t1,.L21
.L8:
	addi	s5,s5,16
	sext.w	a0,s5
	blt	a0,s3,.L22
.L26:
	addiw	s7,s7,16
	addi	s8,s8,16
	bgt	s2,s7,.L23
	blez	a6,.L29
.L6:
#APP
# 244 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L15
	
# 0 "" 2
#NO_APP
	addiw	t1,t1,1
	remw	t1,t1,a6
	bnez	t1,.L29
#APP
# 246 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L18
	
# 0 "" 2
#NO_APP
.L29:
#APP
# 250 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L27
	
# 0 "" 2
#NO_APP
.L28:
#APP
# 252 "gemm_kernel.c" 1
	.insn uj 0x2b, x0, .L28
	
# 0 "" 2
# 358 "gemm_kernel.c" 1
	fence
	
# 0 "" 2
#NO_APP
	j	.L1
.L2:
#APP
# 364 "gemm_kernel.c" 1
	nop
# 0 "" 2
#NO_APP
.L10:
#APP
# 366 "gemm_kernel.c" 1
	nop
# 0 "" 2
#NO_APP
.L12:
#APP
# 368 "gemm_kernel.c" 1
	nop
# 0 "" 2
#NO_APP
.L15:
#APP
# 370 "gemm_kernel.c" 1
	nop
# 0 "" 2
#NO_APP
.L18:
#APP
# 372 "gemm_kernel.c" 1
	nop
# 0 "" 2
#NO_APP
.L20:
#APP
# 374 "gemm_kernel.c" 1
	nop
# 0 "" 2
#NO_APP
.L27:
#APP
# 376 "gemm_kernel.c" 1
	nop
# 0 "" 2
#NO_APP
.L1:
	ld	s0,72(sp)
	ld	s1,64(sp)
	ld	s2,56(sp)
	ld	s3,48(sp)
	ld	s4,40(sp)
	ld	s5,32(sp)
	ld	s6,24(sp)
	ld	s7,16(sp)
	ld	s8,8(sp)
	ld	s9,0(sp)
	addi	sp,sp,80
	jr	ra
.L4:
	li	a3,0
	blt	a7,s2,.L5
#APP
# 250 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L27
	
# 0 "" 2
#NO_APP
	j	.L28
.L43:
	sext.w	t3,s5
	mv	a0,s4
	beq	t3,s1,.L7
	li	a0,0
	j	.L7
	.size	gemm_vec_simd, .-gemm_vec_simd
	.comm	start_barrier,32,8
	.ident	"GCC: (GNU) 8.3.0"
	.section	.note.GNU-stack,"",@progbits
