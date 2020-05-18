	.file	"gemm_kernel.c"
	.option nopic
	.text
	.align	2
	.globl	gemm_vec_simd
	.type	gemm_vec_simd, @function
gemm_vec_simd:
	addi	sp,sp,-144
	sd	s8,72(sp)
	sd	s9,64(sp)
	sd	s10,56(sp)
	sd	s0,136(sp)
	sd	s1,128(sp)
	sd	s2,120(sp)
	sd	s3,112(sp)
	sd	s4,104(sp)
	sd	s5,96(sp)
	sd	s6,88(sp)
	sd	s7,80(sp)
	sd	s11,48(sp)
	lw	s10,144(sp)
	lw	s8,152(sp)
	lw	s9,160(sp)
#APP
# 40 "gemm_kernel.c" 1
	.insn i 0x77, 0, x0, a0, 0x401
	
# 0 "" 2
# 43 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L2
	
# 0 "" 2
#NO_APP
	subw	a5,s9,s8
	sraiw	t4,a5,31
	srliw	t4,t4,29
	addw	t4,t4,a5
	sraiw	t4,t4,3
	sext.w	a5,a6
	blez	a6,.L3
	li	a5,1
.L3:
	sd	a5,40(sp)
	blez	a6,.L4
	slli	a5,a7,2
	add	a0,a1,a5
	li	a3,0
#APP
# 96 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a3, 17(a0)
	
# 0 "" 2
# 97 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a3, 17(a0)
	
# 0 "" 2
#NO_APP
	slli	t5,s8,2
	add	t1,a2,t5
	li	a3,4
#APP
# 100 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a3, 17(t1)
	
# 0 "" 2
# 101 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a3, 17(t1)
	
# 0 "" 2
#NO_APP
	li	a3,4096
#APP
# 96 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a3, 17(a0)
	
# 0 "" 2
# 97 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a3, 17(a0)
	
# 0 "" 2
#NO_APP
	addi	a0,t5,16
	add	a0,a2,a0
	addiw	a3,a3,4
#APP
# 100 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a3, 17(a0)
	
# 0 "" 2
# 101 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a3, 17(a0)
	
# 0 "" 2
#NO_APP
	addi	a5,a5,16
	add	a5,a1,a5
	li	a3,8192
#APP
# 96 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a3, 17(a5)
	
# 0 "" 2
# 97 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a3, 17(a5)
	
# 0 "" 2
#NO_APP
	addiw	a3,a3,4
#APP
# 100 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a3, 17(t1)
	
# 0 "" 2
# 101 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a3, 17(t1)
	
# 0 "" 2
#NO_APP
	li	a3,12288
#APP
# 96 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a3, 17(a5)
	
# 0 "" 2
# 97 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a3, 17(a5)
	
# 0 "" 2
#NO_APP
	addiw	a3,a3,4
#APP
# 100 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a3, 17(a0)
	
# 0 "" 2
# 101 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a3, 17(a0)
	
# 0 "" 2
#NO_APP
	li	t3,1
	li	t1,0
	bge	a7,s10,.L6
.L5:
	sext.w	a5,a6
	sd	a5,16(sp)
	mulw	t4,t4,a5
	add	a5,a2,t5
	mv	s11,a7
	sd	a5,24(sp)
	slli	s4,a4,2
	sd	a7,8(sp)
	li	t1,0
	li	s3,4096
	li	s2,8192
	addi	s7,a1,16
	li	s1,12288
.L23:
	ble	s9,s8,.L26
	ld	a5,24(sp)
	slli	s5,s11,2
	sd	s8,32(sp)
	sub	s5,a5,s5
.L22:
	ld	a3,8(sp)
	li	a5,0
	beq	a7,a3,.L42
.L7:
	ble	a6,a5,.L8
	mulw	a0,a4,a5
	ld	a3,16(sp)
	addi	s6,s5,16
	addw	s0,t1,a3
	subw	s0,s0,a5
	add	a0,a0,s11
	slli	a0,a0,2
.L21:
	remw	a5,t1,t4
	bnez	a5,.L9
#APP
# 148 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L10
	
# 0 "" 2
#NO_APP
.L9:
	remw	a5,t1,a6
	bnez	a5,.L11
#APP
# 149 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L12
	
# 0 "" 2
#NO_APP
.L11:
#APP
# 150 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L15
	
# 0 "" 2
#NO_APP
	slliw	a3,t3,3
	addiw	t1,t1,1
	mv	a2,t1
	addiw	a5,a3,4
	add	t6,a0,a1
#APP
# 181 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a3, 17(t6)
	
# 0 "" 2
# 182 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a3, 17(t6)
	
# 0 "" 2
#NO_APP
	add	t2,s5,a0
#APP
# 185 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a5, 17(t2)
	
# 0 "" 2
# 186 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a5, 17(t2)
	
# 0 "" 2
#NO_APP
	or	t5,a3,s3
	sext.w	t5,t5
#APP
# 181 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t5, 17(t6)
	
# 0 "" 2
# 182 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, t5, 17(t6)
	
# 0 "" 2
#NO_APP
	or	t6,a5,s3
	sext.w	t6,t6
	add	t5,s6,a0
#APP
# 185 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t6, 17(t5)
	
# 0 "" 2
# 186 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, t6, 17(t5)
	
# 0 "" 2
#NO_APP
	or	t0,a3,s2
	sext.w	t0,t0
	add	t6,s7,a0
#APP
# 181 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t0, 17(t6)
	
# 0 "" 2
# 182 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, t0, 17(t6)
	
# 0 "" 2
#NO_APP
	or	t0,a5,s2
	sext.w	t0,t0
#APP
# 185 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t0, 17(t2)
	
# 0 "" 2
# 186 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, t0, 17(t2)
	
# 0 "" 2
#NO_APP
	or	a3,a3,s1
	sext.w	a3,a3
#APP
# 181 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a3, 17(t6)
	
# 0 "" 2
# 182 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a3, 17(t6)
	
# 0 "" 2
#NO_APP
	or	a5,a5,s1
	sext.w	a5,a5
#APP
# 185 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a5, 17(t5)
	
# 0 "" 2
# 186 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a5, 17(t5)
	
# 0 "" 2
#NO_APP
	remw	a5,t1,a6
	addi	t3,t3,1
	andi	t3,t3,63
	bnez	a5,.L16
#APP
# 219 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L18
	
# 0 "" 2
#NO_APP
.L16:
	remw	a2,a2,t4
	bnez	a2,.L19
#APP
# 220 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L20
	
# 0 "" 2
#NO_APP
.L19:
	add	a0,a0,s4
	bne	t1,s0,.L21
.L8:
	ld	a5,32(sp)
	addi	s5,s5,32
	addiw	a5,a5,8
	sd	a5,32(sp)
	bgt	s9,a5,.L22
.L26:
	ld	a5,8(sp)
	addi	s11,s11,8
	addiw	a5,a5,8
	sd	a5,8(sp)
	bgt	s10,a5,.L23
	blez	a6,.L29
.L6:
#APP
# 229 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L15
	
# 0 "" 2
#NO_APP
	addiw	t1,t1,1
	remw	t1,t1,a6
	bnez	t1,.L29
#APP
# 231 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L18
	
# 0 "" 2
#NO_APP
.L29:
#APP
# 235 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L27
	
# 0 "" 2
#NO_APP
.L28:
#APP
# 237 "gemm_kernel.c" 1
	.insn uj 0x2b, x0, .L28
	
# 0 "" 2
# 343 "gemm_kernel.c" 1
	fence
	
# 0 "" 2
#NO_APP
	j	.L1
.L2:
#APP
# 349 "gemm_kernel.c" 1
	nop
# 0 "" 2
#NO_APP
.L10:
#APP
# 351 "gemm_kernel.c" 1
	nop
# 0 "" 2
#NO_APP
.L12:
#APP
# 353 "gemm_kernel.c" 1
	nop
# 0 "" 2
#NO_APP
.L15:
#APP
# 355 "gemm_kernel.c" 1
	nop
# 0 "" 2
#NO_APP
.L18:
#APP
# 357 "gemm_kernel.c" 1
	nop
# 0 "" 2
#NO_APP
.L20:
#APP
# 359 "gemm_kernel.c" 1
	nop
# 0 "" 2
#NO_APP
.L27:
#APP
# 361 "gemm_kernel.c" 1
	nop
# 0 "" 2
#NO_APP
.L1:
	ld	s0,136(sp)
	ld	s1,128(sp)
	ld	s2,120(sp)
	ld	s3,112(sp)
	ld	s4,104(sp)
	ld	s5,96(sp)
	ld	s6,88(sp)
	ld	s7,80(sp)
	ld	s8,72(sp)
	ld	s9,64(sp)
	ld	s10,56(sp)
	ld	s11,48(sp)
	addi	sp,sp,144
	jr	ra
.L4:
	li	t3,0
	slli	t5,s8,2
	blt	a7,s10,.L5
	j	.L29
.L42:
	ld	a3,32(sp)
	ld	a5,40(sp)
	beq	s8,a3,.L7
	li	a5,0
	j	.L7
	.size	gemm_vec_simd, .-gemm_vec_simd
	.comm	start_barrier,32,8
	.ident	"GCC: (GNU) 8.3.0"
	.section	.note.GNU-stack,"",@progbits
