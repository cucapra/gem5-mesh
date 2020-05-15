	.file	"gemm_kernel.c"
	.option nopic
	.text
	.align	2
	.globl	gemm_vec_simd
	.type	gemm_vec_simd, @function
gemm_vec_simd:
	addi	sp,sp,-144
	lw	a5,144(sp)
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
	sd	s8,72(sp)
	sd	s11,48(sp)
	sd	a5,0(sp)
	lw	s9,152(sp)
	lw	s10,160(sp)
#APP
# 34 "gemm_kernel.c" 1
	.insn i 0x77, 0, x0, a0, 0x401
	
# 0 "" 2
# 37 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L2
	
# 0 "" 2
#NO_APP
	subw	a5,s10,s9
	sraiw	s5,a5,31
	srliw	s5,s5,29
	addw	s5,s5,a5
	sraiw	s5,s5,3
	sext.w	a5,a6
	blez	a6,.L3
	li	a5,1
.L3:
	sd	a5,40(sp)
	blez	a6,.L4
	slli	a5,a7,2
	add	a3,a1,a5
	li	a0,0
#APP
# 84 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a0, 4(a3)
	
# 0 "" 2
#NO_APP
	li	a0,4096
#APP
# 84 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a0, 4(a3)
	
# 0 "" 2
#NO_APP
	addi	a3,a5,4
	add	a3,a1,a3
	li	t1,1
#APP
# 84 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t1, 4(a3)
	
# 0 "" 2
#NO_APP
	addiw	t1,a0,1
#APP
# 84 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t1, 4(a3)
	
# 0 "" 2
#NO_APP
	addi	a3,a5,8
	add	a3,a1,a3
	li	t1,2
#APP
# 84 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t1, 4(a3)
	
# 0 "" 2
#NO_APP
	addiw	t1,a0,2
#APP
# 84 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t1, 4(a3)
	
# 0 "" 2
#NO_APP
	addi	a3,a5,12
	add	a3,a1,a3
	li	t1,3
#APP
# 84 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t1, 4(a3)
	
# 0 "" 2
#NO_APP
	addiw	t1,a0,3
#APP
# 84 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t1, 4(a3)
	
# 0 "" 2
#NO_APP
	addi	t3,a5,16
	add	t3,a1,t3
	li	t1,8192
#APP
# 84 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t1, 4(t3)
	
# 0 "" 2
#NO_APP
	li	a3,12288
#APP
# 84 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a3, 4(t3)
	
# 0 "" 2
#NO_APP
	addi	t3,a5,20
	add	t3,a1,t3
	addiw	t4,t1,1
#APP
# 84 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t4, 4(t3)
	
# 0 "" 2
#NO_APP
	addiw	t4,a3,1
#APP
# 84 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t4, 4(t3)
	
# 0 "" 2
#NO_APP
	addi	t3,a5,24
	add	t3,a1,t3
	addiw	t4,t1,2
#APP
# 84 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t4, 4(t3)
	
# 0 "" 2
#NO_APP
	addiw	t4,a3,2
#APP
# 84 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t4, 4(t3)
	
# 0 "" 2
#NO_APP
	addi	a5,a5,28
	add	a5,a1,a5
	addiw	t3,t1,3
#APP
# 84 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t3, 4(a5)
	
# 0 "" 2
#NO_APP
	addiw	t3,a3,3
#APP
# 84 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t3, 4(a5)
	
# 0 "" 2
#NO_APP
	slli	a5,s9,2
	add	t3,a2,a5
	li	t4,4
#APP
# 96 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t4, 4(t3)
	
# 0 "" 2
#NO_APP
	addiw	t4,t1,4
#APP
# 96 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t4, 4(t3)
	
# 0 "" 2
#NO_APP
	addi	t3,a5,4
	add	t3,a2,t3
	li	t4,5
#APP
# 96 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t4, 4(t3)
	
# 0 "" 2
#NO_APP
	addiw	t4,t1,5
#APP
# 96 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t4, 4(t3)
	
# 0 "" 2
#NO_APP
	addi	t3,a5,8
	add	t3,a2,t3
	li	t4,6
#APP
# 96 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t4, 4(t3)
	
# 0 "" 2
#NO_APP
	addiw	t4,t1,6
#APP
# 96 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t4, 4(t3)
	
# 0 "" 2
#NO_APP
	addi	t3,a5,12
	add	t3,a2,t3
	li	t4,7
#APP
# 96 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t4, 4(t3)
	
# 0 "" 2
#NO_APP
	addiw	t1,t1,7
#APP
# 96 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t1, 4(t3)
	
# 0 "" 2
#NO_APP
	addi	t1,a5,16
	add	t1,a2,t1
	addiw	t3,a0,4
#APP
# 96 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t3, 4(t1)
	
# 0 "" 2
#NO_APP
	addiw	t3,a3,4
#APP
# 96 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t3, 4(t1)
	
# 0 "" 2
#NO_APP
	addi	t1,a5,20
	add	t1,a2,t1
	addiw	t3,a0,5
#APP
# 96 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t3, 4(t1)
	
# 0 "" 2
#NO_APP
	addiw	t3,a3,5
#APP
# 96 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t3, 4(t1)
	
# 0 "" 2
#NO_APP
	addi	t1,a5,24
	add	t1,a2,t1
	addiw	t3,a0,6
#APP
# 96 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t3, 4(t1)
	
# 0 "" 2
#NO_APP
	addiw	t3,a3,6
#APP
# 96 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t3, 4(t1)
	
# 0 "" 2
#NO_APP
	addi	a5,a5,28
	add	a5,a2,a5
	addiw	a0,a0,7
#APP
# 96 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a0, 4(a5)
	
# 0 "" 2
#NO_APP
	addiw	a3,a3,7
#APP
# 96 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a3, 4(a5)
	
# 0 "" 2
#NO_APP
	ld	a5,0(sp)
	li	s3,1
	li	t3,0
	bge	a7,a5,.L6
.L5:
	sext.w	a5,a6
	mulw	s5,s5,a5
	sd	a5,32(sp)
	sd	a7,24(sp)
	slli	s6,a4,2
	sd	a7,16(sp)
	li	t3,0
	li	t6,4096
	li	t5,8192
	li	t4,12288
.L23:
	sd	s9,8(sp)
	ble	s10,s9,.L26
.L22:
	ld	a5,16(sp)
	li	a3,0
	beq	a7,a5,.L43
.L7:
	ble	a6,a3,.L8
	mulw	a5,a4,a3
	ld	a0,32(sp)
	addw	s8,t3,a0
	subw	s8,s8,a3
	ld	a0,8(sp)
	ld	a3,24(sp)
	add	a3,a3,a5
	add	a5,a0,a5
	slli	a3,a3,2
	slli	a5,a5,2
	add	a3,a1,a3
	add	a5,a2,a5
.L21:
	remw	a0,t3,s5
	bnez	a0,.L9
#APP
# 118 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L10
	
# 0 "" 2
#NO_APP
.L9:
	remw	a0,t3,a6
	bnez	a0,.L11
#APP
# 119 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L12
	
# 0 "" 2
#NO_APP
.L11:
#APP
# 120 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L15
	
# 0 "" 2
#NO_APP
	slliw	s2,s3,3
	addiw	t3,t3,1
	mv	a0,s2
	mv	t1,t3
	addiw	t0,s2,4
#APP
# 152 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, s2, 4(a3)
	
# 0 "" 2
#NO_APP
	or	t2,s2,t6
#APP
# 152 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t2, 4(a3)
	
# 0 "" 2
#NO_APP
	addiw	s1,s2,1
	addi	t2,a3,4
#APP
# 152 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, s1, 4(t2)
	
# 0 "" 2
#NO_APP
	or	s0,s1,t6
#APP
# 152 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, s0, 4(t2)
	
# 0 "" 2
#NO_APP
	addiw	s0,s2,2
	addi	t2,a3,8
#APP
# 152 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, s0, 4(t2)
	
# 0 "" 2
#NO_APP
	or	s4,s0,t6
#APP
# 152 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, s4, 4(t2)
	
# 0 "" 2
#NO_APP
	addiw	t2,s2,3
	addi	s4,a3,12
#APP
# 152 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t2, 4(s4)
	
# 0 "" 2
#NO_APP
	or	s11,t2,t6
#APP
# 152 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, s11, 4(s4)
	
# 0 "" 2
#NO_APP
	addi	s4,a3,16
	or	s11,s2,t5
#APP
# 152 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, s11, 4(s4)
	
# 0 "" 2
#NO_APP
	or	s2,s2,t4
#APP
# 152 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, s2, 4(s4)
	
# 0 "" 2
#NO_APP
	addi	s2,a3,20
	or	s4,s1,t5
#APP
# 152 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, s4, 4(s2)
	
# 0 "" 2
#NO_APP
	or	s1,s1,t4
#APP
# 152 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, s1, 4(s2)
	
# 0 "" 2
#NO_APP
	addi	s1,a3,24
	or	s2,s0,t5
#APP
# 152 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, s2, 4(s1)
	
# 0 "" 2
#NO_APP
	or	s0,s0,t4
#APP
# 152 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, s0, 4(s1)
	
# 0 "" 2
#NO_APP
	addi	s0,a3,28
	or	s1,t2,t5
#APP
# 152 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, s1, 4(s0)
	
# 0 "" 2
#NO_APP
	or	t2,t2,t4
#APP
# 152 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t2, 4(s0)
	
# 0 "" 2
# 164 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t0, 4(a5)
	
# 0 "" 2
#NO_APP
	or	t2,t0,t5
#APP
# 164 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t2, 4(a5)
	
# 0 "" 2
#NO_APP
	addiw	s1,a0,5
	addi	t2,a5,4
#APP
# 164 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, s1, 4(t2)
	
# 0 "" 2
#NO_APP
	or	s0,s1,t5
#APP
# 164 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, s0, 4(t2)
	
# 0 "" 2
#NO_APP
	addiw	s0,a0,6
	addi	t2,a5,8
#APP
# 164 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, s0, 4(t2)
	
# 0 "" 2
#NO_APP
	or	s2,s0,t5
#APP
# 164 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, s2, 4(t2)
	
# 0 "" 2
#NO_APP
	addiw	t2,a0,7
	addi	s2,a5,12
#APP
# 164 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t2, 4(s2)
	
# 0 "" 2
#NO_APP
	or	a0,t2,t5
#APP
# 164 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a0, 4(s2)
	
# 0 "" 2
#NO_APP
	addi	a0,a5,16
	or	s2,t0,t6
#APP
# 164 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, s2, 4(a0)
	
# 0 "" 2
#NO_APP
	or	t0,t0,t4
#APP
# 164 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t0, 4(a0)
	
# 0 "" 2
#NO_APP
	addi	a0,a5,20
	or	t0,s1,t6
#APP
# 164 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t0, 4(a0)
	
# 0 "" 2
#NO_APP
	or	s1,s1,t4
#APP
# 164 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, s1, 4(a0)
	
# 0 "" 2
#NO_APP
	addi	a0,a5,24
	or	t0,s0,t6
#APP
# 164 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t0, 4(a0)
	
# 0 "" 2
#NO_APP
	or	s0,s0,t4
#APP
# 164 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, s0, 4(a0)
	
# 0 "" 2
#NO_APP
	addi	a0,a5,28
	or	t0,t2,t6
#APP
# 164 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t0, 4(a0)
	
# 0 "" 2
#NO_APP
	or	t2,t2,t4
#APP
# 164 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t2, 4(a0)
	
# 0 "" 2
#NO_APP
	remw	a0,t3,a6
	addi	s3,s3,1
	andi	s3,s3,63
	bnez	a0,.L16
#APP
# 172 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L18
	
# 0 "" 2
#NO_APP
.L16:
	remw	t1,t1,s5
	bnez	t1,.L19
#APP
# 173 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L20
	
# 0 "" 2
#NO_APP
.L19:
	add	a3,a3,s6
	add	a5,a5,s6
	bne	t3,s8,.L21
.L8:
	ld	a5,8(sp)
	addi	a5,a5,8
	sd	a5,8(sp)
	sext.w	a5,a5
	blt	a5,s10,.L22
.L26:
	ld	a3,24(sp)
	ld	a5,16(sp)
	addi	a3,a3,8
	sd	a3,24(sp)
	ld	a3,0(sp)
	addiw	a5,a5,8
	sd	a5,16(sp)
	bgt	a3,a5,.L23
	blez	a6,.L29
.L6:
#APP
# 182 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L15
	
# 0 "" 2
#NO_APP
	addiw	t3,t3,1
	remw	t3,t3,a6
	bnez	t3,.L29
#APP
# 184 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L18
	
# 0 "" 2
#NO_APP
.L29:
#APP
# 188 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L27
	
# 0 "" 2
#NO_APP
.L28:
#APP
# 190 "gemm_kernel.c" 1
	.insn uj 0x2b, x0, .L28
	
# 0 "" 2
# 269 "gemm_kernel.c" 1
	fence
	
# 0 "" 2
#NO_APP
	j	.L1
.L2:
#APP
# 275 "gemm_kernel.c" 1
	nop
# 0 "" 2
#NO_APP
.L10:
#APP
# 277 "gemm_kernel.c" 1
	nop
# 0 "" 2
#NO_APP
.L12:
#APP
# 279 "gemm_kernel.c" 1
	nop
# 0 "" 2
#NO_APP
.L15:
#APP
# 281 "gemm_kernel.c" 1
	nop
# 0 "" 2
#NO_APP
.L18:
#APP
# 283 "gemm_kernel.c" 1
	nop
# 0 "" 2
#NO_APP
.L20:
#APP
# 285 "gemm_kernel.c" 1
	nop
# 0 "" 2
#NO_APP
.L27:
#APP
# 287 "gemm_kernel.c" 1
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
	ld	a5,0(sp)
	li	s3,0
	blt	a7,a5,.L5
#APP
# 188 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L27
	
# 0 "" 2
#NO_APP
	j	.L28
.L43:
	lw	a5,8(sp)
	ld	a3,40(sp)
	beq	a5,s9,.L7
	li	a3,0
	j	.L7
	.size	gemm_vec_simd, .-gemm_vec_simd
	.comm	start_barrier,32,8
	.ident	"GCC: (GNU) 8.3.0"
	.section	.note.GNU-stack,"",@progbits
