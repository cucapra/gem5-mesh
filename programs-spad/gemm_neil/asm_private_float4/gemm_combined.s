	.file	"gemm_kernel.c"
	.option nopic
	.text
	.align	2
	.globl	gemm_vec_simd
	.type	gemm_vec_simd, @function
gemm_vec_simd:
#APP
# 40 "gemm_kernel.c" 1
	.insn i 0x77, 0, x0, a0, 0x401
	
# 0 "" 2
# 43 "gemm_kernel.c" 1
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

addi	sp,sp,-96
	sd	s0,80(sp)
	sd	s3,56(sp)
	sd	s5,40(sp)
	sd	ra,88(sp)
	sd	s1,72(sp)
	sd	s2,64(sp)
	sd	s4,48(sp)
	sd	s6,32(sp)
	sd	s7,24(sp)
	mv	s3,a3
	mv	s5,a5
	mv	s0,a7
	lw	a0,144(sp)
	li	a1,0
	call	getSpAddr
	lw	a5,4(sp)
	lw	s1,128(sp)
	sext.w	a5,a5
	slliw	s1,s1,2
	addw	s1,s1,s0
	
	sext.w	a4,s5
	addiw	s7,s1,1
	addiw	s6,s1,2
	addiw	s5,s1,3
	mulw	s7,s7,a4
	lw	s2,120(sp)
	lw	a5,104(sp)
	slliw	s4,a4,3
	slliw	s2,s2,2
	addw	s2,s2,a5
	li	a5,4096
	li	t4,0
	li	t6,8
	add	a5,a0,a5
	mulw	s1,s1,a4
	mulw	s6,s6,a4
	mulw	s5,s5,a4
.L10:
add	s0,s1,s2
	add	a3,s7,s2
	add	t2,s6,s2
	add	t0,s5,s2
	slli	s0,s0,2
	slli	a3,a3,2
	slli	t2,t2,2
	slli	t0,t0,2
	add	s0,s3,s0
	add	a3,s3,a3
	add	t2,s3,t2
	add	t0,s3,t0
.L12:
#APP
# 353 "gemm_kernel.c" 1
	nop
# 0 "" 2
#NO_APP
.L15:


#APP
# 296 "gemm_kernel.c" 1
	.insn i 0x1b, 0x3, x0, t6, 0
	
# 0 "" 2
#NO_APP
	slli	a2,t4,3
	addi	a4,a2,4
	slli	a4,a4,2
	slli	a2,a2,2
	add	t5,a0,a4
	add	a2,a0,a2
	flw	fa3,0(t5)
	flw	fa5,0(a2)
	flw	fa4,-2048(a5)
	addi	a7,a4,4
	add	a7,a0,a7
	fmadd.s	fa5,fa5,fa3,fa4
	flw	fa3,-2044(a5)
	addi	a6,a4,8
	add	a6,a0,a6
	flw	fa1,-2040(a5)
	fsw	fa5,-2048(a5)
	flw	fa5,0(a7)
	flw	fa4,0(a2)
	addi	a1,a4,12
	add	a1,a0,a1
	fmadd.s	fa4,fa4,fa5,fa3
	flw	fa2,-2036(a5)
	addi	t3,a4,-12
	add	t3,a0,t3
	flw	fa3,-2032(a5)
	fsw	fa4,-2044(a5)
	flw	fa0,0(a6)
	flw	fa5,0(a2)
	flw	fa4,-2028(a5)
	flw	ft0,-2024(a5)
	fmadd.s	fa5,fa5,fa0,fa1
	flw	fa0,-2020(a5)
	addi	t1,a4,-8
	add	t1,a0,t1
	addi	a4,a4,-4
	fsw	fa5,-2040(a5)
	flw	fa5,0(a1)
	flw	fa1,0(a2)
	add	a4,a0,a4
	addi	t4,t4,1
	fmadd.s	fa1,fa1,fa5,fa2
	andi	t4,t4,63
	fsw	fa1,-2036(a5)
	flw	fa5,0(t5)
	flw	fa2,0(t3)
	fmadd.s	fa2,fa2,fa5,fa3
	fsw	fa2,-2032(a5)
	flw	fa5,0(a7)
	flw	fa3,0(t3)
	fmadd.s	fa3,fa3,fa5,fa4
	fsw	fa3,-2028(a5)
	flw	fa5,0(a6)
	flw	fa4,0(t3)
	fmadd.s	fa4,fa4,fa5,ft0
	fsw	fa4,-2024(a5)
	flw	fa4,0(a1)
	flw	fa5,0(t3)
	fmadd.s	fa5,fa5,fa4,fa0
	fsw	fa5,-2020(a5)
	flw	fa5,0(t1)
	flw	fa3,0(t5)
	flw	fa4,-2016(a5)
	flw	fa0,-2012(a5)
	flw	fa1,-2008(a5)
	fmadd.s	fa5,fa5,fa3,fa4
	flw	fa2,-2004(a5)
	flw	fa3,-2000(a5)
	flw	fa4,-1996(a5)
	flw	ft2,-1992(a5)
	fsw	fa5,-2016(a5)
	flw	fa5,0(a7)
	flw	ft0,0(t1)
	flw	ft1,-1988(a5)
	fmadd.s	ft0,ft0,fa5,fa0
	fsw	ft0,-2012(a5)
	flw	fa5,0(a6)
	flw	fa0,0(t1)
	fmadd.s	fa0,fa0,fa5,fa1
	fsw	fa0,-2008(a5)
	flw	fa5,0(a1)
	flw	fa1,0(t1)
	fmadd.s	fa1,fa1,fa5,fa2
	fsw	fa1,-2004(a5)
	flw	fa5,0(t5)
	flw	fa2,0(a4)
	fmadd.s	fa2,fa2,fa5,fa3
	fsw	fa2,-2000(a5)
	flw	fa5,0(a7)
	flw	fa3,0(a4)
	fmadd.s	fa3,fa3,fa5,fa4
	fsw	fa3,-1996(a5)
	flw	fa5,0(a6)
	flw	fa4,0(a4)
	fmadd.s	fa4,fa4,fa5,ft2
	fsw	fa4,-1992(a5)
	flw	fa5,0(a4)
	flw	fa4,0(a1)
	fmadd.s	fa5,fa5,fa4,ft1
	fsw	fa5,-1988(a5)
#APP
# 324 "gemm_kernel.c" 1
	.insn i 0x1b, 0x2, x0, t6, 0
	
# 0 "" 2
#NO_APP

.L18:

lw	a4,-2048(a5)
#APP
# 332 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a4, 0(s0)
	
# 0 "" 2
#NO_APP
	sw	zero,-2048(a5)
	addi	a4,s0,4
	lw	a2,-2044(a5)
#APP
# 332 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a2, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-2044(a5)
	addi	a4,s0,8
	lw	a2,-2040(a5)
#APP
# 332 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a2, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-2040(a5)
	addi	a4,s0,12
	lw	a2,-2036(a5)
#APP
# 332 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a2, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-2036(a5)
	lw	a4,-2032(a5)
#APP
# 332 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a4, 0(a3)
	
# 0 "" 2
#NO_APP
	sw	zero,-2032(a5)
	addi	a4,a3,4
	lw	a2,-2028(a5)
#APP
# 332 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a2, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-2028(a5)
	addi	a4,a3,8
	lw	a2,-2024(a5)
#APP
# 332 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a2, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-2024(a5)
	addi	a4,a3,12
	lw	a2,-2020(a5)
#APP
# 332 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a2, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-2020(a5)
	lw	a4,-2016(a5)
#APP
# 332 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a4, 0(t2)
	
# 0 "" 2
#NO_APP
	sw	zero,-2016(a5)
	addi	a4,t2,4
	lw	a2,-2012(a5)
#APP
# 332 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a2, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-2012(a5)
	addi	a4,t2,8
	lw	a2,-2008(a5)
#APP
# 332 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a2, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-2008(a5)
	addi	a4,t2,12
	lw	a2,-2004(a5)
#APP
# 332 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a2, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-2004(a5)
	lw	a4,-2000(a5)
#APP
# 332 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a4, 0(t0)
	
# 0 "" 2
#NO_APP
	sw	zero,-2000(a5)
	addi	a4,t0,4
	lw	a2,-1996(a5)
#APP
# 332 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a2, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-1996(a5)
	addi	a4,t0,8
	lw	a2,-1992(a5)
#APP
# 332 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a2, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-1992(a5)
	addi	a4,t0,12
	lw	a2,-1988(a5)
#APP
# 332 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a2, 0(a4)
	
# 0 "" 2
#NO_APP
	lw	a4,8(sp)
	sw	zero,-1988(a5)
	addi	s0,s0,32
	sext.w	a4,a4
	addi	a3,a3,32
	addi	t2,t2,32
	addi	t0,t0,32


.L20:


lw	a4,4(sp)
	addw	s1,s4,s1
	addw	s7,s7,s4
	sext.w	a4,a4
	addw	s6,s6,s4
	addw	s5,s5,s4


.L27:

ld	ra,88(sp)
	ld	s0,80(sp)
	ld	s1,72(sp)
	ld	s2,64(sp)
	ld	s3,56(sp)
	ld	s4,48(sp)
	ld	s5,40(sp)
	ld	s6,32(sp)
	ld	s7,24(sp)
	addi	sp,sp,96
	
.L1:
	
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
