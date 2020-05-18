	.file	"gemm_kernel.c"
	.option nopic
	.text
	.align	2
	.globl	gemm_vec_simd
	.type	gemm_vec_simd, @function
gemm_vec_simd:
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
#APP
# 40 "gemm_kernel.c" 1
	.insn i 0x77, 0, x0, a0, 0x401
	
# 0 "" 2
#NO_APP
	lw	a0,144(sp)
	li	a1,0
	call	getSpAddr
	lw	a5,4(sp)
	lw	s1,128(sp)
	sext.w	a5,a5
	slliw	s1,s1,2
	addw	s1,s1,s0
	beqz	a5,.L8
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
.L7:
#APP
# 288 "gemm_kernel.c" 1
	nop
# 0 "" 2
#NO_APP
	lw	a4,8(sp)
	sext.w	a4,a4
	beqz	a4,.L3
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
.L6:
#APP
# 292 "gemm_kernel.c" 1
	nop
# 0 "" 2
#NO_APP
	lw	a4,12(sp)
	sext.w	a4,a4
	beqz	a4,.L4
.L5:
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
	lw	a4,12(sp)
	sext.w	a4,a4
	bnez	a4,.L5
.L4:
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
	bnez	a4,.L6
.L3:
	lw	a4,4(sp)
	addw	s1,s4,s1
	addw	s7,s7,s4
	sext.w	a4,a4
	addw	s6,s6,s4
	addw	s5,s5,s4
	bnez	a4,.L7
.L8:
#APP
# 343 "gemm_kernel.c" 1
	fence
	
# 0 "" 2
#NO_APP
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
	jr	ra
	.size	gemm_vec_simd, .-gemm_vec_simd
	.comm	start_barrier,32,8
	.ident	"GCC: (GNU) 8.3.0"
	.section	.note.GNU-stack,"",@progbits
