	.file	"gemm_kernel.c"
	.option nopic
	.text
	.align	2
	.globl	gemm_vec_simd
	.type	gemm_vec_simd, @function
gemm_vec_simd:
	addi	sp,sp,-224
	sd	s0,208(sp)
	sd	s1,200(sp)
	sd	s2,192(sp)
	sd	s3,184(sp)
	sd	s4,176(sp)
	sd	ra,216(sp)
	sd	s5,168(sp)
	sd	s6,160(sp)
	sd	s7,152(sp)
	sd	s8,144(sp)
	sd	s9,136(sp)
	sd	s10,128(sp)
	sd	s11,120(sp)
	sd	a3,56(sp)
	mv	s4,a5
	mv	s0,a7
	lw	s2,248(sp)
	lw	s3,256(sp)
	ld	s1,280(sp)
#APP
# 39 "gemm_kernel.c" 1
	.insn i 0x77, 0, x0, a0, 0x401
	
# 0 "" 2
#NO_APP
	lw	a0,0(s1)
	li	a1,0
	call	getSpAddr
	sd	a0,80(sp)
	lw	a0,4(s1)
	li	a1,0
	call	getSpAddr
	sd	a0,88(sp)
	lw	a0,8(s1)
	li	a1,0
	call	getSpAddr
	sd	a0,96(sp)
	lw	a0,12(s1)
	li	a1,0
	call	getSpAddr
	sd	a0,104(sp)
	lw	a0,272(sp)
	li	a1,0
	call	getSpAddr
	lw	a2,68(sp)
	slliw	a6,s3,2
	addw	a3,a6,s0
	sext.w	a2,a2
	beqz	a2,.L8
	sext.w	a4,s4
	addiw	a7,a3,1
	addiw	s1,a6,1
	addiw	s0,a6,2
	addiw	t4,a6,3
	mulw	a6,a7,a4
	addiw	s4,a3,2
	addiw	t2,a3,3
	sraiw	a7,t4,31
	sraiw	t3,s1,31
	srliw	a7,a7,30
	addw	t4,t4,a7
	srliw	t3,t3,30
	addw	s1,s1,t3
	andi	s7,t4,3
	mulw	a5,a3,a4
	sd	a6,32(sp)
	subw	a7,s7,a7
	sraiw	t1,s0,31
	srliw	t1,t1,30
	addw	s0,s0,t1
	sraiw	t4,t4,2
	mulw	a6,s4,a4
	sd	a5,24(sp)
	slliw	a5,s2,2
	addiw	a1,a5,1
	addiw	a2,a5,2
	addiw	a3,a5,3
	andi	s4,s1,3
	sraiw	t0,a1,31
	sraiw	t6,a2,31
	sraiw	t5,a3,31
	mulw	t2,t2,a4
	subw	t3,s4,t3
	slli	s4,a7,3
	lw	a7,232(sp)
	srliw	t0,t0,30
	srliw	t6,t6,30
	srliw	t5,t5,30
	addw	a1,a1,t0
	addw	a2,a2,t6
	addw	a3,a3,t5
	andi	s5,a3,3
	addw	a5,a5,a7
	andi	s6,a2,3
	slliw	a4,a4,3
	sd	a6,16(sp)
	andi	a6,a1,3
	subw	a6,a6,t0
	subw	s6,s6,t6
	subw	t5,s5,t5
	sd	t2,8(sp)
	sd	a5,40(sp)
	andi	t2,s0,3
	sext.w	a5,a4
	slli	s5,t5,3
	subw	t1,t2,t1
	slli	s6,s6,3
	slli	a6,a6,3
	sd	a5,48(sp)
	addi	a5,sp,112
	addi	a4,sp,112
	add	s8,a5,s6
	add	s7,a5,s5
	sraiw	a1,a1,2
	sraiw	a2,a2,2
	sraiw	a3,a3,2
	slli	t3,t3,3
	slli	t1,t1,3
	add	t2,a5,a6
	li	a5,4096
	addi	t0,a1,2
	addi	t6,a2,2
	addi	t5,a3,2
	sraiw	s1,s1,2
	sraiw	s0,s0,2
	li	a7,0
	addi	s2,s2,2
	add	a5,a0,a5
	add	s6,a4,t3
	add	s5,a4,t1
	add	s4,a4,s4
.L7:
#APP
# 265 "gemm_kernel.c" 1
	nop
# 0 "" 2
#NO_APP
	lw	a4,72(sp)
	sext.w	a4,a4
	beqz	a4,.L3
	ld	a4,40(sp)
	ld	a3,24(sp)
	li	t3,4
	add	s11,a3,a4
	ld	a3,32(sp)
	slli	s11,s11,2
	add	s10,a3,a4
	ld	a3,16(sp)
	slli	s10,s10,2
	add	s9,a3,a4
	ld	a3,8(sp)
	slli	s9,s9,2
	add	a4,a3,a4
	ld	a3,56(sp)
	slli	a4,a4,2
	add	a4,a3,a4
	add	s11,a3,s11
	add	s10,a3,s10
	add	s9,a3,s9
	sd	a4,0(sp)
.L6:
#APP
# 269 "gemm_kernel.c" 1
	nop
# 0 "" 2
#NO_APP
	lw	a4,76(sp)
	sext.w	a4,a4
	beqz	a4,.L4
.L5:
#APP
# 273 "gemm_kernel.c" 1
	.insn i 0x1b, 0x3, x0, t3, 0
	
# 0 "" 2
#NO_APP
	slli	a4,a7,2
	ld	a2,80(sp)
	add	t1,a4,s3
	add	a0,s2,a4
	slli	a3,t1,2
	slli	a0,a0,2
	add	t1,a2,a3
	add	a0,a2,a0
	flw	fa3,0(t1)
	flw	fa5,0(a0)
	flw	fa4,-2048(a5)
	ld	a3,-32(t2)
	add	a1,t0,a4
	fmadd.s	fa5,fa5,fa3,fa4
	slli	a1,a1,2
	add	a1,a3,a1
	flw	fa4,-2044(a5)
	ld	a3,-32(s8)
	fsw	fa5,-2048(a5)
	flw	fa3,0(t1)
	flw	fa5,0(a1)
	add	a2,t6,a4
	slli	a2,a2,2
	fmadd.s	fa5,fa5,fa3,fa4
	add	a2,a3,a2
	flw	fa4,-2040(a5)
	ld	a6,-32(s7)
	add	a3,t5,a4
	fsw	fa5,-2044(a5)
	flw	fa3,0(t1)
	flw	fa5,0(a2)
	slli	a3,a3,2
	add	a3,a6,a3
	fmadd.s	fa5,fa5,fa3,fa4
	flw	fa3,-2036(a5)
	add	a6,a4,s1
	slli	a6,a6,2
	flw	fa4,-2032(a5)
	fsw	fa5,-2040(a5)
	flw	fa2,0(t1)
	flw	fa5,0(a3)
	ld	t1,-32(s6)
	flw	fa1,-2028(a5)
	fmadd.s	fa5,fa5,fa2,fa3
	add	a6,t1,a6
	flw	fa2,-2024(a5)
	add	t1,a4,s0
	slli	t1,t1,2
	fsw	fa5,-2036(a5)
	flw	fa5,0(a6)
	flw	fa3,0(a0)
	add	a4,a4,t4
	slli	a4,a4,2
	fmadd.s	fa3,fa3,fa5,fa4
	addi	a7,a7,1
	andi	a7,a7,127
	fsw	fa3,-2032(a5)
	flw	fa5,0(a6)
	flw	fa4,0(a1)
	fmadd.s	fa4,fa4,fa5,fa1
	fsw	fa4,-2028(a5)
	flw	fa4,0(a6)
	flw	fa5,0(a2)
	fmadd.s	fa5,fa5,fa4,fa2
	fsw	fa5,-2024(a5)
	flw	fa3,0(a6)
	flw	fa4,0(a3)
	flw	fa5,-2020(a5)
	ld	a6,-32(s5)
	flw	fa1,-2016(a5)
	fmadd.s	fa4,fa4,fa3,fa5
	add	t1,a6,t1
	flw	fa2,-2012(a5)
	flw	fa3,-2008(a5)
	flw	ft1,-2004(a5)
	fsw	fa4,-2020(a5)
	flw	fa4,0(t1)
	flw	fa5,0(a0)
	ld	a6,-32(s4)
	flw	ft0,-2000(a5)
	fmadd.s	fa5,fa5,fa4,fa1
	add	a4,a6,a4
	flw	fa0,-1996(a5)
	fsw	fa5,-2016(a5)
	flw	fa5,0(t1)
	flw	fa1,0(a1)
	fmadd.s	fa1,fa1,fa5,fa2
	fsw	fa1,-2012(a5)
	flw	fa5,0(t1)
	flw	fa2,0(a2)
	fmadd.s	fa2,fa2,fa5,fa3
	fsw	fa2,-2008(a5)
	flw	fa5,0(t1)
	flw	fa3,0(a3)
	fmadd.s	fa3,fa3,fa5,ft1
	fsw	fa3,-2004(a5)
	flw	fa5,0(a0)
	flw	fa4,0(a4)
	fmadd.s	fa4,fa4,fa5,ft0
	fsw	fa4,-2000(a5)
	flw	fa4,0(a1)
	flw	fa5,0(a4)
	fmadd.s	fa5,fa5,fa4,fa0
	fsw	fa5,-1996(a5)
	flw	fa4,0(a4)
	flw	fa2,0(a2)
	flw	fa5,-1992(a5)
	flw	fa3,-1988(a5)
	fmadd.s	fa4,fa4,fa2,fa5
	fsw	fa4,-1992(a5)
	flw	fa5,0(a4)
	flw	fa4,0(a3)
	fmadd.s	fa5,fa5,fa4,fa3
	fsw	fa5,-1988(a5)
#APP
# 301 "gemm_kernel.c" 1
	.insn i 0x1b, 0x2, x0, t3, 0
	
# 0 "" 2
#NO_APP
	lw	a4,76(sp)
	sext.w	a4,a4
	bnez	a4,.L5
.L4:
	lw	a4,-2048(a5)
#APP
# 309 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a4, 0(s11)
	
# 0 "" 2
#NO_APP
	sw	zero,-2048(a5)
	addi	a4,s11,4
	lw	a3,-2044(a5)
#APP
# 309 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a3, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-2044(a5)
	addi	a4,s11,8
	lw	a3,-2040(a5)
#APP
# 309 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a3, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-2040(a5)
	addi	a4,s11,12
	lw	a3,-2036(a5)
#APP
# 309 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a3, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-2036(a5)
	lw	a4,-2032(a5)
#APP
# 309 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a4, 0(s10)
	
# 0 "" 2
#NO_APP
	sw	zero,-2032(a5)
	addi	a4,s10,4
	lw	a3,-2028(a5)
#APP
# 309 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a3, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-2028(a5)
	addi	a4,s10,8
	lw	a3,-2024(a5)
#APP
# 309 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a3, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-2024(a5)
	addi	a4,s10,12
	lw	a3,-2020(a5)
#APP
# 309 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a3, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-2020(a5)
	lw	a4,-2016(a5)
#APP
# 309 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a4, 0(s9)
	
# 0 "" 2
#NO_APP
	sw	zero,-2016(a5)
	addi	a4,s9,4
	lw	a3,-2012(a5)
#APP
# 309 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a3, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-2012(a5)
	addi	a4,s9,8
	lw	a3,-2008(a5)
#APP
# 309 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a3, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-2008(a5)
	addi	a4,s9,12
	lw	a3,-2004(a5)
#APP
# 309 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a3, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-2004(a5)
	lw	a4,-2000(a5)
	ld	a3,0(sp)
#APP
# 309 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a4, 0(a3)
	
# 0 "" 2
#NO_APP
	sw	zero,-2000(a5)
	addi	a4,a3,4
	lw	a2,-1996(a5)
#APP
# 309 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a2, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-1996(a5)
	addi	a4,a3,8
	lw	a2,-1992(a5)
#APP
# 309 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a2, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-1992(a5)
	addi	a4,a3,12
	lw	a2,-1988(a5)
#APP
# 309 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a2, 0(a4)
	
# 0 "" 2
#NO_APP
	lw	a4,72(sp)
	addi	a3,a3,32
	sw	zero,-1988(a5)
	sext.w	a4,a4
	sd	a3,0(sp)
	addi	s11,s11,32
	addi	s10,s10,32
	addi	s9,s9,32
	bnez	a4,.L6
.L3:
	ld	a3,48(sp)
	ld	a2,24(sp)
	lw	a4,68(sp)
	addw	a2,a2,a3
	sd	a2,24(sp)
	ld	a2,32(sp)
	sext.w	a4,a4
	addw	a2,a3,a2
	sd	a2,32(sp)
	ld	a2,16(sp)
	addw	a2,a2,a3
	sd	a2,16(sp)
	ld	a2,8(sp)
	addw	a3,a2,a3
	sd	a3,8(sp)
	bnez	a4,.L7
.L8:
#APP
# 320 "gemm_kernel.c" 1
	fence
	
# 0 "" 2
#NO_APP
	ld	ra,216(sp)
	ld	s0,208(sp)
	ld	s1,200(sp)
	ld	s2,192(sp)
	ld	s3,184(sp)
	ld	s4,176(sp)
	ld	s5,168(sp)
	ld	s6,160(sp)
	ld	s7,152(sp)
	ld	s8,144(sp)
	ld	s9,136(sp)
	ld	s10,128(sp)
	ld	s11,120(sp)
	addi	sp,sp,224
	jr	ra
	.size	gemm_vec_simd, .-gemm_vec_simd
	.comm	start_barrier,32,8
	.ident	"GCC: (GNU) 8.3.0"
	.section	.note.GNU-stack,"",@progbits
