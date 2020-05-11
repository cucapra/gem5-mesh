	.file	"gemm_kernel.c"
	.option nopic
	.text
	.align	2
	.globl	gemm_vec_simd
	.type	gemm_vec_simd, @function
gemm_vec_simd:
	addi	sp,sp,-112
	sd	s4,72(sp)
	sd	s0,104(sp)
	sd	s1,96(sp)
	sd	s2,88(sp)
	sd	s3,80(sp)
	sd	s5,64(sp)
	sd	s6,56(sp)
	sd	s7,48(sp)
	mv	s4,a3
#APP
# 36 "gemm_kernel.c" 1
	.insn i 0x77, 0, x0, a0, 0x401
	
# 0 "" 2
#NO_APP
	lui	a4,%hi(spm_base_ptr_arr)
	addi	a4,a4,%lo(spm_base_ptr_arr)
	ld	a2,48(a4)
	ld	a6,8(a4)
	ld	a0,16(a4)
	ld	a1,40(a4)
	lw	a4,152(sp)
	lw	a3,4(sp)
	lw	s1,144(sp)
	addi	a2,a2,16
	sd	a2,40(sp)
	addi	a6,a6,16
	addi	a0,a0,16
	addi	a1,a1,16
	slli	a4,a4,3
	addi	a2,sp,48
	sd	a6,16(sp)
	sd	a0,24(sp)
	sd	a1,32(sp)
	add	a4,a2,a4
	slliw	s1,s1,2
	sext.w	a3,a3
	ld	t4,-32(a4)
	addw	s1,s1,a7
	beqz	a3,.L9
	sext.w	a4,a5
	addiw	s7,s1,1
	addiw	s6,s1,2
	addiw	s5,s1,3
	mulw	s7,s7,a4
	lw	s2,136(sp)
	lw	a5,120(sp)
	slliw	s3,a4,3
	slliw	s2,s2,2
	addw	s2,s2,a5
	li	a5,4096
	li	t5,0
	add	a5,t4,a5
	li	t6,8
	mulw	s1,s1,a4
	mulw	s6,s6,a4
	mulw	s5,s5,a4
.L8:
#APP
# 252 "gemm_kernel.c" 1
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
	add	s0,s4,s0
	add	a3,s4,a3
	add	t2,s4,t2
	add	t0,s4,t0
.L7:
#APP
# 256 "gemm_kernel.c" 1
	nop
# 0 "" 2
#NO_APP
	lw	a4,12(sp)
	flw	fa7,-2048(a5)
	flw	fa6,-2044(a5)
	sext.w	a4,a4
	flw	ft7,-2040(a5)
	flw	ft6,-2036(a5)
	flw	ft5,-2032(a5)
	flw	ft4,-2028(a5)
	flw	ft3,-2024(a5)
	flw	ft2,-2020(a5)
	flw	ft1,-2016(a5)
	flw	ft0,-2012(a5)
	flw	fa0,-2008(a5)
	flw	fa1,-2004(a5)
	flw	fa2,-2000(a5)
	flw	fa3,-1996(a5)
	flw	fa4,-1992(a5)
	flw	fa5,-1988(a5)
	beqz	a4,.L6
.L5:
	slli	a2,t5,3
	addi	a4,a2,4
	slli	a4,a4,2
	slli	a2,a2,2
	add	t3,t4,a4
	add	a2,t4,a2
	flw	ft9,0(a2)
	flw	ft8,0(t3)
	addi	a6,a4,4
	add	a6,t4,a6
	fmadd.s	fa7,ft9,ft8,fa7
	addi	a0,a4,8
	add	a0,t4,a0
	addi	a1,a4,12
	add	a1,t4,a1
	fsw	fa7,-2048(a5)
	flw	ft9,0(a2)
	flw	ft8,0(a6)
	addi	t1,a4,-12
	add	t1,t4,t1
	fmadd.s	fa6,ft9,ft8,fa6
	addi	a7,a4,-8
	add	a7,t4,a7
	addi	a4,a4,-4
	add	a4,t4,a4
	fsw	fa6,-2044(a5)
	flw	ft9,0(a2)
	flw	ft8,0(a0)
	addi	t5,t5,1
	andi	t5,t5,63
	fmadd.s	ft7,ft9,ft8,ft7
	fsw	ft7,-2040(a5)
	flw	ft9,0(a2)
	flw	ft8,0(a1)
	fmadd.s	ft6,ft9,ft8,ft6
	fsw	ft6,-2036(a5)
	flw	ft9,0(t1)
	flw	ft8,0(t3)
	fmadd.s	ft5,ft9,ft8,ft5
	fsw	ft5,-2032(a5)
	flw	ft9,0(t1)
	flw	ft8,0(a6)
	fmadd.s	ft4,ft9,ft8,ft4
	fsw	ft4,-2028(a5)
	flw	ft9,0(t1)
	flw	ft8,0(a0)
	fmadd.s	ft3,ft9,ft8,ft3
	fsw	ft3,-2024(a5)
	flw	ft9,0(t1)
	flw	ft8,0(a1)
	fmadd.s	ft2,ft9,ft8,ft2
	fsw	ft2,-2020(a5)
	flw	ft9,0(a7)
	flw	ft8,0(t3)
	fmadd.s	ft1,ft9,ft8,ft1
	fsw	ft1,-2016(a5)
	flw	ft9,0(a7)
	flw	ft8,0(a6)
	fmadd.s	ft0,ft9,ft8,ft0
	fsw	ft0,-2012(a5)
	flw	ft9,0(a7)
	flw	ft8,0(a0)
	fmadd.s	fa0,ft9,ft8,fa0
	fsw	fa0,-2008(a5)
	flw	ft9,0(a7)
	flw	ft8,0(a1)
	fmadd.s	fa1,ft9,ft8,fa1
	fsw	fa1,-2004(a5)
	flw	ft9,0(a4)
	flw	ft8,0(t3)
	fmadd.s	fa2,ft9,ft8,fa2
	fsw	fa2,-2000(a5)
	flw	ft9,0(a4)
	flw	ft8,0(a6)
	fmadd.s	fa3,ft9,ft8,fa3
	fsw	fa3,-1996(a5)
	flw	ft9,0(a4)
	flw	ft8,0(a0)
	fmadd.s	fa4,ft9,ft8,fa4
	fsw	fa4,-1992(a5)
	flw	ft9,0(a4)
	flw	ft8,0(a1)
	fmadd.s	fa5,ft9,ft8,fa5
	fsw	fa5,-1988(a5)
#APP
# 287 "gemm_kernel.c" 1
	.insn i 0x1b, 0x2, x0, t6, 0
	
# 0 "" 2
#NO_APP
	lw	a4,12(sp)
	sext.w	a4,a4
	bnez	a4,.L5
.L6:
	fmv.x.s	a4,fa7
#APP
# 295 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a4, 0(s0)
	
# 0 "" 2
#NO_APP
	sw	zero,-2048(a5)
	addi	a4,s0,4
	fmv.x.s	a2,fa6
#APP
# 295 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a2, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-2044(a5)
	addi	a4,s0,8
	fmv.x.s	a2,ft7
#APP
# 295 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a2, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-2040(a5)
	addi	a4,s0,12
	fmv.x.s	a2,ft6
#APP
# 295 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a2, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-2036(a5)
	fmv.x.s	a4,ft5
#APP
# 295 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a4, 0(a3)
	
# 0 "" 2
#NO_APP
	sw	zero,-2032(a5)
	addi	a4,a3,4
	fmv.x.s	a2,ft4
#APP
# 295 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a2, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-2028(a5)
	addi	a4,a3,8
	fmv.x.s	a2,ft3
#APP
# 295 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a2, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-2024(a5)
	addi	a4,a3,12
	fmv.x.s	a2,ft2
#APP
# 295 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a2, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-2020(a5)
	fmv.x.s	a4,ft1
#APP
# 295 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a4, 0(t2)
	
# 0 "" 2
#NO_APP
	sw	zero,-2016(a5)
	addi	a4,t2,4
	fmv.x.s	a2,ft0
#APP
# 295 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a2, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-2012(a5)
	addi	a4,t2,8
	fmv.x.s	a2,fa0
#APP
# 295 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a2, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-2008(a5)
	addi	a4,t2,12
	fmv.x.s	a2,fa1
#APP
# 295 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a2, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-2004(a5)
	fmv.x.s	a4,fa2
#APP
# 295 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a4, 0(t0)
	
# 0 "" 2
#NO_APP
	sw	zero,-2000(a5)
	addi	a4,t0,4
	fmv.x.s	a2,fa3
#APP
# 295 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a2, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-1996(a5)
	addi	a4,t0,8
	fmv.x.s	a2,fa4
#APP
# 295 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a2, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-1992(a5)
	addi	a4,t0,12
	fmv.x.s	a2,fa5
#APP
# 295 "gemm_kernel.c" 1
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
	bnez	a4,.L7
.L3:
	lw	a4,4(sp)
	addw	s1,s3,s1
	addw	s7,s7,s3
	sext.w	a4,a4
	addw	s6,s6,s3
	addw	s5,s5,s3
	bnez	a4,.L8
.L9:
#APP
# 306 "gemm_kernel.c" 1
	fence
	
# 0 "" 2
#NO_APP
	ld	s0,104(sp)
	ld	s1,96(sp)
	ld	s2,88(sp)
	ld	s3,80(sp)
	ld	s4,72(sp)
	ld	s5,64(sp)
	ld	s6,56(sp)
	ld	s7,48(sp)
	addi	sp,sp,112
	jr	ra
	.size	gemm_vec_simd, .-gemm_vec_simd
	.comm	start_barrier,32,8
	.ident	"GCC: (GNU) 8.3.0"
	.section	.note.GNU-stack,"",@progbits
