	.file	"gemm_kernel.c"
	.option nopic
	.text
	.align	2
	.globl	gemm_vec_simd
	.type	gemm_vec_simd, @function
gemm_vec_simd:
	addi	sp,sp,-96
	sd	s1,80(sp)
	sd	s0,88(sp)
	sd	s2,72(sp)
	sd	s3,64(sp)
	sd	s4,56(sp)
	sd	s5,48(sp)
	mv	s1,a3
#APP
# 26 "gemm_kernel.c" 1
	.insn i 0x77, 0, x0, a0, 0x401
	
# 0 "" 2
#NO_APP
	lui	a4,%hi(spm_base_ptr_arr)
	addi	a4,a4,%lo(spm_base_ptr_arr)
	ld	a2,48(a4)
	ld	a6,8(a4)
	ld	a0,16(a4)
	ld	a1,40(a4)
	lw	a4,136(sp)
	lw	a3,4(sp)
	lw	t2,128(sp)
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
	slliw	t2,t2,2
	sext.w	a3,a3
	ld	t1,-32(a4)
	addw	t2,t2,a7
	beqz	a3,.L9
	sext.w	a5,a5
	addiw	s4,t2,1
	addiw	s3,t2,2
	addiw	s2,t2,3
	mulw	s4,s4,a5
	lw	a3,120(sp)
	lw	a4,104(sp)
	li	t3,4096
	slliw	a3,a3,2
	slliw	s0,a5,3
	addw	a3,a3,a4
	li	a7,0
	add	t3,t1,t3
	mulw	t2,t2,a5
	mulw	s3,s3,a5
	mulw	s2,s2,a5
.L8:
#APP
# 140 "gemm_kernel.c" 1
	nop
# 0 "" 2
#NO_APP
	lw	a5,8(sp)
	sext.w	a5,a5
	beqz	a5,.L3
	add	t0,t2,a3
	add	t6,s4,a3
	add	t5,s3,a3
	add	t4,s2,a3
	slli	t0,t0,2
	slli	t6,t6,2
	slli	t5,t5,2
	slli	t4,t4,2
	add	t0,s1,t0
	add	t6,s1,t6
	add	t5,s1,t5
	add	t4,s1,t4
.L7:
#APP
# 144 "gemm_kernel.c" 1
	nop
# 0 "" 2
#NO_APP
	lw	a5,12(sp)
	flw	fa7,-2048(t3)
	flw	fa6,-2044(t3)
	sext.w	a5,a5
	flw	ft7,-2040(t3)
	flw	ft6,-2036(t3)
	flw	ft5,-2032(t3)
	flw	ft4,-2028(t3)
	flw	ft3,-2024(t3)
	flw	ft2,-2020(t3)
	flw	ft1,-2016(t3)
	flw	ft0,-2012(t3)
	flw	fa0,-2008(t3)
	flw	fa1,-2004(t3)
	flw	fa2,-2000(t3)
	flw	fa3,-1996(t3)
	flw	fa4,-1992(t3)
	flw	fa5,-1988(t3)
	beqz	a5,.L5
.L6:
	slli	a4,a7,3
	addi	a5,a4,4
	slli	a5,a5,2
	slli	a4,a4,2
	add	a4,t1,a4
	add	a0,t1,a5
#APP
# 165 "gemm_kernel.c" 1
	.insn s 0x03, 0x7, a6, 0(a4)
	
# 0 "" 2
# 166 "gemm_kernel.c" 1
	.insn s 0x03, 0x7, a2, 0(a0)
	
# 0 "" 2
#NO_APP
	fmv.s.x	ft8,a6
	fmv.s.x	ft9,a2
	addi	a1,a5,4
	add	a1,t1,a1
	fmadd.s	fa7,ft8,ft9,fa7
#APP
# 165 "gemm_kernel.c" 1
	.insn s 0x03, 0x7, s5, 0(a4)
	
# 0 "" 2
# 166 "gemm_kernel.c" 1
	.insn s 0x03, 0x7, a6, 0(a1)
	
# 0 "" 2
#NO_APP
	fmv.s.x	ft8,s5
	fmv.s.x	ft9,a6
	addi	a2,a5,8
	add	a2,t1,a2
	fmadd.s	fa6,ft8,ft9,fa6
#APP
# 165 "gemm_kernel.c" 1
	.insn s 0x03, 0x7, a6, 0(a4)
	
# 0 "" 2
#NO_APP
	fmv.s.x	ft8,a6
#APP
# 166 "gemm_kernel.c" 1
	.insn s 0x03, 0x7, a6, 0(a2)
	
# 0 "" 2
#NO_APP
	fmv.s.x	ft9,a6
	addi	s5,a5,12
	add	s5,t1,s5
	fmadd.s	ft7,ft8,ft9,ft7
#APP
# 165 "gemm_kernel.c" 1
	.insn s 0x03, 0x7, a4, 0(a4)
	
# 0 "" 2
# 166 "gemm_kernel.c" 1
	.insn s 0x03, 0x7, a6, 0(s5)
	
# 0 "" 2
#NO_APP
	fmv.s.x	ft8,a6
	fmv.s.x	ft9,a4
	addi	a6,a5,-12
	add	a6,t1,a6
	fmadd.s	ft6,ft9,ft8,ft6
#APP
# 165 "gemm_kernel.c" 1
	.insn s 0x03, 0x7, a4, 0(a6)
	
# 0 "" 2
#NO_APP
	fmv.s.x	ft8,a4
#APP
# 166 "gemm_kernel.c" 1
	.insn s 0x03, 0x7, a4, 0(a0)
	
# 0 "" 2
#NO_APP
	fmv.s.x	ft9,a4
	fmadd.s	ft5,ft8,ft9,ft5
#APP
# 165 "gemm_kernel.c" 1
	.insn s 0x03, 0x7, a4, 0(a6)
	
# 0 "" 2
#NO_APP
	fmv.s.x	ft8,a4
#APP
# 166 "gemm_kernel.c" 1
	.insn s 0x03, 0x7, a4, 0(a1)
	
# 0 "" 2
#NO_APP
	fmv.s.x	ft9,a4
	fmadd.s	ft4,ft8,ft9,ft4
#APP
# 165 "gemm_kernel.c" 1
	.insn s 0x03, 0x7, a4, 0(a6)
	
# 0 "" 2
#NO_APP
	fmv.s.x	ft8,a4
#APP
# 166 "gemm_kernel.c" 1
	.insn s 0x03, 0x7, a4, 0(a2)
	
# 0 "" 2
#NO_APP
	fmv.s.x	ft9,a4
	fmadd.s	ft3,ft8,ft9,ft3
#APP
# 165 "gemm_kernel.c" 1
	.insn s 0x03, 0x7, a6, 0(a6)
	
# 0 "" 2
# 166 "gemm_kernel.c" 1
	.insn s 0x03, 0x7, a4, 0(s5)
	
# 0 "" 2
#NO_APP
	fmv.s.x	ft8,a4
	fmv.s.x	ft9,a6
	addi	a4,a5,-8
	add	a6,t1,a4
	fmadd.s	ft2,ft9,ft8,ft2
#APP
# 165 "gemm_kernel.c" 1
	.insn s 0x03, 0x7, a4, 0(a6)
	
# 0 "" 2
#NO_APP
	fmv.s.x	ft8,a4
#APP
# 166 "gemm_kernel.c" 1
	.insn s 0x03, 0x7, a4, 0(a0)
	
# 0 "" 2
#NO_APP
	fmv.s.x	ft9,a4
	fmadd.s	ft1,ft8,ft9,ft1
#APP
# 165 "gemm_kernel.c" 1
	.insn s 0x03, 0x7, a4, 0(a6)
	
# 0 "" 2
#NO_APP
	fmv.s.x	ft8,a4
#APP
# 166 "gemm_kernel.c" 1
	.insn s 0x03, 0x7, a4, 0(a1)
	
# 0 "" 2
#NO_APP
	fmv.s.x	ft9,a4
	fmadd.s	ft0,ft8,ft9,ft0
#APP
# 165 "gemm_kernel.c" 1
	.insn s 0x03, 0x7, a4, 0(a6)
	
# 0 "" 2
#NO_APP
	fmv.s.x	ft8,a4
#APP
# 166 "gemm_kernel.c" 1
	.insn s 0x03, 0x7, a4, 0(a2)
	
# 0 "" 2
#NO_APP
	fmv.s.x	ft9,a4
	fmadd.s	fa0,ft8,ft9,fa0
#APP
# 165 "gemm_kernel.c" 1
	.insn s 0x03, 0x7, a6, 0(a6)
	
# 0 "" 2
# 166 "gemm_kernel.c" 1
	.insn s 0x03, 0x7, a4, 0(s5)
	
# 0 "" 2
#NO_APP
	fmv.s.x	ft8,a6
	fmv.s.x	ft9,a4
	addi	a5,a5,-4
	add	a5,t1,a5
	fmadd.s	fa1,ft8,ft9,fa1
#APP
# 165 "gemm_kernel.c" 1
	.insn s 0x03, 0x7, a4, 0(a5)
	
# 0 "" 2
# 166 "gemm_kernel.c" 1
	.insn s 0x03, 0x7, a0, 0(a0)
	
# 0 "" 2
#NO_APP
	fmv.s.x	ft8,a4
	fmv.s.x	ft9,a0
	fmadd.s	fa2,ft8,ft9,fa2
#APP
# 165 "gemm_kernel.c" 1
	.insn s 0x03, 0x7, a4, 0(a5)
	
# 0 "" 2
# 166 "gemm_kernel.c" 1
	.insn s 0x03, 0x7, a1, 0(a1)
	
# 0 "" 2
#NO_APP
	fmv.s.x	ft8,a4
	fmv.s.x	ft9,a1
	fmadd.s	fa3,ft8,ft9,fa3
#APP
# 165 "gemm_kernel.c" 1
	.insn s 0x03, 0x7, a4, 0(a5)
	
# 0 "" 2
# 166 "gemm_kernel.c" 1
	.insn s 0x03, 0x7, a2, 0(a2)
	
# 0 "" 2
#NO_APP
	fmv.s.x	ft8,a4
	fmv.s.x	ft9,a2
	fmadd.s	fa4,ft8,ft9,fa4
#APP
# 165 "gemm_kernel.c" 1
	.insn s 0x03, 0x7, a5, 0(a5)
	
# 0 "" 2
# 166 "gemm_kernel.c" 1
	.insn s 0x03, 0x7, a4, 0(s5)
	
# 0 "" 2
#NO_APP
	fmv.s.x	ft8,a5
	fmv.s.x	ft9,a4
	addi	a7,a7,1
	andi	a7,a7,63
	fmadd.s	fa5,ft8,ft9,fa5
#APP
# 172 "gemm_kernel.c" 1
	.insn u 0x0b, x0, 0
	
# 0 "" 2
#NO_APP
	lw	a5,12(sp)
	sext.w	a5,a5
	bnez	a5,.L6
.L5:
	fmv.x.s	a5,fa7
#APP
# 180 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a5, 0(t0)
	
# 0 "" 2
#NO_APP
	sw	zero,-2048(t3)
	addi	a5,t0,4
	fmv.x.s	a4,fa6
#APP
# 180 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a4, 0(a5)
	
# 0 "" 2
#NO_APP
	sw	zero,-2044(t3)
	addi	a5,t0,8
	fmv.x.s	a4,ft7
#APP
# 180 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a4, 0(a5)
	
# 0 "" 2
#NO_APP
	sw	zero,-2040(t3)
	addi	a5,t0,12
	fmv.x.s	a4,ft6
#APP
# 180 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a4, 0(a5)
	
# 0 "" 2
#NO_APP
	sw	zero,-2036(t3)
	fmv.x.s	a5,ft5
#APP
# 180 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a5, 0(t6)
	
# 0 "" 2
#NO_APP
	sw	zero,-2032(t3)
	addi	a5,t6,4
	fmv.x.s	a4,ft4
#APP
# 180 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a4, 0(a5)
	
# 0 "" 2
#NO_APP
	sw	zero,-2028(t3)
	addi	a5,t6,8
	fmv.x.s	a4,ft3
#APP
# 180 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a4, 0(a5)
	
# 0 "" 2
#NO_APP
	sw	zero,-2024(t3)
	addi	a5,t6,12
	fmv.x.s	a4,ft2
#APP
# 180 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a4, 0(a5)
	
# 0 "" 2
#NO_APP
	sw	zero,-2020(t3)
	fmv.x.s	a5,ft1
#APP
# 180 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a5, 0(t5)
	
# 0 "" 2
#NO_APP
	sw	zero,-2016(t3)
	addi	a5,t5,4
	fmv.x.s	a4,ft0
#APP
# 180 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a4, 0(a5)
	
# 0 "" 2
#NO_APP
	sw	zero,-2012(t3)
	addi	a5,t5,8
	fmv.x.s	a4,fa0
#APP
# 180 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a4, 0(a5)
	
# 0 "" 2
#NO_APP
	sw	zero,-2008(t3)
	addi	a5,t5,12
	fmv.x.s	a4,fa1
#APP
# 180 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a4, 0(a5)
	
# 0 "" 2
#NO_APP
	sw	zero,-2004(t3)
	fmv.x.s	a5,fa2
#APP
# 180 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a5, 0(t4)
	
# 0 "" 2
#NO_APP
	sw	zero,-2000(t3)
	addi	a5,t4,4
	fmv.x.s	a4,fa3
#APP
# 180 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a4, 0(a5)
	
# 0 "" 2
#NO_APP
	sw	zero,-1996(t3)
	addi	a5,t4,8
	fmv.x.s	a4,fa4
#APP
# 180 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a4, 0(a5)
	
# 0 "" 2
#NO_APP
	sw	zero,-1992(t3)
	addi	a5,t4,12
	fmv.x.s	a4,fa5
#APP
# 180 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a4, 0(a5)
	
# 0 "" 2
#NO_APP
	lw	a5,8(sp)
	sw	zero,-1988(t3)
	addi	t0,t0,32
	sext.w	a5,a5
	addi	t6,t6,32
	addi	t5,t5,32
	addi	t4,t4,32
	bnez	a5,.L7
.L3:
	lw	a5,4(sp)
	addw	t2,s0,t2
	addw	s4,s4,s0
	sext.w	a5,a5
	addw	s3,s3,s0
	addw	s2,s2,s0
	bnez	a5,.L8
.L9:
#APP
# 191 "gemm_kernel.c" 1
	fence
	
# 0 "" 2
#NO_APP
	ld	s0,88(sp)
	ld	s1,80(sp)
	ld	s2,72(sp)
	ld	s3,64(sp)
	ld	s4,56(sp)
	ld	s5,48(sp)
	addi	sp,sp,96
	jr	ra
	.size	gemm_vec_simd, .-gemm_vec_simd
	.comm	start_barrier,32,8
	.ident	"GCC: (GNU) 8.3.0"
	.section	.note.GNU-stack,"",@progbits
