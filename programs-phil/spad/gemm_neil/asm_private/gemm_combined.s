	.file	"gemm_kernel.c"
	.option nopic
	.text
	.align	2
	.globl	gemm_vec_simd
	.type	gemm_vec_simd, @function
gemm_vec_simd:
#APP
# 26 "gemm_kernel.c" 1
	.insn i 0x77, 0, x0, a0, 0x401
	
# 0 "" 2
# 45 "gemm_kernel.c" 1
	addi	sp,sp,-80
	sd	s3,48(sp)
	sd	s4,40(sp)
	sd	s0,72(sp)
	sd	s1,64(sp)
	sd	s2,56(sp)
	sd	s5,32(sp)
	sd	s6,24(sp)
	sd	s7,16(sp)
	sd	s8,8(sp)
	sd	s9,0(sp)
	lw	a5,80(sp)
	lw	s3,88(sp)
	lw	s4,96(sp)

	.insn uj 0x6b, x0, .L2
	
# 0 "" 2
#NO_APP
	bge	a7,a5,.L16
	addiw	a5,a5,-1
	addiw	a3,s4,-1
	subw	a5,a5,a7
	subw	a3,a3,s3
	srliw	a5,a5,3
	srliw	a3,a3,3
	slli	a5,a5,3
	slli	a3,a3,3
	add	a5,a5,a7
	add	a3,a3,s3
	addi	t2,a1,32
	addi	s0,a2,32
	slli	a7,a7,2
	slli	a5,a5,2
	slli	s2,s3,2
	slli	a3,a3,2
	add	s1,a1,a7
	add	s2,a2,s2
	add	t2,a5,t2
	add	s0,a3,s0
	slli	t0,a4,2
	li	t5,0
	li	a0,4096
	li	a1,8192
	li	a2,12288
.L15:
#APP
# 51 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L6
	
# 0 "" 2
#NO_APP
	bge	s3,s4,.L7
	mv	s5,s2
.L13:
#APP
# 55 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L8
	
# 0 "" 2
#NO_APP
	blez	a6,.L9
	mv	a3,s5
	mv	a4,s1
	li	t6,0
.L11:
	slliw	s6,t5,3
	mv	a5,s6
	addiw	t4,s6,4
#APP
# 88 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, s6, 1(a4)
	
# 0 "" 2
#NO_APP
	or	a7,s6,a0
#APP
# 88 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a7, 1(a4)
	
# 0 "" 2
#NO_APP
	addiw	t3,s6,1
	addi	a7,a4,4
#APP
# 88 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t3, 1(a7)
	
# 0 "" 2
#NO_APP
	or	t1,t3,a0
#APP
# 88 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t1, 1(a7)
	
# 0 "" 2
#NO_APP
	addiw	t1,s6,2
	addi	a7,a4,8
#APP
# 88 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t1, 1(a7)
	
# 0 "" 2
#NO_APP
	or	s8,t1,a0
#APP
# 88 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, s8, 1(a7)
	
# 0 "" 2
#NO_APP
	addiw	a7,s6,3
	addi	s8,a4,12
#APP
# 88 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a7, 1(s8)
	
# 0 "" 2
#NO_APP
	or	s9,a7,a0
#APP
# 88 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, s9, 1(s8)
	
# 0 "" 2
#NO_APP
	addi	s8,a4,16
	or	s9,s6,a1
#APP
# 88 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, s9, 1(s8)
	
# 0 "" 2
#NO_APP
	or	s6,s6,a2
#APP
# 88 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, s6, 1(s8)
	
# 0 "" 2
#NO_APP
	addi	s6,a4,20
	or	s8,t3,a1
#APP
# 88 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, s8, 1(s6)
	
# 0 "" 2
#NO_APP
	or	t3,t3,a2
#APP
# 88 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t3, 1(s6)
	
# 0 "" 2
#NO_APP
	addi	t3,a4,24
	or	s6,t1,a1
#APP
# 88 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, s6, 1(t3)
	
# 0 "" 2
#NO_APP
	or	t1,t1,a2
#APP
# 88 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t1, 1(t3)
	
# 0 "" 2
#NO_APP
	addi	t1,a4,28
	or	t3,a7,a1
#APP
# 88 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t3, 1(t1)
	
# 0 "" 2
#NO_APP
	or	a7,a7,a2
#APP
# 88 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a7, 1(t1)
	
# 0 "" 2
# 100 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t4, 1(a3)
	
# 0 "" 2
#NO_APP
	or	a7,t4,a1
#APP
# 100 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a7, 1(a3)
	
# 0 "" 2
#NO_APP
	addiw	t3,a5,5
	addi	a7,a3,4
#APP
# 100 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t3, 1(a7)
	
# 0 "" 2
#NO_APP
	or	t1,t3,a1
#APP
# 100 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t1, 1(a7)
	
# 0 "" 2
#NO_APP
	addiw	t1,a5,6
	addi	a7,a3,8
#APP
# 100 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t1, 1(a7)
	
# 0 "" 2
#NO_APP
	or	s6,t1,a1
#APP
# 100 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, s6, 1(a7)
	
# 0 "" 2
#NO_APP
	addiw	a7,a5,7
	addi	s6,a3,12
#APP
# 100 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a7, 1(s6)
	
# 0 "" 2
#NO_APP
	or	a5,a7,a1
#APP
# 100 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a5, 1(s6)
	
# 0 "" 2
#NO_APP
	addi	a5,a3,16
	or	s6,t4,a0
#APP
# 100 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, s6, 1(a5)
	
# 0 "" 2
#NO_APP
	or	t4,t4,a2
#APP
# 100 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t4, 1(a5)
	
# 0 "" 2
#NO_APP
	addi	a5,a3,20
	or	t4,t3,a0
#APP
# 100 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t4, 1(a5)
	
# 0 "" 2
#NO_APP
	or	t3,t3,a2
#APP
# 100 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t3, 1(a5)
	
# 0 "" 2
#NO_APP
	addi	a5,a3,24
	or	t3,t1,a0
#APP
# 100 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t3, 1(a5)
	
# 0 "" 2
#NO_APP
	or	t1,t1,a2
#APP
# 100 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t1, 1(a5)
	
# 0 "" 2
#NO_APP
	addi	a5,a3,28
	or	t1,a7,a0
#APP
# 100 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t1, 1(a5)
	
# 0 "" 2
#NO_APP
	or	a7,a7,a2
#APP
# 100 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a7, 1(a5)
	
# 0 "" 2
# 105 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L10
	
# 0 "" 2
#NO_APP
	addi	t5,t5,1
	addiw	t6,t6,1
	andi	t5,t5,63
	add	a4,a4,t0
	add	a3,a3,t0
	bne	a6,t6,.L11
.L9:
#APP
# 108 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L12
	
# 0 "" 2
#NO_APP
	addi	s5,s5,32
	bne	s0,s5,.L13
.L7:
#APP
# 110 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L14
	
# 0 "" 2
#NO_APP
	addi	s1,s1,32
	bne	t2,s1,.L15
.L16:

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
	
#APP
# 113 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L4
	
# 0 "" 2
#NO_APP
.L5:
#APP
# 115 "gemm_kernel.c" 1
	.insn uj 0x2b, x0, .L5
	
# 0 "" 2
# 191 "gemm_kernel.c" 1
	fence
	
# 0 "" 2
#NO_APP
	j	.L1
.L2:

addi	sp,sp,-96
	sd	s1,80(sp)
	sd	s0,88(sp)
	sd	s2,72(sp)
	sd	s3,64(sp)
	sd	s4,56(sp)
	sd	s5,48(sp)
	mv	s1,a3

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

.L6:


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

.L8:


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
	

.L10:


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
	


.L12:

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
	

.L14:


lw	a5,4(sp)
	addw	t2,s0,t2
	addw	s4,s4,s0
	sext.w	a5,a5
	addw	s3,s3,s0
	addw	s2,s2,s0
	

.L4:


ld	s0,88(sp)
	ld	s1,80(sp)
	ld	s2,72(sp)
	ld	s3,64(sp)
	ld	s4,56(sp)
	ld	s5,48(sp)
	addi	sp,sp,96


.L1:
	
	jr	ra
	.size	gemm_vec_simd, .-gemm_vec_simd
	.comm	start_barrier,32,8
	.ident	"GCC: (GNU) 8.3.0"
	.section	.note.GNU-stack,"",@progbits
