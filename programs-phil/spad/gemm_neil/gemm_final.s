	.file	"gemm_kernel.c"
	.option nopic
	.text
	.align	2
	.globl	gemm_vec_simd
	.type	gemm_vec_simd, @function
gemm_vec_simd:
#APP
# 20 "gemm_kernel.c" 1
	.insn i 0x77, 0, x0, a0, 0x401
	
# 0 "" 2
#NO_APP
	addi	sp,sp,-16
	sd	s0,8(sp)
	sd	s1,0(sp)
	lw	a5,16(sp)
	lw	t6,24(sp)
	lw	t0,32(sp)
	lw	a3,64(sp)

	li	a0,15
	bgt	a3,a0,.L19
	lui	a0,%hi(spm_base_ptr_arr)
	addi	a0,a0,%lo(spm_base_ptr_arr)
	slli	a3,a3,3
	add	a3,a3,a0
	ld	a0,0(a3)
	addi	a0,a0,16
.L2:
#APP
# 36 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L3
	
# 0 "" 2
#NO_APP
	bge	a7,a5,.L17
	addiw	a5,a5,-1
	addiw	a3,t0,-1
	subw	a5,a5,a7
	subw	a3,a3,t6
	srliw	a5,a5,3
	srliw	a3,a3,3
	slli	a5,a5,3
	slli	a3,a3,3
	add	a5,a5,a7
	add	a3,a3,t6
	addi	t2,a1,32
	slli	a3,a3,2
	addi	s0,a2,32
	slli	a7,a7,2
	slli	a5,a5,2
	slli	t1,t6,2
	add	s0,a3,s0
	add	a1,a1,a7
	add	t2,a5,t2
	add	a2,a2,t1
	slli	a4,a4,2
	li	a3,0
.L16:
#APP
# 42 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L7
	
# 0 "" 2
#NO_APP
	bge	t6,t0,.L8
	mv	s1,a2
.L14:
#APP
# 46 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L9
	
# 0 "" 2
#NO_APP
	blez	a6,.L10
	mv	t3,s1
	mv	t1,a1
	li	t4,0
.L12:
	slli	a5,a3,2
	slli	a7,a5,2
	add	a7,a0,a7
#APP
# 55 "gemm_kernel.c" 1
	.insn sb 0x23, 0x4, a7, 0(t1)
	
# 0 "" 2
#NO_APP
	addi	a5,a5,1
	slli	a5,a5,2
	add	a7,a0,a5
	addi	t5,t1,16
#APP
# 55 "gemm_kernel.c" 1
	.insn sb 0x23, 0x4, a7, 0(t5)
	
# 0 "" 2
#NO_APP
	addi	a7,a5,4
	add	a7,a0,a7
#APP
# 61 "gemm_kernel.c" 1
	.insn sb 0x23, 0x4, a7, 0(t3)
	
# 0 "" 2
#NO_APP
	addi	a5,a5,8
	add	a5,a0,a5
	addi	a7,t3,16
#APP
# 61 "gemm_kernel.c" 1
	.insn sb 0x23, 0x4, a5, 0(a7)
	
# 0 "" 2
# 63 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L11
	
# 0 "" 2
#NO_APP
	addi	a3,a3,1
	addiw	t4,t4,1
	andi	a3,a3,127
	add	t1,t1,a4
	add	t3,t3,a4
	bne	a6,t4,.L12
.L10:
#APP
# 66 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L13
	
# 0 "" 2
#NO_APP
	addi	s1,s1,32
	bne	s0,s1,.L14
.L8:
#APP
# 68 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L15
	
# 0 "" 2
#NO_APP
	addi	a1,a1,32
	bne	t2,a1,.L16
.L17:

ld	s0,8(sp)
	ld	s1,0(sp)
	addi	sp,sp,16
	
#APP
# 71 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L5
	
# 0 "" 2
#NO_APP
.L6:
#APP
# 73 "gemm_kernel.c" 1
	.insn uj 0x2b, x1, .L6
	
# 0 "" 2
# 145 "gemm_kernel.c" 1
	fence
	
# 0 "" 2
#NO_APP
	j	.L1
.L3:



addi	sp,sp,-256
	sd	s6,200(sp)
	sd	s7,192(sp)
	sd	s0,248(sp)
	sd	s1,240(sp)
	sd	s2,232(sp)
	sd	s3,224(sp)
	sd	s4,216(sp)
	sd	s5,208(sp)
	sd	s8,184(sp)
	sd	s9,176(sp)
	sd	s10,168(sp)
	sd	s11,160(sp)
	sd	a3,104(sp)
	lw	s6,280(sp)
	lw	s7,288(sp)

	lui	a4,%hi(spm_base_ptr_arr)
	addi	a4,a4,%lo(spm_base_ptr_arr)
	ld	a2,48(a4)
	ld	a6,8(a4)
	ld	a0,16(a4)
	ld	a1,40(a4)
	lw	a4,296(sp)
	lw	a3,116(sp)
	addi	a2,a2,16
	addi	a1,a1,16
	sd	a2,152(sp)
	addi	a6,a6,16
	addi	a0,a0,16
	slli	a4,a4,3
	addi	a2,sp,160
	sd	a1,144(sp)
	add	a4,a2,a4
	sd	a6,128(sp)
	sd	a0,136(sp)
	slliw	a1,s7,2
	sext.w	a3,a3
	ld	s5,-32(a4)
	addw	a4,a1,a7
	
	sext.w	a5,a5
	addiw	t1,a4,1
	addiw	s0,a4,2
	addiw	t6,a4,3
	mulw	a4,a4,a5
	addiw	t2,a1,1
	addiw	t0,a1,2
	addiw	t3,a1,3
	slliw	a7,s6,2
	addiw	a2,a7,1
	sraiw	a6,a2,31
	addiw	a3,a7,2
	srliw	a6,a6,30
	addw	a2,a2,a6
	mulw	a1,t1,a5
	sraiw	a0,a3,31
	srliw	a0,a0,30
	andi	s4,a2,3
	sd	a4,72(sp)
	addiw	a4,a7,3
	addw	a3,a3,a0
	subw	a6,s4,a6
	andi	s3,a3,3
	slli	a6,a6,3
	mulw	s0,s0,a5
	sd	a1,80(sp)
	sraiw	a1,a4,31
	srliw	a1,a1,30
	addw	a4,a4,a1
	sraiw	t5,t2,31
	subw	a0,s3,a0
	sraiw	t4,t0,31
	srliw	t5,t5,30
	andi	s2,a4,3
	mulw	t6,t6,a5
	slliw	a5,a5,3
	sd	a5,96(sp)
	addi	a5,sp,160
	add	a5,a5,a6
	slli	a0,a0,3
	sd	a5,8(sp)
	addi	a5,sp,160
	addw	t2,t2,t5
	srliw	t4,t4,30
	add	a5,a5,a0
	subw	a1,s2,a1
	addw	t0,t0,t4
	andi	s1,t2,3
	slli	a1,a1,3
	sd	a5,16(sp)
	addi	a5,sp,160
	subw	t5,s1,t5
	sraiw	t1,t3,31
	add	a5,a5,a1
	sd	s0,64(sp)
	andi	s0,t0,3
	subw	t4,s0,t4
	srliw	t1,t1,30
	slli	s0,t5,3
	sd	a5,24(sp)
	addi	a5,sp,160
	addw	t3,t3,t1
	add	a5,a5,s0
	slli	s1,t4,3
	sd	t6,56(sp)
	lw	t4,264(sp)
	andi	t6,t3,3
	sd	a5,32(sp)
	addi	a5,sp,160
	subw	t1,t6,t1
	add	a5,a5,s1
	slli	t1,t1,3
	sd	a5,40(sp)
	addi	a5,sp,160
	addw	a7,a7,t4
	sraiw	a2,a2,2
	sraiw	a3,a3,2
	sraiw	a4,a4,2
	li	a6,4096
	add	a5,a5,t1
	sd	a7,88(sp)
	addi	t6,a2,2
	addi	t5,a3,2
	addi	t4,a4,2
	sraiw	t2,t2,2
	sraiw	t0,t0,2
	sraiw	t3,t3,2
	li	a7,0
	addi	s6,s6,2
	add	a6,s5,a6
	sd	a5,48(sp)



	.insn i 0x1b, 0x7, x0, x0, 0
.L7:



ld	a5,88(sp)
	ld	a4,72(sp)
	add	s11,a4,a5
	ld	a4,80(sp)
	slli	s11,s11,2
	add	s10,a4,a5
	ld	a4,64(sp)
	slli	s10,s10,2
	add	s9,a4,a5
	ld	a4,56(sp)
	slli	s9,s9,2
	add	s8,a4,a5
	ld	a5,104(sp)
	slli	s8,s8,2
	add	s11,a5,s11
	add	s10,a5,s10
	add	s9,a5,s9
	add	a5,a5,s8
	sd	a5,0(sp)


	.insn i 0x1b, 0x7, x0, x0, 0
.L9:


ld	a5,8(sp)
	ld	t1,128(sp)
	flw	ft8,-2048(a6)
	ld	s5,-32(a5)
	ld	a5,16(sp)
	flw	fa7,-2044(a6)
	flw	fa6,-2040(a6)
	ld	s4,-32(a5)
	ld	a5,24(sp)
	flw	ft7,-2036(a6)
	flw	ft6,-2032(a6)
	ld	s3,-32(a5)
	ld	a5,32(sp)
	flw	ft5,-2028(a6)
	flw	ft4,-2024(a6)
	ld	s2,-32(a5)
	ld	a5,40(sp)
	flw	ft3,-2020(a6)
	flw	ft2,-2016(a6)
	ld	s1,-32(a5)
	ld	a5,48(sp)
	flw	ft1,-2012(a6)
	flw	ft0,-2008(a6)
	flw	fa0,-2004(a6)
	ld	s0,-32(a5)
	flw	fa1,-2000(a6)
	flw	fa2,-1996(a6)
	flw	fa3,-1992(a6)
	flw	fa4,-1988(a6)


	.insn i 0x1b, 0x7, x0, x0, 0
.L11:



slli	a5,a7,2
	add	a1,s6,a5
	add	a2,t6,a5
	add	a3,t5,a5
	add	a4,t4,a5
	add	a0,a5,s7
	slli	a1,a1,2
	slli	a2,a2,2
	slli	a3,a3,2
	slli	a4,a4,2
	slli	a0,a0,2
	add	a1,t1,a1
	add	a2,s5,a2
	add	a3,s4,a3
	add	a4,s3,a4
	add	a0,t1,a0
#APP
# 119 "gemm_kernel.c" 1
	.insn s 0x03, 0x7, s8, 0(a0)
	
# 0 "" 2
#NO_APP
	fmv.s.x	fa5,s8
#APP
# 120 "gemm_kernel.c" 1
	.insn s 0x03, 0x7, s8, 0(a1)
	
# 0 "" 2
#NO_APP
	fmv.s.x	ft9,s8
	fmadd.s	ft8,ft9,fa5,ft8
#APP
# 119 "gemm_kernel.c" 1
	.insn s 0x03, 0x7, s8, 0(a0)
	
# 0 "" 2
#NO_APP
	fmv.s.x	fa5,s8
#APP
# 120 "gemm_kernel.c" 1
	.insn s 0x03, 0x7, s8, 0(a2)
	
# 0 "" 2
#NO_APP
	fmv.s.x	ft9,s8
	fmadd.s	fa7,ft9,fa5,fa7
#APP
# 119 "gemm_kernel.c" 1
	.insn s 0x03, 0x7, s8, 0(a0)
	
# 0 "" 2
#NO_APP
	fmv.s.x	fa5,s8
#APP
# 120 "gemm_kernel.c" 1
	.insn s 0x03, 0x7, s8, 0(a3)
	
# 0 "" 2
#NO_APP
	fmv.s.x	ft9,s8
	fmadd.s	fa6,ft9,fa5,fa6
#APP
# 119 "gemm_kernel.c" 1
	.insn s 0x03, 0x7, a0, 0(a0)
	
# 0 "" 2
#NO_APP
	fmv.s.x	fa5,a0
#APP
# 120 "gemm_kernel.c" 1
	.insn s 0x03, 0x7, a0, 0(a4)
	
# 0 "" 2
#NO_APP
	fmv.s.x	ft9,a0
	add	a0,a5,t2
	slli	a0,a0,2
	fmadd.s	ft7,ft9,fa5,ft7
	add	a0,s2,a0
#APP
# 119 "gemm_kernel.c" 1
	.insn s 0x03, 0x7, s8, 0(a0)
	
# 0 "" 2
#NO_APP
	fmv.s.x	fa5,s8
#APP
# 120 "gemm_kernel.c" 1
	.insn s 0x03, 0x7, s8, 0(a1)
	
# 0 "" 2
#NO_APP
	fmv.s.x	ft9,s8
	fmadd.s	ft6,ft9,fa5,ft6
#APP
# 119 "gemm_kernel.c" 1
	.insn s 0x03, 0x7, s8, 0(a0)
	
# 0 "" 2
#NO_APP
	fmv.s.x	fa5,s8
#APP
# 120 "gemm_kernel.c" 1
	.insn s 0x03, 0x7, s8, 0(a2)
	
# 0 "" 2
#NO_APP
	fmv.s.x	ft9,s8
	fmadd.s	ft5,ft9,fa5,ft5
#APP
# 119 "gemm_kernel.c" 1
	.insn s 0x03, 0x7, s8, 0(a0)
	
# 0 "" 2
#NO_APP
	fmv.s.x	fa5,s8
#APP
# 120 "gemm_kernel.c" 1
	.insn s 0x03, 0x7, s8, 0(a3)
	
# 0 "" 2
#NO_APP
	fmv.s.x	ft9,s8
	fmadd.s	ft4,ft9,fa5,ft4
#APP
# 119 "gemm_kernel.c" 1
	.insn s 0x03, 0x7, a0, 0(a0)
	
# 0 "" 2
#NO_APP
	fmv.s.x	fa5,a0
#APP
# 120 "gemm_kernel.c" 1
	.insn s 0x03, 0x7, a0, 0(a4)
	
# 0 "" 2
#NO_APP
	fmv.s.x	ft9,a0
	add	a0,a5,t0
	slli	a0,a0,2
	fmadd.s	ft3,ft9,fa5,ft3
	add	a0,s1,a0
#APP
# 119 "gemm_kernel.c" 1
	.insn s 0x03, 0x7, s8, 0(a0)
	
# 0 "" 2
#NO_APP
	fmv.s.x	fa5,s8
#APP
# 120 "gemm_kernel.c" 1
	.insn s 0x03, 0x7, s8, 0(a1)
	
# 0 "" 2
#NO_APP
	fmv.s.x	ft9,s8
	fmadd.s	ft2,ft9,fa5,ft2
#APP
# 119 "gemm_kernel.c" 1
	.insn s 0x03, 0x7, s8, 0(a0)
	
# 0 "" 2
#NO_APP
	fmv.s.x	fa5,s8
#APP
# 120 "gemm_kernel.c" 1
	.insn s 0x03, 0x7, s8, 0(a2)
	
# 0 "" 2
#NO_APP
	fmv.s.x	ft9,s8
	fmadd.s	ft1,ft9,fa5,ft1
#APP
# 119 "gemm_kernel.c" 1
	.insn s 0x03, 0x7, s8, 0(a0)
	
# 0 "" 2
#NO_APP
	fmv.s.x	fa5,s8
#APP
# 120 "gemm_kernel.c" 1
	.insn s 0x03, 0x7, s8, 0(a3)
	
# 0 "" 2
#NO_APP
	fmv.s.x	ft9,s8
	fmadd.s	ft0,ft9,fa5,ft0
#APP
# 119 "gemm_kernel.c" 1
	.insn s 0x03, 0x7, a0, 0(a0)
	
# 0 "" 2
# 120 "gemm_kernel.c" 1
	.insn s 0x03, 0x7, s8, 0(a4)
	
# 0 "" 2
#NO_APP
	fmv.s.x	fa5,s8
	fmv.s.x	ft9,a0
	add	a5,a5,t3
	slli	a5,a5,2
	fmadd.s	fa0,fa5,ft9,fa0
	add	a5,s0,a5
#APP
# 119 "gemm_kernel.c" 1
	.insn s 0x03, 0x7, a0, 0(a5)
	
# 0 "" 2
# 120 "gemm_kernel.c" 1
	.insn s 0x03, 0x7, a1, 0(a1)
	
# 0 "" 2
#NO_APP
	fmv.s.x	fa5,a0
	fmv.s.x	ft9,a1
	fmadd.s	fa1,fa5,ft9,fa1
#APP
# 119 "gemm_kernel.c" 1
	.insn s 0x03, 0x7, a1, 0(a5)
	
# 0 "" 2
# 120 "gemm_kernel.c" 1
	.insn s 0x03, 0x7, a2, 0(a2)
	
# 0 "" 2
#NO_APP
	fmv.s.x	fa5,a1
	fmv.s.x	ft9,a2
	fmadd.s	fa2,fa5,ft9,fa2
#APP
# 119 "gemm_kernel.c" 1
	.insn s 0x03, 0x7, a2, 0(a5)
	
# 0 "" 2
# 120 "gemm_kernel.c" 1
	.insn s 0x03, 0x7, a3, 0(a3)
	
# 0 "" 2
#NO_APP
	fmv.s.x	fa5,a2
	fmv.s.x	ft9,a3
	fmadd.s	fa3,fa5,ft9,fa3
#APP
# 119 "gemm_kernel.c" 1
	.insn s 0x03, 0x7, a5, 0(a5)
	
# 0 "" 2
# 120 "gemm_kernel.c" 1
	.insn s 0x03, 0x7, a4, 0(a4)
	
# 0 "" 2
#NO_APP
	fmv.s.x	fa5,a5
	fmv.s.x	ft9,a4
	addi	a7,a7,1
	andi	a7,a7,127
	fmadd.s	fa4,fa5,ft9,fa4
#APP
# 126 "gemm_kernel.c" 1
	.insn u 0x0b, x0, 0
	
# 0 "" 2
#NO_APP
	lw	a5,124(sp)
	sext.w	a5,a5
	

	.insn i 0x1b, 0x7, x0, x0, 0
.L13:


fsw	ft8,-2048(a6)
	fsw	fa7,-2044(a6)
	fsw	fa6,-2040(a6)
	fsw	ft7,-2036(a6)
	fsw	ft6,-2032(a6)
	fsw	ft5,-2028(a6)
	fsw	ft4,-2024(a6)
	fsw	ft3,-2020(a6)
	fsw	ft2,-2016(a6)
	fsw	ft1,-2012(a6)
	fsw	ft0,-2008(a6)
	fsw	fa0,-2004(a6)
	fsw	fa1,-2000(a6)
	fsw	fa2,-1996(a6)
	fsw	fa3,-1992(a6)
	fsw	fa4,-1988(a6)

	lw	a5,-2048(a6)
#APP
# 134 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a5, 0(s11)
	
# 0 "" 2
#NO_APP
	sw	zero,-2048(a6)
	addi	a5,s11,4
	lw	a4,-2044(a6)
#APP
# 134 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a4, 0(a5)
	
# 0 "" 2
#NO_APP
	sw	zero,-2044(a6)
	addi	a5,s11,8
	lw	a4,-2040(a6)
#APP
# 134 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a4, 0(a5)
	
# 0 "" 2
#NO_APP
	sw	zero,-2040(a6)
	addi	a5,s11,12
	lw	a4,-2036(a6)
#APP
# 134 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a4, 0(a5)
	
# 0 "" 2
#NO_APP
	sw	zero,-2036(a6)
	lw	a5,-2032(a6)
#APP
# 134 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a5, 0(s10)
	
# 0 "" 2
#NO_APP
	sw	zero,-2032(a6)
	addi	a5,s10,4
	lw	a4,-2028(a6)
#APP
# 134 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a4, 0(a5)
	
# 0 "" 2
#NO_APP
	sw	zero,-2028(a6)
	addi	a5,s10,8
	lw	a4,-2024(a6)
#APP
# 134 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a4, 0(a5)
	
# 0 "" 2
#NO_APP
	sw	zero,-2024(a6)
	addi	a5,s10,12
	lw	a4,-2020(a6)
#APP
# 134 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a4, 0(a5)
	
# 0 "" 2
#NO_APP
	sw	zero,-2020(a6)
	lw	a5,-2016(a6)
#APP
# 134 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a5, 0(s9)
	
# 0 "" 2
#NO_APP
	sw	zero,-2016(a6)
	addi	a5,s9,4
	lw	a4,-2012(a6)
#APP
# 134 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a4, 0(a5)
	
# 0 "" 2
#NO_APP
	sw	zero,-2012(a6)
	addi	a5,s9,8
	lw	a4,-2008(a6)
#APP
# 134 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a4, 0(a5)
	
# 0 "" 2
#NO_APP
	sw	zero,-2008(a6)
	addi	a5,s9,12
	lw	a4,-2004(a6)
#APP
# 134 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a4, 0(a5)
	
# 0 "" 2
#NO_APP
	sw	zero,-2004(a6)
	lw	a5,-2000(a6)
	ld	a3,0(sp)
#APP
# 134 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a5, 0(a3)
	
# 0 "" 2
#NO_APP
	sw	zero,-2000(a6)
	addi	a5,a3,4
	lw	a4,-1996(a6)
#APP
# 134 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a4, 0(a5)
	
# 0 "" 2
#NO_APP
	sw	zero,-1996(a6)
	addi	a5,a3,8
	lw	a4,-1992(a6)
#APP
# 134 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a4, 0(a5)
	
# 0 "" 2
#NO_APP
	sw	zero,-1992(a6)
	addi	a5,a3,12
	lw	a4,-1988(a6)
#APP
# 134 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a4, 0(a5)
	
# 0 "" 2
#NO_APP
	lw	a5,120(sp)
	addi	a4,a3,32
	sw	zero,-1988(a6)
	sext.w	a5,a5
	sd	a4,0(sp)
	addi	s11,s11,32
	addi	s10,s10,32
	addi	s9,s9,32


	.insn i 0x1b, 0x7, x0, x0, 0
.L15:


ld	a4,96(sp)
	ld	a3,72(sp)
	lw	a5,116(sp)
	addw	a3,a3,a4
	sd	a3,72(sp)
	ld	a3,80(sp)
	sext.w	a5,a5
	addw	a3,a4,a3
	sd	a3,80(sp)
	ld	a3,64(sp)
	addw	a3,a3,a4
	sd	a3,64(sp)
	ld	a3,56(sp)
	addw	a4,a3,a4
	sd	a4,56(sp)

	
	.insn i 0x1b, 0x7, x0, x0, 0
.L5:
ld	s0,248(sp)
	ld	s1,240(sp)
	ld	s2,232(sp)
	ld	s3,224(sp)
	ld	s4,216(sp)
	ld	s5,208(sp)
	ld	s6,200(sp)
	ld	s7,192(sp)
	ld	s8,184(sp)
	ld	s9,176(sp)
	ld	s10,168(sp)
	ld	s11,160(sp)
	addi	sp,sp,256

	.insn i 0x1b, 0x7, x0, x0, 0
.L1:
	
	jr	ra
.L19:
	li	a0,0
	j	.L2
	.size	gemm_vec_simd, .-gemm_vec_simd
	.comm	start_barrier,32,8
	.ident	"GCC: (GNU) 8.3.0"
	.section	.note.GNU-stack,"",@progbits
