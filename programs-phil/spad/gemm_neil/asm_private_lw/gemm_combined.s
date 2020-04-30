	.file	"gemm_kernel.c"
	.option nopic
	.text
	.align	2
	.globl	gemm_vec_simd
	.type	gemm_vec_simd, @function
gemm_vec_simd:
#APP
# 32 "gemm_kernel.c" 1
	.insn i 0x77, 0, x0, a0, 0x401
	
# 0 "" 2
# 51 "gemm_kernel.c" 1
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
# 57 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L6
	
# 0 "" 2
#NO_APP
	bge	s3,s4,.L7
	mv	s5,s2
.L13:
#APP
# 61 "gemm_kernel.c" 1
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
# 94 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, s6, 1(a4)
	
# 0 "" 2
#NO_APP
	or	a7,s6,a0
#APP
# 94 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a7, 1(a4)
	
# 0 "" 2
#NO_APP
	addiw	t3,s6,1
	addi	a7,a4,4
#APP
# 94 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t3, 1(a7)
	
# 0 "" 2
#NO_APP
	or	t1,t3,a0
#APP
# 94 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t1, 1(a7)
	
# 0 "" 2
#NO_APP
	addiw	t1,s6,2
	addi	a7,a4,8
#APP
# 94 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t1, 1(a7)
	
# 0 "" 2
#NO_APP
	or	s8,t1,a0
#APP
# 94 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, s8, 1(a7)
	
# 0 "" 2
#NO_APP
	addiw	a7,s6,3
	addi	s8,a4,12
#APP
# 94 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a7, 1(s8)
	
# 0 "" 2
#NO_APP
	or	s9,a7,a0
#APP
# 94 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, s9, 1(s8)
	
# 0 "" 2
#NO_APP
	addi	s8,a4,16
	or	s9,s6,a1
#APP
# 94 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, s9, 1(s8)
	
# 0 "" 2
#NO_APP
	or	s6,s6,a2
#APP
# 94 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, s6, 1(s8)
	
# 0 "" 2
#NO_APP
	addi	s6,a4,20
	or	s8,t3,a1
#APP
# 94 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, s8, 1(s6)
	
# 0 "" 2
#NO_APP
	or	t3,t3,a2
#APP
# 94 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t3, 1(s6)
	
# 0 "" 2
#NO_APP
	addi	t3,a4,24
	or	s6,t1,a1
#APP
# 94 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, s6, 1(t3)
	
# 0 "" 2
#NO_APP
	or	t1,t1,a2
#APP
# 94 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t1, 1(t3)
	
# 0 "" 2
#NO_APP
	addi	t1,a4,28
	or	t3,a7,a1
#APP
# 94 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t3, 1(t1)
	
# 0 "" 2
#NO_APP
	or	a7,a7,a2
#APP
# 94 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a7, 1(t1)
	
# 0 "" 2
# 106 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t4, 1(a3)
	
# 0 "" 2
#NO_APP
	or	a7,t4,a1
#APP
# 106 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a7, 1(a3)
	
# 0 "" 2
#NO_APP
	addiw	t3,a5,5
	addi	a7,a3,4
#APP
# 106 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t3, 1(a7)
	
# 0 "" 2
#NO_APP
	or	t1,t3,a1
#APP
# 106 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t1, 1(a7)
	
# 0 "" 2
#NO_APP
	addiw	t1,a5,6
	addi	a7,a3,8
#APP
# 106 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t1, 1(a7)
	
# 0 "" 2
#NO_APP
	or	s6,t1,a1
#APP
# 106 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, s6, 1(a7)
	
# 0 "" 2
#NO_APP
	addiw	a7,a5,7
	addi	s6,a3,12
#APP
# 106 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a7, 1(s6)
	
# 0 "" 2
#NO_APP
	or	a5,a7,a1
#APP
# 106 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a5, 1(s6)
	
# 0 "" 2
#NO_APP
	addi	a5,a3,16
	or	s6,t4,a0
#APP
# 106 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, s6, 1(a5)
	
# 0 "" 2
#NO_APP
	or	t4,t4,a2
#APP
# 106 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t4, 1(a5)
	
# 0 "" 2
#NO_APP
	addi	a5,a3,20
	or	t4,t3,a0
#APP
# 106 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t4, 1(a5)
	
# 0 "" 2
#NO_APP
	or	t3,t3,a2
#APP
# 106 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t3, 1(a5)
	
# 0 "" 2
#NO_APP
	addi	a5,a3,24
	or	t3,t1,a0
#APP
# 106 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t3, 1(a5)
	
# 0 "" 2
#NO_APP
	or	t1,t1,a2
#APP
# 106 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t1, 1(a5)
	
# 0 "" 2
#NO_APP
	addi	a5,a3,28
	or	t1,a7,a0
#APP
# 106 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t1, 1(a5)
	
# 0 "" 2
#NO_APP
	or	a7,a7,a2
#APP
# 106 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a7, 1(a5)
	
# 0 "" 2
# 111 "gemm_kernel.c" 1
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
# 114 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L12
	
# 0 "" 2
#NO_APP
	addi	s5,s5,32
	bne	s0,s5,.L13
.L7:
#APP
# 116 "gemm_kernel.c" 1
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
# 119 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L4
	
# 0 "" 2
#NO_APP
.L5:
#APP
# 121 "gemm_kernel.c" 1
	.insn uj 0x2b, x0, .L5
	
# 0 "" 2
# 200 "gemm_kernel.c" 1
	fence
	
# 0 "" 2
#NO_APP
	j	.L1
.L2:


addi	sp,sp,-112
	sd	s3,80(sp)
	sd	s0,104(sp)
	sd	s1,96(sp)
	sd	s2,88(sp)
	sd	s4,72(sp)
	sd	s5,64(sp)
	sd	s6,56(sp)
	mv	s3,a3
	lui	a4,%hi(spm_base_ptr_arr)
	addi	a4,a4,%lo(spm_base_ptr_arr)
	ld	a2,48(a4)
	ld	a6,8(a4)
	ld	a0,16(a4)
	ld	a1,40(a4)
	lw	a4,152(sp)
	lw	a3,4(sp)
	lw	s0,144(sp)
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
	slliw	s0,s0,2
	sext.w	a3,a3
	ld	t4,-32(a4)
	addw	s0,s0,a7

	sext.w	a4,a5
	addiw	s6,s0,1
	addiw	s5,s0,2
	addiw	s4,s0,3
	mulw	s6,s6,a4
	lw	s1,136(sp)
	lw	a5,120(sp)
	slliw	s2,a4,3
	slliw	s1,s1,2
	addw	s1,s1,a5
	li	a5,4096
	li	t5,0
	add	a5,t4,a5
	mulw	s0,s0,a4
	mulw	s5,s5,a4
	mulw	s4,s4,a4

.L6:


	add	a3,s0,s1
	add	t2,s6,s1
	add	t0,s5,s1
	add	t6,s4,s1
	slli	a3,a3,2
	slli	t2,t2,2
	slli	t0,t0,2
	slli	t6,t6,2
	add	a3,s3,a3
	add	t2,s3,t2
	add	t0,s3,t0
	add	t6,s3,t6

.L8:


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

.L10:


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
# 181 "gemm_kernel.c" 1
	.insn u 0x0b, x0, 0
	
# 0 "" 2
#NO_APP
	lw	a4,12(sp)
	sext.w	a4,a4


.L12:

fmv.x.s	a4,fa7
#APP
# 189 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a4, 0(a3)
	
# 0 "" 2
#NO_APP
	sw	zero,-2048(a5)
	addi	a4,a3,4
	fmv.x.s	a2,fa6
#APP
# 189 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a2, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-2044(a5)
	addi	a4,a3,8
	fmv.x.s	a2,ft7
#APP
# 189 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a2, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-2040(a5)
	addi	a4,a3,12
	fmv.x.s	a2,ft6
#APP
# 189 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a2, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-2036(a5)
	fmv.x.s	a4,ft5
#APP
# 189 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a4, 0(t2)
	
# 0 "" 2
#NO_APP
	sw	zero,-2032(a5)
	addi	a4,t2,4
	fmv.x.s	a2,ft4
#APP
# 189 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a2, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-2028(a5)
	addi	a4,t2,8
	fmv.x.s	a2,ft3
#APP
# 189 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a2, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-2024(a5)
	addi	a4,t2,12
	fmv.x.s	a2,ft2
#APP
# 189 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a2, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-2020(a5)
	fmv.x.s	a4,ft1
#APP
# 189 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a4, 0(t0)
	
# 0 "" 2
#NO_APP
	sw	zero,-2016(a5)
	addi	a4,t0,4
	fmv.x.s	a2,ft0
#APP
# 189 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a2, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-2012(a5)
	addi	a4,t0,8
	fmv.x.s	a2,fa0
#APP
# 189 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a2, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-2008(a5)
	addi	a4,t0,12
	fmv.x.s	a2,fa1
#APP
# 189 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a2, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-2004(a5)
	fmv.x.s	a4,fa2
#APP
# 189 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a4, 0(t6)
	
# 0 "" 2
#NO_APP
	sw	zero,-2000(a5)
	addi	a4,t6,4
	fmv.x.s	a2,fa3
#APP
# 189 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a2, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-1996(a5)
	addi	a4,t6,8
	fmv.x.s	a2,fa4
#APP
# 189 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a2, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-1992(a5)
	addi	a4,t6,12
	fmv.x.s	a2,fa5
#APP
# 189 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a2, 0(a4)
	
# 0 "" 2
#NO_APP
	lw	a4,8(sp)
	sw	zero,-1988(a5)
	addi	a3,a3,32
	sext.w	a4,a4
	addi	t2,t2,32
	addi	t0,t0,32
	addi	t6,t6,32

.L14:

lw	a4,4(sp)
	addw	s0,s2,s0
	addw	s6,s6,s2
	sext.w	a4,a4
	addw	s5,s5,s2
	addw	s4,s4,s2

.L4:
ld	s0,104(sp)
	ld	s1,96(sp)
	ld	s2,88(sp)
	ld	s3,80(sp)
	ld	s4,72(sp)
	ld	s5,64(sp)
	ld	s6,56(sp)
	addi	sp,sp,112

.L1:
	
	jr	ra
	.size	gemm_vec_simd, .-gemm_vec_simd
	.comm	start_barrier,32,8
	.ident	"GCC: (GNU) 8.3.0"
	.section	.note.GNU-stack,"",@progbits
