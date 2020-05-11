	.file	"gemm_kernel.c"
	.option nopic
	.text
	.align	2
	.globl	gemm_vec_simd
	.type	gemm_vec_simd, @function
gemm_vec_simd:
#APP
# 36 "gemm_kernel.c" 1
	.insn i 0x77, 0, x0, a0, 0x401
	
# 0 "" 2
# 39 "gemm_kernel.c" 1
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
# 86 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a3, 17(a0)
	
# 0 "" 2
# 87 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a3, 17(a0)
	
# 0 "" 2
#NO_APP
	slli	t5,s8,2
	add	t1,a2,t5
	li	a3,4
#APP
# 90 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a3, 17(t1)
	
# 0 "" 2
# 91 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a3, 17(t1)
	
# 0 "" 2
#NO_APP
	li	a3,4096
#APP
# 86 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a3, 17(a0)
	
# 0 "" 2
# 87 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a3, 17(a0)
	
# 0 "" 2
#NO_APP
	addi	a0,t5,16
	add	a0,a2,a0
	addiw	a3,a3,4
#APP
# 90 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a3, 17(a0)
	
# 0 "" 2
# 91 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a3, 17(a0)
	
# 0 "" 2
#NO_APP
	addi	a5,a5,16
	add	a5,a1,a5
	li	a3,8192
#APP
# 86 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a3, 17(a5)
	
# 0 "" 2
# 87 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a3, 17(a5)
	
# 0 "" 2
#NO_APP
	addiw	a3,a3,4
#APP
# 90 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a3, 17(t1)
	
# 0 "" 2
# 91 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a3, 17(t1)
	
# 0 "" 2
#NO_APP
	li	a3,12288
#APP
# 86 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a3, 17(a5)
	
# 0 "" 2
# 87 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a3, 17(a5)
	
# 0 "" 2
#NO_APP
	addiw	a3,a3,4
#APP
# 90 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a3, 17(a0)
	
# 0 "" 2
# 91 "gemm_kernel.c" 1
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
# 138 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L10
	
# 0 "" 2
#NO_APP
.L9:
	remw	a5,t1,a6
	bnez	a5,.L11
#APP
# 139 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L12
	
# 0 "" 2
#NO_APP
.L11:
#APP
# 140 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L15
	
# 0 "" 2
#NO_APP
	slliw	a3,t3,3
	addiw	t1,t1,1
	mv	a2,t1
	addiw	a5,a3,4
	add	t6,a0,a1
#APP
# 171 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a3, 17(t6)
	
# 0 "" 2
# 172 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a3, 17(t6)
	
# 0 "" 2
#NO_APP
	add	t2,s5,a0
#APP
# 175 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a5, 17(t2)
	
# 0 "" 2
# 176 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a5, 17(t2)
	
# 0 "" 2
#NO_APP
	or	t5,a3,s3
	sext.w	t5,t5
#APP
# 171 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t5, 17(t6)
	
# 0 "" 2
# 172 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, t5, 17(t6)
	
# 0 "" 2
#NO_APP
	or	t6,a5,s3
	sext.w	t6,t6
	add	t5,s6,a0
#APP
# 175 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t6, 17(t5)
	
# 0 "" 2
# 176 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, t6, 17(t5)
	
# 0 "" 2
#NO_APP
	or	t0,a3,s2
	sext.w	t0,t0
	add	t6,s7,a0
#APP
# 171 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t0, 17(t6)
	
# 0 "" 2
# 172 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, t0, 17(t6)
	
# 0 "" 2
#NO_APP
	or	t0,a5,s2
	sext.w	t0,t0
#APP
# 175 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t0, 17(t2)
	
# 0 "" 2
# 176 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, t0, 17(t2)
	
# 0 "" 2
#NO_APP
	or	a3,a3,s1
	sext.w	a3,a3
#APP
# 171 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a3, 17(t6)
	
# 0 "" 2
# 172 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a3, 17(t6)
	
# 0 "" 2
#NO_APP
	or	a5,a5,s1
	sext.w	a5,a5
#APP
# 175 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a5, 17(t5)
	
# 0 "" 2
# 176 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a5, 17(t5)
	
# 0 "" 2
#NO_APP
	remw	a5,t1,a6
	addi	t3,t3,1
	andi	t3,t3,63
	bnez	a5,.L16
#APP
# 209 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L18
	
# 0 "" 2
#NO_APP
.L16:
	remw	a2,a2,t4
	bnez	a2,.L19
#APP
# 210 "gemm_kernel.c" 1
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
# 219 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L15
	
# 0 "" 2
#NO_APP
	addiw	t1,t1,1
	remw	t1,t1,a6
	bnez	t1,.L29
#APP
# 221 "gemm_kernel.c" 1
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
# 225 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L27
	
# 0 "" 2
#NO_APP
.L28:
#APP
# 227 "gemm_kernel.c" 1
	.insn uj 0x2b, x0, .L28
	
# 0 "" 2
# 306 "gemm_kernel.c" 1
	fence
	
# 0 "" 2
#NO_APP
	j	.L1
.L2:
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
.L10:
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
.L12:

	flw	fa7,-2048(a5)
	flw	fa6,-2044(a5)
	
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
.L15:
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
.L18:

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

.L20:

lw	a4,4(sp)
	addw	s1,s3,s1
	addw	s7,s7,s3
	sext.w	a4,a4
	addw	s6,s6,s3
	addw	s5,s5,s3

.L27:
ld	s0,104(sp)
	ld	s1,96(sp)
	ld	s2,88(sp)
	ld	s3,80(sp)
	ld	s4,72(sp)
	ld	s5,64(sp)
	ld	s6,56(sp)
	ld	s7,48(sp)
	addi	sp,sp,112
	
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
