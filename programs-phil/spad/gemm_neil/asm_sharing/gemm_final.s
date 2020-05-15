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
	addi	sp,sp,-96
	sd	s0,88(sp)
	sd	s2,72(sp)
	sd	s3,64(sp)
	sd	s1,80(sp)
	sd	s4,56(sp)
	sd	s5,48(sp)
	sd	s6,40(sp)
	sd	s7,32(sp)
	sd	s8,24(sp)
	sd	s9,16(sp)
	sd	s10,8(sp)
	sd	s11,0(sp)
	lw	s3,96(sp)
	lw	s0,104(sp)
	lw	s2,112(sp)

	.insn uj 0x6b, x0, .L2
	
# 0 "" 2
#NO_APP
	subw	a5,s2,s0
	sraiw	t3,a5,31
	srliw	t3,t3,29
	addw	t3,t3,a5
	sraiw	t3,t3,3
	sext.w	s4,a6
	blez	a6,.L3
	li	s4,1
.L3:
	bgtz	a6,.L42
	li	t1,0
	slli	s1,s0,2
	bge	a7,s3,.L29
.L5:
	sext.w	s6,a6
	mulw	t3,t3,s6
	mv	s5,a7
	add	s1,a2,s1
	slli	t5,a4,2
	mv	s7,a7
	li	a0,0
	addi	t2,a1,16
.L23:
	ble	s2,s0,.L26
	slli	t6,s5,2
	sub	t6,s1,t6
	mv	s8,s0
.L22:
	li	a5,0
	beq	s7,a7,.L43
.L7:
	ble	a6,a5,.L8
	mulw	a2,a4,a5
	addw	t4,a0,s6
	subw	t4,t4,a5
	addi	t0,t6,16
	add	a2,a2,s5
	slli	a2,a2,2
.L21:
	remw	a5,a0,t3
	bnez	a5,.L9
#APP
# 138 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L10
	
# 0 "" 2
#NO_APP
.L9:
	remw	a5,a0,a6
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
	addiw	a0,a0,1
	slliw	a5,t1,2
	mv	a3,a0
	addiw	s9,a5,2
	add	s10,a1,a2
#APP
# 153 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a5, 16(s10)
	
# 0 "" 2
# 154 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a5, 16(s10)
	
# 0 "" 2
#NO_APP
	addiw	s10,a5,1
	add	s11,t2,a2
#APP
# 153 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, s10, 16(s11)
	
# 0 "" 2
# 154 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, s10, 16(s11)
	
# 0 "" 2
#NO_APP
	add	s10,t6,a2
#APP
# 161 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, s9, 16(s10)
	
# 0 "" 2
# 162 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, s9, 16(s10)
	
# 0 "" 2
#NO_APP
	addiw	a5,a5,3
	add	s9,t0,a2
#APP
# 161 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a5, 16(s9)
	
# 0 "" 2
# 162 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a5, 16(s9)
	
# 0 "" 2
#NO_APP
	remw	a5,a0,a6
	addi	t1,t1,1
	andi	t1,t1,127
	bnez	a5,.L16
#APP
# 209 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L18
	
# 0 "" 2
#NO_APP
.L16:
	remw	a3,a3,t3
	bnez	a3,.L19
#APP
# 210 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L20
	
# 0 "" 2
#NO_APP
.L19:
	add	a2,a2,t5
	bne	t4,a0,.L21
.L8:
	addiw	s8,s8,8
	addi	t6,t6,32
	bgt	s2,s8,.L22
.L26:
	addiw	s7,s7,8
	addi	s5,s5,8
	bgt	s3,s7,.L23
	blez	a6,.L29
.L6:
#APP
# 219 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L15
	
# 0 "" 2
#NO_APP
	addiw	a0,a0,1
	remw	a0,a0,a6
	bnez	a0,.L29
#APP
# 221 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L18
	
# 0 "" 2
#NO_APP
.L29:

	ld	s0,88(sp)
	ld	s1,80(sp)
	ld	s2,72(sp)
	ld	s3,64(sp)
	ld	s4,56(sp)
	ld	s5,48(sp)
	ld	s6,40(sp)
	ld	s7,32(sp)
	ld	s8,24(sp)
	ld	s9,16(sp)
	ld	s10,8(sp)
	ld	s11,0(sp)
	addi	sp,sp,96

#APP
# 225 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L27
	
# 0 "" 2
#NO_APP
.L28:
#APP
# 227 "gemm_kernel.c" 1
	.insn uj 0x2b, x1, .L28
	
# 0 "" 2
# 306 "gemm_kernel.c" 1
	fence
	
# 0 "" 2
#NO_APP
	j	.L1
.L2:
addi	sp,sp,-272
	sd	s7,208(sp)
	sd	s8,200(sp)
	sd	s0,264(sp)
	sd	s1,256(sp)
	sd	s2,248(sp)
	sd	s3,240(sp)
	sd	s4,232(sp)
	sd	s5,224(sp)
	sd	s6,216(sp)
	sd	s9,192(sp)
	sd	s10,184(sp)
	sd	s11,176(sp)
	sd	a3,120(sp)
	lw	s7,296(sp)
	lw	s8,304(sp)
	lui	a4,%hi(spm_base_ptr_arr)
	addi	a4,a4,%lo(spm_base_ptr_arr)
	ld	a2,48(a4)
	ld	a6,8(a4)
	ld	a0,16(a4)
	ld	a1,40(a4)
	lw	a4,312(sp)
	lw	a3,132(sp)
	addi	a2,a2,16
	sd	a2,168(sp)
	addi	a6,a6,16
	addi	a2,sp,176
	addi	a0,a0,16
	addi	a1,a1,16
	slli	a4,a4,3
	add	a4,a2,a4
	sd	a6,144(sp)
	sd	a0,152(sp)
	sd	a1,160(sp)
	slliw	a2,s8,2
	sext.w	a3,a3
	ld	s5,-32(a4)
	addw	a4,a2,a7
	
	sext.w	a5,a5
	addiw	a1,a4,1
	addiw	t0,a4,2
	addiw	t6,a4,3
	mulw	a4,a4,a5
	slliw	t5,s7,2
	addiw	t1,t5,1
	addiw	s0,a2,1
	addiw	t2,a2,2
	addiw	t4,a2,3
	sraiw	t3,t1,31
	addiw	a3,t5,2
	srliw	t3,t3,30
	addw	t1,t1,t3
	mulw	a2,a1,a5
	sraiw	a7,a3,31
	srliw	a7,a7,30
	andi	s4,t1,3
	sd	a4,88(sp)
	addiw	a4,t5,3
	addw	a3,a3,a7
	sraiw	a6,a4,31
	subw	t3,s4,t3
	srliw	a6,a6,30
	mulw	t0,t0,a5
	andi	s3,a3,3
	slli	t3,t3,3
	addw	a4,a4,a6
	sraiw	a0,s0,31
	subw	a7,s3,a7
	srliw	a0,a0,30
	andi	s2,a4,3
	slli	a7,a7,3
	addw	s0,s0,a0
	mulw	t6,t6,a5
	slliw	a5,a5,3
	sd	a5,112(sp)
	addi	a5,sp,176
	add	a5,a5,t3
	sd	a5,24(sp)
	addi	a5,sp,176
	sraiw	a1,t2,31
	add	a5,a5,a7
	subw	a6,s2,a6
	srliw	a1,a1,30
	andi	s1,s0,3
	slli	a6,a6,3
	sd	a5,32(sp)
	addi	a5,sp,176
	addw	t2,t2,a1
	add	a5,a5,a6
	sd	a2,96(sp)
	subw	a0,s1,a0
	sraiw	a2,t4,31
	srliw	a2,a2,30
	sd	t0,80(sp)
	slli	a0,a0,3
	andi	t0,t2,3
	sd	a5,40(sp)
	addi	a5,sp,176
	addw	t4,t4,a2
	subw	a1,t0,a1
	add	a5,a5,a0
	sd	t6,72(sp)
	slli	a1,a1,3
	andi	t6,t4,3
	sd	a5,48(sp)
	addi	a5,sp,176
	subw	a2,t6,a2
	add	a5,a5,a1
	lw	t6,280(sp)
	slli	a2,a2,3
	sd	a5,56(sp)
	addi	a5,sp,176
	add	a5,a5,a2
	addw	t5,t5,t6
	sraiw	t1,t1,2
	sraiw	a3,a3,2
	sraiw	a4,a4,2
	sd	a5,64(sp)
	li	a5,4096
	sd	t5,104(sp)
	addi	t0,t1,2
	addi	t6,a3,2
	addi	t5,a4,2
	sraiw	s0,s0,2
	sraiw	t2,t2,2
	sraiw	t4,t4,2
	li	t1,0
	addi	s7,s7,2
	add	a5,s5,a5
	.insn i 0x1b, 0x7, x0, x0, 0
.L10:
ld	a4,104(sp)
	ld	a3,88(sp)
	ld	a2,72(sp)
	li	s9,4
	add	s11,a3,a4
	ld	a3,96(sp)
	slli	s11,s11,2
	add	s10,a3,a4
	ld	a3,80(sp)
	slli	s10,s10,2
	add	a3,a3,a4
	add	a4,a2,a4
	ld	a2,120(sp)
	slli	a3,a3,2
	slli	a4,a4,2
	add	a3,a2,a3
	add	a4,a2,a4
	add	s11,a2,s11
	add	s10,a2,s10
	sd	a3,16(sp)
	sd	a4,8(sp)
	.insn i 0x1b, 0x7, x0, x0, 0
.L12:
ld	a4,24(sp)
	ld	t3,144(sp)
	ld	s6,-32(a4)
	ld	a4,32(sp)
	ld	s5,-32(a4)
	ld	a4,40(sp)
	ld	s4,-32(a4)
	ld	a4,48(sp)
	ld	s3,-32(a4)
	ld	a4,56(sp)
	ld	s2,-32(a4)
	ld	a4,64(sp)
	ld	s1,-32(a4)
	.insn i 0x1b, 0x7, x0, x0, 0
.L15:
slli	a4,t1,2
	add	a0,s7,a4
	add	a6,a4,s8
	slli	a0,a0,2
	slli	a6,a6,2
	add	a6,t3,a6
	add	a0,t3,a0
	flw	fa3,0(a6)
	flw	fa5,0(a0)
	flw	fa4,-2048(a5)
	add	a1,t0,a4
	slli	a1,a1,2
	fmadd.s	fa5,fa5,fa3,fa4
	add	a1,s6,a1
	flw	fa2,-2044(a5)
	add	a2,t6,a4
	slli	a2,a2,2
	fsw	fa5,-2048(a5)
	flw	fa5,0(a6)
	flw	fa4,0(a1)
	add	a2,s5,a2
	flw	fa3,-2040(a5)
	fmadd.s	fa4,fa4,fa5,fa2
	add	a3,t5,a4
	slli	a3,a3,2
	add	a3,s4,a3
	flw	fa2,-2036(a5)
	fsw	fa4,-2044(a5)
	flw	fa4,0(a6)
	flw	fa5,0(a2)
	add	a7,a4,s0
	slli	a7,a7,2
	fmadd.s	fa5,fa5,fa4,fa3
	add	a7,s3,a7
	flw	fa3,-2032(a5)
	flw	fa4,-2028(a5)
	flw	ft0,-2024(a5)
	fsw	fa5,-2040(a5)
	flw	fa5,0(a6)
	flw	fa1,0(a3)
	flw	fa0,-2020(a5)
	add	a6,a4,t2
	fmadd.s	fa1,fa1,fa5,fa2
	slli	a6,a6,2
	add	a6,s2,a6
	add	a4,a4,t4
	slli	a4,a4,2
	fsw	fa1,-2036(a5)
	flw	fa5,0(a7)
	flw	fa2,0(a0)
	add	a4,s1,a4
	addi	t1,t1,1
	fmadd.s	fa2,fa2,fa5,fa3
	andi	t1,t1,127
	fsw	fa2,-2032(a5)
	flw	fa5,0(a7)
	flw	fa3,0(a1)
	fmadd.s	fa3,fa3,fa5,fa4
	fsw	fa3,-2028(a5)
	flw	fa5,0(a7)
	flw	fa4,0(a2)
	fmadd.s	fa4,fa4,fa5,ft0
	fsw	fa4,-2024(a5)
	flw	fa4,0(a7)
	flw	fa5,0(a3)
	fmadd.s	fa5,fa5,fa4,fa0
	fsw	fa5,-2020(a5)
	flw	fa5,0(a0)
	flw	fa3,0(a6)
	flw	fa4,-2016(a5)
	flw	fa0,-2012(a5)
	flw	fa1,-2008(a5)
	fmadd.s	fa5,fa5,fa3,fa4
	flw	fa2,-2004(a5)
	flw	fa3,-2000(a5)
	flw	fa4,-1996(a5)
	flw	ft2,-1992(a5)
	fsw	fa5,-2016(a5)
	flw	fa5,0(a6)
	flw	ft0,0(a1)
	flw	ft1,-1988(a5)
	fmadd.s	ft0,ft0,fa5,fa0
	fsw	ft0,-2012(a5)
	flw	fa5,0(a6)
	flw	fa0,0(a2)
	fmadd.s	fa0,fa0,fa5,fa1
	fsw	fa0,-2008(a5)
	flw	fa5,0(a6)
	flw	fa1,0(a3)
	fmadd.s	fa1,fa1,fa5,fa2
	fsw	fa1,-2004(a5)
	flw	fa5,0(a0)
	flw	fa2,0(a4)
	fmadd.s	fa2,fa2,fa5,fa3
	fsw	fa2,-2000(a5)
	flw	fa5,0(a1)
	flw	fa3,0(a4)
	fmadd.s	fa3,fa3,fa5,fa4
	fsw	fa3,-1996(a5)
	flw	fa5,0(a2)
	flw	fa4,0(a4)
	fmadd.s	fa4,fa4,fa5,ft2
	fsw	fa4,-1992(a5)
	flw	fa5,0(a4)
	flw	fa4,0(a3)
	fmadd.s	fa5,fa5,fa4,ft1
	fsw	fa5,-1988(a5)
#APP
# 287 "gemm_kernel.c" 1
	.insn i 0x1b, 0x2, x0, s9, 0
	
# 0 "" 2
#NO_APP


	.insn i 0x1b, 0x7, x0, x0, 0
.L18:
lw	a4,-2048(a5)
#APP
# 295 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a4, 0(s11)
	
# 0 "" 2
#NO_APP
	sw	zero,-2048(a5)
	addi	a4,s11,4
	lw	a3,-2044(a5)
#APP
# 295 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a3, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-2044(a5)
	addi	a4,s11,8
	lw	a3,-2040(a5)
#APP
# 295 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a3, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-2040(a5)
	addi	a4,s11,12
	lw	a3,-2036(a5)
#APP
# 295 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a3, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-2036(a5)
	lw	a4,-2032(a5)
#APP
# 295 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a4, 0(s10)
	
# 0 "" 2
#NO_APP
	sw	zero,-2032(a5)
	addi	a4,s10,4
	lw	a3,-2028(a5)
#APP
# 295 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a3, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-2028(a5)
	addi	a4,s10,8
	lw	a3,-2024(a5)
#APP
# 295 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a3, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-2024(a5)
	addi	a4,s10,12
	lw	a3,-2020(a5)
#APP
# 295 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a3, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-2020(a5)
	lw	a4,-2016(a5)
	ld	a3,16(sp)
#APP
# 295 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a4, 0(a3)
	
# 0 "" 2
#NO_APP
	sw	zero,-2016(a5)
	addi	a4,a3,4
	lw	a2,-2012(a5)
#APP
# 295 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a2, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-2012(a5)
	addi	a4,a3,8
	lw	a2,-2008(a5)
#APP
# 295 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a2, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-2008(a5)
	addi	a4,a3,12
	lw	a2,-2004(a5)
#APP
# 295 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a2, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-2004(a5)
	lw	a4,-2000(a5)
	ld	a2,8(sp)
#APP
# 295 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a4, 0(a2)
	
# 0 "" 2
#NO_APP
	sw	zero,-2000(a5)
	addi	a4,a2,4
	lw	a1,-1996(a5)
#APP
# 295 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a1, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-1996(a5)
	addi	a4,a2,8
	lw	a1,-1992(a5)
#APP
# 295 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a1, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-1992(a5)
	addi	a4,a2,12
	lw	a1,-1988(a5)
#APP
# 295 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a1, 0(a4)
	
# 0 "" 2
#NO_APP
	lw	a4,136(sp)
	addi	a3,a3,32
	sd	a3,16(sp)
	addi	a3,a2,32
	sw	zero,-1988(a5)
	sext.w	a4,a4
	sd	a3,8(sp)
	addi	s11,s11,32
	addi	s10,s10,32


	.insn i 0x1b, 0x7, x0, x0, 0
.L20:
ld	a3,112(sp)
	ld	a2,88(sp)
	lw	a4,132(sp)
	addw	a2,a2,a3
	sd	a2,88(sp)
	ld	a2,96(sp)
	sext.w	a4,a4
	addw	a2,a3,a2
	sd	a2,96(sp)
	ld	a2,80(sp)
	addw	a2,a2,a3
	sd	a2,80(sp)
	ld	a2,72(sp)
	addw	a3,a2,a3
	sd	a3,72(sp)

	.insn i 0x1b, 0x7, x0, x0, 0
.L27:
ld	s0,264(sp)
	ld	s1,256(sp)
	ld	s2,248(sp)
	ld	s3,240(sp)
	ld	s4,232(sp)
	ld	s5,224(sp)
	ld	s6,216(sp)
	ld	s7,208(sp)
	ld	s8,200(sp)
	ld	s9,192(sp)
	ld	s10,184(sp)
	ld	s11,176(sp)
	addi	sp,sp,272

	
	.insn i 0x1b, 0x7, x0, x0, 0
.L1:

	jr	ra
.L42:
	slli	a5,a7,2
	add	a0,a1,a5
	li	a3,0
#APP
# 67 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a3, 16(a0)
	
# 0 "" 2
# 68 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a3, 16(a0)
	
# 0 "" 2
#NO_APP
	addi	a5,a5,16
	add	a5,a1,a5
	li	a3,1
#APP
# 67 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a3, 16(a5)
	
# 0 "" 2
# 68 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a3, 16(a5)
	
# 0 "" 2
#NO_APP
	slli	s1,s0,2
	add	a3,a2,s1
	li	a5,2
#APP
# 75 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a5, 16(a3)
	
# 0 "" 2
# 76 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a5, 16(a3)
	
# 0 "" 2
#NO_APP
	addi	a5,s1,16
	add	a5,a2,a5
	li	a3,3
#APP
# 75 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a3, 16(a5)
	
# 0 "" 2
# 76 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a3, 16(a5)
	
# 0 "" 2
#NO_APP
	li	t1,1
	li	a0,0
	blt	a7,s3,.L5
	j	.L6
.L43:
	mv	a5,s4
	beq	s0,s8,.L7
	li	a5,0
	j	.L7
	.size	gemm_vec_simd, .-gemm_vec_simd
	.comm	start_barrier,32,8
	.ident	"GCC: (GNU) 8.3.0"
	.section	.note.GNU-stack,"",@progbits
