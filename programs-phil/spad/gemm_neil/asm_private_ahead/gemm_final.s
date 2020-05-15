	.file	"gemm_kernel.c"
	.option nopic
	.text
	.align	2
	.globl	gemm_vec_simd
	.type	gemm_vec_simd, @function
gemm_vec_simd:
#APP
# 34 "gemm_kernel.c" 1
	.insn i 0x77, 0, x0, a0, 0x401
	
# 0 "" 2
# 37 "gemm_kernel.c" 1
	addi	sp,sp,-144
	lw	a5,144(sp)
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
	sd	s8,72(sp)
	sd	s11,48(sp)
	sd	a5,0(sp)
	lw	s9,152(sp)
	lw	s10,160(sp)

	.insn uj 0x6b, x0, .L2
	
# 0 "" 2
#NO_APP
	subw	a5,s10,s9
	sraiw	s5,a5,31
	srliw	s5,s5,29
	addw	s5,s5,a5
	sraiw	s5,s5,3
	sext.w	a5,a6
	blez	a6,.L3
	li	a5,1
.L3:
	sd	a5,40(sp)
	blez	a6,.L4
	slli	a5,a7,2
	add	a3,a1,a5
	li	a0,0
#APP
# 84 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a0, 4(a3)
	
# 0 "" 2
#NO_APP
	li	a0,4096
#APP
# 84 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a0, 4(a3)
	
# 0 "" 2
#NO_APP
	addi	a3,a5,4
	add	a3,a1,a3
	li	t1,1
#APP
# 84 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t1, 4(a3)
	
# 0 "" 2
#NO_APP
	addiw	t1,a0,1
#APP
# 84 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t1, 4(a3)
	
# 0 "" 2
#NO_APP
	addi	a3,a5,8
	add	a3,a1,a3
	li	t1,2
#APP
# 84 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t1, 4(a3)
	
# 0 "" 2
#NO_APP
	addiw	t1,a0,2
#APP
# 84 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t1, 4(a3)
	
# 0 "" 2
#NO_APP
	addi	a3,a5,12
	add	a3,a1,a3
	li	t1,3
#APP
# 84 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t1, 4(a3)
	
# 0 "" 2
#NO_APP
	addiw	t1,a0,3
#APP
# 84 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t1, 4(a3)
	
# 0 "" 2
#NO_APP
	addi	t3,a5,16
	add	t3,a1,t3
	li	t1,8192
#APP
# 84 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t1, 4(t3)
	
# 0 "" 2
#NO_APP
	li	a3,12288
#APP
# 84 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a3, 4(t3)
	
# 0 "" 2
#NO_APP
	addi	t3,a5,20
	add	t3,a1,t3
	addiw	t4,t1,1
#APP
# 84 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t4, 4(t3)
	
# 0 "" 2
#NO_APP
	addiw	t4,a3,1
#APP
# 84 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t4, 4(t3)
	
# 0 "" 2
#NO_APP
	addi	t3,a5,24
	add	t3,a1,t3
	addiw	t4,t1,2
#APP
# 84 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t4, 4(t3)
	
# 0 "" 2
#NO_APP
	addiw	t4,a3,2
#APP
# 84 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t4, 4(t3)
	
# 0 "" 2
#NO_APP
	addi	a5,a5,28
	add	a5,a1,a5
	addiw	t3,t1,3
#APP
# 84 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t3, 4(a5)
	
# 0 "" 2
#NO_APP
	addiw	t3,a3,3
#APP
# 84 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t3, 4(a5)
	
# 0 "" 2
#NO_APP
	slli	a5,s9,2
	add	t3,a2,a5
	li	t4,4
#APP
# 96 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t4, 4(t3)
	
# 0 "" 2
#NO_APP
	addiw	t4,t1,4
#APP
# 96 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t4, 4(t3)
	
# 0 "" 2
#NO_APP
	addi	t3,a5,4
	add	t3,a2,t3
	li	t4,5
#APP
# 96 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t4, 4(t3)
	
# 0 "" 2
#NO_APP
	addiw	t4,t1,5
#APP
# 96 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t4, 4(t3)
	
# 0 "" 2
#NO_APP
	addi	t3,a5,8
	add	t3,a2,t3
	li	t4,6
#APP
# 96 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t4, 4(t3)
	
# 0 "" 2
#NO_APP
	addiw	t4,t1,6
#APP
# 96 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t4, 4(t3)
	
# 0 "" 2
#NO_APP
	addi	t3,a5,12
	add	t3,a2,t3
	li	t4,7
#APP
# 96 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t4, 4(t3)
	
# 0 "" 2
#NO_APP
	addiw	t1,t1,7
#APP
# 96 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t1, 4(t3)
	
# 0 "" 2
#NO_APP
	addi	t1,a5,16
	add	t1,a2,t1
	addiw	t3,a0,4
#APP
# 96 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t3, 4(t1)
	
# 0 "" 2
#NO_APP
	addiw	t3,a3,4
#APP
# 96 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t3, 4(t1)
	
# 0 "" 2
#NO_APP
	addi	t1,a5,20
	add	t1,a2,t1
	addiw	t3,a0,5
#APP
# 96 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t3, 4(t1)
	
# 0 "" 2
#NO_APP
	addiw	t3,a3,5
#APP
# 96 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t3, 4(t1)
	
# 0 "" 2
#NO_APP
	addi	t1,a5,24
	add	t1,a2,t1
	addiw	t3,a0,6
#APP
# 96 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t3, 4(t1)
	
# 0 "" 2
#NO_APP
	addiw	t3,a3,6
#APP
# 96 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t3, 4(t1)
	
# 0 "" 2
#NO_APP
	addi	a5,a5,28
	add	a5,a2,a5
	addiw	a0,a0,7
#APP
# 96 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a0, 4(a5)
	
# 0 "" 2
#NO_APP
	addiw	a3,a3,7
#APP
# 96 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a3, 4(a5)
	
# 0 "" 2
#NO_APP
	ld	a5,0(sp)
	li	s3,1
	li	t3,0
	bge	a7,a5,.L6
.L5:
	sext.w	a5,a6
	mulw	s5,s5,a5
	sd	a5,32(sp)
	sd	a7,24(sp)
	slli	s6,a4,2
	sd	a7,16(sp)
	li	t3,0
	li	t6,4096
	li	t5,8192
	li	t4,12288
.L23:
	sd	s9,8(sp)
	ble	s10,s9,.L26
.L22:
	ld	a5,16(sp)
	li	a3,0
	beq	a7,a5,.L43
.L7:
	ble	a6,a3,.L8
	mulw	a5,a4,a3
	ld	a0,32(sp)
	addw	s8,t3,a0
	subw	s8,s8,a3
	ld	a0,8(sp)
	ld	a3,24(sp)
	add	a3,a3,a5
	add	a5,a0,a5
	slli	a3,a3,2
	slli	a5,a5,2
	add	a3,a1,a3
	add	a5,a2,a5
.L21:
	remw	a0,t3,s5
	bnez	a0,.L9
#APP
# 118 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L10
	
# 0 "" 2
#NO_APP
.L9:
	remw	a0,t3,a6
	bnez	a0,.L11
#APP
# 119 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L12
	
# 0 "" 2
#NO_APP
.L11:
#APP
# 120 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L15
	
# 0 "" 2
#NO_APP
	slliw	s2,s3,3
	addiw	t3,t3,1
	mv	a0,s2
	mv	t1,t3
	addiw	t0,s2,4
#APP
# 152 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, s2, 4(a3)
	
# 0 "" 2
#NO_APP
	or	t2,s2,t6
#APP
# 152 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t2, 4(a3)
	
# 0 "" 2
#NO_APP
	addiw	s1,s2,1
	addi	t2,a3,4
#APP
# 152 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, s1, 4(t2)
	
# 0 "" 2
#NO_APP
	or	s0,s1,t6
#APP
# 152 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, s0, 4(t2)
	
# 0 "" 2
#NO_APP
	addiw	s0,s2,2
	addi	t2,a3,8
#APP
# 152 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, s0, 4(t2)
	
# 0 "" 2
#NO_APP
	or	s4,s0,t6
#APP
# 152 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, s4, 4(t2)
	
# 0 "" 2
#NO_APP
	addiw	t2,s2,3
	addi	s4,a3,12
#APP
# 152 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t2, 4(s4)
	
# 0 "" 2
#NO_APP
	or	s11,t2,t6
#APP
# 152 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, s11, 4(s4)
	
# 0 "" 2
#NO_APP
	addi	s4,a3,16
	or	s11,s2,t5
#APP
# 152 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, s11, 4(s4)
	
# 0 "" 2
#NO_APP
	or	s2,s2,t4
#APP
# 152 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, s2, 4(s4)
	
# 0 "" 2
#NO_APP
	addi	s2,a3,20
	or	s4,s1,t5
#APP
# 152 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, s4, 4(s2)
	
# 0 "" 2
#NO_APP
	or	s1,s1,t4
#APP
# 152 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, s1, 4(s2)
	
# 0 "" 2
#NO_APP
	addi	s1,a3,24
	or	s2,s0,t5
#APP
# 152 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, s2, 4(s1)
	
# 0 "" 2
#NO_APP
	or	s0,s0,t4
#APP
# 152 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, s0, 4(s1)
	
# 0 "" 2
#NO_APP
	addi	s0,a3,28
	or	s1,t2,t5
#APP
# 152 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, s1, 4(s0)
	
# 0 "" 2
#NO_APP
	or	t2,t2,t4
#APP
# 152 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t2, 4(s0)
	
# 0 "" 2
# 164 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t0, 4(a5)
	
# 0 "" 2
#NO_APP
	or	t2,t0,t5
#APP
# 164 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t2, 4(a5)
	
# 0 "" 2
#NO_APP
	addiw	s1,a0,5
	addi	t2,a5,4
#APP
# 164 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, s1, 4(t2)
	
# 0 "" 2
#NO_APP
	or	s0,s1,t5
#APP
# 164 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, s0, 4(t2)
	
# 0 "" 2
#NO_APP
	addiw	s0,a0,6
	addi	t2,a5,8
#APP
# 164 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, s0, 4(t2)
	
# 0 "" 2
#NO_APP
	or	s2,s0,t5
#APP
# 164 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, s2, 4(t2)
	
# 0 "" 2
#NO_APP
	addiw	t2,a0,7
	addi	s2,a5,12
#APP
# 164 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t2, 4(s2)
	
# 0 "" 2
#NO_APP
	or	a0,t2,t5
#APP
# 164 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a0, 4(s2)
	
# 0 "" 2
#NO_APP
	addi	a0,a5,16
	or	s2,t0,t6
#APP
# 164 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, s2, 4(a0)
	
# 0 "" 2
#NO_APP
	or	t0,t0,t4
#APP
# 164 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t0, 4(a0)
	
# 0 "" 2
#NO_APP
	addi	a0,a5,20
	or	t0,s1,t6
#APP
# 164 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t0, 4(a0)
	
# 0 "" 2
#NO_APP
	or	s1,s1,t4
#APP
# 164 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, s1, 4(a0)
	
# 0 "" 2
#NO_APP
	addi	a0,a5,24
	or	t0,s0,t6
#APP
# 164 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t0, 4(a0)
	
# 0 "" 2
#NO_APP
	or	s0,s0,t4
#APP
# 164 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, s0, 4(a0)
	
# 0 "" 2
#NO_APP
	addi	a0,a5,28
	or	t0,t2,t6
#APP
# 164 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t0, 4(a0)
	
# 0 "" 2
#NO_APP
	or	t2,t2,t4
#APP
# 164 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t2, 4(a0)
	
# 0 "" 2
#NO_APP
	remw	a0,t3,a6
	addi	s3,s3,1
	andi	s3,s3,63
	bnez	a0,.L16
#APP
# 172 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L18
	
# 0 "" 2
#NO_APP
.L16:
	remw	t1,t1,s5
	bnez	t1,.L19
#APP
# 173 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L20
	
# 0 "" 2
#NO_APP
.L19:
	add	a3,a3,s6
	add	a5,a5,s6
	bne	t3,s8,.L21
.L8:
	ld	a5,8(sp)
	addi	a5,a5,8
	sd	a5,8(sp)
	sext.w	a5,a5
	blt	a5,s10,.L22
.L26:
	ld	a3,24(sp)
	ld	a5,16(sp)
	addi	a3,a3,8
	sd	a3,24(sp)
	ld	a3,0(sp)
	addiw	a5,a5,8
	sd	a5,16(sp)
	bgt	a3,a5,.L23
	blez	a6,.L29
.L6:
#APP
# 182 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L15
	
# 0 "" 2
#NO_APP
	addiw	t3,t3,1
	remw	t3,t3,a6
	bnez	t3,.L29
#APP
# 184 "gemm_kernel.c" 1
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
# 188 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L27
	
# 0 "" 2
#NO_APP
.L28:
#APP
# 190 "gemm_kernel.c" 1
	.insn uj 0x2b, x1, .L28
	
# 0 "" 2
# 269 "gemm_kernel.c" 1
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
	.insn i 0x1b, 0x7, x0, x0, 0
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

	.insn i 0x1b, 0x7, x0, x0, 0
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
	.insn i 0x1b, 0x7, x0, x0, 0
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
# 250 "gemm_kernel.c" 1
	.insn i 0x1b, 0x2, x0, t6, 0
	
# 0 "" 2
#NO_APP
	.insn i 0x1b, 0x7, x0, x0, 0
.L18:
fmv.x.s	a4,fa7
#APP
# 258 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a4, 0(s0)
	
# 0 "" 2
#NO_APP
	sw	zero,-2048(a5)
	addi	a4,s0,4
	fmv.x.s	a2,fa6
#APP
# 258 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a2, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-2044(a5)
	addi	a4,s0,8
	fmv.x.s	a2,ft7
#APP
# 258 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a2, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-2040(a5)
	addi	a4,s0,12
	fmv.x.s	a2,ft6
#APP
# 258 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a2, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-2036(a5)
	fmv.x.s	a4,ft5
#APP
# 258 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a4, 0(a3)
	
# 0 "" 2
#NO_APP
	sw	zero,-2032(a5)
	addi	a4,a3,4
	fmv.x.s	a2,ft4
#APP
# 258 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a2, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-2028(a5)
	addi	a4,a3,8
	fmv.x.s	a2,ft3
#APP
# 258 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a2, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-2024(a5)
	addi	a4,a3,12
	fmv.x.s	a2,ft2
#APP
# 258 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a2, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-2020(a5)
	fmv.x.s	a4,ft1
#APP
# 258 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a4, 0(t2)
	
# 0 "" 2
#NO_APP
	sw	zero,-2016(a5)
	addi	a4,t2,4
	fmv.x.s	a2,ft0
#APP
# 258 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a2, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-2012(a5)
	addi	a4,t2,8
	fmv.x.s	a2,fa0
#APP
# 258 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a2, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-2008(a5)
	addi	a4,t2,12
	fmv.x.s	a2,fa1
#APP
# 258 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a2, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-2004(a5)
	fmv.x.s	a4,fa2
#APP
# 258 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a4, 0(t0)
	
# 0 "" 2
#NO_APP
	sw	zero,-2000(a5)
	addi	a4,t0,4
	fmv.x.s	a2,fa3
#APP
# 258 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a2, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-1996(a5)
	addi	a4,t0,8
	fmv.x.s	a2,fa4
#APP
# 258 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a2, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-1992(a5)
	addi	a4,t0,12
	fmv.x.s	a2,fa5
#APP
# 258 "gemm_kernel.c" 1
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
	.insn i 0x1b, 0x7, x0, x0, 0
.L20:
lw	a4,4(sp)
	addw	s1,s3,s1
	addw	s7,s7,s3
	sext.w	a4,a4
	addw	s6,s6,s3
	addw	s5,s5,s3
	.insn i 0x1b, 0x7, x0, x0, 0
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


	.insn i 0x1b, 0x7, x0, x0, 0
.L1:
	
	jr	ra
.L4:
	ld	a5,0(sp)
	li	s3,0
	blt	a7,a5,.L5
#APP
# 188 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L27
	
# 0 "" 2
#NO_APP
	j	.L28
.L43:
	lw	a5,8(sp)
	ld	a3,40(sp)
	beq	a5,s9,.L7
	li	a3,0
	j	.L7
	.size	gemm_vec_simd, .-gemm_vec_simd
	.comm	start_barrier,32,8
	.ident	"GCC: (GNU) 8.3.0"
	.section	.note.GNU-stack,"",@progbits
