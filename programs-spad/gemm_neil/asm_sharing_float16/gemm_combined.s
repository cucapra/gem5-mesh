	.file	"gemm_kernel.c"
	.option nopic
	.text
	.align	2
	.globl	gemm_vec_simd
	.type	gemm_vec_simd, @function
gemm_vec_simd:
#APP
# 55 "gemm_kernel.c" 1
	.insn i 0x77, 0, x0, a0, 0x401
	
# 0 "" 2
# 58 "gemm_kernel.c" 1
	addi	sp,sp,-80
	sd	s1,64(sp)
	sd	s2,56(sp)
	sd	s3,48(sp)
	sd	s0,72(sp)
	sd	s4,40(sp)
	sd	s5,32(sp)
	sd	s6,24(sp)
	sd	s7,16(sp)
	sd	s8,8(sp)
	sd	s9,0(sp)
	lw	s2,80(sp)
	lw	s1,88(sp)
	lw	s3,96(sp)

	.insn uj 0x6b, x0, .L2
	
# 0 "" 2
#NO_APP
	subw	a3,s3,s1
	sraiw	t6,a3,31
	srliw	t6,t6,28
	addw	t6,t6,a3
	sraiw	t6,t6,4
	sext.w	s4,a6
	blez	a6,.L3
	li	s4,1
.L3:
	blez	a6,.L4
	slli	a3,a7,2
	add	a3,a1,a3
	li	a0,0
#APP
# 92 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a0, 64(a3)
	
# 0 "" 2
# 93 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a0, 64(a3)
	
# 0 "" 2
#NO_APP
	slli	a3,s1,2
	add	a3,a2,a3
	li	a0,1
#APP
# 100 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a0, 64(a3)
	
# 0 "" 2
# 101 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a0, 64(a3)
	
# 0 "" 2
#NO_APP
	li	a3,1
	li	t1,0
	bge	a7,s2,.L6
.L5:
	sext.w	s6,a6
	mulw	t6,t6,s6
	mv	s8,a7
	slli	s0,a4,2
	slli	t2,a5,2
	mv	s7,a7
	li	t1,0
.L23:
	mv	s5,s1
	ble	s3,s1,.L26
.L22:
	li	a0,0
	beq	s7,a7,.L43
.L7:
	ble	a6,a0,.L8
	mulw	t4,a4,a0
	addw	t0,t1,s6
	subw	t0,t0,a0
	mulw	t3,a5,a0
	add	t4,t4,s8
	slli	t4,t4,2
	add	t4,a1,t4
	add	t3,t3,s5
	slli	t3,t3,2
	add	t3,a2,t3
.L21:
	remw	a0,t1,t6
	bnez	a0,.L9
#APP
# 163 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L10
	
# 0 "" 2
#NO_APP
.L9:
	remw	a0,t1,a6
	bnez	a0,.L11
#APP
# 164 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L12
	
# 0 "" 2
#NO_APP
.L11:
#APP
# 165 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L15
	
# 0 "" 2
#NO_APP
	addiw	t1,t1,1
	slliw	t5,a3,1
	mv	a0,t1
	addiw	s9,t5,1
#APP
# 178 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t5, 64(t4)
	
# 0 "" 2
# 179 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, t5, 64(t4)
	
# 0 "" 2
# 186 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, s9, 64(t3)
	
# 0 "" 2
# 187 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, s9, 64(t3)
	
# 0 "" 2
#NO_APP
	remw	t5,t1,a6
	bnez	t5,.L16
#APP
# 234 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L18
	
# 0 "" 2
#NO_APP
.L16:
	remw	a0,a0,t6
	bnez	a0,.L19
#APP
# 235 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L20
	
# 0 "" 2
#NO_APP
.L19:
	addi	a3,a3,1
	andi	a3,a3,15
	add	t4,t4,s0
	add	t3,t3,t2
	bne	t0,t1,.L21
.L8:
	addi	s5,s5,16
	sext.w	a0,s5
	blt	a0,s3,.L22
.L26:
	addiw	s7,s7,16
	addi	s8,s8,16
	bgt	s2,s7,.L23
	blez	a6,.L29
.L6:
#APP
# 244 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L15
	
# 0 "" 2
#NO_APP
	addiw	t1,t1,1
	remw	t1,t1,a6
	bnez	t1,.L29
#APP
# 246 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L18
	
# 0 "" 2
#NO_APP
.L29:

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
# 250 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L27
	
# 0 "" 2
#NO_APP
.L28:
#APP
# 252 "gemm_kernel.c" 1
	.insn uj 0x2b, x0, .L28
	
# 0 "" 2
# 358 "gemm_kernel.c" 1
	fence
	
# 0 "" 2
#NO_APP
	j	.L1
.L2:

addi	sp,sp,-352
	sd	s0,336(sp)
	sd	s1,328(sp)
	sd	s2,320(sp)
	sd	ra,344(sp)
	sd	s3,312(sp)
	sd	s4,304(sp)
	sd	s5,296(sp)
	sd	s6,288(sp)
	sd	s7,280(sp)
	sd	s8,272(sp)
	sd	s9,264(sp)
	sd	s10,256(sp)
	sd	s11,248(sp)
	sd	a3,88(sp)
	mv	s2,a5
	mv	s1,a7
	ld	s0,408(sp)

	lw	a0,0(s0)
	li	a1,0
	call	getSpAddr
	sd	a0,112(sp)
	lw	a0,4(s0)
	li	a1,0
	call	getSpAddr
	sd	a0,120(sp)
	lw	a0,8(s0)
	li	a1,0
	call	getSpAddr
	sd	a0,128(sp)
	lw	a0,12(s0)
	li	a1,0
	call	getSpAddr
	sd	a0,136(sp)
	lw	a0,16(s0)
	li	a1,0
	call	getSpAddr
	sd	a0,144(sp)
	lw	a0,20(s0)
	li	a1,0
	call	getSpAddr
	sd	a0,152(sp)
	lw	a0,24(s0)
	li	a1,0
	call	getSpAddr
	sd	a0,160(sp)
	lw	a0,28(s0)
	li	a1,0
	call	getSpAddr
	sd	a0,168(sp)
	lw	a0,32(s0)
	li	a1,0
	call	getSpAddr
	sd	a0,176(sp)
	lw	a0,36(s0)
	li	a1,0
	call	getSpAddr
	sd	a0,184(sp)
	lw	a0,40(s0)
	li	a1,0
	call	getSpAddr
	sd	a0,192(sp)
	lw	a0,44(s0)
	li	a1,0
	call	getSpAddr
	sd	a0,200(sp)
	lw	a0,48(s0)
	li	a1,0
	call	getSpAddr
	sd	a0,208(sp)
	lw	a0,52(s0)
	li	a1,0
	call	getSpAddr
	sd	a0,216(sp)
	lw	a0,56(s0)
	li	a1,0
	call	getSpAddr
	sd	a0,224(sp)
	lw	a0,60(s0)
	li	a1,0
	call	getSpAddr
	sd	a0,232(sp)
	lw	a0,400(sp)
	li	a1,0
	call	getSpAddr
	lw	a3,100(sp)
	lw	t5,384(sp)
	sext.w	a3,a3
	slliw	t5,t5,2
	addw	a4,t5,s1
	
	sext.w	a2,s2
	mulw	a5,a4,a2
	addiw	a1,a4,1
	addiw	t3,a4,2
	addiw	s4,a4,3
	addiw	s1,t5,1
	addiw	s0,t5,2
	addiw	t2,t5,3
	sraiw	a7,t5,31
	sraiw	a6,s1,31
	sraiw	t4,t2,31
	mulw	a1,a1,a2
	sd	a5,64(sp)
	lw	a5,376(sp)
	srliw	a7,a7,28
	srliw	a6,a6,28
	slliw	a5,a5,2
	addiw	t1,a5,3
	sraiw	t6,t1,31
	srliw	t6,t6,28
	addw	t1,t1,t6
	mulw	t3,t3,a2
	addiw	a3,a5,1
	addiw	a4,a5,2
	andi	s7,t1,15
	sraiw	s3,a5,31
	sraiw	s2,a3,31
	sraiw	t0,a4,31
	subw	s7,s7,t6
	sd	a1,56(sp)
	lw	t6,360(sp)
	mulw	s4,s4,a2
	sraiw	a1,s0,31
	srliw	s2,s2,28
	srliw	t0,t0,28
	srliw	s3,s3,28
	srliw	a1,a1,28
	srliw	t4,t4,28
	addw	a3,a3,s2
	addw	a4,a4,t0
	addw	t5,t5,a7
	addw	s1,s1,a6
	addw	s0,s0,a1
	addw	t2,t2,t4
	sd	t3,48(sp)
	addw	t3,a5,s3
	andi	s6,t3,15
	andi	s8,t5,15
	andi	s9,s1,15
	andi	s10,s0,15
	andi	s11,t2,15
	addw	a5,a5,t6
	andi	s5,a3,15
	sd	s4,40(sp)
	slliw	a2,a2,4
	andi	s4,a4,15
	subw	s3,s6,s3
	subw	s5,s5,s2
	subw	s4,s4,t0
	subw	a7,s8,a7
	subw	a6,s9,a6
	subw	a1,s10,a1
	subw	t4,s11,t4
	sd	a5,72(sp)
	sext.w	a5,a2
	sraiw	t1,t1,4
	slli	s3,s3,3
	slli	s5,s5,3
	slli	s4,s4,3
	sraiw	t3,t3,4
	sraiw	a3,a3,4
	sraiw	a4,a4,4
	slli	s7,s7,3
	slli	a7,a7,3
	slli	a6,a6,3
	slli	a1,a1,3
	slli	t4,t4,3
	sd	a5,80(sp)
	addi	a5,sp,240
	addi	s11,t1,1
	add	s10,a5,s3
	add	s9,a5,s5
	add	s8,a5,s4
	addi	s2,t3,1
	addi	t0,a3,1
	addi	t6,a4,1
	sraiw	t5,t5,4
	sraiw	s1,s1,4
	sraiw	s0,s0,4
	sraiw	t2,t2,4
	li	t1,0
	add	s7,a5,s7
	add	s6,a5,a7
	add	s5,a5,a6
	add	s4,a5,a1
	add	s3,a5,t4
.L10:
ld	a2,72(sp)
	ld	a5,64(sp)
	ld	a1,40(sp)
	ld	a3,48(sp)
	add	a4,a5,a2
	ld	a5,56(sp)
	add	a3,a3,a2
	slli	a4,a4,2
	add	a5,a5,a2
	add	a2,a1,a2
	ld	a1,88(sp)
	slli	a5,a5,2
	slli	a3,a3,2
	add	a4,a1,a4
	add	a5,a1,a5
	slli	a2,a2,2
	sd	a4,32(sp)
	sd	a5,8(sp)
	add	a4,a1,a3
	add	a5,a1,a2
	sd	a4,24(sp)
	sd	a5,16(sp)
	li	t4,2
.L12:
#APP
# 368 "gemm_kernel.c" 1
	nop
# 0 "" 2
#NO_APP
.L15:
#APP
# 311 "gemm_kernel.c" 1
	.insn i 0x1b, 0x3, x0, t4, 0
	
# 0 "" 2
#NO_APP
	slli	a5,t1,1
	ld	a4,-128(s10)
	ld	a3,-128(s6)
	add	a1,s2,a5
	add	a7,a5,t5
	slli	a1,a1,2
	slli	a7,a7,2
	add	a1,a4,a1
	add	a7,a3,a7
	flw	fa3,0(a7)
	flw	fa5,0(a1)
	flw	fa4,128(a0)
	ld	a4,-128(s9)
	add	a2,t0,a5
	fmadd.s	fa5,fa5,fa3,fa4
	slli	a2,a2,2
	add	a2,a4,a2
	flw	fa4,132(a0)
	ld	a4,-128(s8)
	fsw	fa5,128(a0)
	flw	fa3,0(a7)
	flw	fa5,0(a2)
	add	a3,t6,a5
	slli	a3,a3,2
	fmadd.s	fa5,fa5,fa3,fa4
	add	a3,a4,a3
	flw	fa4,136(a0)
	ld	a6,-128(s7)
	add	a4,s11,a5
	fsw	fa5,132(a0)
	flw	fa3,0(a7)
	flw	fa5,0(a3)
	slli	a4,a4,2
	add	a4,a6,a4
	fmadd.s	fa5,fa5,fa3,fa4
	flw	fa4,140(a0)
	ld	t3,-128(s5)
	add	a6,a5,s1
	slli	a6,a6,2
	fsw	fa5,136(a0)
	flw	fa3,0(a7)
	flw	fa5,0(a4)
	add	a6,t3,a6
	flw	fa2,144(a0)
	fmadd.s	fa5,fa5,fa3,fa4
	flw	fa3,148(a0)
	add	t3,a5,s0
	slli	t3,t3,2
	add	a5,a5,t2
	fsw	fa5,140(a0)
	flw	fa5,0(a6)
	flw	fa4,0(a1)
	slli	a5,a5,2
	addi	t1,t1,1
	fmadd.s	fa4,fa4,fa5,fa2
	andi	t1,t1,15
	fsw	fa4,144(a0)
	flw	fa4,0(a6)
	flw	fa5,0(a2)
	fmadd.s	fa5,fa5,fa4,fa3
	fsw	fa5,148(a0)
	flw	fa3,0(a6)
	flw	fa4,0(a3)
	flw	fa5,152(a0)
	flw	fa0,156(a0)
	ld	a7,-128(s4)
	fmadd.s	fa4,fa4,fa3,fa5
	flw	fa1,160(a0)
	add	a7,a7,t3
	flw	fa2,164(a0)
	flw	fa3,168(a0)
	fsw	fa4,152(a0)
	flw	ft0,0(a6)
	flw	fa5,0(a4)
	flw	fa4,172(a0)
	ld	a6,-128(s3)
	fmadd.s	fa5,fa5,ft0,fa0
	flw	ft1,176(a0)
	add	a5,a6,a5
	flw	ft0,180(a0)
	fsw	fa5,156(a0)
	flw	fa5,0(a7)
	flw	fa0,0(a1)
	fmadd.s	fa0,fa0,fa5,fa1
	fsw	fa0,160(a0)
	flw	fa5,0(a7)
	flw	fa1,0(a2)
	fmadd.s	fa1,fa1,fa5,fa2
	fsw	fa1,164(a0)
	flw	fa5,0(a7)
	flw	fa2,0(a3)
	fmadd.s	fa2,fa2,fa5,fa3
	fsw	fa2,168(a0)
	flw	fa5,0(a7)
	flw	fa3,0(a4)
	fmadd.s	fa3,fa3,fa5,fa4
	fsw	fa3,172(a0)
	flw	fa5,0(a1)
	flw	fa4,0(a5)
	fmadd.s	fa4,fa4,fa5,ft1
	fsw	fa4,176(a0)
	flw	fa5,0(a5)
	flw	fa4,0(a2)
	fmadd.s	fa5,fa5,fa4,ft0
	fsw	fa5,180(a0)
	flw	fa4,0(a5)
	flw	fa2,0(a3)
	flw	fa5,184(a0)
	flw	fa3,188(a0)
	fmadd.s	fa4,fa4,fa2,fa5
	fsw	fa4,184(a0)
	flw	fa5,0(a5)
	flw	fa4,0(a4)
	fmadd.s	fa5,fa5,fa4,fa3
	fsw	fa5,188(a0)
#APP
# 339 "gemm_kernel.c" 1
	.insn i 0x1b, 0x2, x0, t4, 0
	
# 0 "" 2
#NO_APP
.L18:
lw	a5,128(a0)
	ld	a4,32(sp)
#APP
# 347 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a5, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,128(a0)
	addi	a5,a4,4
	lw	a3,132(a0)
#APP
# 347 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a3, 0(a5)
	
# 0 "" 2
#NO_APP
	sw	zero,132(a0)
	addi	a5,a4,8
	lw	a3,136(a0)
#APP
# 347 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a3, 0(a5)
	
# 0 "" 2
#NO_APP
	sw	zero,136(a0)
	addi	a5,a4,12
	lw	a3,140(a0)
#APP
# 347 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a3, 0(a5)
	
# 0 "" 2
#NO_APP
	sw	zero,140(a0)
	lw	a5,144(a0)
	ld	a3,8(sp)
#APP
# 347 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a5, 0(a3)
	
# 0 "" 2
#NO_APP
	sw	zero,144(a0)
	addi	a5,a3,4
	lw	a2,148(a0)
#APP
# 347 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a2, 0(a5)
	
# 0 "" 2
#NO_APP
	sw	zero,148(a0)
	addi	a5,a3,8
	lw	a2,152(a0)
#APP
# 347 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a2, 0(a5)
	
# 0 "" 2
#NO_APP
	sw	zero,152(a0)
	addi	a5,a3,12
	lw	a2,156(a0)
#APP
# 347 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a2, 0(a5)
	
# 0 "" 2
#NO_APP
	sw	zero,156(a0)
	lw	a5,160(a0)
	ld	a2,24(sp)
#APP
# 347 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a5, 0(a2)
	
# 0 "" 2
#NO_APP
	sw	zero,160(a0)
	addi	a5,a2,4
	lw	a1,164(a0)
#APP
# 347 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a1, 0(a5)
	
# 0 "" 2
#NO_APP
	sw	zero,164(a0)
	addi	a5,a2,8
	lw	a1,168(a0)
#APP
# 347 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a1, 0(a5)
	
# 0 "" 2
#NO_APP
	sw	zero,168(a0)
	addi	a5,a2,12
	lw	a1,172(a0)
#APP
# 347 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a1, 0(a5)
	
# 0 "" 2
#NO_APP
	sw	zero,172(a0)
	lw	a5,176(a0)
	ld	a1,16(sp)
#APP
# 347 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a5, 0(a1)
	
# 0 "" 2
#NO_APP
	sw	zero,176(a0)
	addi	a5,a1,4
	lw	a6,180(a0)
#APP
# 347 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a6, 0(a5)
	
# 0 "" 2
#NO_APP
	sw	zero,180(a0)
	addi	a5,a1,8
	lw	a6,184(a0)
#APP
# 347 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a6, 0(a5)
	
# 0 "" 2
#NO_APP
	sw	zero,184(a0)
	addi	a5,a1,12
	lw	a6,188(a0)
#APP
# 347 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a6, 0(a5)
	
# 0 "" 2
#NO_APP
	addi	a4,a4,64
	lw	a5,104(sp)
	sd	a4,32(sp)
	addi	a4,a3,64
	sd	a4,8(sp)
	addi	a4,a2,64
	sd	a4,24(sp)
	addi	a4,a1,64
	sw	zero,188(a0)
	sext.w	a5,a5
	sd	a4,16(sp)
.L20:
ld	a4,80(sp)
	ld	a3,64(sp)
	lw	a5,100(sp)
	addw	a3,a3,a4
	sd	a3,64(sp)
	ld	a3,56(sp)
	sext.w	a5,a5
	addw	a3,a4,a3
	sd	a3,56(sp)
	ld	a3,48(sp)
	addw	a3,a3,a4
	sd	a3,48(sp)
	ld	a3,40(sp)
	addw	a4,a3,a4
	sd	a4,40(sp)
.L27:
	ld	ra,344(sp)
	ld	s0,336(sp)
	ld	s1,328(sp)
	ld	s2,320(sp)
	ld	s3,312(sp)
	ld	s4,304(sp)
	ld	s5,296(sp)
	ld	s6,288(sp)
	ld	s7,280(sp)
	ld	s8,272(sp)
	ld	s9,264(sp)
	ld	s10,256(sp)
	ld	s11,248(sp)
	addi	sp,sp,352
	
.L1:
	
	jr	ra
.L4:
	li	a3,0
	blt	a7,s2,.L5
#APP
# 250 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L27
	
# 0 "" 2
#NO_APP
	j	.L28
.L43:
	sext.w	t3,s5
	mv	a0,s4
	beq	t3,s1,.L7
	li	a0,0
	j	.L7
	.size	gemm_vec_simd, .-gemm_vec_simd
	.comm	start_barrier,32,8
	.ident	"GCC: (GNU) 8.3.0"
	.section	.note.GNU-stack,"",@progbits
