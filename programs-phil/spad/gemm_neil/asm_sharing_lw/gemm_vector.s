	.file	"gemm_kernel.c"
	.option nopic
	.text
	.align	2
	.globl	gemm_vec_simd
	.type	gemm_vec_simd, @function
gemm_vec_simd:
	addi	sp,sp,-256
	sd	s7,192(sp)
	sd	s8,184(sp)
	sd	s0,248(sp)
	sd	s1,240(sp)
	sd	s2,232(sp)
	sd	s3,224(sp)
	sd	s4,216(sp)
	sd	s5,208(sp)
	sd	s6,200(sp)
	sd	s9,176(sp)
	sd	s10,168(sp)
	sd	s11,160(sp)
	sd	a3,104(sp)
	lw	s7,280(sp)
	lw	s8,288(sp)
#APP
# 32 "gemm_kernel.c" 1
	.insn i 0x77, 0, x0, a0, 0x401
	
# 0 "" 2
#NO_APP
	lui	a4,%hi(spm_base_ptr_arr)
	addi	a4,a4,%lo(spm_base_ptr_arr)
	ld	a2,48(a4)
	ld	a6,8(a4)
	ld	a0,16(a4)
	ld	a1,40(a4)
	lw	a4,296(sp)
	lw	a3,116(sp)
	addi	a2,a2,16
	sd	a2,152(sp)
	addi	a6,a6,16
	addi	a2,sp,160
	addi	a0,a0,16
	addi	a1,a1,16
	slli	a4,a4,3
	add	a4,a2,a4
	sd	a6,128(sp)
	sd	a0,136(sp)
	sd	a1,144(sp)
	slliw	a2,s8,2
	sext.w	a3,a3
	ld	s5,-32(a4)
	addw	a4,a2,a7
	beqz	a3,.L8
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
	sd	a4,72(sp)
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
	sd	a5,96(sp)
	addi	a5,sp,160
	add	a5,a5,t3
	sd	a5,8(sp)
	addi	a5,sp,160
	sraiw	a1,t2,31
	add	a5,a5,a7
	subw	a6,s2,a6
	srliw	a1,a1,30
	andi	s1,s0,3
	slli	a6,a6,3
	sd	a5,16(sp)
	addi	a5,sp,160
	addw	t2,t2,a1
	add	a5,a5,a6
	sd	a2,80(sp)
	subw	a0,s1,a0
	sraiw	a2,t4,31
	srliw	a2,a2,30
	sd	t0,64(sp)
	slli	a0,a0,3
	andi	t0,t2,3
	sd	a5,24(sp)
	addi	a5,sp,160
	addw	t4,t4,a2
	subw	a1,t0,a1
	add	a5,a5,a0
	sd	t6,56(sp)
	slli	a1,a1,3
	andi	t6,t4,3
	sd	a5,32(sp)
	addi	a5,sp,160
	subw	a2,t6,a2
	add	a5,a5,a1
	lw	t6,264(sp)
	slli	a2,a2,3
	sd	a5,40(sp)
	addi	a5,sp,160
	add	a5,a5,a2
	addw	t5,t5,t6
	sraiw	t1,t1,2
	sraiw	a3,a3,2
	sraiw	a4,a4,2
	sd	a5,48(sp)
	li	a5,4096
	sd	t5,88(sp)
	addi	t0,t1,2
	addi	t6,a3,2
	addi	t5,a4,2
	sraiw	s0,s0,2
	sraiw	t2,t2,2
	sraiw	t4,t4,2
	li	t1,0
	addi	s7,s7,2
	add	a5,s5,a5
.L7:
#APP
# 146 "gemm_kernel.c" 1
	nop
# 0 "" 2
#NO_APP
	lw	a4,120(sp)
	sext.w	a4,a4
	beqz	a4,.L3
	ld	a4,88(sp)
	ld	a3,72(sp)
	add	s11,a3,a4
	ld	a3,80(sp)
	slli	s11,s11,2
	add	s10,a3,a4
	ld	a3,64(sp)
	slli	s10,s10,2
	add	s9,a3,a4
	ld	a3,56(sp)
	slli	s9,s9,2
	add	a4,a3,a4
	ld	a3,104(sp)
	slli	a4,a4,2
	add	a4,a3,a4
	add	s11,a3,s11
	add	s10,a3,s10
	add	s9,a3,s9
	sd	a4,0(sp)
.L6:
#APP
# 150 "gemm_kernel.c" 1
	nop
# 0 "" 2
#NO_APP
	lw	a4,124(sp)
	sext.w	a4,a4
	beqz	a4,.L4
	ld	a4,8(sp)
	ld	t3,128(sp)
	ld	s6,-32(a4)
	ld	a4,16(sp)
	ld	s5,-32(a4)
	ld	a4,24(sp)
	ld	s4,-32(a4)
	ld	a4,32(sp)
	ld	s3,-32(a4)
	ld	a4,40(sp)
	ld	s2,-32(a4)
	ld	a4,48(sp)
	ld	s1,-32(a4)
.L5:
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
# 181 "gemm_kernel.c" 1
	.insn u 0x0b, x0, 0
	
# 0 "" 2
#NO_APP
	lw	a4,124(sp)
	sext.w	a4,a4
	bnez	a4,.L5
.L4:
	lw	a4,-2048(a5)
#APP
# 189 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a4, 0(s11)
	
# 0 "" 2
#NO_APP
	sw	zero,-2048(a5)
	addi	a4,s11,4
	lw	a3,-2044(a5)
#APP
# 189 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a3, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-2044(a5)
	addi	a4,s11,8
	lw	a3,-2040(a5)
#APP
# 189 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a3, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-2040(a5)
	addi	a4,s11,12
	lw	a3,-2036(a5)
#APP
# 189 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a3, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-2036(a5)
	lw	a4,-2032(a5)
#APP
# 189 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a4, 0(s10)
	
# 0 "" 2
#NO_APP
	sw	zero,-2032(a5)
	addi	a4,s10,4
	lw	a3,-2028(a5)
#APP
# 189 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a3, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-2028(a5)
	addi	a4,s10,8
	lw	a3,-2024(a5)
#APP
# 189 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a3, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-2024(a5)
	addi	a4,s10,12
	lw	a3,-2020(a5)
#APP
# 189 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a3, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-2020(a5)
	lw	a4,-2016(a5)
#APP
# 189 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a4, 0(s9)
	
# 0 "" 2
#NO_APP
	sw	zero,-2016(a5)
	addi	a4,s9,4
	lw	a3,-2012(a5)
#APP
# 189 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a3, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-2012(a5)
	addi	a4,s9,8
	lw	a3,-2008(a5)
#APP
# 189 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a3, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-2008(a5)
	addi	a4,s9,12
	lw	a3,-2004(a5)
#APP
# 189 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a3, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-2004(a5)
	lw	a4,-2000(a5)
	ld	a3,0(sp)
#APP
# 189 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a4, 0(a3)
	
# 0 "" 2
#NO_APP
	sw	zero,-2000(a5)
	addi	a4,a3,4
	lw	a2,-1996(a5)
#APP
# 189 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a2, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-1996(a5)
	addi	a4,a3,8
	lw	a2,-1992(a5)
#APP
# 189 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a2, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-1992(a5)
	addi	a4,a3,12
	lw	a2,-1988(a5)
#APP
# 189 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a2, 0(a4)
	
# 0 "" 2
#NO_APP
	lw	a4,120(sp)
	addi	a3,a3,32
	sw	zero,-1988(a5)
	sext.w	a4,a4
	sd	a3,0(sp)
	addi	s11,s11,32
	addi	s10,s10,32
	addi	s9,s9,32
	bnez	a4,.L6
.L3:
	ld	a3,96(sp)
	ld	a2,72(sp)
	lw	a4,116(sp)
	addw	a2,a2,a3
	sd	a2,72(sp)
	ld	a2,80(sp)
	sext.w	a4,a4
	addw	a2,a3,a2
	sd	a2,80(sp)
	ld	a2,64(sp)
	addw	a2,a2,a3
	sd	a2,64(sp)
	ld	a2,56(sp)
	addw	a3,a2,a3
	sd	a3,56(sp)
	bnez	a4,.L7
.L8:
#APP
# 200 "gemm_kernel.c" 1
	fence
	
# 0 "" 2
#NO_APP
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
	jr	ra
	.size	gemm_vec_simd, .-gemm_vec_simd
	.comm	start_barrier,32,8
	.ident	"GCC: (GNU) 8.3.0"
	.section	.note.GNU-stack,"",@progbits
