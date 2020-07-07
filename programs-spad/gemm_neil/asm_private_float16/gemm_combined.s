	.file	"gemm_kernel.c"
	.option nopic
	.text
	.align	2
	.globl	gemm_vec_simd
	.type	gemm_vec_simd, @function
gemm_vec_simd:
#APP
# 40 "gemm_kernel.c" 1
	.insn i 0x77, 0, x0, a0, 0x401
	
# 0 "" 2
# 43 "gemm_kernel.c" 1
	addi	sp,sp,-224
	sd	a5,56(sp)
	lw	a5,224(sp)
	sd	a4,48(sp)
	lw	a4,240(sp)
	sd	a5,72(sp)
	lw	a5,232(sp)
	sd	s0,216(sp)
	sd	s1,208(sp)
	sd	s2,200(sp)
	sd	s3,192(sp)
	sd	s4,184(sp)
	sd	s5,176(sp)
	sd	s6,168(sp)
	sd	s7,160(sp)
	sd	s8,152(sp)
	sd	s9,144(sp)
	sd	s10,136(sp)
	sd	s11,128(sp)
	sd	a1,32(sp)
	sd	a2,40(sp)
	sd	a7,24(sp)
	sd	a5,64(sp)
	sd	a4,88(sp)

	.insn uj 0x6b, x0, .L2
	
# 0 "" 2
#NO_APP
	subw	a5,a4,a5
	sraiw	t6,a5,31
	srliw	t6,t6,28
	addw	t6,t6,a5
	sraiw	t6,t6,4
	sext.w	a5,a6
	blez	a6,.L3
	li	a5,1
.L3:
	sd	a5,120(sp)
	blez	a6,.L4
	ld	t4,24(sp)
	ld	t1,32(sp)
	li	a3,0
	slli	a4,t4,2
	add	a5,t1,a4
#APP
# 96 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a3, 17(a5)
	
# 0 "" 2
# 97 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a3, 17(a5)
	
# 0 "" 2
#NO_APP
	ld	a3,64(sp)
	ld	t3,40(sp)
	slli	a7,a3,2
	add	a0,t3,a7
	li	a3,4
#APP
# 100 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a3, 17(a0)
	
# 0 "" 2
# 101 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a3, 17(a0)
	
# 0 "" 2
#NO_APP
	li	a3,4096
#APP
# 96 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a3, 17(a5)
	
# 0 "" 2
# 97 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a3, 17(a5)
	
# 0 "" 2
#NO_APP
	addi	a2,a7,16
	add	a2,t3,a2
	addiw	a3,a3,4
#APP
# 100 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a3, 17(a2)
	
# 0 "" 2
# 101 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a3, 17(a2)
	
# 0 "" 2
#NO_APP
	li	a1,8192
#APP
# 96 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a1, 17(a5)
	
# 0 "" 2
# 97 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a1, 17(a5)
	
# 0 "" 2
#NO_APP
	addi	a3,a7,32
	add	a3,t3,a3
	addiw	a1,a1,4
#APP
# 100 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a1, 17(a3)
	
# 0 "" 2
# 101 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a1, 17(a3)
	
# 0 "" 2
#NO_APP
	li	a1,12288
#APP
# 96 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a1, 17(a5)
	
# 0 "" 2
# 97 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a1, 17(a5)
	
# 0 "" 2
#NO_APP
	addi	a5,a7,48
	add	a5,t3,a5
	addiw	a1,a1,4
#APP
# 100 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a1, 17(a5)
	
# 0 "" 2
# 101 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a1, 17(a5)
	
# 0 "" 2
#NO_APP
	addi	a1,a4,16
	add	a1,t1,a1
	li	a7,16384
#APP
# 96 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a7, 17(a1)
	
# 0 "" 2
# 97 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a7, 17(a1)
	
# 0 "" 2
#NO_APP
	addiw	a7,a7,4
#APP
# 100 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a7, 17(a0)
	
# 0 "" 2
# 101 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a7, 17(a0)
	
# 0 "" 2
#NO_APP
	li	a7,20480
#APP
# 96 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a7, 17(a1)
	
# 0 "" 2
# 97 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a7, 17(a1)
	
# 0 "" 2
#NO_APP
	addiw	a7,a7,4
#APP
# 100 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a7, 17(a2)
	
# 0 "" 2
# 101 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a7, 17(a2)
	
# 0 "" 2
#NO_APP
	li	a7,24576
#APP
# 96 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a7, 17(a1)
	
# 0 "" 2
# 97 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a7, 17(a1)
	
# 0 "" 2
#NO_APP
	addiw	a7,a7,4
#APP
# 100 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a7, 17(a3)
	
# 0 "" 2
# 101 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a7, 17(a3)
	
# 0 "" 2
#NO_APP
	li	a7,28672
#APP
# 96 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a7, 17(a1)
	
# 0 "" 2
# 97 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a7, 17(a1)
	
# 0 "" 2
#NO_APP
	addiw	a7,a7,4
#APP
# 100 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a7, 17(a5)
	
# 0 "" 2
# 101 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a7, 17(a5)
	
# 0 "" 2
#NO_APP
	addi	a1,a4,32
	add	a1,t1,a1
	li	a7,32768
#APP
# 96 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a7, 17(a1)
	
# 0 "" 2
# 97 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a7, 17(a1)
	
# 0 "" 2
#NO_APP
	addiw	a7,a7,4
#APP
# 100 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a7, 17(a0)
	
# 0 "" 2
# 101 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a7, 17(a0)
	
# 0 "" 2
#NO_APP
	li	a7,36864
#APP
# 96 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a7, 17(a1)
	
# 0 "" 2
# 97 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a7, 17(a1)
	
# 0 "" 2
#NO_APP
	addiw	a7,a7,4
#APP
# 100 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a7, 17(a2)
	
# 0 "" 2
# 101 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a7, 17(a2)
	
# 0 "" 2
#NO_APP
	li	a7,40960
#APP
# 96 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a7, 17(a1)
	
# 0 "" 2
# 97 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a7, 17(a1)
	
# 0 "" 2
#NO_APP
	addiw	a7,a7,4
#APP
# 100 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a7, 17(a3)
	
# 0 "" 2
# 101 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a7, 17(a3)
	
# 0 "" 2
#NO_APP
	li	a7,45056
#APP
# 96 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a7, 17(a1)
	
# 0 "" 2
# 97 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a7, 17(a1)
	
# 0 "" 2
#NO_APP
	addiw	a7,a7,4
#APP
# 100 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a7, 17(a5)
	
# 0 "" 2
# 101 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a7, 17(a5)
	
# 0 "" 2
#NO_APP
	addi	a4,a4,48
	add	a4,t1,a4
	li	a1,49152
#APP
# 96 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a1, 17(a4)
	
# 0 "" 2
# 97 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a1, 17(a4)
	
# 0 "" 2
#NO_APP
	addiw	a1,a1,4
#APP
# 100 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a1, 17(a0)
	
# 0 "" 2
# 101 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a1, 17(a0)
	
# 0 "" 2
#NO_APP
	li	a1,53248
#APP
# 96 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a1, 17(a4)
	
# 0 "" 2
# 97 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a1, 17(a4)
	
# 0 "" 2
#NO_APP
	addiw	a1,a1,4
#APP
# 100 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a1, 17(a2)
	
# 0 "" 2
# 101 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a1, 17(a2)
	
# 0 "" 2
#NO_APP
	li	a2,57344
#APP
# 96 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a2, 17(a4)
	
# 0 "" 2
# 97 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a2, 17(a4)
	
# 0 "" 2
#NO_APP
	addiw	a2,a2,4
#APP
# 100 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a2, 17(a3)
	
# 0 "" 2
# 101 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a2, 17(a3)
	
# 0 "" 2
#NO_APP
	li	a3,61440
#APP
# 96 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a3, 17(a4)
	
# 0 "" 2
# 97 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a3, 17(a4)
	
# 0 "" 2
#NO_APP
	addiw	a3,a3,4
#APP
# 100 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a3, 17(a5)
	
# 0 "" 2
# 101 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a3, 17(a5)
	
# 0 "" 2
#NO_APP
	ld	a5,72(sp)
	li	t5,1
	bge	t4,a5,.L43
.L5:
	ld	a4,48(sp)
	sext.w	a5,a6
	sd	a5,112(sp)
	slli	a4,a4,2
	sd	a4,16(sp)
	ld	a4,56(sp)
	mulw	t6,t6,a5
	ld	a5,24(sp)
	slli	a4,a4,2
	sd	a4,8(sp)
	sd	a5,104(sp)
	sd	a5,96(sp)
	li	t4,0
	li	s7,4096
	li	s6,8192
	li	s5,12288
	li	s4,16384
	li	s3,20480
.L23:
	ld	a5,64(sp)
	ld	a4,88(sp)
	sd	a5,80(sp)
	ble	a4,a5,.L26
	li	s10,36864
	li	s9,40960
	li	s8,45056
.L22:
	ld	a4,24(sp)
	ld	a3,96(sp)
	li	a5,0
	beq	a4,a3,.L44
.L7:
	ble	a6,a5,.L8
	ld	a4,48(sp)
	li	s2,49152
	li	s1,53248
	mulw	a2,a4,a5
	ld	a4,112(sp)
	li	s0,57344
	li	t2,61440
	addw	t0,t4,a4
	ld	a4,56(sp)
	subw	t0,t0,a5
	mulw	a3,a4,a5
	ld	a5,104(sp)
	add	a2,a2,a5
	ld	a5,32(sp)
	slli	a2,a2,2
	add	a2,a5,a2
	ld	a5,80(sp)
	add	a3,a3,a5
	ld	a5,40(sp)
	slli	a3,a3,2
	add	a3,a5,a3
.L21:
	remw	a5,t4,t6
	bnez	a5,.L9
#APP
# 148 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L10
	
# 0 "" 2
#NO_APP
.L9:
	remw	a5,t4,a6
	bnez	a5,.L11
#APP
# 149 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L12
	
# 0 "" 2
#NO_APP
.L11:
#APP
# 150 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L15
	
# 0 "" 2
#NO_APP
	slliw	a4,t5,3
	addiw	t4,t4,1
	mv	t3,t4
	addiw	a5,a4,4
	addi	t1,a3,16
	addi	a7,a3,32
	addi	s11,a3,48
#APP
# 181 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a4, 17(a2)
	
# 0 "" 2
# 182 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a4, 17(a2)
	
# 0 "" 2
# 185 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a5, 17(a3)
	
# 0 "" 2
# 186 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a5, 17(a3)
	
# 0 "" 2
#NO_APP
	or	a1,a4,s7
	sext.w	a1,a1
#APP
# 181 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a1, 17(a2)
	
# 0 "" 2
# 182 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a1, 17(a2)
	
# 0 "" 2
#NO_APP
	or	a1,a5,s7
	sext.w	a1,a1
#APP
# 185 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a1, 17(t1)
	
# 0 "" 2
# 186 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a1, 17(t1)
	
# 0 "" 2
#NO_APP
	or	a1,a4,s6
	sext.w	a1,a1
#APP
# 181 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a1, 17(a2)
	
# 0 "" 2
# 182 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a1, 17(a2)
	
# 0 "" 2
#NO_APP
	or	a1,a5,s6
	sext.w	a1,a1
#APP
# 185 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a1, 17(a7)
	
# 0 "" 2
# 186 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a1, 17(a7)
	
# 0 "" 2
#NO_APP
	or	a1,a4,s5
	sext.w	a1,a1
#APP
# 181 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a1, 17(a2)
	
# 0 "" 2
# 182 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a1, 17(a2)
	
# 0 "" 2
#NO_APP
	or	a0,a5,s5
	sext.w	a0,a0
	mv	a1,s11
#APP
# 185 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a0, 17(s11)
	
# 0 "" 2
# 186 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a0, 17(s11)
	
# 0 "" 2
#NO_APP
	or	s11,a4,s4
	sext.w	s11,s11
	addi	a0,a2,16
#APP
# 181 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, s11, 17(a0)
	
# 0 "" 2
# 182 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, s11, 17(a0)
	
# 0 "" 2
#NO_APP
	or	s11,a5,s4
	sext.w	s11,s11
#APP
# 185 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, s11, 17(a3)
	
# 0 "" 2
# 186 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, s11, 17(a3)
	
# 0 "" 2
#NO_APP
	or	s11,a4,s3
	sext.w	s11,s11
#APP
# 181 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, s11, 17(a0)
	
# 0 "" 2
# 182 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, s11, 17(a0)
	
# 0 "" 2
#NO_APP
	or	s11,a5,s3
	sext.w	s11,s11
#APP
# 185 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, s11, 17(t1)
	
# 0 "" 2
# 186 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, s11, 17(t1)
	
# 0 "" 2
#NO_APP
	li	s11,24576
	or	s11,a4,s11
	sext.w	s11,s11
#APP
# 181 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, s11, 17(a0)
	
# 0 "" 2
# 182 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, s11, 17(a0)
	
# 0 "" 2
#NO_APP
	li	s11,24576
	or	s11,a5,s11
	sext.w	s11,s11
#APP
# 185 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, s11, 17(a7)
	
# 0 "" 2
# 186 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, s11, 17(a7)
	
# 0 "" 2
#NO_APP
	li	s11,28672
	or	s11,a4,s11
	sext.w	s11,s11
#APP
# 181 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, s11, 17(a0)
	
# 0 "" 2
# 182 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, s11, 17(a0)
	
# 0 "" 2
#NO_APP
	li	s11,28672
	or	a0,a5,s11
	sext.w	a0,a0
#APP
# 185 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a0, 17(a1)
	
# 0 "" 2
# 186 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a0, 17(a1)
	
# 0 "" 2
#NO_APP
	li	a0,32768
	or	s11,a4,a0
	sext.w	s11,s11
	addi	a0,a2,32
#APP
# 181 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, s11, 17(a0)
	
# 0 "" 2
# 182 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, s11, 17(a0)
	
# 0 "" 2
#NO_APP
	li	s11,32768
	or	s11,a5,s11
	sext.w	s11,s11
#APP
# 185 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, s11, 17(a3)
	
# 0 "" 2
# 186 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, s11, 17(a3)
	
# 0 "" 2
#NO_APP
	or	s11,a4,s10
	sext.w	s11,s11
#APP
# 181 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, s11, 17(a0)
	
# 0 "" 2
# 182 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, s11, 17(a0)
	
# 0 "" 2
#NO_APP
	or	s11,a5,s10
	sext.w	s11,s11
#APP
# 185 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, s11, 17(t1)
	
# 0 "" 2
# 186 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, s11, 17(t1)
	
# 0 "" 2
#NO_APP
	or	s11,a4,s9
	sext.w	s11,s11
#APP
# 181 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, s11, 17(a0)
	
# 0 "" 2
# 182 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, s11, 17(a0)
	
# 0 "" 2
#NO_APP
	or	s11,a5,s9
	sext.w	s11,s11
#APP
# 185 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, s11, 17(a7)
	
# 0 "" 2
# 186 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, s11, 17(a7)
	
# 0 "" 2
#NO_APP
	or	s11,a4,s8
	sext.w	s11,s11
#APP
# 181 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, s11, 17(a0)
	
# 0 "" 2
# 182 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, s11, 17(a0)
	
# 0 "" 2
#NO_APP
	or	a0,a5,s8
	sext.w	a0,a0
#APP
# 185 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a0, 17(a1)
	
# 0 "" 2
# 186 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a0, 17(a1)
	
# 0 "" 2
#NO_APP
	or	s11,a4,s2
	addi	a0,a2,48
#APP
# 181 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, s11, 17(a0)
	
# 0 "" 2
# 182 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, s11, 17(a0)
	
# 0 "" 2
#NO_APP
	or	s11,a5,s2
#APP
# 185 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, s11, 17(a3)
	
# 0 "" 2
# 186 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, s11, 17(a3)
	
# 0 "" 2
#NO_APP
	or	s11,a4,s1
#APP
# 181 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, s11, 17(a0)
	
# 0 "" 2
# 182 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, s11, 17(a0)
	
# 0 "" 2
#NO_APP
	or	s11,a5,s1
#APP
# 185 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, s11, 17(t1)
	
# 0 "" 2
# 186 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, s11, 17(t1)
	
# 0 "" 2
#NO_APP
	or	t1,a4,s0
#APP
# 181 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t1, 17(a0)
	
# 0 "" 2
# 182 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, t1, 17(a0)
	
# 0 "" 2
#NO_APP
	or	t1,a5,s0
#APP
# 185 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, t1, 17(a7)
	
# 0 "" 2
# 186 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, t1, 17(a7)
	
# 0 "" 2
#NO_APP
	or	a4,a4,t2
#APP
# 181 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a4, 17(a0)
	
# 0 "" 2
# 182 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a4, 17(a0)
	
# 0 "" 2
#NO_APP
	or	a5,a5,t2
#APP
# 185 "gemm_kernel.c" 1
	.insn sb 0x23, 0x6, a5, 17(a1)
	
# 0 "" 2
# 186 "gemm_kernel.c" 1
	.insn sb 0x23, 0x7, a5, 17(a1)
	
# 0 "" 2
#NO_APP
	remw	a5,t4,a6
	addi	t5,t5,1
	andi	t5,t5,63
	bnez	a5,.L16
#APP
# 219 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L18
	
# 0 "" 2
#NO_APP
.L16:
	remw	t3,t3,t6
	bnez	t3,.L19
#APP
# 220 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L20
	
# 0 "" 2
#NO_APP
.L19:
	ld	a5,16(sp)
	add	a2,a2,a5
	ld	a5,8(sp)
	add	a3,a3,a5
	bne	t4,t0,.L21
.L8:
	ld	a5,80(sp)
	ld	a4,88(sp)
	addi	a5,a5,16
	sd	a5,80(sp)
	sext.w	a5,a5
	blt	a5,a4,.L22
.L26:
	ld	a4,104(sp)
	ld	a5,96(sp)
	addi	a4,a4,16
	sd	a4,104(sp)
	ld	a4,72(sp)
	addiw	a5,a5,16
	sd	a5,96(sp)
	bgt	a4,a5,.L23
	blez	a6,.L29
.L6:
#APP
# 229 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L15
	
# 0 "" 2
#NO_APP
	addiw	t4,t4,1
	remw	t4,t4,a6
	bnez	t4,.L29
#APP
# 231 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L18
	
# 0 "" 2
#NO_APP
.L29:

ld	s0,216(sp)
	ld	s1,208(sp)
	ld	s2,200(sp)
	ld	s3,192(sp)
	ld	s4,184(sp)
	ld	s5,176(sp)
	ld	s6,168(sp)
	ld	s7,160(sp)
	ld	s8,152(sp)
	ld	s9,144(sp)
	ld	s10,136(sp)
	ld	s11,128(sp)
	addi	sp,sp,224

#APP
# 235 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L27
	
# 0 "" 2
#NO_APP
.L28:
#APP
# 237 "gemm_kernel.c" 1
	.insn uj 0x2b, x0, .L28
	
# 0 "" 2
# 343 "gemm_kernel.c" 1
	fence
	
# 0 "" 2
#NO_APP
	j	.L1
.L2:
addi	sp,sp,-96
	sd	s0,80(sp)
	sd	s3,56(sp)
	sd	s5,40(sp)
	sd	ra,88(sp)
	sd	s1,72(sp)
	sd	s2,64(sp)
	sd	s4,48(sp)
	sd	s6,32(sp)
	sd	s7,24(sp)
	mv	s3,a3
	mv	s5,a5
	mv	s0,a7
	lw	a0,144(sp)
	li	a1,0
	call	getSpAddr
	lw	a5,4(sp)
	lw	s1,128(sp)
	sext.w	a5,a5
	slliw	s1,s1,2
	addw	s1,s1,s0
	
	sext.w	a4,s5
	addiw	s7,s1,1
	addiw	s6,s1,2
	addiw	s5,s1,3
	mulw	s7,s7,a4
	lw	s2,120(sp)
	lw	a5,104(sp)
	slliw	s4,a4,4
	slliw	s2,s2,2
	addw	s2,s2,a5
	li	a5,4096
	li	t4,0
	li	t6,8
	add	a5,a0,a5
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
	add	s0,s3,s0
	add	a3,s3,a3
	add	t2,s3,t2
	add	t0,s3,t0

.L12:
#APP
# 353 "gemm_kernel.c" 1
	nop
# 0 "" 2
#NO_APP
.L15:
#APP
# 296 "gemm_kernel.c" 1
	.insn i 0x1b, 0x3, x0, t6, 0
	
# 0 "" 2
#NO_APP
	slli	a2,t4,3
	addi	a4,a2,4
	slli	a4,a4,2
	slli	a2,a2,2
	add	t5,a0,a4
	add	a2,a0,a2
	flw	fa3,0(t5)
	flw	fa5,0(a2)
	flw	fa4,-2048(a5)
	addi	a7,a4,4
	add	a7,a0,a7
	fmadd.s	fa5,fa5,fa3,fa4
	flw	fa3,-2044(a5)
	addi	a6,a4,8
	add	a6,a0,a6
	flw	fa1,-2040(a5)
	fsw	fa5,-2048(a5)
	flw	fa5,0(a7)
	flw	fa4,0(a2)
	addi	a1,a4,12
	add	a1,a0,a1
	fmadd.s	fa4,fa4,fa5,fa3
	flw	fa2,-2036(a5)
	addi	t3,a4,-12
	add	t3,a0,t3
	flw	fa3,-2032(a5)
	fsw	fa4,-2044(a5)
	flw	fa0,0(a6)
	flw	fa5,0(a2)
	flw	fa4,-2028(a5)
	flw	ft0,-2024(a5)
	fmadd.s	fa5,fa5,fa0,fa1
	flw	fa0,-2020(a5)
	addi	t1,a4,-8
	add	t1,a0,t1
	addi	a4,a4,-4
	fsw	fa5,-2040(a5)
	flw	fa5,0(a1)
	flw	fa1,0(a2)
	add	a4,a0,a4
	addi	t4,t4,1
	fmadd.s	fa1,fa1,fa5,fa2
	andi	t4,t4,63
	fsw	fa1,-2036(a5)
	flw	fa5,0(t5)
	flw	fa2,0(t3)
	fmadd.s	fa2,fa2,fa5,fa3
	fsw	fa2,-2032(a5)
	flw	fa5,0(a7)
	flw	fa3,0(t3)
	fmadd.s	fa3,fa3,fa5,fa4
	fsw	fa3,-2028(a5)
	flw	fa5,0(a6)
	flw	fa4,0(t3)
	fmadd.s	fa4,fa4,fa5,ft0
	fsw	fa4,-2024(a5)
	flw	fa4,0(a1)
	flw	fa5,0(t3)
	fmadd.s	fa5,fa5,fa4,fa0
	fsw	fa5,-2020(a5)
	flw	fa5,0(t1)
	flw	fa3,0(t5)
	flw	fa4,-2016(a5)
	flw	fa0,-2012(a5)
	flw	fa1,-2008(a5)
	fmadd.s	fa5,fa5,fa3,fa4
	flw	fa2,-2004(a5)
	flw	fa3,-2000(a5)
	flw	fa4,-1996(a5)
	flw	ft2,-1992(a5)
	fsw	fa5,-2016(a5)
	flw	fa5,0(a7)
	flw	ft0,0(t1)
	flw	ft1,-1988(a5)
	fmadd.s	ft0,ft0,fa5,fa0
	fsw	ft0,-2012(a5)
	flw	fa5,0(a6)
	flw	fa0,0(t1)
	fmadd.s	fa0,fa0,fa5,fa1
	fsw	fa0,-2008(a5)
	flw	fa5,0(a1)
	flw	fa1,0(t1)
	fmadd.s	fa1,fa1,fa5,fa2
	fsw	fa1,-2004(a5)
	flw	fa5,0(t5)
	flw	fa2,0(a4)
	fmadd.s	fa2,fa2,fa5,fa3
	fsw	fa2,-2000(a5)
	flw	fa5,0(a7)
	flw	fa3,0(a4)
	fmadd.s	fa3,fa3,fa5,fa4
	fsw	fa3,-1996(a5)
	flw	fa5,0(a6)
	flw	fa4,0(a4)
	fmadd.s	fa4,fa4,fa5,ft2
	fsw	fa4,-1992(a5)
	flw	fa5,0(a4)
	flw	fa4,0(a1)
	fmadd.s	fa5,fa5,fa4,ft1
	fsw	fa5,-1988(a5)
#APP
# 324 "gemm_kernel.c" 1
	.insn i 0x1b, 0x2, x0, t6, 0
	
# 0 "" 2
#NO_APP


.L18:
lw	a4,-2048(a5)
#APP
# 332 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a4, 0(s0)
	
# 0 "" 2
#NO_APP
	sw	zero,-2048(a5)
	addi	a4,s0,4
	lw	a2,-2044(a5)
#APP
# 332 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a2, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-2044(a5)
	addi	a4,s0,8
	lw	a2,-2040(a5)
#APP
# 332 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a2, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-2040(a5)
	addi	a4,s0,12
	lw	a2,-2036(a5)
#APP
# 332 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a2, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-2036(a5)
	lw	a4,-2032(a5)
#APP
# 332 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a4, 0(a3)
	
# 0 "" 2
#NO_APP
	sw	zero,-2032(a5)
	addi	a4,a3,4
	lw	a2,-2028(a5)
#APP
# 332 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a2, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-2028(a5)
	addi	a4,a3,8
	lw	a2,-2024(a5)
#APP
# 332 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a2, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-2024(a5)
	addi	a4,a3,12
	lw	a2,-2020(a5)
#APP
# 332 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a2, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-2020(a5)
	lw	a4,-2016(a5)
#APP
# 332 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a4, 0(t2)
	
# 0 "" 2
#NO_APP
	sw	zero,-2016(a5)
	addi	a4,t2,4
	lw	a2,-2012(a5)
#APP
# 332 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a2, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-2012(a5)
	addi	a4,t2,8
	lw	a2,-2008(a5)
#APP
# 332 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a2, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-2008(a5)
	addi	a4,t2,12
	lw	a2,-2004(a5)
#APP
# 332 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a2, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-2004(a5)
	lw	a4,-2000(a5)
#APP
# 332 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a4, 0(t0)
	
# 0 "" 2
#NO_APP
	sw	zero,-2000(a5)
	addi	a4,t0,4
	lw	a2,-1996(a5)
#APP
# 332 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a2, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-1996(a5)
	addi	a4,t0,8
	lw	a2,-1992(a5)
#APP
# 332 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a2, 0(a4)
	
# 0 "" 2
#NO_APP
	sw	zero,-1992(a5)
	addi	a4,t0,12
	lw	a2,-1988(a5)
#APP
# 332 "gemm_kernel.c" 1
	.insn sb 0x23, 0x5, a2, 0(a4)
	
# 0 "" 2
#NO_APP
	lw	a4,8(sp)
	sw	zero,-1988(a5)
	addi	s0,s0,64
	sext.w	a4,a4
	addi	a3,a3,64
	addi	t2,t2,64
	addi	t0,t0,64


.L20:

lw	a4,4(sp)
	addw	s1,s4,s1
	addw	s7,s7,s4
	sext.w	a4,a4
	addw	s6,s6,s4
	addw	s5,s5,s4

.L27:

ld	ra,88(sp)
	ld	s0,80(sp)
	ld	s1,72(sp)
	ld	s2,64(sp)
	ld	s3,56(sp)
	ld	s4,48(sp)
	ld	s5,40(sp)
	ld	s6,32(sp)
	ld	s7,24(sp)
	addi	sp,sp,96
	
.L1:
	
	jr	ra
.L4:
	ld	a5,24(sp)
	ld	a4,72(sp)
	li	t5,0
	blt	a5,a4,.L5
#APP
# 235 "gemm_kernel.c" 1
	.insn uj 0x6b, x0, .L27
	
# 0 "" 2
#NO_APP
	j	.L28
.L44:
	lw	a4,80(sp)
	ld	a3,64(sp)
	ld	a5,120(sp)
	beq	a4,a3,.L7
	li	a5,0
	j	.L7
.L43:
	li	t4,0
	j	.L6
	.size	gemm_vec_simd, .-gemm_vec_simd
	.comm	start_barrier,32,8
	.ident	"GCC: (GNU) 8.3.0"
	.section	.note.GNU-stack,"",@progbits
