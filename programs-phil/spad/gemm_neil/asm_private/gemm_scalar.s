	.file	"gemm_kernel.c"
	.option nopic
	.text
	.align	2
	.globl	gemm_vec_simd
	.type	gemm_vec_simd, @function
gemm_vec_simd:
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
#APP
# 26 "gemm_kernel.c" 1
	.insn i 0x77, 0, x0, a0, 0x401
	
# 0 "" 2
# 45 "gemm_kernel.c" 1
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
#APP
# 197 "gemm_kernel.c" 1
	nop
# 0 "" 2
#NO_APP
.L6:
#APP
# 199 "gemm_kernel.c" 1
	nop
# 0 "" 2
#NO_APP
.L8:
#APP
# 201 "gemm_kernel.c" 1
	nop
# 0 "" 2
#NO_APP
.L10:
#APP
# 203 "gemm_kernel.c" 1
	nop
# 0 "" 2
#NO_APP
.L12:
#APP
# 205 "gemm_kernel.c" 1
	nop
# 0 "" 2
#NO_APP
.L14:
#APP
# 207 "gemm_kernel.c" 1
	nop
# 0 "" 2
#NO_APP
.L4:
#APP
# 209 "gemm_kernel.c" 1
	nop
# 0 "" 2
#NO_APP
.L1:
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
	jr	ra
	.size	gemm_vec_simd, .-gemm_vec_simd
	.comm	start_barrier,32,8
	.ident	"GCC: (GNU) 8.3.0"
	.section	.note.GNU-stack,"",@progbits
