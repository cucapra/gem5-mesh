	.file	"atax_kernel.c"
	.option nopic
	.text
	.align	2
	.globl	atax_vec
	.type	atax_vec, @function
atax_vec:
#APP
# 15 "atax_kernel.c" 1
	.insn i 0x77, 0, x0, a0, 0x401
	
# 0 "" 2
# 18 "atax_kernel.c" 1
	addi	sp,sp,-48
	sd	s2,24(sp)
	sd	s0,40(sp)
	sd	s1,32(sp)
	sd	s3,16(sp)
	sd	s4,8(sp)
	lw	s2,48(sp)
	lw	t1,72(sp)

	.insn uj 0x6b, x0, .L2
	
# 0 "" 2
#NO_APP
	bge	a7,s2,.L3
	sext.w	s1,a6
	addiw	s0,s1,-1
	mulw	s3,t1,s1
	srliw	s0,s0,2
	addi	s0,s0,1
	slli	s0,s0,4
	slli	t3,a6,2
	li	t0,0
	mulw	s1,a7,s1
.L17:
#APP
# 27 "atax_kernel.c" 1
	.insn uj 0x6b, x0, .L4
	
# 0 "" 2
#NO_APP
	blez	a6,.L5
	slli	s4,s1,2
	add	s4,a1,s4
	li	t2,0
.L9:
	slliw	t4,t0,3
	sext.w	t5,t4
	addi	t4,t4,4
	blez	t1,.L6
	add	t6,a2,t2
	add	a3,t2,s4
	li	a4,0
.L7:
	slliw	a5,a4,12
	or	a0,a5,t5
#APP
# 33 "atax_kernel.c" 1
	.insn sb 0x23, 0x6, a0, 17(a3)
	
# 0 "" 2
#NO_APP
	or	a5,a5,t4
#APP
# 34 "atax_kernel.c" 1
	.insn sb 0x23, 0x6, a5, 17(t6)
	
# 0 "" 2
#NO_APP
	addiw	a4,a4,1
	add	a3,a3,t3
	bne	t1,a4,.L7
.L6:
	addi	t0,t0,1
	andi	t0,t0,63
#APP
# 38 "atax_kernel.c" 1
	.insn uj 0x6b, x0, .L8
	
# 0 "" 2
#NO_APP
	addi	t2,t2,16
	bne	s0,t2,.L9
.L5:
#APP
# 40 "atax_kernel.c" 1
	.insn uj 0x6b, x0, .L10
	
# 0 "" 2
#NO_APP
	blez	a6,.L11
	slli	t4,s1,2
	add	t4,a1,t4
	li	t5,0
.L15:
	slli	a0,t0,3
	blez	t1,.L12
	mv	a3,t4
	li	a5,0
.L13:
	slliw	a4,a5,12
	or	a4,a0,a4
#APP
# 44 "atax_kernel.c" 1
	.insn sb 0x23, 0x6, a4, 33(a3)
	
# 0 "" 2
#NO_APP
	addiw	a5,a5,1
	add	a3,a3,t3
	bne	t1,a5,.L13
.L12:
	addi	t0,t0,1
	andi	t0,t0,63
#APP
# 47 "atax_kernel.c" 1
	.insn uj 0x6b, x0, .L14
	
# 0 "" 2
#NO_APP
	addiw	t5,t5,8
	addi	t4,t4,32
	bgt	a6,t5,.L15
.L11:
#APP
# 49 "atax_kernel.c" 1
	.insn uj 0x6b, x0, .L16
	
# 0 "" 2
#NO_APP
	addw	a7,t1,a7
	addw	s1,s1,s3
	bgt	s2,a7,.L17
.L3:

ld	s0,40(sp)
	ld	s1,32(sp)
	ld	s2,24(sp)
	ld	s3,16(sp)
	ld	s4,8(sp)
	addi	sp,sp,48

#APP
# 52 "atax_kernel.c" 1
	.insn uj 0x6b, x0, .L18
	
# 0 "" 2
#NO_APP
.L19:
#APP
# 54 "atax_kernel.c" 1
	.insn uj 0x2b, x0, .L19
	
# 0 "" 2
# 106 "atax_kernel.c" 1
	fence
	
# 0 "" 2
#NO_APP
	j	.L1
.L2:
addi	sp,sp,-64
	sd	s0,48(sp)
	sd	s1,40(sp)
	sd	s3,24(sp)
	sd	ra,56(sp)
	sd	s2,32(sp)
	mv	s3,a3
	mv	s1,a4
	mv	s0,a7
	lw	a0,72(sp)
	li	a1,0
	call	getSpAddr
	lw	a2,4(sp)
	lw	a5,80(sp)
	sext.w	a2,a2
	addw	a5,s0,a5
	
	lw	t5,88(sp)
	slli	a5,a5,2
	add	t4,s1,a5
	slli	t5,t5,2
	li	a1,0
	li	t3,8
.L4:

	li	a2,0
.L8:
#APP
# 73 "atax_kernel.c" 1
	.insn i 0x1b, 0x3, x0, t3, 0
	
# 0 "" 2
#NO_APP
	slli	a5,a1,3
	slli	a4,a5,2
	add	a4,a0,a4
	lw	a7,0(a4)
	lw	s0,16(a4)
	addi	a5,a5,1
	slli	a5,a5,2
	mulw	a7,a7,s0
	add	t6,a0,a5
	addi	t1,a5,4
	add	t1,a0,t1
	lw	a6,0(t6)
	lw	t2,16(t6)
	addi	a5,a5,8
	add	a5,a0,a5
	lw	a4,0(t1)
	lw	t0,16(t1)
	lw	t6,16(a5)
	lw	t1,0(a5)
	mulw	a5,a6,t2
	addw	a6,a7,a2
	addi	a1,a1,1
	andi	a1,a1,63
	mulw	a2,a4,t0
	addw	a5,a5,a6
	mulw	a4,t1,t6
	addw	a2,a2,a5
	addw	a2,a4,a2
#APP
# 82 "atax_kernel.c" 1
	.insn i 0x1b, 0x2, x0, t3, 0
	
# 0 "" 2
#NO_APP
	lw	a5,8(sp)
	sext.w	a5,a5
.L10:
#APP
# 85 "atax_kernel.c" 1
	.insn sb 0x23, 0x5, a2, 0(t4)
	
# 0 "" 2
#NO_APP
	lw	a5,12(sp)
	sext.w	a5,a5
	mv	a5,s3
.L14:
#APP
# 90 "atax_kernel.c" 1
	.insn i 0x1b, 0x3, x0, t3, 0
	
# 0 "" 2
#NO_APP
	slli	a4,a1,3
	slli	a6,a4,2
	add	a6,a0,a6
	lw	s2,0(a6)
	lw	s1,0(a5)
	addi	a4,a4,1
	mulw	s2,s2,a2
	slli	a4,a4,2
	add	a4,a0,a4
	lw	s0,4(a5)
	lw	t2,8(a5)
	lw	t0,12(a5)
	lw	t6,16(a5)
	lw	t1,20(a5)
	lw	a7,24(a5)
	lw	a6,28(a5)
	addw	s1,s1,s2
	sw	s1,0(a5)
	lw	s1,0(a4)
	addi	a1,a1,1
	andi	a1,a1,63
	mulw	s1,s1,a2
	addw	s0,s0,s1
	sw	s0,4(a5)
	lw	s0,4(a4)
	mulw	s0,s0,a2
	addw	t2,t2,s0
	sw	t2,8(a5)
	lw	t2,8(a4)
	mulw	t2,t2,a2
	addw	t0,t0,t2
	sw	t0,12(a5)
	lw	t0,12(a4)
	mulw	t0,t0,a2
	addw	t6,t6,t0
	sw	t6,16(a5)
	lw	t6,16(a4)
	mulw	t6,t6,a2
	addw	t1,t1,t6
	sw	t1,20(a5)
	lw	t1,20(a4)
	mulw	t1,t1,a2
	addw	a7,a7,t1
	sw	a7,24(a5)
	lw	a4,24(a4)
	mulw	a4,a4,a2
	addw	a4,a6,a4
	sw	a4,28(a5)
#APP
# 98 "atax_kernel.c" 1
	.insn i 0x1b, 0x2, x0, t3, 0
	
# 0 "" 2
#NO_APP
	lw	a4,12(sp)
	addi	a5,a5,32
	sext.w	a4,a4
	

.L16:
lw	a5,4(sp)
	add	t4,t4,t5
	sext.w	a5,a5

	
.L18:
ld	ra,56(sp)
	ld	s0,48(sp)
	ld	s1,40(sp)
	ld	s2,32(sp)
	ld	s3,24(sp)
	addi	sp,sp,64
	
.L1:
	
	jr	ra
	.size	atax_vec, .-atax_vec
	.comm	start_barrier,32,8
	.ident	"GCC: (GNU) 8.3.0"
	.section	.note.GNU-stack,"",@progbits
