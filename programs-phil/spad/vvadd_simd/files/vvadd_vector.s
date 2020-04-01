	.file	"vvadd.c"
	.option nopic
	.text
	.align	2
	.type	vvadd_execute.constprop.6, @function
vvadd_execute.constprop.6:
	addi	sp,sp,-48
	sd	s0,32(sp)
	sd	s1,24(sp)
	mv	s0,a0
	mv	s1,a1
	mv	a0,a2
	li	a1,0
	sd	s2,16(sp)
	sd	ra,40(sp)
	mv	s2,a3
	call	getSpAddr
	lw	a5,8(sp)
	add	a3,s1,s2
	slli	a3,a3,2
	sext.w	a5,a5
	add	a3,s0,a3
	beqz	a5,.L6
	li	a5,0
.L3:
	slli	a4,a5,2
	add	a2,a0,a4
#APP
# 197 "vvadd.c" 1
	.insn s 0x03, 0x7, a2, 0(a2)
	
# 0 "" 2
#NO_APP
	addi	a4,a4,4
	add	a4,a0,a4
#APP
# 198 "vvadd.c" 1
	.insn s 0x03, 0x7, a4, 0(a4)
	
# 0 "" 2
# 202 "vvadd.c" 1
	.insn u 0x0b, x0, 0
	
# 0 "" 2
#NO_APP
	addw	a4,a2,a4
#APP
# 206 "vvadd.c" 1
	.insn sb 0x23, 0x5, a4, 0(a3)
	
# 0 "" 2
#NO_APP
	lw	a4,8(sp)
	addi	a5,a5,2
	addi	a3,a3,16
	sext.w	a4,a4
	andi	a5,a5,511
	bnez	a4,.L3
	j	.L2
.L6:
	li	a5,0
.L2:
	lw	a4,12(sp)
	sext.w	a4,a4
	beqz	a4,.L4
.L5:
	slli	a4,a5,2
	add	a2,a0,a4
#APP
# 246 "vvadd.c" 1
	.insn s 0x03, 0x7, a2, 0(a2)
	
# 0 "" 2
#NO_APP
	addi	a4,a4,4
	add	a4,a0,a4
#APP
# 247 "vvadd.c" 1
	.insn s 0x03, 0x7, a4, 0(a4)
	
# 0 "" 2
# 251 "vvadd.c" 1
	.insn u 0x0b, x0, 0
	
# 0 "" 2
#NO_APP
	addw	a4,a2,a4
#APP
# 255 "vvadd.c" 1
	.insn sb 0x23, 0x5, a4, 0(a3)
	
# 0 "" 2
#NO_APP
	lw	a4,12(sp)
	addi	a5,a5,2
	addi	a3,a3,16
	sext.w	a4,a4
	andi	a5,a5,511
	bnez	a4,.L5
.L4:
#APP
# 270 "vvadd.c" 1
	fence
	
# 0 "" 2
#NO_APP
	ld	ra,40(sp)
	ld	s0,32(sp)
	ld	s1,24(sp)
	ld	s2,16(sp)
	addi	sp,sp,48
	jr	ra
	.size	vvadd_execute.constprop.6, .-vvadd_execute.constprop.6
	.align	2
	.type	getSIMDMask.constprop.8, @function
getSIMDMask.constprop.8:
	lui	a7,%hi(.LANCHOR0)
	addi	a7,a7,%lo(.LANCHOR0)
	ld	t1,0(a7)
	ld	t5,16(a7)
	ld	t3,32(a7)
	ld	t6,8(a7)
	ld	t4,24(a7)
	ld	a7,40(a7)
	addi	sp,sp,-48
	sd	t1,0(sp)
	sd	t5,16(sp)
	sd	t3,32(sp)
	sd	a7,40(sp)
	sd	t6,8(sp)
	sd	t4,24(sp)
	addw	t1,t1,a0
	li	t3,0
	li	t5,0
	li	a7,0
	bgt	a2,t1,.L15
	addiw	t4,a2,2
	bgt	t4,t1,.L48
.L15:
	lw	t1,12(sp)
	addw	t1,t1,a0
	bgt	a2,t1,.L16
	addiw	t4,a2,2
	bgt	t4,t1,.L49
.L16:
	lw	t1,24(sp)
	addw	t1,t1,a0
	bgt	a2,t1,.L17
	addiw	t4,a2,2
	bgt	t4,t1,.L50
.L17:
	lw	t1,36(sp)
	addw	a0,t1,a0
	blt	a0,a2,.L18
	addiw	t1,a2,2
	bgt	t1,a0,.L51
.L18:
	bnez	a6,.L19
	subw	t1,t3,a2
	subw	t4,t5,a3
	mv	a0,t1
	mv	a1,t4
	beq	t1,a4,.L52
.L20:
	subw	a1,a5,a1
	blez	a1,.L22
	li	a4,1
	li	a7,4
	beq	a5,a4,.L19
	li	a7,20
.L19:
	slliw	a6,a6,17
	or	a7,a7,a6
	slliw	a3,a3,25
	slliw	a0,a2,18
	or	a7,a7,a3
	or	a7,a7,a0
	sext.w	a0,a7
	li	a7,8192
	addi	a7,a7,256
	or	a0,a0,a7
	addi	sp,sp,48
	jr	ra
.L51:
	lw	t1,40(sp)
	addw	a1,t1,a1
	blt	a1,a3,.L18
	addiw	t1,a3,2
	ble	t1,a1,.L18
	lw	a7,44(sp)
	mv	t5,a1
	mv	t3,a0
	j	.L18
.L50:
	lw	t4,28(sp)
	addw	t4,t4,a1
	bgt	a3,t4,.L17
	addiw	t6,a3,2
	ble	t6,t4,.L17
	lw	a7,32(sp)
	mv	t5,t4
	mv	t3,t1
	j	.L17
.L49:
	lw	t4,16(sp)
	addw	t4,t4,a1
	bgt	a3,t4,.L16
	addiw	t6,a3,2
	ble	t6,t4,.L16
	lw	a7,20(sp)
	mv	t5,t4
	mv	t3,t1
	j	.L16
.L48:
	lw	t4,4(sp)
	addw	t4,t4,a1
	bgt	a3,t4,.L15
	addiw	t6,a3,2
	ble	t6,t4,.L15
	lw	a7,8(sp)
	mv	t5,t4
	mv	t3,t1
	j	.L15
.L52:
	bne	t4,a5,.L20
	li	a5,64
	beq	a7,a5,.L33
	li	a5,16
	beq	a7,a5,.L34
	li	a5,32
	li	a1,1
	beq	a7,a5,.L27
	li	a5,8
	li	a1,3
	bne	a7,a5,.L53
.L27:
	bge	a2,t3,.L47
.L42:
	li	a7,96
	li	a5,48
	j	.L26
.L22:
	beqz	a1,.L24
	li	a7,2
	beqz	a5,.L19
	li	a7,66
	j	.L19
.L24:
	subw	a0,a4,a0
	blez	a0,.L25
	bnez	a4,.L39
.L47:
	li	a7,72
	li	a5,24
.L26:
	or	a7,a7,a1
	blt	a3,t5,.L19
	or	a7,a5,a1
	j	.L19
.L39:
	li	a7,67
	li	a5,19
	j	.L26
.L34:
	li	a1,4
	j	.L27
.L53:
	li	a1,-1
	j	.L27
.L25:
	beqz	a0,.L27
	li	a5,1
	beq	a4,a5,.L42
	mv	a1,a4
	li	a7,65
	li	a5,17
	j	.L26
.L33:
	li	a1,2
	j	.L27
	.size	getSIMDMask.constprop.8, .-getSIMDMask.constprop.8
	.align	2
	.type	kernel.constprop.5, @function
kernel.constprop.5:
	mulw	a6,a6,a5
	addi	sp,sp,-48
	sd	s0,32(sp)
	sd	s1,24(sp)
	sd	ra,40(sp)
	sd	s2,16(sp)
	sd	s3,8(sp)
	sd	s4,0(sp)
	or	a5,a4,a5
	mv	s1,a2
	addw	s0,a6,a4
	bnez	a5,.L55
	li	a5,1
#APP
# 102 "../../common/bind_defs.h" 1
	csrw 0x7C1, a5;
# 0 "" 2
#NO_APP
.L56:
	li	a5,6
	li	s3,3
	beq	s0,a5,.L59
	li	s3,0
.L59:
	li	a5,8
	beq	s0,a5,.L74
	li	a5,9
	bne	s0,a5,.L89
	li	s3,1
.L60:
	andi	a5,s0,-6
	li	a4,8
	beq	a5,a4,.L61
	li	s2,0
.L69:
	li	a5,10
	beq	s0,a5,.L78
	li	a5,11
	bne	s0,a5,.L90
	li	s3,1
.L64:
	andi	a5,s0,-6
	li	a4,10
	beq	a5,a4,.L65
.L66:
	li	a5,4096
	addiw	a5,a5,-1792
#APP
# 736 "vvadd.c" 1
	csrw 0x402, a5
	
# 0 "" 2
#NO_APP
	lui	a0,%hi(start_barrier)
	addi	a0,a0,%lo(start_barrier)
	call	pthread_barrier_wait
	li	a5,3
	beq	s0,a5,.L54
	mv	a0,s0
	call	getSpTop
	addi	a0,a0,-32
#APP
# 766 "vvadd.c" 1
	ld t0, 0(sp)
	sd t0, 0(a0)
	ld t0, 8(sp)
	sd t0, 8(a0)
	ld t0, 16(sp)
	sd t0, 16(a0)
	ld t0, 24(sp)
	sd t0, 24(a0)
	addi s4, sp, 0
	addi sp, a0, 0
	
# 0 "" 2
#NO_APP
	mv	a3,s3
	mv	a2,s0
	mv	a1,s2
	mv	a0,s1
	call	vvadd_execute.constprop.6
#APP
# 799 "vvadd.c" 1
	addi sp, s4, 0
	
# 0 "" 2
#NO_APP
.L54:
	ld	ra,40(sp)
	ld	s0,32(sp)
	ld	s1,24(sp)
	ld	s2,16(sp)
	ld	s3,8(sp)
	ld	s4,0(sp)
	addi	sp,sp,48
	jr	ra
.L55:
	li	a5,2
	li	s3,1
	beq	s0,a5,.L59
	li	a5,5
	li	s3,2
	beq	s0,a5,.L59
	j	.L56
.L89:
	li	a5,12
	bne	s0,a5,.L91
	li	s3,2
	j	.L61
.L91:
	li	a5,13
	bne	s0,a5,.L92
	li	s3,3
	j	.L61
.L92:
	li	a5,4
	bne	s0,a5,.L60
.L61:
	li	a4,3
	divw	s2,a3,a4
	sraiw	a2,s2,31
	xor	a5,s2,a2
	subw	a5,a5,a2
	andi	a2,a5,63
	mv	a4,s2
	beqz	a2,.L69
	li	a1,-2
	subw	s2,a2,a5
	blt	a3,a1,.L69
	addiw	a4,a4,64
	subw	s2,a4,a2
	j	.L69
.L90:
	li	a5,14
	bne	s0,a5,.L93
	li	s3,2
	j	.L65
.L93:
	li	a5,15
	bne	s0,a5,.L94
	li	s3,3
	j	.L65
.L94:
	li	a5,7
	bne	s0,a5,.L64
.L65:
	slliw	a4,a3,1
	li	a5,3
	divw	s2,a4,a5
	sraiw	a2,s2,31
	xor	a3,s2,a2
	subw	a3,a3,a2
	andi	a2,a3,63
	mv	a5,s2
	beqz	a2,.L66
	li	a1,-2
	subw	s2,a2,a3
	blt	a4,a1,.L66
	addiw	a5,a5,64
	subw	s2,a5,a2
	j	.L66
.L78:
	li	s3,0
	j	.L64
.L74:
	li	s3,0
	j	.L60
	.size	kernel.constprop.5, .-kernel.constprop.5
	.align	2
	.globl	roundUp
	.type	roundUp, @function
roundUp:
	beqz	a1,.L96
	sraiw	a4,a0,31
	xor	a5,a0,a4
	subw	a5,a5,a4
	remw	a3,a5,a1
	beqz	a3,.L96
	bltz	a0,.L104
	addw	a0,a1,a0
	subw	a0,a0,a3
.L96:
	ret
.L104:
	subw	a0,a3,a5
	ret
	.size	roundUp, .-roundUp
	.align	2
	.globl	vvadd_execute
	.type	vvadd_execute, @function
vvadd_execute:
	addi	sp,sp,-64
	mv	a0,a5
	li	a1,0
	sd	s0,48(sp)
	sd	s1,40(sp)
	sd	s2,32(sp)
	sd	s3,24(sp)
	mv	s2,a2
	mv	s1,a3
	mv	s3,a6
	sd	ra,56(sp)
	mv	s0,a7
	call	getSpAddr
	lw	a5,8(sp)
	add	s1,s1,s3
	slli	s1,s1,2
	sext.w	a5,a5
	add	a2,s2,s1
	beqz	a5,.L110
	slli	a1,s0,2
	li	a5,0
.L107:
	slli	a4,a5,2
	add	a3,a0,a4
#APP
# 197 "vvadd.c" 1
	.insn s 0x03, 0x7, a3, 0(a3)
	
# 0 "" 2
#NO_APP
	addi	a4,a4,4
	add	a4,a0,a4
#APP
# 198 "vvadd.c" 1
	.insn s 0x03, 0x7, a4, 0(a4)
	
# 0 "" 2
# 202 "vvadd.c" 1
	.insn u 0x0b, x0, 0
	
# 0 "" 2
#NO_APP
	addw	a4,a3,a4
#APP
# 206 "vvadd.c" 1
	.insn sb 0x23, 0x5, a4, 0(a2)
	
# 0 "" 2
#NO_APP
	lw	a4,8(sp)
	addi	a5,a5,2
	add	a2,a2,a1
	sext.w	a4,a4
	andi	a5,a5,511
	bnez	a4,.L107
	j	.L106
.L110:
	li	a5,0
.L106:
	lw	a4,12(sp)
	sext.w	a4,a4
	beqz	a4,.L108
	slli	a7,s0,2
.L109:
	slli	a4,a5,2
	add	a3,a0,a4
#APP
# 246 "vvadd.c" 1
	.insn s 0x03, 0x7, a3, 0(a3)
	
# 0 "" 2
#NO_APP
	addi	a4,a4,4
	add	a4,a0,a4
#APP
# 247 "vvadd.c" 1
	.insn s 0x03, 0x7, a4, 0(a4)
	
# 0 "" 2
# 251 "vvadd.c" 1
	.insn u 0x0b, x0, 0
	
# 0 "" 2
#NO_APP
	addw	a4,a3,a4
#APP
# 255 "vvadd.c" 1
	.insn sb 0x23, 0x5, a4, 0(a2)
	
# 0 "" 2
#NO_APP
	lw	a4,12(sp)
	addi	a5,a5,2
	add	a2,a2,a7
	sext.w	a4,a4
	andi	a5,a5,511
	bnez	a4,.L109
.L108:
#APP
# 270 "vvadd.c" 1
	fence
	
# 0 "" 2
#NO_APP
	ld	ra,56(sp)
	ld	s0,48(sp)
	ld	s1,40(sp)
	ld	s2,32(sp)
	ld	s3,24(sp)
	addi	sp,sp,64
	jr	ra
	.size	vvadd_execute, .-vvadd_execute
	.align	2
	.globl	kernel
	.type	kernel, @function
kernel:
	mulw	a6,a6,a5
	addi	sp,sp,-48
	sd	s0,32(sp)
	sd	s1,24(sp)
	sd	ra,40(sp)
	sd	s2,16(sp)
	sd	s3,8(sp)
	sd	s4,0(sp)
	or	a5,a4,a5
	mv	s1,a2
	addw	s0,a6,a4
	bnez	a5,.L118
	li	a5,1
#APP
# 102 "../../common/bind_defs.h" 1
	csrw 0x7C1, a5;
# 0 "" 2
#NO_APP
.L119:
	li	a5,6
	li	s3,3
	beq	s0,a5,.L122
	li	s3,0
.L122:
	li	a5,8
	beq	s0,a5,.L137
	li	a5,9
	bne	s0,a5,.L152
	li	s3,1
.L123:
	andi	a5,s0,-6
	li	a4,8
	beq	a5,a4,.L124
	li	s2,0
.L132:
	li	a5,10
	beq	s0,a5,.L141
	li	a5,11
	bne	s0,a5,.L153
	li	s3,1
.L127:
	andi	a5,s0,-6
	li	a4,10
	beq	a5,a4,.L128
.L129:
	li	a5,4096
	addiw	a5,a5,-1792
#APP
# 736 "vvadd.c" 1
	csrw 0x402, a5
	
# 0 "" 2
#NO_APP
	lui	a0,%hi(start_barrier)
	addi	a0,a0,%lo(start_barrier)
	call	pthread_barrier_wait
	li	a5,3
	beq	s0,a5,.L117
	mv	a0,s0
	call	getSpTop
	addi	a0,a0,-32
#APP
# 766 "vvadd.c" 1
	ld t0, 0(sp)
	sd t0, 0(a0)
	ld t0, 8(sp)
	sd t0, 8(a0)
	ld t0, 16(sp)
	sd t0, 16(a0)
	ld t0, 24(sp)
	sd t0, 24(a0)
	addi s4, sp, 0
	addi sp, a0, 0
	
# 0 "" 2
#NO_APP
	mv	a3,s3
	mv	a2,s0
	mv	a1,s2
	mv	a0,s1
	call	vvadd_execute.constprop.6
#APP
# 799 "vvadd.c" 1
	addi sp, s4, 0
	
# 0 "" 2
#NO_APP
.L117:
	ld	ra,40(sp)
	ld	s0,32(sp)
	ld	s1,24(sp)
	ld	s2,16(sp)
	ld	s3,8(sp)
	ld	s4,0(sp)
	addi	sp,sp,48
	jr	ra
.L118:
	li	a5,2
	li	s3,1
	beq	s0,a5,.L122
	li	a5,5
	li	s3,2
	beq	s0,a5,.L122
	j	.L119
.L152:
	li	a5,12
	bne	s0,a5,.L154
	li	s3,2
	j	.L124
.L154:
	li	a5,13
	bne	s0,a5,.L155
	li	s3,3
	j	.L124
.L155:
	li	a5,4
	bne	s0,a5,.L123
.L124:
	li	a4,3
	divw	s2,a3,a4
	sraiw	a2,s2,31
	xor	a5,s2,a2
	subw	a5,a5,a2
	andi	a2,a5,63
	mv	a4,s2
	beqz	a2,.L132
	li	a1,-2
	subw	s2,a2,a5
	blt	a3,a1,.L132
	addiw	a4,a4,64
	subw	s2,a4,a2
	j	.L132
.L153:
	li	a5,14
	bne	s0,a5,.L156
	li	s3,2
	j	.L128
.L156:
	li	a5,15
	bne	s0,a5,.L157
	li	s3,3
	j	.L128
.L157:
	li	a5,7
	bne	s0,a5,.L127
.L128:
	slliw	a4,a3,1
	li	a5,3
	divw	s2,a4,a5
	sraiw	a2,s2,31
	xor	a3,s2,a2
	subw	a3,a3,a2
	andi	a2,a3,63
	mv	a5,s2
	beqz	a2,.L129
	li	a1,-2
	subw	s2,a2,a3
	blt	a4,a1,.L129
	addiw	a5,a5,64
	subw	s2,a5,a2
	j	.L129
.L141:
	li	s3,0
	j	.L127
.L137:
	li	s3,0
	j	.L123
	.size	kernel, .-kernel
	.align	2
	.globl	construct_args
	.type	construct_args, @function
construct_args:
	addi	sp,sp,-80
	sd	s7,8(sp)
	mv	s7,a0
	li	a0,48
	sd	s0,64(sp)
	sd	s1,56(sp)
	sd	s2,48(sp)
	sd	s3,40(sp)
	sd	s4,32(sp)
	sd	s5,24(sp)
	sd	s6,16(sp)
	sd	ra,72(sp)
	mv	s6,a1
	mv	s5,a2
	mv	s4,a3
	mv	s3,a4
	mv	s2,a5
	mv	s1,a6
	mv	s0,a7
	call	malloc
	sw	s0,40(a0)
	ld	ra,72(sp)
	ld	s0,64(sp)
	sd	s7,0(a0)
	sd	s6,8(a0)
	sd	s5,16(a0)
	sw	s4,24(a0)
	sw	s3,28(a0)
	sw	s2,32(a0)
	sw	s1,36(a0)
	ld	s2,48(sp)
	ld	s1,56(sp)
	ld	s3,40(sp)
	ld	s4,32(sp)
	ld	s5,24(sp)
	ld	s6,16(sp)
	ld	s7,8(sp)
	addi	sp,sp,80
	jr	ra
	.size	construct_args, .-construct_args
	.align	2
	.globl	pthread_kernel
	.type	pthread_kernel, @function
pthread_kernel:
	addi	sp,sp,-32
	sd	s1,8(sp)
	lui	s1,%hi(start_barrier)
	sd	s0,16(sp)
	mv	s0,a0
	addi	a0,s1,%lo(start_barrier)
	sd	ra,24(sp)
	call	pthread_barrier_wait
	lw	a5,32(s0)
	lw	a6,36(s0)
	lw	a4,28(s0)
	lw	a3,24(s0)
	ld	a2,16(s0)
	ld	a1,8(s0)
	ld	a0,0(s0)
	call	kernel.constprop.5
	addi	a0,s1,%lo(start_barrier)
	call	pthread_barrier_wait
	lw	a5,28(s0)
	bnez	a5,.L161
	lw	a5,32(s0)
	bnez	a5,.L161
	li	a5,10
#APP
# 113 "../../common/bind_defs.h" 1
	csrw 0x7C1, a5;
# 0 "" 2
#NO_APP
.L161:
	ld	ra,24(sp)
	ld	s0,16(sp)
	ld	s1,8(sp)
	li	a0,0
	addi	sp,sp,32
	jr	ra
	.size	pthread_kernel, .-pthread_kernel
	.comm	start_barrier,32,8
	.section	.rodata
	.align	3
	.set	.LANCHOR0,. + 0
.LC0:
	.word	1
	.word	0
	.word	8
	.word	0
	.word	1
	.word	16
	.word	-1
	.word	0
	.word	32
	.word	0
	.word	-1
	.word	64
	.ident	"GCC: (GNU) 8.3.0"
	.section	.note.GNU-stack,"",@progbits
