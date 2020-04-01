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
	mv	a0,a4
	li	a1,0
	sd	s2,16(sp)
	sd	s3,8(sp)
	sd	s4,0(sp)
	sd	ra,40(sp)
	mv	s2,a2
	mv	s3,a3
	mv	s4,a5
	call	getSpAddr
#APP
# 124 "vvadd.c" 1
	.insn i 0x77, 0, x0, s4, 0x401
	
# 0 "" 2
#NO_APP
	subw	a3,s3,s2
	sraiw	a7,a3,31
	srliw	a7,a7,30
	addw	a7,a7,a3
	sraiw	t3,a7,2
	li	a5,63
	mv	a7,t3
	bgt	a3,a5,.L14
	li	a5,3
	mv	t1,t3
	ble	a3,a5,.L3
	j	.L2
.L14:
	li	t1,16
.L2:
	slli	a4,s2,2
	add	a1,s0,a4
	mv	a5,a0
	add	a4,s1,a4
	li	a3,0
.L4:
#APP
# 131 "vvadd.c" 1
	.insn sb 0x23, 0x4, a5, 0(a1)
	
# 0 "" 2
#NO_APP
	addi	a6,a5,4
#APP
# 132 "vvadd.c" 1
	.insn sb 0x23, 0x4, a6, 0(a4)
	
# 0 "" 2
#NO_APP
	addiw	a3,a3,1
	addi	a5,a5,8
	addi	a1,a1,16
	addi	a4,a4,16
	blt	a3,t1,.L4
.L3:
#APP
# 136 "vvadd.c" 1
	.insn uj 0x6b, x0, .L5
	
# 0 "" 2
#NO_APP
	slli	a3,t1,1
	ble	t3,t1,.L6
	addiw	a5,a7,-1
	subw	a5,a5,t1
	slli	a5,a5,32
	srli	a5,a5,32
	add	a5,a5,t1
	slli	a4,t1,2
	slli	a5,a5,2
	addi	a2,s2,4
	add	a4,a4,s2
	add	a2,a5,a2
	slli	a4,a4,2
	slli	a2,a2,2
	li	t4,512
	j	.L7
.L16:
	li	a3,0
.L7:
#APP
# 152 "vvadd.c" 1
	.insn uj 0x6b, x0, .L8
	
# 0 "" 2
#NO_APP
	slli	a5,a3,2
	add	a1,a0,a5
	add	a6,s0,a4
#APP
# 155 "vvadd.c" 1
	.insn sb 0x23, 0x4, a1, 0(a6)
	
# 0 "" 2
#NO_APP
	addi	a5,a5,4
	add	a5,a0,a5
	add	a1,s1,a4
#APP
# 156 "vvadd.c" 1
	.insn sb 0x23, 0x4, a5, 0(a1)
	
# 0 "" 2
#NO_APP
	addiw	a3,a3,2
	addi	a4,a4,16
	bne	a3,t4,.L9
	bne	a2,a4,.L16
	j	.L6
.L9:
	bne	a2,a4,.L7
.L6:
	subw	a7,a7,t1
	ble	t3,a7,.L17
.L12:
#APP
# 176 "vvadd.c" 1
	.insn uj 0x6b, x0, .L8
	
# 0 "" 2
#NO_APP
	addiw	a7,a7,1
	bne	t3,a7,.L12
.L11:
.L17:
#APP
# 187 "vvadd.c" 1
	.insn uj 0x2b, x0, .L17
	
# 0 "" 2
# 190 "vvadd.c" 1
	fence
	
# 0 "" 2
#NO_APP
	j	.L1
.L5:
	lw	a5,8(sp)
	add	a3,s1,s2
	slli	a3,a3,2
	sext.w	a5,a5
	add	a3,s0,a3
	beqz	a5,.L6
	li	a5,0
.L8:
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
	andi	a5,a5,511

.L1:
	ld	ra,40(sp)
	ld	s0,32(sp)
	ld	s1,24(sp)
	ld	s2,16(sp)
	ld	s3,8(sp)
	ld	s4,0(sp)
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
	bgt	a2,t1,.L24
	addiw	t4,a2,2
	bgt	t4,t1,.L57
.L24:
	lw	t1,12(sp)
	addw	t1,t1,a0
	bgt	a2,t1,.L25
	addiw	t4,a2,2
	bgt	t4,t1,.L58
.L25:
	lw	t1,24(sp)
	addw	t1,t1,a0
	bgt	a2,t1,.L26
	addiw	t4,a2,2
	bgt	t4,t1,.L59
.L26:
	lw	t1,36(sp)
	addw	a0,t1,a0
	blt	a0,a2,.L27
	addiw	t1,a2,2
	bgt	t1,a0,.L60
.L27:
	bnez	a6,.L28
	subw	t1,t3,a2
	subw	t4,t5,a3
	mv	a0,t1
	mv	a1,t4
	beq	t1,a4,.L61
.L29:
	subw	a1,a5,a1
	blez	a1,.L31
	li	a4,1
	li	a7,4
	beq	a5,a4,.L28
	li	a7,20
.L28:
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
.L60:
	lw	t1,40(sp)
	addw	a1,t1,a1
	blt	a1,a3,.L27
	addiw	t1,a3,2
	ble	t1,a1,.L27
	lw	a7,44(sp)
	mv	t5,a1
	mv	t3,a0
	j	.L27
.L59:
	lw	t4,28(sp)
	addw	t4,t4,a1
	bgt	a3,t4,.L26
	addiw	t6,a3,2
	ble	t6,t4,.L26
	lw	a7,32(sp)
	mv	t5,t4
	mv	t3,t1
	j	.L26
.L58:
	lw	t4,16(sp)
	addw	t4,t4,a1
	bgt	a3,t4,.L25
	addiw	t6,a3,2
	ble	t6,t4,.L25
	lw	a7,20(sp)
	mv	t5,t4
	mv	t3,t1
	j	.L25
.L57:
	lw	t4,4(sp)
	addw	t4,t4,a1
	bgt	a3,t4,.L24
	addiw	t6,a3,2
	ble	t6,t4,.L24
	lw	a7,8(sp)
	mv	t5,t4
	mv	t3,t1
	j	.L24
.L61:
	bne	t4,a5,.L29
	li	a5,64
	beq	a7,a5,.L42
	li	a5,16
	beq	a7,a5,.L43
	li	a5,32
	li	a1,1
	beq	a7,a5,.L36
	li	a5,8
	li	a1,3
	bne	a7,a5,.L62
.L36:
	bge	a2,t3,.L56
.L51:
	li	a7,96
	li	a5,48
	j	.L35
.L31:
	beqz	a1,.L33
	li	a7,2
	beqz	a5,.L28
	li	a7,66
	j	.L28
.L33:
	subw	a0,a4,a0
	blez	a0,.L34
	bnez	a4,.L48
.L56:
	li	a7,72
	li	a5,24
.L35:
	or	a7,a7,a1
	blt	a3,t5,.L28
	or	a7,a5,a1
	j	.L28
.L48:
	li	a7,67
	li	a5,19
	j	.L35
.L43:
	li	a1,4
	j	.L36
.L62:
	li	a1,-1
	j	.L36
.L34:
	beqz	a0,.L36
	li	a5,1
	beq	a4,a5,.L51
	mv	a1,a4
	li	a7,65
	li	a5,17
	j	.L35
.L42:
	li	a1,2
	j	.L36
	.size	getSIMDMask.constprop.8, .-getSIMDMask.constprop.8
	.align	2
	.type	kernel.constprop.5, @function
kernel.constprop.5:
	mulw	a6,a6,a5
	addi	sp,sp,-128
	sd	s0,112(sp)
	sd	s1,104(sp)
	sd	s5,72(sp)
	sd	s6,64(sp)
	or	a5,a4,a5
	sd	ra,120(sp)
	sd	s2,96(sp)
	sd	s3,88(sp)
	addw	s0,a6,a4
	sd	s4,80(sp)
	sd	s7,56(sp)
	sd	s8,48(sp)
	sd	s9,40(sp)
	sd	s10,32(sp)
	sd	s11,24(sp)
	mv	s5,a0
	mv	s6,a1
	mv	s1,a3
	mv	a4,s0
	bnez	a5,.L64
	li	a5,1
#APP
# 102 "../../common/bind_defs.h" 1
	csrw 0x7C1, a5;
# 0 "" 2
#NO_APP
.L65:
	li	a5,6
	li	a0,3
	li	s4,0
	beq	s0,a5,.L66
	li	a5,2
	seqz	s4,s0
	li	a0,0
	bleu	s0,a5,.L66
	addiw	a6,a4,-5
	li	a5,1
	li	s3,0
	li	a4,0
	bgtu	a6,a5,.L67
	j	.L66
.L64:
	li	a5,2
	beq	s0,a5,.L82
	li	a5,5
	li	a0,2
	li	s4,0
	bne	s0,a5,.L65
.L66:
	li	a5,3
	divw	a4,s1,a5
	li	s3,1
	sraiw	a2,a4,31
	xor	a3,a4,a2
	subw	a3,a3,a2
	andi	a2,a3,63
	mv	a5,a4
	beqz	a2,.L67
	li	a4,-2
	blt	s1,a4,.L104
	addiw	a5,a5,64
	subw	a4,a5,a2
.L67:
	li	a5,8
	beq	s0,a5,.L88
	li	a5,9
	bne	s0,a5,.L105
	li	a0,1
.L69:
	andi	a5,s0,-6
	li	a3,8
	beq	a5,a3,.L70
	li	s10,0
	li	s9,0
	li	s2,0
.L73:
	li	a5,10
	beq	s0,a5,.L94
	li	a5,11
	bne	s0,a5,.L106
	li	s8,0
	li	s7,1
.L77:
	andi	a5,s0,-6
	li	a3,10
	beq	a5,a3,.L78
	mv	s1,a4
	li	s11,0
.L79:
	li	a5,4096
	addiw	a5,a5,-1792
#APP
# 655 "vvadd.c" 1
	csrw 0x402, a5
	
# 0 "" 2
#NO_APP
	lui	a0,%hi(start_barrier)
	addi	a0,a0,%lo(start_barrier)
	call	pthread_barrier_wait
	li	a5,3
	beq	s0,a5,.L63
	mv	a0,s0
	call	getSpTop
	addi	a0,a0,-32
#APP
# 685 "vvadd.c" 1
	ld t0, 0(sp)
	sd t0, 0(a0)
	ld t0, 8(sp)
	sd t0, 8(a0)
	ld t0, 16(sp)
	sd t0, 16(a0)
	ld t0, 24(sp)
	sd t0, 24(a0)
	addi a7, sp, 0
	addi sp, a0, 0
	
# 0 "" 2
#NO_APP
	mv	a6,s4
	mv	a5,s8
	mv	a4,s7
	mv	a3,s9
	mv	a2,s3
	mv	a1,s10
	mv	a0,s11
	sd	a7,8(sp)
	call	getSIMDMask.constprop.8
	mv	a5,a0
	mv	a4,s0
	mv	a3,s1
	mv	a2,s2
	mv	a1,s6
	mv	a0,s5
	call	vvadd_execute.constprop.6
	ld	a7,8(sp)
#APP
# 718 "vvadd.c" 1
	addi sp, a7, 0
	
# 0 "" 2
#NO_APP
.L63:
	ld	ra,120(sp)
	ld	s0,112(sp)
	ld	s1,104(sp)
	ld	s2,96(sp)
	ld	s3,88(sp)
	ld	s4,80(sp)
	ld	s5,72(sp)
	ld	s6,64(sp)
	ld	s7,56(sp)
	ld	s8,48(sp)
	ld	s9,40(sp)
	ld	s10,32(sp)
	ld	s11,24(sp)
	addi	sp,sp,128
	jr	ra
.L82:
	li	a0,1
	li	s4,0
	j	.L66
.L104:
	subw	a4,a2,a3
	j	.L67
.L105:
	li	a5,12
	bne	s0,a5,.L107
	li	a0,2
	j	.L70
.L107:
	li	a5,13
	bne	s0,a5,.L108
	li	a0,3
	j	.L70
.L108:
	li	a5,4
	bne	s0,a5,.L69
	li	s4,1
.L70:
	li	a4,3
	divw	s2,s1,a4
	slliw	t1,s1,1
	divw	a5,t1,a4
	sraiw	a4,s2,31
	xor	a2,s2,a4
	subw	a2,a2,a4
	andi	a7,a2,63
	sraiw	a6,a5,31
	sext.w	a4,a5
	xor	a3,a4,a6
	subw	a3,a3,a6
	andi	s3,a3,63
	beqz	a7,.L71
	li	a6,-2
	blt	s1,a6,.L109
	addiw	s2,s2,64
	subw	s2,s2,a7
.L71:
	beqz	s3,.L93
	li	a4,-2
	blt	t1,a4,.L74
	addiw	a5,a5,64
	subw	a4,a5,s3
	li	s10,1
	li	s9,2
	li	s3,0
	j	.L73
.L109:
	subw	s2,a7,a2
	beqz	s3,.L93
.L74:
	subw	a4,s3,a3
	li	s10,1
	li	s9,2
	li	s3,0
	j	.L73
.L93:
	li	s10,1
	li	s9,2
	j	.L73
.L106:
	li	a5,14
	li	s8,1
	li	s7,0
	bne	s0,a5,.L110
.L78:
	slliw	a4,s1,1
	li	a5,3
	divw	s2,a4,a5
	sraiw	a2,s2,31
	xor	a3,s2,a2
	subw	a3,a3,a2
	andi	a2,a3,63
	beqz	a2,.L98
	li	a1,-2
	blt	a4,a1,.L111
	addiw	s2,s2,64
	subw	s2,s2,a2
	li	s10,1
	li	s11,3
	li	s9,2
	li	s3,2
	j	.L79
.L110:
	li	a5,15
	li	s7,1
	beq	s0,a5,.L78
	j	.L76
.L94:
	li	a0,0
.L76:
	li	a5,7
	andi	s7,a0,1
	srli	s8,a0,1
	bne	s0,a5,.L77
	li	s4,1
	j	.L78
.L111:
	subw	s2,a2,a3
	li	s10,1
	li	s11,3
	li	s9,2
	li	s3,2
	j	.L79
.L98:
	li	s10,1
	li	s11,3
	li	s9,2
	li	s3,2
	j	.L79
.L88:
	li	a0,0
	j	.L69
	.size	kernel.constprop.5, .-kernel.constprop.5
	.align	2
	.globl	roundUp
	.type	roundUp, @function
roundUp:
	beqz	a1,.L113
	sraiw	a4,a0,31
	xor	a5,a0,a4
	subw	a5,a5,a4
	remw	a3,a5,a1
	beqz	a3,.L113
	bltz	a0,.L121
	addw	a0,a1,a0
	subw	a0,a0,a3
.L113:
	ret
.L121:
	subw	a0,a3,a5
	ret
	.size	roundUp, .-roundUp
	.align	2
	.globl	vvadd_execute
	.type	vvadd_execute, @function
vvadd_execute:
	addi	sp,sp,-48
	sd	s0,32(sp)
	sd	s1,24(sp)
	mv	s0,a0
	mv	s1,a1
	mv	a0,a5
	li	a1,0
	sd	s2,16(sp)
	sd	s3,8(sp)
	sd	s4,0(sp)
	sd	ra,40(sp)
	mv	s2,a3
	mv	s4,a4
	mv	s3,a7
	call	getSpAddr
	lw	a5,48(sp)
#APP
# 124 "vvadd.c" 1
	.insn i 0x77, 0, x0, a5, 0x401
	
# 0 "" 2
#NO_APP
	subw	a2,s4,s2
	divw	a1,a2,s3
	li	a5,15
	mv	a2,a1
	bgt	a1,a5,.L134
	mv	a6,a1
	blez	a1,.L124
	j	.L123
.L134:
	li	a6,16
.L123:
	slli	a5,s2,2
	add	a4,s0,a5
#APP
# 131 "vvadd.c" 1
	.insn sb 0x23, 0x4, a0, 0(a4)
	
# 0 "" 2
#NO_APP
	addi	a4,a0,4
	add	a5,s1,a5
#APP
# 132 "vvadd.c" 1
	.insn sb 0x23, 0x4, a4, 0(a5)
	
# 0 "" 2
#NO_APP
	li	a5,1
	beq	a6,a5,.L124
	add	a5,s3,s2
	slli	a5,a5,2
	addi	a4,a0,8
	add	a3,s0,a5
#APP
# 131 "vvadd.c" 1
	.insn sb 0x23, 0x4, a4, 0(a3)
	
# 0 "" 2
#NO_APP
	addi	a4,a0,12
	add	a5,s1,a5
#APP
# 132 "vvadd.c" 1
	.insn sb 0x23, 0x4, a4, 0(a5)
	
# 0 "" 2
#NO_APP
	li	a5,2
	beq	a6,a5,.L124
	slliw	a5,s3,1
	mv	a4,a5
	add	a5,a5,s2
	slli	a5,a5,2
	addi	a3,a0,16
	add	a7,s0,a5
#APP
# 131 "vvadd.c" 1
	.insn sb 0x23, 0x4, a3, 0(a7)
	
# 0 "" 2
#NO_APP
	addi	a3,a0,20
	add	a5,s1,a5
#APP
# 132 "vvadd.c" 1
	.insn sb 0x23, 0x4, a3, 0(a5)
	
# 0 "" 2
#NO_APP
	li	a5,3
	beq	a6,a5,.L124
	addw	a5,s3,a4
	mv	a4,a5
	add	a5,a5,s2
	slli	a5,a5,2
	addi	a3,a0,24
	add	a7,s0,a5
#APP
# 131 "vvadd.c" 1
	.insn sb 0x23, 0x4, a3, 0(a7)
	
# 0 "" 2
#NO_APP
	addi	a3,a0,28
	add	a5,s1,a5
#APP
# 132 "vvadd.c" 1
	.insn sb 0x23, 0x4, a3, 0(a5)
	
# 0 "" 2
#NO_APP
	li	a5,4
	beq	a6,a5,.L124
	addw	a5,s3,a4
	mv	a4,a5
	add	a5,a5,s2
	slli	a5,a5,2
	addi	a3,a0,32
	add	a7,s0,a5
#APP
# 131 "vvadd.c" 1
	.insn sb 0x23, 0x4, a3, 0(a7)
	
# 0 "" 2
#NO_APP
	addi	a3,a0,36
	add	a5,s1,a5
#APP
# 132 "vvadd.c" 1
	.insn sb 0x23, 0x4, a3, 0(a5)
	
# 0 "" 2
#NO_APP
	li	a5,5
	beq	a6,a5,.L124
	addw	a5,s3,a4
	mv	a4,a5
	add	a5,a5,s2
	slli	a5,a5,2
	addi	a3,a0,40
	add	a7,s0,a5
#APP
# 131 "vvadd.c" 1
	.insn sb 0x23, 0x4, a3, 0(a7)
	
# 0 "" 2
#NO_APP
	addi	a3,a0,44
	add	a5,s1,a5
#APP
# 132 "vvadd.c" 1
	.insn sb 0x23, 0x4, a3, 0(a5)
	
# 0 "" 2
#NO_APP
	li	a5,6
	beq	a6,a5,.L124
	addw	a5,s3,a4
	mv	a4,a5
	add	a5,a5,s2
	slli	a5,a5,2
	addi	a3,a0,48
	add	a7,s0,a5
#APP
# 131 "vvadd.c" 1
	.insn sb 0x23, 0x4, a3, 0(a7)
	
# 0 "" 2
#NO_APP
	addi	a3,a0,52
	add	a5,s1,a5
#APP
# 132 "vvadd.c" 1
	.insn sb 0x23, 0x4, a3, 0(a5)
	
# 0 "" 2
#NO_APP
	li	a5,7
	beq	a6,a5,.L124
	addw	a5,s3,a4
	mv	a4,a5
	add	a5,a5,s2
	slli	a5,a5,2
	addi	a3,a0,56
	add	a7,s0,a5
#APP
# 131 "vvadd.c" 1
	.insn sb 0x23, 0x4, a3, 0(a7)
	
# 0 "" 2
#NO_APP
	addi	a3,a0,60
	add	a5,s1,a5
#APP
# 132 "vvadd.c" 1
	.insn sb 0x23, 0x4, a3, 0(a5)
	
# 0 "" 2
#NO_APP
	li	a5,8
	beq	a6,a5,.L124
	addw	a5,s3,a4
	mv	a4,a5
	add	a5,a5,s2
	slli	a5,a5,2
	addi	a3,a0,64
	add	a7,s0,a5
#APP
# 131 "vvadd.c" 1
	.insn sb 0x23, 0x4, a3, 0(a7)
	
# 0 "" 2
#NO_APP
	addi	a3,a0,68
	add	a5,s1,a5
#APP
# 132 "vvadd.c" 1
	.insn sb 0x23, 0x4, a3, 0(a5)
	
# 0 "" 2
#NO_APP
	li	a5,9
	beq	a6,a5,.L124
	addw	a5,s3,a4
	mv	a4,a5
	add	a5,a5,s2
	slli	a5,a5,2
	addi	a3,a0,72
	add	a7,s0,a5
#APP
# 131 "vvadd.c" 1
	.insn sb 0x23, 0x4, a3, 0(a7)
	
# 0 "" 2
#NO_APP
	addi	a3,a0,76
	add	a5,s1,a5
#APP
# 132 "vvadd.c" 1
	.insn sb 0x23, 0x4, a3, 0(a5)
	
# 0 "" 2
#NO_APP
	li	a5,10
	beq	a6,a5,.L124
	addw	a5,s3,a4
	mv	a4,a5
	add	a5,a5,s2
	slli	a5,a5,2
	addi	a3,a0,80
	add	a7,s0,a5
#APP
# 131 "vvadd.c" 1
	.insn sb 0x23, 0x4, a3, 0(a7)
	
# 0 "" 2
#NO_APP
	addi	a3,a0,84
	add	a5,s1,a5
#APP
# 132 "vvadd.c" 1
	.insn sb 0x23, 0x4, a3, 0(a5)
	
# 0 "" 2
#NO_APP
	li	a5,11
	beq	a6,a5,.L124
	addw	a5,s3,a4
	mv	a4,a5
	add	a5,a5,s2
	slli	a5,a5,2
	addi	a3,a0,88
	add	a7,s0,a5
#APP
# 131 "vvadd.c" 1
	.insn sb 0x23, 0x4, a3, 0(a7)
	
# 0 "" 2
#NO_APP
	addi	a3,a0,92
	add	a5,s1,a5
#APP
# 132 "vvadd.c" 1
	.insn sb 0x23, 0x4, a3, 0(a5)
	
# 0 "" 2
#NO_APP
	li	a5,12
	beq	a6,a5,.L124
	addw	a5,s3,a4
	mv	a4,a5
	add	a5,a5,s2
	slli	a5,a5,2
	addi	a3,a0,96
	add	a7,s0,a5
#APP
# 131 "vvadd.c" 1
	.insn sb 0x23, 0x4, a3, 0(a7)
	
# 0 "" 2
#NO_APP
	addi	a3,a0,100
	add	a5,s1,a5
#APP
# 132 "vvadd.c" 1
	.insn sb 0x23, 0x4, a3, 0(a5)
	
# 0 "" 2
#NO_APP
	li	a5,13
	beq	a6,a5,.L124
	addw	a5,s3,a4
	mv	a4,a5
	add	a5,a5,s2
	slli	a5,a5,2
	addi	a3,a0,104
	add	a7,s0,a5
#APP
# 131 "vvadd.c" 1
	.insn sb 0x23, 0x4, a3, 0(a7)
	
# 0 "" 2
#NO_APP
	addi	a3,a0,108
	add	a5,s1,a5
#APP
# 132 "vvadd.c" 1
	.insn sb 0x23, 0x4, a3, 0(a5)
	
# 0 "" 2
#NO_APP
	li	a5,14
	beq	a6,a5,.L124
	addw	a5,s3,a4
	mv	a4,a5
	add	a5,a5,s2
	slli	a5,a5,2
	addi	a3,a0,112
	add	a7,s0,a5
#APP
# 131 "vvadd.c" 1
	.insn sb 0x23, 0x4, a3, 0(a7)
	
# 0 "" 2
#NO_APP
	addi	a3,a0,116
	add	a5,s1,a5
#APP
# 132 "vvadd.c" 1
	.insn sb 0x23, 0x4, a3, 0(a5)
	
# 0 "" 2
#NO_APP
	li	a5,16
	bne	a6,a5,.L136
	addw	a4,a4,s3
	add	a5,a4,s2
	slli	a5,a5,2
	addi	a4,a0,120
	add	a3,s0,a5
#APP
# 131 "vvadd.c" 1
	.insn sb 0x23, 0x4, a4, 0(a3)
	
# 0 "" 2
#NO_APP
	addi	a4,a0,124
	add	a5,s1,a5
#APP
# 132 "vvadd.c" 1
	.insn sb 0x23, 0x4, a4, 0(a5)
	
# 0 "" 2
#NO_APP
	j	.L124
.L136:
	li	a6,15
.L124:
#APP
# 136 "vvadd.c" 1
	.insn uj 0x6b, x0, .L125
	
# 0 "" 2
#NO_APP
	slliw	a4,a6,1
	bge	a6,a1,.L126
	mulw	a5,a6,s3
	mv	a7,a6
	slli	s3,s3,2
	li	t4,512
	add	a3,a5,s2
	slli	a3,a3,2
	j	.L127
.L137:
	li	a4,0
.L127:
#APP
# 152 "vvadd.c" 1
	.insn uj 0x6b, x0, .L128
	
# 0 "" 2
#NO_APP
	slli	a5,a4,2
	add	t1,a0,a5
	add	t3,s0,a3
#APP
# 155 "vvadd.c" 1
	.insn sb 0x23, 0x4, t1, 0(t3)
	
# 0 "" 2
#NO_APP
	addi	a5,a5,4
	add	a5,a0,a5
	add	t1,s1,a3
#APP
# 156 "vvadd.c" 1
	.insn sb 0x23, 0x4, a5, 0(t1)
	
# 0 "" 2
#NO_APP
	addiw	a4,a4,2
	addiw	a7,a7,1
	add	a3,a3,s3
	bne	a4,t4,.L129
	bne	a1,a7,.L137
	j	.L126
.L129:
	bne	a1,a7,.L127
.L126:
	subw	a2,a2,a6
	ble	a1,a2,.L138
.L132:
#APP
# 176 "vvadd.c" 1
	.insn uj 0x6b, x0, .L128
	
# 0 "" 2
#NO_APP
	addiw	a2,a2,1
	bne	a1,a2,.L132
.L131:
.L138:
#APP
# 187 "vvadd.c" 1
	.insn uj 0x2b, x0, .L138
	
# 0 "" 2
# 190 "vvadd.c" 1
	fence
	
# 0 "" 2
#NO_APP
	j	.L122
.L125:
#APP
# 196 "vvadd.c" 1
	nop
# 0 "" 2
#NO_APP
.L128:
#APP
# 199 "vvadd.c" 1
	nop
# 0 "" 2
#NO_APP
.L122:
	ld	ra,40(sp)
	ld	s0,32(sp)
	ld	s1,24(sp)
	ld	s2,16(sp)
	ld	s3,8(sp)
	ld	s4,0(sp)
	addi	sp,sp,48
	jr	ra
	.size	vvadd_execute, .-vvadd_execute
	.align	2
	.globl	kernel
	.type	kernel, @function
kernel:
	mulw	a6,a6,a5
	addi	sp,sp,-128
	sd	s0,112(sp)
	sd	s1,104(sp)
	sd	s5,72(sp)
	sd	s6,64(sp)
	or	a5,a4,a5
	sd	ra,120(sp)
	sd	s2,96(sp)
	sd	s3,88(sp)
	addw	s0,a6,a4
	sd	s4,80(sp)
	sd	s7,56(sp)
	sd	s8,48(sp)
	sd	s9,40(sp)
	sd	s10,32(sp)
	sd	s11,24(sp)
	mv	s5,a0
	mv	s6,a1
	mv	s1,a3
	mv	a4,s0
	bnez	a5,.L143
	li	a5,1
#APP
# 102 "../../common/bind_defs.h" 1
	csrw 0x7C1, a5;
# 0 "" 2
#NO_APP
.L144:
	li	a5,6
	li	a0,3
	li	s4,0
	beq	s0,a5,.L145
	li	a5,2
	seqz	s4,s0
	li	a0,0
	bleu	s0,a5,.L145
	addiw	a6,a4,-5
	li	a5,1
	li	s3,0
	li	a4,0
	bgtu	a6,a5,.L146
	j	.L145
.L143:
	li	a5,2
	beq	s0,a5,.L161
	li	a5,5
	li	a0,2
	li	s4,0
	bne	s0,a5,.L144
.L145:
	li	a5,3
	divw	a4,s1,a5
	li	s3,1
	sraiw	a2,a4,31
	xor	a3,a4,a2
	subw	a3,a3,a2
	andi	a2,a3,63
	mv	a5,a4
	beqz	a2,.L146
	li	a4,-2
	blt	s1,a4,.L183
	addiw	a5,a5,64
	subw	a4,a5,a2
.L146:
	li	a5,8
	beq	s0,a5,.L167
	li	a5,9
	bne	s0,a5,.L184
	li	a0,1
.L148:
	andi	a5,s0,-6
	li	a3,8
	beq	a5,a3,.L149
	li	s10,0
	li	s9,0
	li	s2,0
.L152:
	li	a5,10
	beq	s0,a5,.L173
	li	a5,11
	bne	s0,a5,.L185
	li	s8,0
	li	s7,1
.L156:
	andi	a5,s0,-6
	li	a3,10
	beq	a5,a3,.L157
	mv	s1,a4
	li	s11,0
.L158:
	li	a5,4096
	addiw	a5,a5,-1792
#APP
# 655 "vvadd.c" 1
	csrw 0x402, a5
	
# 0 "" 2
#NO_APP
	lui	a0,%hi(start_barrier)
	addi	a0,a0,%lo(start_barrier)
	call	pthread_barrier_wait
	li	a5,3
	beq	s0,a5,.L142
	mv	a0,s0
	call	getSpTop
	addi	a0,a0,-32
#APP
# 685 "vvadd.c" 1
	ld t0, 0(sp)
	sd t0, 0(a0)
	ld t0, 8(sp)
	sd t0, 8(a0)
	ld t0, 16(sp)
	sd t0, 16(a0)
	ld t0, 24(sp)
	sd t0, 24(a0)
	addi a7, sp, 0
	addi sp, a0, 0
	
# 0 "" 2
#NO_APP
	mv	a6,s4
	mv	a5,s8
	mv	a4,s7
	mv	a3,s9
	mv	a2,s3
	mv	a1,s10
	mv	a0,s11
	sd	a7,8(sp)
	call	getSIMDMask.constprop.8
	mv	a5,a0
	mv	a4,s0
	mv	a3,s1
	mv	a2,s2
	mv	a1,s6
	mv	a0,s5
	call	vvadd_execute.constprop.6
	ld	a7,8(sp)
#APP
# 718 "vvadd.c" 1
	addi sp, a7, 0
	
# 0 "" 2
#NO_APP
.L142:
	ld	ra,120(sp)
	ld	s0,112(sp)
	ld	s1,104(sp)
	ld	s2,96(sp)
	ld	s3,88(sp)
	ld	s4,80(sp)
	ld	s5,72(sp)
	ld	s6,64(sp)
	ld	s7,56(sp)
	ld	s8,48(sp)
	ld	s9,40(sp)
	ld	s10,32(sp)
	ld	s11,24(sp)
	addi	sp,sp,128
	jr	ra
.L161:
	li	a0,1
	li	s4,0
	j	.L145
.L183:
	subw	a4,a2,a3
	j	.L146
.L184:
	li	a5,12
	bne	s0,a5,.L186
	li	a0,2
	j	.L149
.L186:
	li	a5,13
	bne	s0,a5,.L187
	li	a0,3
	j	.L149
.L187:
	li	a5,4
	bne	s0,a5,.L148
	li	s4,1
.L149:
	li	a4,3
	divw	s2,s1,a4
	slliw	t1,s1,1
	divw	a5,t1,a4
	sraiw	a4,s2,31
	xor	a2,s2,a4
	subw	a2,a2,a4
	andi	a7,a2,63
	sraiw	a6,a5,31
	sext.w	a4,a5
	xor	a3,a4,a6
	subw	a3,a3,a6
	andi	s3,a3,63
	beqz	a7,.L150
	li	a6,-2
	blt	s1,a6,.L188
	addiw	s2,s2,64
	subw	s2,s2,a7
.L150:
	beqz	s3,.L172
	li	a4,-2
	blt	t1,a4,.L153
	addiw	a5,a5,64
	subw	a4,a5,s3
	li	s10,1
	li	s9,2
	li	s3,0
	j	.L152
.L188:
	subw	s2,a7,a2
	beqz	s3,.L172
.L153:
	subw	a4,s3,a3
	li	s10,1
	li	s9,2
	li	s3,0
	j	.L152
.L172:
	li	s10,1
	li	s9,2
	j	.L152
.L185:
	li	a5,14
	li	s8,1
	li	s7,0
	bne	s0,a5,.L189
.L157:
	slliw	a4,s1,1
	li	a5,3
	divw	s2,a4,a5
	sraiw	a2,s2,31
	xor	a3,s2,a2
	subw	a3,a3,a2
	andi	a2,a3,63
	beqz	a2,.L177
	li	a1,-2
	blt	a4,a1,.L190
	addiw	s2,s2,64
	subw	s2,s2,a2
	li	s10,1
	li	s11,3
	li	s9,2
	li	s3,2
	j	.L158
.L189:
	li	a5,15
	li	s7,1
	beq	s0,a5,.L157
	j	.L155
.L173:
	li	a0,0
.L155:
	li	a5,7
	andi	s7,a0,1
	srli	s8,a0,1
	bne	s0,a5,.L156
	li	s4,1
	j	.L157
.L190:
	subw	s2,a2,a3
	li	s10,1
	li	s11,3
	li	s9,2
	li	s3,2
	j	.L158
.L177:
	li	s10,1
	li	s11,3
	li	s9,2
	li	s3,2
	j	.L158
.L167:
	li	a0,0
	j	.L148
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
	bnez	a5,.L194
	lw	a5,32(s0)
	bnez	a5,.L194
	li	a5,10
#APP
# 113 "../../common/bind_defs.h" 1
	csrw 0x7C1, a5;
# 0 "" 2
#NO_APP
.L194:
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
