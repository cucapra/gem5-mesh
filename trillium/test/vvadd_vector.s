	.file	"vvadd_kernel_be.c"
	.option nopic
	.text
	.align	2
	.globl	tril_vvadd
	.type	tril_vvadd, @function
tril_vvadd:
	addi	sp,sp,-32
	sd	ra,24(sp)
	sd	s0,16(sp)
	mv	a0,a6
#APP
# 45 "vvadd_kernel_be.c" 1
	trillium vissue_delim begin vector_init
# 0 "" 2
#NO_APP
	add	s0,a4,a7
	li	a1,0
	slli	s0,s0,2
	add	s0,a3,s0
	call	getSpAddr
#APP
# 50 "vvadd_kernel_be.c" 1
	trillium vissue_delim end
# 0 "" 2
#NO_APP
	lw	a5,12(sp)
	sext.w	a5,a5
	beqz	a5,.L2
	lw	a1,32(sp)
	li	a5,0
	li	a3,2
	slli	a1,a1,2
.L3:
#APP
# 104 "vvadd_kernel_be.c" 1
	trillium vissue_delim begin vector_body
# 0 "" 2
# 113 "vvadd_kernel_be.c" 1
	.insn i 0x1b, 0x3, x0, a3, 0
	
# 0 "" 2
#NO_APP
	slli	a4,a5,2
	add	a4,a0,a4
	lw	a2,0(a4)
	lw	a4,4(a4)
#APP
# 123 "vvadd_kernel_be.c" 1
	.insn i 0x1b, 0x2, x0, a3, 0
	
# 0 "" 2
#NO_APP
	addw	a4,a2,a4
#APP
# 127 "vvadd_kernel_be.c" 1
	.insn sb 0x23, 0x5, a4, 0(s0)
	
# 0 "" 2
#NO_APP
	addi	a5,a5,2
	add	s0,s0,a1
	andi	a5,a5,511
#APP
# 133 "vvadd_kernel_be.c" 1
	trillium vissue_delim end
# 0 "" 2
#NO_APP
	lw	a4,12(sp)
	sext.w	a4,a4
	bnez	a4,.L3
.L2:
#APP
# 181 "vvadd_kernel_be.c" 1
	trillium vissue_delim return vector_return
# 0 "" 2
#NO_APP
	ld	ra,24(sp)
	ld	s0,16(sp)
	addi	sp,sp,32
	jr	ra
	.size	tril_vvadd, .-tril_vvadd
	.comm	start_barrier,32,8
	.ident	"GCC: (GNU) 8.3.0"
	.section	.note.GNU-stack,"",@progbits
