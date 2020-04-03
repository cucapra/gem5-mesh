        .file	"vvadd_kernel.c"
        .option nopic
        .text
        .align	2
        .globl	vvadd_execute_simd
        .type	vvadd_execute_simd, @function
vvadd_execute_simd:
        li	a4,15
        bgt	a5,a4,.L5
        lui	a4,%hi(spm_base_ptr_arr)
        slli	a5,a5,3
        addi	a4,a4,%lo(spm_base_ptr_arr)
        add	a5,a5,a4
        ld	a1,0(a5)
        addi	a1,a1,16
.L2:
        lw	a5,0(sp)
#APP
# 26 "vvadd_kernel.c" 1
        .insn i 0x77, 0, x0, a5, 0x401

# 0 "" 2
# 95 "vvadd_kernel.c" 1
        header_block_start
# 0 "" 2
#NO_APP
        add	a3,a3,a6
        slli	a3,a3,2
        add	a2,a2,a3
#APP
# 99 "vvadd_kernel.c" 1
        header_block_end
# 0 "" 2
#NO_APP
        lui	a0,%hi(bh)
        lw	a5,%lo(bh)(a0)
        sext.w	a5,a5
        beqz	a5,.L1
        slli	a7,a7,2
        li	a3,0
.L4:
#APP
# 102 "vvadd_kernel.c" 1
        vector_body_start
# 0 "" 2
#NO_APP
        slli	a5,a3,2
        add	a4,a1,a5
#APP
# 104 "vvadd_kernel.c" 1
        .insn s 0x03, 0x7, a4, 0(a4)

# 0 "" 2
#NO_APP
        addi	a5,a5,4
        add	a5,a1,a5
#APP
# 105 "vvadd_kernel.c" 1
        .insn s 0x03, 0x7, a5, 0(a5)

# 0 "" 2
# 109 "vvadd_kernel.c" 1
        .insn u 0x0b, x0, 0

# 0 "" 2
#NO_APP
        addw	a5,a4,a5
#APP
# 113 "vvadd_kernel.c" 1
        .insn sb 0x23, 0x5, a5, 0(a2)

# 0 "" 2
#NO_APP
        addi	a3,a3,2
        add	a2,a2,a7
        andi	a3,a3,511
#APP
# 119 "vvadd_kernel.c" 1
        vector_body_end
# 0 "" 2
#NO_APP
        lw	a5,%lo(bh)(a0)
        sext.w	a5,a5
        bnez	a5,.L4
.L1:
        ret
.L5:
        li	a1,0
        j	.L2
        .size	vvadd_execute_simd, .-vvadd_execute_simd
        .comm	bh,4,4
        .comm	spm_done_flag_ptr_arr,128,8
        .comm	spm_go_flag_ptr_arr,128,8
        .comm	spm_base_addr_ptr_arr,128,8
        .comm	spm_next_ptr_arr,128,8
        .comm	spm_base_ptr_arr,128,8
        .globl	spm
        .comm	start_barrier,32,8
        .section	.spm,"aw"
        .align	3
        .type	spm, @object
        .size	spm, 65536
spm:
        .zero	65536
        .ident	"GCC: (GNU) 8.3.0"
        .section	.note.GNU-stack,"",@progbits
