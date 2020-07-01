	; ARGS: multi_vector.s
	SCALAR_HEADER
	tril_somefunc1:
	.insn i 0x77, 0, x0, a0, 0x401
	F1_BEFORE_VECTOR_EPOCH
	# trillium: scalar stack cleanup begin
	F1_SCALAR_STACK_CLEANUP
	# trillium: scalar stack cleanup end
.SCALAR1:
	.insn uj 0x2b, x0, .SCALAR14
.SCALAR2:
	F1_SCALAR_AFTER_DEVEC
	ret
	# trillium: auxiliary blocks begin
.tril_somefunc1_anon_aux_bb:
	F1_SCALAR_AFTER_RETURN
.SCALAR4:
	F1_AN_AUXILIARY_BLOCK
.SCALAR6:
	F1_ANOTHER_AUXILIARY_BLOCK
	# trillium: auxiliary blocks end
	# trillium: vector vissue blocks begin
.SCALAR3:  # f1_block_one vissue block
	#prepended trillium_init block here (See docs for more info)
	#trillium_init begin
	F1_TRILLIUM_INIT_BLOCK
.VEC1:
	#trillium_init end
.VEC2:
	F1_VECTOR_BLOCK_ONE
.VEC3:
	.insn i 0x1b, 0x7, x0, x0, 0
.SCALAR5:  # f1_block_two vissue block
.VEC6:
	F1_VECTOR_BLOCK_TWO
	.insn i 0x1b, 0x7, x0, x0, 0
	# trillium: vector vissue blocks end
	# trillium: footer begin
	.size whatever
	# trillium: footer end
	F1_SCALAR_FOOTER
	tril_somefunc2:
	.insn i 0x77, 0, x0, a0, 0x401
	F2_BEFORE_VECTOR_EPOCH
	# trillium: scalar stack cleanup begin
	F2_SCALAR_STACK_CLEANUP
	# trillium: scalar stack cleanup end
.M1:
	.insn uj 0x2b, x0, .M14
.M2:
	F2_SCALAR_AFTER_DEVEC
	ret
	# trillium: auxiliary blocks begin
.tril_somefunc2_anon_aux_bb:
	F2_SCALAR_AFTER_RETURN
.M4:
	F2_AN_AUXILIARY_BLOCK
.M6:
	F2_ANOTHER_AUXILIARY_BLOCK
	# trillium: auxiliary blocks end
	# trillium: vector vissue blocks begin
.M3:  # f2_block_one vissue block
	#prepended trillium_init block here (See docs for more info)
	#trillium_init begin
	F2_TRILLIUM_INIT_BLOCK
.M1:
	#trillium_init end
.M2:
	F2_VECTOR_BLOCK_ONE
.M3:
	.insn i 0x1b, 0x7, x0, x0, 0
.M5:  # f2_block_two vissue block
.M6:
	F2_VECTOR_BLOCK_TWO
	.insn i 0x1b, 0x7, x0, x0, 0
	# trillium: vector vissue blocks end
	# trillium: footer begin
	.size whatever
	# trillium: footer end
	F2_SCALAR_FOOTER
