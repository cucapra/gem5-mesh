	; ARGS: simple_vector.s
	SCALAR_HEADER
	tril_somefunc:
	.insn i 0x77, 0, x0, a0, 0x401
	BEFORE_VECTOR_EPOCH
.SCALAR1:
	# trillium: scalar stack cleanup begin
	SCALAR_STACK_CLEANUP
	# trillium: scalar stack cleanup end
.SCALAR14DEVEC:
	.insn uj 0x2b, x0, .SCALAR14DEVEC
.SCALAR2:
	SCALAR_AFTER_DEVEC
	ret
	# trillium: auxiliary blocks begin
.tril_somefunc_anon_aux_bb:
	SCALAR_AFTER_RETURN
.SCALAR4:
	AN_AUXILIARY_BLOCK
.SCALAR6:
	ANOTHER_AUXILIARY_BLOCK
	# trillium: auxiliary blocks end
	# trillium: vector vissue blocks begin
.SCALAR3:  # block_one vissue block
	#prepended trillium_init block here (See docs for more info)
	#trillium_init begin
	TRILLIUM_INIT_BLOCK
.VEC1:
	#trillium_init end
.VEC2:
	VECTOR_BLOCK_ONE
.VEC3:
	.insn i 0x1b, 0x7, x0, x0, 0
.SCALAR5:  # block_two vissue block
.VEC6:
	VECTOR_BLOCK_TWO
	.insn i 0x1b, 0x7, x0, x0, 0
	# trillium: vector vissue blocks end
	# trillium: footer begin
	.size whatever
	# trillium: footer end
	SCALAR_FOOTER
