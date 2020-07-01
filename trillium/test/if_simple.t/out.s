	; ARGS: if_simple_vector.s
	SCALAR_HEADER
	tril_somefunc:
	.insn i 0x77, 0, x0, a0, 0x401
	BEFORE_VECTOR_EPOCH
	.insn uj 0x6b, x0, .SCALAR3
	.insn uj 0x6b, x0, .SCALAR4
	# trillium: scalar stack cleanup begin
	SCALAR_STACK_CLEANUP
	# trillium: scalar stack cleanup end
.SCALAR1:
	.insn uj 0x2b, x0, .SCALAR1
.SCALAR2:
	SCALAR_AFTER_DEVEC
	ret
	# trillium: auxiliary blocks begin
.tril_somefunc_anon_aux_bb:
	SCALAR_AFTER_RETURN
.SCALAR5:
	AN_AUXILIARY_BLOCK
	# trillium: auxiliary blocks end
	# trillium: vector vissue blocks begin
.SCALAR3:  # if_block vissue block
	#prepended trillium_init block here (See docs for more info)
	#trillium_init begin
	TRILLIUM_INIT_BLOCK
.VEC1:
	#trillium_init end
.VEC2:
	VECTOR_IF_BLOCK
	bnez r0 .VEC4
	TRUE_BRANCH
.VEC4:
	FALSE_BRANCH
.VEC3:
	.insn i 0x1b, 0x7, x0, x0, 0
.SCALAR4:  # vector_return vissue block
.VEC6:
	VECTOR_AFTER_RETURN_DELIM
	.insn i 0x1b, 0x7, x0, x0, 0
	# trillium: vector vissue blocks end
	# trillium: footer begin
	.size whatever
	# trillium: footer end
	SCALAR_FOOTER
