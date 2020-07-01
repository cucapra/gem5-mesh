; ARGS: if_simple_vector.s
SCALAR_HEADER
tril_somefunc:
	BEFORE_VECTOR_EPOCH
	.insn i 0x77, 0, x0, a0, 0x401
	.insn uj 0x6b, x0, .L3 #vissue if block
	.insn uj 0x6b, x0, .L4 #vissue vector return block
.L1:
	.insn uj 0x2b, x0, .L1
.L2:
	SCALAR_AFTER_DEVEC
	trillium vissue_delim return scalar_return
  SCALAR_STACK_CLEANUP
  ret
  SCALAR_AFTER_RETURN
.L3:
	trillium glue_point if_block
.L4:
	trillium glue_point vector_return
.L5:
  AN_AUXILIARY_BLOCK
.size whatever
SCALAR_FOOTER
