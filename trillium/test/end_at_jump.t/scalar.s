; ARGS: end_at_jump_vector.s

SCALAR_HEADER
tril_somefunc:
	BEFORE_VECTOR_EPOCH
	.insn i 0x77, 0, x0, a0, 0x401
.L1:
	.insn uj 0x2b, x0, .L14
.L2:
	SCALAR_AFTER_DEVEC
	trillium vissue_delim return scalar_return
  SCALAR_STACK_CLEANUP
  ret
  SCALAR_AFTER_RETURN
.L3:
	trillium glue_point block_one
.L4:
  AN_AUXILIARY_BLOCK
.L5:
	trillium glue_point block_two
.L6:
  ANOTHER_AUXILIARY_BLOCK
.size whatever
SCALAR_FOOTER
