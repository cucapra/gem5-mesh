; ARGS: somefunc1 somefun2 multi_vector.s

SCALAR_HEADER
somefunc1:
	BEFORE_VECTOR_EPOCH
	.insn i 0x77, 0, x0, a0, 0x401
.L1:
	SCALAR_BEFORE_DEVEC
	.insn uj 0x2b, x0, .L14
.L2:
	SCALAR_AFTER_DEVEC
	trillium vissue_delim return scalar_return
  SCALAR_STACK_CLEANUP
  ret
  SCALAR_AFTER_RETURN
.L3:
	trillium glue_point somefunc1_block_one
.L4:
  AN_AUXILIARY_BLOCK
.L5:
	trillium glue_point somefunc1_block_two
.L6:
  ANOTHER_AUXILIARY_BLOCK
.size whatever
SCALAR_FOOTER

somefunc2:
	BEFORE_VECTOR_EPOCH
	.insn i 0x77, 0, x0, a0, 0x401
.M1:
	SCALAR_BEFORE_DEVEC
	.insn uj 0x2b, x0, .L14
.M2:
	SCALAR_AFTER_DEVEC
	trillium vissue_delim return scalar_return
  SCALAR_STACK_CLEANUP
  ret
  SCALAR_AFTER_RETURN
.M3:
	trillium glue_point somefunc2_block_one
.M4:
  AN_AUXILIARY_BLOCK
.M5:
	trillium glue_point somefunc2_block_two
.M6:
  ANOTHER_AUXILIARY_BLOCK
.size whatever
SCALAR_FOOTER
