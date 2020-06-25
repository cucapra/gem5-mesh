; ARGS: multi_vector.s

SCALAR_HEADER
tril_somefunc1:
	F1_BEFORE_VECTOR_EPOCH
	.insn i 0x77, 0, x0, a0, 0x401
.L1:
	.insn uj 0x2b, x0, .L14
.L2:
	F1_SCALAR_AFTER_DEVEC
	trillium vissue_delim return scalar_return
  F1_SCALAR_STACK_CLEANUP
  ret
  F1_SCALAR_AFTER_RETURN
.L3:
	trillium glue_point f1_block_one
.L4:
  F1_AN_AUXILIARY_BLOCK
.L5:
	trillium glue_point f1_block_two
.L6:
  F1_ANOTHER_AUXILIARY_BLOCK
.size whatever
F1_SCALAR_FOOTER

tril_somefunc2:
	F2_BEFORE_VECTOR_EPOCH
	.insn i 0x77, 0, x0, a0, 0x401
.M1:
	.insn uj 0x2b, x0, .M14
.M2:
	F2_SCALAR_AFTER_DEVEC
	trillium vissue_delim return scalar_return
  F2_SCALAR_STACK_CLEANUP
  ret
  F2_SCALAR_AFTER_RETURN
.M3:
	trillium glue_point f2_block_one
.M4:
  F2_AN_AUXILIARY_BLOCK
.M5:
	trillium glue_point f2_block_two
.M6:
  F2_ANOTHER_AUXILIARY_BLOCK
.size whatever
F2_SCALAR_FOOTER
