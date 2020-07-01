VECTOR_HEADER
tril_somefunc:
	TRILLIUM_INIT_BLOCK
  .L1:
	trillium vissue_delim begin block_one
  .L2:
  VECTOR_BLOCK_ONE
  .L3:
	trillium vissue_delim end
  .L4:
  VECTOR_AFTER_BLOCK_ONE
  lui a5,%hi(.LC0)
  flw fs6,%lo(.LC0)(a5)
  .L5:
	trillium vissue_delim return block_two
  .L6:
  VECTOR_BLOCK_TWO
  lui s2,%hi(.LC1)
  addi a0,s2,%lo(.LC1)
  ret
.size whatever
.section    .rodata.str1.8,"aMS",@progbits,1
.align  3
.LC0:
    .word 1065353216
.LC1:
    .string "sup"
VECTOR_FOOTER
