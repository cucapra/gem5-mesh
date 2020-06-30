VECTOR_HEADER
tril_somefunc:
	TRILLIUM_INIT_BLOCK
  .L1:
	trillium vissue_delim begin block_one
  .L2:
  VECTOR_BLOCK_ONE
	trillium vissue_delim end at_jump
  VECTOR_BLOCK_ONE_CONTINUED
  beqz .L1
  VECTOR_JUNK
  .L3:
	trillium vissue_delim return block_two
  .L6:
  VECTOR_BLOCK_TWO
  ret
.size whatever
VECTOR_FOOTER
