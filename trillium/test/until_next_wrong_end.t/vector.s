VECTOR_HEADER
tril_somefunc:
	TRILLIUM_INIT_BLOCK
  .L1:
	trillium vissue_delim until_next block_one
  .L2:
  VECTOR_BLOCK_ONE
  .L3:
	trillium vissue_delim end
  .L6:
  VECTOR_BLOCK_TWO
  ret
.size whatever
VECTOR_FOOTER
