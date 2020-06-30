VECTOR_HEADER
tril_somefunc:
	TRILLIUM_INIT_BLOCK
  .L1:
	trillium vissue_delim if_begin if_block
  .L2:
  VECTOR_IF_BLOCK
  bnez r0 .L4
  TRUE_BRANCH
  .L4:
  FALSE_BRANCH
  .L3:
	trillium vissue_delim end
  .L5:
	trillium vissue_delim return vector_return
  .L6:
  VECTOR_AFTER_RETURN_DELIM
  ret
.size whatever
VECTOR_FOOTER
