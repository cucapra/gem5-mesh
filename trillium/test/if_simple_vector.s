VECTOR_HEADER
tril_somefunc:
	TRILLIUM_INIT_BLOCK
  .L1:
	trillium vissue_delim if_begin if_block
  .L2:
  VECTOR_IF_BLOCK
  j .L4
  .L3:
	trillium vissue_delim if_end
  .L4:
  VECTOR_JUNK0
  .L5:
	trillium vissue_delim return vector_return
  .L6:
  VECTOR_AFTER_RETURN_DELIM
  ret
.size whatever
VECTOR_FOOTER
