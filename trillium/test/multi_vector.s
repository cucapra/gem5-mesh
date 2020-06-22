VECTOR_HEADER
tril_somefunc1:
	F1_TRILLIUM_INIT_BLOCK
  .L1:
	trillium vissue_delim begin f1_block_one
  .L2:
  F1_VECTOR_BLOCK_ONE
  .L3:
	trillium vissue_delim end
  .L4:
  F1_VECTOR_AFTER_BLOCK_ONE
  .L5:
	trillium vissue_delim return f1_block_two
  .L6:
  F1_VECTOR_BLOCK_TWO
  ret
.size whatever
F1_VECTOR_FOOTER

tril_somefunc2:
	F2_TRILLIUM_INIT_BLOCK
  .M1:
	trillium vissue_delim begin f2_block_one
  .M2:
  F2_VECTOR_BLOCK_ONE
  .M3:
	trillium vissue_delim end
  .M4:
  F2_VECTOR_AFTER_BLOCK_ONE
  .M5:
	trillium vissue_delim return f2_block_two
  .M6:
  F2_VECTOR_BLOCK_TWO
  ret
.size whatever
F2_VECTOR_FOOTER
