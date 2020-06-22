VECTOR_HEADER
somefunc1:
	TRILLIUM_INIT_BLOCK
  .L1:
	trillium vissue_delim begin somefunc1_block_one
  .L2:
  VECTOR_BLOCK_ONE
  .L3:
	trillium vissue_delim end
  .L4:
  VECTOR_AFTER_BLOCK_ONE
  .L5:
	trillium vissue_delim return somefunc1_block_two
  .L6:
  VECTOR_BLOCK_TWO
  ret
.size whatever
VECTOR_FOOTER

somefunc2:
	TRILLIUM_INIT_BLOCK
  .M1:
	trillium vissue_delim begin somefunc2_block_one
  .M2:
  VECTOR_BLOCK_ONE
  .M3:
	trillium vissue_delim end
  .M4:
  VECTOR_AFTER_BLOCK_ONE
  .M5:
	trillium vissue_delim return somefunc2_block_two
  .M6:
  VECTOR_BLOCK_TWO
  ret
.size whatever
VECTOR_FOOTER
