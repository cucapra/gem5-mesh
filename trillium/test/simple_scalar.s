; ARGS: somefunc simple_vector.s

    SCALAR_HEADER
somefunc:
	SCALAR_FUNC_TOP
	.insn i 0x77, 0, x0, a0, 0x401
.L1:
	SCALAR_BEFORE_DEVEC
	.insn uj 0x2b, x0, .L14
	SCALAR_AFTER_DEVEC
	trillium vissue_delim return scalar_return
    ret
    SCALAR_AFTER_RETURN
.L2:
	trillium glue_point block_one
    SCALAR_AFTER_GLUE_POINT
.L13:
	trillium glue_point block_two
    SCALAR_AFTER_ANOTHER_GLUE_POINT
	.size whatever
    SCALAR_FOOTER
