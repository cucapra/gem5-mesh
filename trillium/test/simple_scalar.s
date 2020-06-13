; ARGS: somefunc simple_vector.s

    SCALAR_0
somefunc:
	SCALAR_1
	.insn i 0x77, 0, x0, a0, 0x401
.L2:
	SCALAR_2
	.insn uj 0x2b, x0, .L14
	SCALAR_2A
	trillium vissue_delim return scalar_return
    ret
    SCALAR_3
.L4:
	trillium glue_point vector_init
    SCALAR_4
.L8:
	trillium glue_point vector_body
    SCALAR_5
.L5:
	trillium glue_point trillium_junk0
    SCALAR_6
.L13:
	trillium glue_point vector_return
    SCALAR_7
