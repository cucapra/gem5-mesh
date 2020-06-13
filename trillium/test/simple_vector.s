	VECTOR_0
somefunc:
	VECTOR_1
	trillium vissue_delim begin vector_init
    VECTOR_2
	trillium vissue_delim end
    VECTOR_3
.L3:
	trillium vissue_delim begin vector_body
    VECTOR_4A
	.insn i 0x1b, 0x3, x0, a3, 0
    VECTOR_4B
	.insn i 0x1b, 0x2, x0, a3, 0
    VECTOR_4C
	.insn sb 0x23, 0x5, a4, 0(s0)
    VECTOR_4D
	trillium vissue_delim end
    VECTOR_5
.L2:
	trillium vissue_delim return vector_return
    VECTOR_6
