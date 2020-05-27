from glue_util import *
import itertools, argparse
from enum import Enum, auto

# 3 kinds of delimiters demarcate the boundaries of vissue blocks in vector code
# `until_next`: boundary immediately extends until the next delimiter is found (of any type)
#               the compiler will explode if no other `until_next` delimiter is found
#   Example: asm("trillium vissue_delim until_next vector_init");
#             ... vissue boundary of "vector_init" block ...
#             asm("trillium vissue_delim until_next vector_body");
# TODO: implement `begin`/`end` delimiters
# `begin`, `end`: explicitly marks the beginning (`begin`) and end (`end`) of the boundaries of a vissue block
# `return`: this delimiter MUST ALWAYS be placed before the `return` statements of both scalar and vector code.
#           they mark the boundary of a special vissue block whose boundary extends until the next "jump to return address"-ish instruction (e.g., `ret`, `jl ra`, etc)

class VectorParseState(Enum):
    START = auto()
    # INIT = auto()
    UNTIL_NEXT = auto()
    RETURN = auto()

# NOTE: kernel_fun_name MUST equal the name of the trillium-asm Vector-SIMD function
def read_vector_bbs(kernel_fun_name, raw_vector_code):
    vector_code = vector_preprocess(raw_vector_code)

    # dissects vector assembly into the following:
    # # init cfg
    # init = []
    # # body cfg
    # body = []
    # # return stack manipulation cfg
    # ret_block = []

    curr_vissue_key = "trillium_init"
    blocks = {curr_vissue_key : "trillium_init"}

    state = VectorParseState.START
    for (line_no, l) in vector_code:
        if state == VectorParseState.START:
            if l == kernel_name + ":":
                print("found start label at line {}".format(kernel_name, line_no))
                state = VectorParseState.UNTIL_NEXT

        elif state == VectorParseState.UNTIL_NEXT:
            delim_parse = parse_delim(l)
            if delim_parse != None:
                print("parsed 'until_next'-delimited block {}".format(curr_vissue_key))
                delim, vissue_key = delim_parse
                curr_vissue_key = vissue_key
                if delim == TrilliumAsmDelim.UNTIL_NEXT:
                    state = VectorParseState.UNTIL_NEXT
                elif delim == TrilliumAsmDelim.RETURN:
                    state = VectorParseState.RETURN
            else:
                blocks[curr_vissue_key].append(l)

        elif state == VectorParseState.RETURN:
            if is_return_inst(l):
                print("parsed return block {}".format(curr_vissue_key))
                break #return block should be the last one
            else:
                blocks[curr_vissue_key].append(l)


    # insert terminator in each block
    terminator = ".insn i 0x1b, 0x7, x0, x0, 0"
    for b in blocks.values():
        b.append(terminator)

    return blocks


class ScalarParseState(Enum):
    HEADER = auto()
    BEFORE_VECTOR_EPOCH = auto()
    AFTER_VECTOR_EPOCH = auto()
    AFTER_DEVEC = auto()
    RETURN_STACK_MANIP = auto()
    GET_BBS = auto()
    REPLACE_BB_PLACEHOLDERS = auto()
    GET_NONVEC_BBS = auto()
    FOOTER = auto()
    # SIFT_BB = auto()
    # NON_VECTOR_BB = auto()

def glue(raw_scalar_code, vector_bbs):
    scalar_return = "scalar_return"

    scalar_code = scalar_preprocess(raw_scalar_code)

    # dissects scalar assembly into the following non-overlapping components:
    # interval notation: open, closed, half-open intervals
    # [start of file, kernel launch label]
    header = []
    # (kernel launch label, first VECTOR_EPOCH call)
    before_VECTOR_EPOCH = []
    # [first VECTOR_EPOCH call, first DEVEC call)
    after_VECTOR_EPOCH = []
    # [first DEVEC call, end of file]
    after_DEVEC = []
    # [scalar_ret label, "return, jump-like instruction"]
    scalar_ret = []
    # ("return, jump-like instruction", labels pointing to vector blocks]
    bbs = []
    # (return statement after labels pointing to vector blocks, rest of file]
    footer = []

    label = None
    bb = []
    state = ScalarParseState.HEADER
    for (line_no, l) in scalar_code:


        if state == ScalarParseState.HEADER:
          if l.strip() == kernel_name + ":":
            header.append(l)
            state = ScalarParseState.BEFORE_VECTOR_EPOCH
          else:
            header.append(l)



        elif state == ScalarParseState.BEFORE_VECTOR_EPOCH:
            if is_VECTOR_EPOCH_inst(l):
                after_VECTOR_EPOCH.append(l);
                state = ScalarParseState.AFTER_VECTOR_EPOCH
            else:
                before_VECTOR_EPOCH.append(l)



        elif state == ScalarParseState.AFTER_VECTOR_EPOCH:
            if is_DEVEC(l):
                after_DEVEC.append(l)
                state = ScalarParseState.AFTER_DEVEC
            else:
                after_VECTOR_EPOCH.append(l)



        elif state == ScalarParseState.AFTER_DEVEC:
            delim = parse_delim(l)
            if delim != None and delim[0] == TrilliumAsmDelim.RETURN:
                state = ScalarParseState.RETURN_STACK_MANIP
            else:
                after_DEVEC.append(l)



        # assumption: label happens right after return jump instr
        elif state == ScalarParseState.RETURN_STACK_MANIP:
            if is_return_inst(l):
                after_DEVEC.append(l)
                state = ScalarParseState.GET_BBS
            else:
                scalar_ret.append(l)



        elif state == ScalarParseState.GET_BBS:
            if l == "ret":
                continue

            elif ".size " in l:
                print("found footer after vector block")
                bbs.extend(bb)
                footer.append(l)
                bb = []
                state = FOOTER

            elif is_label(l) and label == None:
                label = l
                state = ScalarParseState.REPLACE_BB_PLACEHOLDERS

            elif is_label(l) and label != None:
                print("Warning: passing empty label at line {}".format(line_no))
                bb.append(l)
                label = None
                state = ScalarParseState.REPLACE_BB_PLACEHOLDERS

            else:
                raise(Exception("expected label at line: {}".format(line_no)))




        elif state == ScalarParseState.REPLACE_BB_PLACEHOLDERS:
            vissue_key = parse_gluepoint(l)
            is_bb_key = vissue_key != None

            # vector gluepoint detected: perform gluing
            if is_bb_key and label != None:
                bb.extend(vector_bbs[vissue_key])
                print("gluing {} into label {}".format(l, label))
                start = "#Start {}".format(l)
                bbs.append(label)
                bbs.append(start)
                bbs.extend(bb)
                bbs.append("#End {}".format(l))

                bb = []
                label = None
                state = ScalarParseState.GET_BBS

            # non-vector block detected: pass basic block through
            # empty labels may precede this bb
            elif not is_bb_key and label != None:
                print("detected start of non-vector block at label: {}".format(label))
                bb.append(l)
                state = ScalarParseState.GET_NONVEC_BBS

            elif is_bb_key and label == None:
                raise(Exception("found placeholder name for vector block outside of its own label at line {}".format(line_no)))




        elif state == ScalarParseState.GET_NONVEC_BBS:
            # collect non-vector instruction
            if not is_label(l) and label != None:
                bb.append(l)

            # emit non-vector block
            elif is_label(l) and label != None:
                print("detected end of non-vector block for label: {}".format(label))
                bbs.append(label)
                bbs.extend(bb)
                bb = []
                state = ScalarParseState.REPLACE_BB_PLACEHOLDERS
                label = l

            elif not is_label(l) and label == None:
                raise(Exception("lost label for non-vector bb at line {}".format(line_no)))




        elif state == ScalarParseState.FOOTER:
            footer.append(l)

    # get last basic block, if any
    bbs.append(label)
    bbs.extend(bb)

    # rearrange components as follows
    return (
        header +
        [after_VECTOR_EPOCH[0]] +
        before_VECTOR_EPOCH +
        after_VECTOR_EPOCH[1:] +
        scalar_ret +
        after_DEVEC +
        bbs +
        footer)



if __name__ == "__main__":
    vector_file = open("vvadd_vector.s", "r")
    scalar_file = open("vvadd_scalar.s", "r")
    combined_file = open("vvadd_kernel.s", "w+")

    vector_code = vector_file.readlines()
    scalar_code = scalar_file.readlines()

    vector_blocks = read_vector_bbs("vvadd_execute_simd", vector_code)
    for block_name in vector_blocks.keys():
        block = vector_blocks[block_name]
        print("printing {} CFG of length {}".format(block_name, len(block)))
        print(pretty(block))

    combined_code = glue(scalar_code, vector_blocks)
    combined_file.write(pretty(combined_code))

    print("Finished.")
