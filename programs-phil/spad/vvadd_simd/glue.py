import itertools, argparse
from enum import Enum, auto

def is_return_inst(inst):
    inst_components = inst.split()
    opcode = inst_components[0]
    arg = inst_components[1] if len(inst_components) >= 2 else None
    return opcode == "ret" or (opcode == "jr" and arg == "ra")

def is_jump(inst):
    jump_opcodes = ["bgt", "beqz", "bnez", "jr", "j"]
    return inst.split()[0] in jump_opcodes

def is_whitespace_or_comment(inst):
    stripped = inst.strip()
    return stripped == "" or stripped[0] == "#"

def is_DEVEC(inst):
    return ".insn uj 0x2b" in inst

def is_VECTOR_EPOCH_inst(inst):
    return ".insn i 0x77" in inst

def is_label(inst):
    return "." == inst[0] and ":" == inst[-1]

def is_footer_start(inst):
    return ".size " in inst

def remove_whitespace_and_comments(code):
    pass1 = filter(lambda l: not is_whitespace_or_comment(l), code)
    pass2 = map(lambda l: l.strip(), pass1)
    return list(pass2)

def change_label_prefix(old_prefix, new_prefix, code):
    return ["."+new_prefix+l[2:]
                if is_label(l)
                else l.replace("."+old_prefix,"."+new_prefix)
                    for l in code]

class VectorParseState(Enum):
    SIFTING = auto()
    INIT = auto()
    BODY = auto()
    RETURN_BLOCK = auto()
    EXTRA_BBS = auto()

def print_parsing_bb(bb_label):
    print("parsing bb {}...".format(bb_label))

def read_vector_bbs(raw_vector_code):
    vector_init = "vector_init"
    vector_body = "vector_body"
    vector_return = "vector_return"

    vector_code = vector_preprocess(raw_vector_code)

    # dissects vector assembly into the following:
    # init cfg
    init = []
    # body cfg
    body = []
    # return stack manipulation cfg
    ret_block = []

    state = VectorParseState.SIFTING
    for l in vector_code:
        if state == VectorParseState.SIFTING:
            if l == vector_init:
                print("found vector_init")
                state = VectorParseState.INIT

        elif state == VectorParseState.INIT:
            if l == vector_body:
                print("found vector_body")
                state = VectorParseState.BODY
            else:
                init.append(l)

        elif state == VectorParseState.BODY:
            if l == vector_return:
                print("found vector_return")
                state = VectorParseState.RETURN_BLOCK
            else:
                body.append(l)

        elif state == VectorParseState.RETURN_BLOCK:
            if(is_return_inst(l)):
                print("found return jump")
                state = VectorParseState.SIFTING
            else:
                ret_block.append(l)

    blocks = {  vector_init:init,
                vector_body:body,
                vector_return:ret_block}

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
    REPLACE_BB_PLACEHOLDERS = auto()
    FOOTER = auto()
    # SIFT_BB = auto()
    # NON_VECTOR_BB = auto()

def glue(raw_scalar_code, vector_bbs):
    scalar_return = "scalar_return"
    kernel_name = "vvadd_execute_simd"

    scalar_code = scalar_preprocess(raw_scalar_code)

    # dissects scalar assembly into the following non-overlapping components:
    # interval notation: open, closed, half-open intervals
    # [start of file, kernel launch label)
    header = []
    # [kernel launch label, first VECTOR_EPOCH call)
    before_VECTOR_EPOCH = []
    # [first VECTOR_EPOCH call, first DEVEC call)
    after_VECTOR_EPOCH = []
    # [first DEVEC call, end of file]
    after_DEVEC = []
    # [scalar_ret label, "return, jump-like instruction"]
    scalar_ret = []
    # ("return, jump-like instruction", labels pointing to vector blocks]
    v_bbs = []
    # (return statement after labels pointing to vector blocks, rest of file]
    footer = []

    state = ScalarParseState.HEADER
    for l in scalar_code:
        if state == ScalarParseState.HEADER:
          if l.strip() == kernel_name + ":":
            before_VECTOR_EPOCH.append(l)
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
            if scalar_return in l:
                state = ScalarParseState.RETURN_STACK_MANIP
            else:
                after_DEVEC.append(l)

        # assumption: label happens right after return jump instr
        elif state == ScalarParseState.RETURN_STACK_MANIP:
            if is_return_inst(l):
                after_DEVEC.append(l)
                state = ScalarParseState.REPLACE_BB_PLACEHOLDERS
            else:
                scalar_ret.append(l)

        # elif state == ScalarParseState.REPLACE_BB_PLACEHOLDERS:
        #     if is_label(l):
        #         prevLabel = l
        #     elif l in vector_bbs.keys():
        #         assert(is_label(prevLabel))
        #         comment = "#Gluing {}-sized block, {}...".format(len(vector_bbs[l]), l)
        #         print(comment)
        #         v_bbs.extend([comment, prevLabel] + vector_bbs[l])
        #     else:
        #         assert(is_label(prevLabel))
        #         non_vector_bbs.add(prevLabel)
        #         prevLabel = None
        #         state = ScalarParseState.NON_VECTOR_BB
        #
        # elif state == ScalarParseState.NON_VECTOR_BB:
        #     if is_label(l):
        #         state = ScalarParseState.REPLACE_BB_PLACEHOLDERS
        #     else:
        #         non_vector_bbs.add(l)

        elif state == ScalarParseState.REPLACE_BB_PLACEHOLDERS:
            if is_label(l):
                v_bbs.append(l)
            elif l in vector_bbs.keys():
                bb = vector_bbs[l]
                print("Gluing {}-sized block: {}".format(len(bb), l))
                start = "#Start {}".format(l)
                v_bbs.append(start)
                v_bbs.extend(bb)
                v_bbs.append("#End {}".format(l))
            elif is_return_inst(l):
                state = ScalarParseState.FOOTER

        elif state == ScalarParseState.FOOTER:
            footer.append(l)

    # rearrange components as follows
    return (
        header +
        [after_VECTOR_EPOCH[0]] +
        before_VECTOR_EPOCH +
        after_VECTOR_EPOCH[1:] +
        scalar_ret +
        after_DEVEC +
        v_bbs +
        footer)



# assume assembly stripped of whitespace and comments
def pretty(code):
    return "\n".join(["\t"+l if not is_label(l) else l for l in code])

def vector_preprocess(code):
    pass1 = remove_whitespace_and_comments(code)
    return list(filter(lambda inst: not (is_label(inst) or
                                        (is_jump(inst) and not is_return_inst(inst))), pass1))

def scalar_preprocess(code):
    pass1 = remove_whitespace_and_comments(code)
    return change_label_prefix("L", "SCALAR", pass1)





if __name__ == "__main__":
    vector_file = open("vvadd_vector.s", "r")
    scalar_file = open("vvadd_scalar.s", "r")
    combined_file = open("vvadd_kernel.s", "w+")

    vector_code = vector_file.readlines()
    scalar_code = scalar_file.readlines()

    vector_blocks = read_vector_bbs(vector_code)
    for block_name in vector_blocks.keys():
        block = vector_blocks[block_name]
        print("printing {} CFG of length {}".format(block_name, len(block)))
        print(pretty(block))

    combined_code = glue(scalar_code, vector_blocks)
    combined_file.write(pretty(combined_code))

    print("Finished.")
