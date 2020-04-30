import itertools
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
    RETURN_MANIP = auto()
    EXTRA_BBS = auto()

def print_parsing_bb(bb_label):
    print("parsing bb {}...".format(bb_label))

def read_vector_cfgs(vector_code):
    vector_init_label = "vector_init_label"
    vector_body_label = "vector_body_label"
    vector_ret_label = "vector_return_label"

    init_block_start = "init_block_start"
    init_block_end = "init_block_end"
    vector_body_start = "vector_body_start"
    vector_body_end = "vector_body_end"
    vector_return = "vector_return"

    # dissects vector assembly into the following:
    # init cfg
    init = []
    # body cfg
    body = []
    # return stack manipulation cfg
    ret_manip = []

    state = VectorParseState.SIFTING
    for l in vector_code:
        if state == VectorParseState.SIFTING:
            if l == init_block_start:
                print_parsing_bb(init_block_start)
                state = VectorParseState.INIT
            # elif vector_body_start in l:
            #     state = VectorParseState.BODY
            elif l == vector_return:
                print_parsing_bb(vector_return)
                state = VectorParseState.RETURN_MANIP
            else:
                continue

        elif state == VectorParseState.INIT:
            # if(init_block_end in l):
            #     state = VectorParseState.SIFTING
            if l == vector_body_start:
                print_parsing_bb(vector_body_start)
                state = VectorParseState.BODY
            elif (not (init_block_end in l or
                        "beqz" in l)):
                init.append(l)

        elif state == VectorParseState.BODY:
            if l == vector_body_end:
                print_parsing_bb(vector_body_end)
                state = VectorParseState.SIFTING
            else:
                body.append(l)

        elif state == VectorParseState.RETURN_MANIP:
            if(is_return_inst(l)):
                print_parsing_bb(vector_return)
                state = VectorParseState.SIFTING
            else:
                ret_manip.append(l)
                state = VectorParseState.SIFTING

    return {vector_init_label:init,
            vector_body_label:body,
            vector_ret_label:ret_manip}


class ScalarParseState(Enum):
    HEADER = auto()
    BEFORE_VECTOR_EPOCH = auto()
    AFTER_VECTOR_EPOCH = auto()
    AFTER_DEVEC = auto()
    RETURN_STACK_MANIP = auto()
    REPLACE_BB_PLACEHOLDERS = auto()

def glue(scalar_control_flow, vector_bbs):
    scalar_ret_label = "scalar_ret"
    kernel_name = "vvadd_execute_simd"

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

    state = ScalarParseState.HEADER
    for l in scalar_control_flow:
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
            if scalar_ret_label in l:
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

        elif state == ScalarParseState.REPLACE_BB_PLACEHOLDERS:
            if l in vector_bbs.keys():
                after_DEVEC.extend(vector_bbs[l])
            else:
               after_DEVEC.append(l)

      # rearrange components as follows
    return (
        header +
        [after_VECTOR_EPOCH[0]] +
        before_VECTOR_EPOCH +
        after_VECTOR_EPOCH[1:] +
        scalar_ret +
        after_DEVEC)



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
    combined_file = open("vvadd_combined.s", "w+")

    vector_code = vector_preprocess(vector_file.readlines())
    scalar_code = scalar_preprocess(scalar_file.readlines())

    vector_cfgs = read_vector_cfgs(vector_code)
    for cfg_name in vector_cfgs.keys():
        cfg = vector_cfgs[cfg_name]
        print("printing {} CFG of length {}".format(cfg_name, len(cfg)))
        print(pretty(cfg))

    combined_code = glue(scalar_code, vector_cfgs)
    combined_file.write(pretty(combined_code))

    print("Finished.")
