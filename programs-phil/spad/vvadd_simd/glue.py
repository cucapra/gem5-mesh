import itertools
from enum import Enum, auto

def is_return_inst(inst):
    stripped = inst.strip()
    return (
        ("jr" in stripped and "ra" in stripped) or
        stripped == "ret")

def is_whitespace_or_comment(inst):
    stripped = inst.strip()
    return stripped == "" or stripped[0] == "#"

def is_DEVEC(inst):
    return ".insn uj 0x2b" in inst

def is_VECTOR_EPOCH_inst(inst):
    return ".insn i 0x77" in inst

def is_label(inst):
    return "." == inst[0] and ":" == inst[-1]

def is_placeholder(inst):
    return (
        vector_init_label in inst or
        vector_body_label in inst or
        vector_ret_label in inst)

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

def vector_preprocess(code):
    pass1 = remove_whitespace_and_comments(code)
    return list(filter(lambda l: not is_label(l), pass1))

def scalar_preprocess(code):
    pass1 = remove_whitespace_and_comments(code)
    return change_label_prefix("L", "SCALAR", pass1)





class VectorParseState(Enum):
    SIFTING = auto()
    INIT = auto()
    BODY = auto()
    RETURN_MANIP = auto()
    EXTRA_BBS = auto()

def print_cfg_parse(cfg_label):
    print("parsing cfg {}...".format(cfg_label))

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
    # extra labels shared among cfgs
    extra_bbs = []

    prevState = VectorParseState.SIFTING
    state = VectorParseState.SIFTING
    for l in vector_code:
        prevState = state
        if state == VectorParseState.SIFTING:
            if l == init_block_start:
                print_cfg_parse(init_block_start)
                state = VectorParseState.INIT
            # elif vector_body_start in l:
            #     state = VectorParseState.BODY
            elif l == vector_return:
                print_cfg_parse(vector_return)
                state = VectorParseState.RETURN_MANIP

            elif is_label(l):
               extra_bbs.append(l)
               state = VectorParseState.EXTRA_BBS

        elif state == VectorParseState.INIT:
            # if(init_block_end in l):
            #     state = VectorParseState.SIFTING
            if l == vector_body_start:
                print_cfg_parse(vector_body_start)
                state = VectorParseState.BODY
            elif (not (init_block_end in l or
                        "beqz" in l)):
                init.append(l)

        elif state == VectorParseState.BODY:
            if l == vector_body_end:
                print_cfg_parse(vector_body_end)
                state = VectorParseState.SIFTING
            else:
                body.append(l)

        elif state == VectorParseState.RETURN_MANIP:
            if(is_return_inst(l)):
                print_cfg_parse(vector_return)
                state = VectorParseState.SIFTING
            else:
                ret_manip.append(l)
                state = VectorParseState.SIFTING

        elif state == VectorParseState.EXTRA_BBS:
            if not is_footer_start(l):
                extra_bbs.append(l)
            else:
                break

    return {vector_init_label:init,
            vector_body_label:body,
            vector_ret_label:ret_manip,
            "extra":extra_bbs}



class ScalarParseState(Enum):
    HEADER = auto()
    BEFORE_VECTOR_EPOCH = auto()
    AFTER_VECTOR_EPOCH = auto()
    AFTER_DEVEC = auto()
    RETURN_STACK_MANIP = auto()
    REPLACE_PLACEHOLDER_CFGS = auto()

def glue(control_flow, cfgs):
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
    for l in control_flow:
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
                state = ScalarParseState.REPLACE_PLACEHOLDER_CFGS
            else:
                scalar_ret.append(l)

        elif state == ScalarParseState.REPLACE_PLACEHOLDER_CFGS:
            if l in cfgs.keys():
                after_DEVEC.extend(cfgs[l])
            else:
               after_DEVEC.append(l)

      # rearrange components as follows
    return (
        header +
        [after_VECTOR_EPOCH[0]] +
        before_VECTOR_EPOCH +
        after_VECTOR_EPOCH[1:] +
        scalar_ret +
        after_DEVEC +
        cfgs["extra"])


# assume assembly stripped of whitespace and comments
def pretty(code):
    return "\n".join(["\t"+l if not is_label(l) else l for l in code])

if __name__ == "__main__":
    vector_file = open("vvadd_vector.s", "r")
    scalar_file = open("vvadd_scalar.s", "r")
    combined_file = open("vvadd_combined.s", "w+")

    vector_code = vector_preprocess(vector_file.readlines())
    scalar_code = scalar_preprocess(scalar_file.readlines())

    vector_cfgs = read_vector_cfgs(vector_code)
    for cfg in vector_cfgs.values():
        print("printing a vector CFG of length {}".format(len(cfg)))
        print(pretty(cfg))

    combined_code = glue(scalar_code, vector_cfgs)
    combined_file.write(pretty(combined_code))

    print("Finished.")
