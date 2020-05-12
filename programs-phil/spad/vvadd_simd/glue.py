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

def is_DEVEC(inst):
    return ".insn uj 0x2b" in inst

def is_VECTOR_EPOCH_inst(inst):
    return ".insn i 0x77" in inst

def is_label(inst):
    return "." == inst[0] and ":" == inst[-1]

def is_footer_start(inst):
    return ".size " in inst

def change_label_prefix(old_prefix, new_prefix, instr):
    return ("."+new_prefix+instr[2:]
                if is_label(instr)
                else instr.replace("."+old_prefix,"."+new_prefix))

def pretty(code):
    return "\n".join(["\t"+l
        if not is_label(l)
        else l for l in code])

def strip_whitespace_and_comments(instr):
    return instr.split("#",1)[0].strip()

def vector_preprocess(code):
    with_line_nos = list(enumerate(code))
    pass1 = apply_transformation(strip_whitespace_and_comments, with_line_nos)
    pass2 = apply_filter(lambda instr: instr != "", pass1)
    pass3 = apply_filter(lambda instr: not (is_label(instr) or
                                           (is_jump(instr) and not is_return_inst(instr))), pass2)
    return pass3

def scalar_preprocess(code):
    with_line_nos = list(enumerate(code))
    pass1 = apply_transformation(strip_whitespace_and_comments, with_line_nos)
    pass2 = apply_filter(lambda instr: instr != "", pass1)
    pass3 = apply_transformation(lambda instr: change_label_prefix("L", "SCALAR", instr), pass2)
    return pass3

def apply_transformation(f, code_with_line_no):
    apply_instr_transform = lambda f, line: (line[0], f(line[1]))
    return [apply_instr_transform(f,line)  for line in code_with_line_no]

def apply_filter(f, code_with_line_no):
    return list(filter(lambda code_line: f(code_line[1]), code_with_line_no))


class VectorParseState(Enum):
    SIFTING = auto()
    INIT = auto()
    BODY = auto()
    RETURN_BLOCK = auto()
    EXTRA_BBS = auto()

def print_parsing_bb(bb_label):
    print("parsing bb {}...".format(bb_label))

kernel_name = "vvadd_execute_simd"
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
    for (line_no, l) in vector_code:
        if state == VectorParseState.SIFTING:
            if l == kernel_name + ":":
                print("found vector_init (delimited by {}:) at line {}".format(kernel_name, line_no))
                state = VectorParseState.INIT

        elif state == VectorParseState.INIT:
            if l == vector_init:
                continue
            elif l == vector_body:
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
            if scalar_return in l:
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
            is_bb_key = l in vector_bbs.keys()

            # vector block detected: perform gluing
            if is_bb_key and label != None:
                bb.extend(vector_bbs[l])
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



# assume assembly stripped of whitespace and comments
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
