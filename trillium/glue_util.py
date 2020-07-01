import regex
from glue_log import *
from enum import Enum, auto


FUNC_PREFIX = 'tril_'


# utilities for parsing instructions
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

def parse_label(inst):
    m = regex.match("\.(\w+):", inst)
    return m.group(1) if m else None
    #return "." == inst[0] and ":" == inst[-1]

def is_func_end(inst):
    return regex.match(r'\.size\b', inst)

class RV_Inst(Enum):
    JUMP = auto()
    FOOTER_START = auto()

def parse_jump_inst(inst):
    jump_inst_match = regex.compile("j\s+\.(\S+)").match(inst)
    if jump_inst_match:
        return RV_Inst.JUMP, jump_inst_match.group(1)
    else:
        return None

def parse_footer(inst):
    footer_match = regex.compile(".size").match(inst)
    return RV_Inst.FOOTER_START if footer_match else None

class TrilliumAsmDelim(Enum):
    UNTIL_NEXT = auto()
    BEGIN = auto()
    IF_BEGIN = auto()
    END = auto()
    RETURN = auto()


def parse_delim(inst):
    delim_prefix = "trillium\s+vissue_delim"
    until_next_delim_match = regex.compile(delim_prefix + "\s+until_next\s+(\w+)").match(inst)
    begin_delim_match = regex.compile(delim_prefix + "\s+begin\s+(\w+)").match(inst)
    end_delim_match = regex.compile(delim_prefix + "\s+end(\s+\w+)?").match(inst)
    if_begin_delim_match = regex.compile(delim_prefix + "\s+if_begin\s+(\w+)").match(inst)
    return_delim_match = regex.compile(delim_prefix + "\s+return\s+(\w+)").match(inst)
    if until_next_delim_match:
        return TrilliumAsmDelim.UNTIL_NEXT, until_next_delim_match.group(1)
    elif begin_delim_match:
        return TrilliumAsmDelim.BEGIN, begin_delim_match.group(1)
    elif end_delim_match:
        return TrilliumAsmDelim.END, (end_delim_match.group(1) or '').strip()
    elif if_begin_delim_match:
        return TrilliumAsmDelim.IF_BEGIN, if_begin_delim_match.group(1)
    elif return_delim_match:
        return TrilliumAsmDelim.RETURN, return_delim_match.group(1)
    else:
        return None

def parse_gluepoint(inst):
    gluepoint_prefix = "trillium glue_point"
    gluepoint_match = regex.compile(gluepoint_prefix + " (\w+)").match(inst)
    if gluepoint_match:
        return gluepoint_match.group(1)
    else:
        return None

# utilities for preprocessing scalar/vector assembly files
def vector_preprocess(code):
    with_line_nos = list(enumerate(code))
    pass1 = apply_transformation(strip_whitespace_and_comments, with_line_nos)
    pass2 = apply_filter(lambda instr: instr != "", pass1)
    #pass3 = apply_filter(lambda instr: not (is_jump(instr) and not is_return_inst(instr)), pass2)
    pass3 = rename_labels("L", "VEC", pass2)
    return pass3

def scalar_preprocess(code):
    with_line_nos = list(enumerate(code))
    pass1 = apply_transformation(strip_whitespace_and_comments, with_line_nos)
    pass2 = apply_filter(lambda instr: instr != "", pass1)
    pass3 = rename_labels("L", "SCALAR", pass2)
    return pass3

def rename_labels(curr_prefix, new_prefix, code):
    label_regex = regex.compile(r"(.*)\.({})(\d+)".format(curr_prefix))
    sub_regex = "{1}." + new_prefix + "{3}"
    log.info("applying regex for relabeling: {}".format(r"(.*)\.({})(\d+)".format(curr_prefix)))
    renamed_code = []
    for line_no, instr in code:
        relabeled_instr = label_regex.subf(sub_regex, instr)
        renamed_code.append((line_no, relabeled_instr))
        if relabeled_instr != instr:
            log.info("relabeling at line {}: transformed {} to {}".format(str(line_no), instr, relabeled_instr))

    return renamed_code

def change_label_prefix(old_prefix, new_prefix, instr):
    return ("."+new_prefix+instr[2:]
                if parse_label(instr)
                else instr.replace("."+old_prefix,"."+new_prefix))

def strip_whitespace_and_comments(instr):
    return instr.split("#",1)[0].strip()


def apply_transformation(f, code_with_line_no):
    apply_instr_transform = lambda f, line: (line[0], f(line[1]))
    return [apply_instr_transform(f,line)  for line in code_with_line_no]

def apply_filter(f, code_with_line_no):
    return list(filter(lambda code_line: f(code_line[1]), code_with_line_no))




# pretty print list of assembly instructions
def pretty(code):
    return "\n".join(["\t"+l
        if not parse_label(l)
        else l for l in code])

def is_kernel_func_label(l):
    """Check whether a line of assembly is the label for a kernel function, as
    indicated by the naming convention. Return the function name if so and
    `None` otherwise.
    """
    m = regex.match(r'^({}\w+):$'.format(FUNC_PREFIX), l.strip())
    if m:
        return m.group(1)
    else:
        return None
