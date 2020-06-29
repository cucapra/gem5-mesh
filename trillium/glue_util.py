import re
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

def is_label(inst):
    return re.compile("\.\S+:").match(inst) != None
    #return "." == inst[0] and ":" == inst[-1]

def is_footer_start(inst):
    return ".size " in inst

class RV_Inst(Enum):
    JUMP = auto()
    FOOTER_START = auto()

def parse_jump_inst(inst):
    jump_inst_match = re.compile("j\s+\.(\S+)").match(inst)
    if jump_inst_match:
        return RV_Inst.JUMP, jump_inst_match.group(1)
    else:
        return None

def parse_label(inst):
    label_match = re.compile("\.(\S+):").match(inst)
    return label_match.group(1) if label_match else None

def parse_footer(inst):
    footer_match = re.compile(".size").match(inst)
    return RV_Inst.FOOTER_START if footer_match else None

class TrilliumAsmDelim(Enum):
    UNTIL_NEXT = auto()
    BEGIN = auto()
    END = auto()
    RETURN = auto()


def parse_delim(inst):
    delim_prefix = "trillium\s+vissue_delim"
    until_next_delim_match = re.compile(delim_prefix + "\s+until_next\s+(\w+)").match(inst)
    begin_delim_match = re.compile(delim_prefix + "\s+begin\s+(\w+)").match(inst)
    end_delim_match = re.compile(delim_prefix + "\s+end").match(inst)
    return_delim_match = re.compile(delim_prefix + "\s+return\s+(\w+)").match(inst)
    if until_next_delim_match:
        return TrilliumAsmDelim.UNTIL_NEXT, until_next_delim_match.group(1)
    elif begin_delim_match:
        return TrilliumAsmDelim.BEGIN, begin_delim_match.group(1)
    elif end_delim_match:
        return TrilliumAsmDelim.END#, end_delim_match.group(1)
    elif return_delim_match:
        return TrilliumAsmDelim.RETURN, return_delim_match.group(1)
    else:
        return None

def parse_gluepoint(inst):
    gluepoint_prefix = "trillium glue_point"
    gluepoint_match = re.compile(gluepoint_prefix + " (\w+)").match(inst)
    if gluepoint_match:
        return gluepoint_match.group(1)
    else:
        return None

# utilities for preprocessing scalar/vector assembly files
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
    # This pass renames the labels in the scalar file so they don't conflict with labels that come from the vector code. 
    # It's currently broken because we don't update the corresponding references. 
    # But this will become a problem in the future when we try to use labels from the vector code, at which point we will need to rename one or the other.
    #pass3 = apply_transformation(lambda instr: change_label_prefix("L", "SCALAR", instr), pass2)
    return pass2

def change_label_prefix(old_prefix, new_prefix, instr):
    return ("."+new_prefix+instr[2:]
                if is_label(instr)
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
        if not is_label(l)
        else l for l in code])

def is_kernel_func_label(l):
    """Check whether a line of assembly is the label for a kernel function, as
    indicated by the naming convention. Return the function name if so and
    `None` otherwise.
    """
    m = re.match(r'^({}\w+):$'.format(FUNC_PREFIX), l.strip())
    if m:
        return m.group(1)
    else:
        return None
