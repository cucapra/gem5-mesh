from enum import Enum, auto

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
    return "." == inst[0] and ":" == inst[-1]

def is_footer_start(inst):
    return ".size " in inst

#TODO: implement begin/end
class TrilliumAsmDelim(Enum):
    UNTIL_NEXT = auto()
    RETURN = auto()

def parse_delim(inst):
    inst_comp = inst.split()
    if sublist("trillium vissue_delim until_next".split(), inst_comp):
        return TrilliumAsmDelim.UNTIL_NEXT, inst_comp[-1]
    elif sublist("trillium vissue_delim return".split(), inst_comp):
        return TrilliumAsmDelim.RETURN, inst_comp[-1]
    else:
        return None

def parse_gluepoint(inst):
    inst_comp = inst.split()
    if sublist("trillium glue_point".split(), inst_comp):
        return inst_comp[-1]
    else:
        return None

# thanks to https://stackoverflow.com/questions/35964155/checking-if-list-is-a-sublist
def sublist(lst1, lst2):
   ls1 = [element for element in lst1 if element in lst2]
   ls2 = [element for element in lst2 if element in lst1]
   return ls1 == ls2


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
    pass3 = apply_transformation(lambda instr: change_label_prefix("L", "SCALAR", instr), pass2)
    return pass3

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

