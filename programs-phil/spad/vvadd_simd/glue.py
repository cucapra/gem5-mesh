import itertools
from enum import Enum, auto

def is_return_inst(inst):
    stripped = inst.strip()
    return (
        ("jr" in stripped and "ra" in stripped) or
        stripped == "ret")

def is_whitespace(inst):
    stripped = inst.strip()
    return stripped == "" or stripped[0] == "#"

def is_DEVEC(inst):
  return ".insn uj 0x2b" in inst

def is_VECTOR_EPOCH_inst(inst):
  return ".insn i 0x77" in inst

def is_label(inst):
  return ".L" == inst[0:2]

def is_placeholder(inst):
  return (
    vector_init_label in inst or
    vector_body_label in inst or
    vector_ret_label in inst)


class VectorParseState(Enum):
    SIFTING = auto()
    INIT = auto()
    BODY = auto()
    RETURN_MANIP = auto()



init_block_start = "init_block_start"
init_block_end = "init_block_end"
vector_body_start = "vector_body_start"
vector_body_end = "vector_body_end"
vector_return = "vector_return"

def read_vector_cfgs(vector_file):
    # dissects vector assembly into the following:
    # init code
    init = []
    # body code
    body = []
    # return stack manipulation
    ret_manip = []

    state = VectorParseState.SIFTING
    for l in vector_file.readlines():
        if is_whitespace(l): continue
        if state == VectorParseState.SIFTING:
            if init_block_start in l:
                state = VectorParseState.INIT
            # elif vector_body_start in l:
            #     state = VectorParseState.BODY
            elif vector_return in l:
                state = VectorParseState.RETURN_MANIP

        elif state == VectorParseState.INIT:
            # if(init_block_end in l):
            #     state = VectorParseState.SIFTING
            if vector_body_start in l:
                state = VectorParseState.BODY
            elif (not (init_block_end in l or
                        "beqz" in l)):
                init.append(l)

        elif state == VectorParseState.BODY:
            if vector_body_end in l:
                state = VectorParseState.SIFTING
            else:
                body.append(l)

        elif(state == VectorParseState.RETURN_MANIP):
            if(is_return_inst(l)):
                state = VectorParseState.SIFTING
            else:
                ret_manip.append(l)
    return init, body, ret_manip



class ScalarParseState(Enum):
    HEADER = auto()
    BEFORE_VECTOR_EPOCH = auto()
    AFTER_VECTOR_EPOCH = auto()
    AFTER_DEVEC = auto()
    RETURN_STACK_MANIP = auto()
    #REPLACE_PLACEHOLDER_BB = auto()

scalar_ret_label = "scalar_ret"
kernel_name = "vvadd_execute_simd"

def scalar_control_flow(scalar_file):
  return lambda x:
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
    for l in scalar_file.readlines():
        if is_whitespace(l): continue

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
                print("found scalar ret")
                after_DEVEC.append(l)
                state = ScalarParseState.AFTER_DEVEC
            else:
                scalar_ret.append(l)
         

      # rearrange components as follows
    return (
        header +
        [after_VECTOR_EPOCH[0]] +
        before_VECTOR_EPOCH +
        after_VECTOR_EPOCH[1:] +
        scalar_ret +
        after_DEVEC )

vector_init_label = "vector_init_label"
vector_body_label = "vector_body_label"
vector_ret_label = "vector_return_label"

def glue(vector_components, scalar_code):
    vector_init, vector_body, vector_ret_manip = vector_components

    combined = []
    for l in scalar_code:
         if vector_init_label in l:
            combined.extend(vector_init)
         elif vector_body_label in l:
            combined.extend(vector_body)
         elif vector_ret_label in l:
            combined.extend(vector_ret_manip)
         else:
            combined.append(l)

    return "\n".join(combined)

if __name__ == "__main__":
    vector_file = open("vvadd_vector.s", "r")
    scalar_file = open("vvadd_scalar.s", "r")
    combined_file = open("vvadd_combined.s", "w+")

    vector_components = copy_vector_code(vector_file)
    scalar_components = copy_scalar_code(scalar_file)
    combined_file.write(glue(vector_components, scalar_components))
    print("Finished.")
