import itertools
from enum import Enum, auto

def is_return_inst(inst):
  return ("jr" in inst and "ra" in inst) or inst == "ret"

class VectorParseState(Enum):
    SIFTING = auto()
    HEADER = auto()
    BODY = auto()
    RETURN_MANIP = auto()

header_block_start = "header_block_start"
header_block_end = "header_block_end"
vector_body_start = "vector_body_start"
vector_body_end = "vector_body_end"
vector_ret = "vector_ret"

# dissects vector assembly into 3 components:
# 1. header code
# 2. body code
# 3. return stack manipulation
def copy_vector_code(vector_file):
    header, body, ret_manip = [], [], []

    state = VectorParseState.SIFTING
    for l in vector_file.readlines():
        if state == VectorParseState.SIFTING:
            if header_block_start in l:
                state = VectorParseState.HEADER
            elif vector_body_start in l:
                state = VectorParseState.BODY
            elif vector_ret in l:
                state = VectorParseState.RETURN_MANIP

        elif state == VectorParseState.HEADER:
            if(header_block_end in l):
                state = VectorParseState.SIFTING
            else:
                header.append(l)
        elif state == VectorParseState.BODY:
            if(vector_body_end in l):
                state = VectorParseState.SIFTING
            else:
                body.append(l)

        elif(state == VectorParseState.RETURN_MANIP):
            if(is_return_inst(l)):
                state = VectorParseState.SIFTING
            else:
                ret_manip.append(l)

    return "\n".join(header), "\n".join(body), "\n".join(ret_manip)



class ScalarParseState(Enum):
    BODY = auto()
    RETURN_STACK_MANIP = auto()

vector_header_label = "vector_header_label"
vector_body_label = "vector_body_label"
vector_epoch_instr_format = ".insn i 0x77"
scalar_ret = "scalar_ret"

# dissects scalar assembly into 2 components:
# 1. the scalar code body
# 2. stack manipulations occuring immediately after a marker (placed immediately after fence) and before returning
def copy_scalar_code(scalar_file):
    state = ScalarParseState.BODY
    body, return_stack_manip = [], []
    for l in scalar_file.readlines():
        if state == ScalarParseState.BODY:
            if scalar_ret in l:
                state = ScalarParseState.RETURN_STACK_MANIP
            else:
                body.append(l)

        elif state == ScalarParseState.RETURN_STACK_MANIP:
            if is_return_inst(l):
                body.append(l)
                state = ScalarParseState.CODE
            else:
                return_stack_manip.append(l)

    return "\n".join(body), "\n".join(return_stack_manip)



#class CombinedParseState(Enum):
#    WRITING_SCALAR_CODE = auto()
#    WRITING_VECTOR_HEADER = auto()
#    COPYING_VECTOR_BODY = auto()
#    WRITING_RETURN_STACK_MANIP = auto()
#def glue(combined_file, scalar_file, vector_file):
#    vector_header, vector_body = copy_vector_code(vector_file)
#
#    state = LIFTING_VECTOR_EPOCH
#    lines_before_VECTOR_EPOCH = []
#    devec_inst = None
#    for l in scalar_file.readlines():
#        if state == LIFTING_VECTOR_EPOCH:
#            if vector_epoch_instr_format in l:
#                combined_file.write(lines_before_VECTOR_EPOCH)
#                combined_file.write(l)
#                state = WRITING_SCALAR_CODE
#            else:
#                lines_before_VECTOR_EPOCH.append(l)
#
#        elif state == WRITING_SCALAR_CODE:
#            if vector_header_label in l:
#                state = COPYING_VECTOR_HEADER
#            elif vector_body_label in l:
#                state = COPYING_VECTOR_BODY
#            elif devec_inst_format in l:
#                devec_inst = l
#                state = WRITING_RETURN_STACK_MANIP
#            else:
#                combined_file.write(l)
#
#        elif state == COPYING_VECTOR_HEADER:
#            combined_file.write(vector_header)
#            state = WRITING_SCALAR_CODE
#
#        elif state == COPYING_VECTOR_BODY:
#            combined_file.write(vector_body)
#            state = WRITING_SCALAR_CODE
#
#        elif state == WRITING_RETURN_STACK_MANIP:
#
#
#        else:
#            combined_file.write(l)
#    return combined_file

if __name__ == "__main__":
    vector_file = open("vvadd_vector.s", "r")
    scalar_file = open("vvadd_scalar.s", "r")
    combined_file = open("vvadd_combined.s", "w+")

    header, vector_body, vector_manip= copy_vector_code(vector_file)
    print("vector file dissection:")
    print("header part:")
    print(header)
    print("body part:")
    print(vector_body)
    print("return stack manipulation part:")
    print(vector_manip)

    scalar_body, scalar_manip = copy_scalar_code(scalar_file)
    print("scalar file dissection:")
    print("body part:")
    print(scalar_body)
    print("scalar file dissection:")
    print(scalar_manip)
    print("return stack manipulation part:")
    
    #glue(combined_file, scalar_file, vector_file)
