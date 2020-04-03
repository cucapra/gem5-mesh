import itertools
from enum import Enum, auto

class VectorParseState(Enum):
    SIFTING = auto()
    HEADER = auto()
    BODY = auto()

header_block_start = "header_block_start"
header_block_end = "header_block_end"
vector_body_start = "vector_body_start"
vector_body_end = "vector_body_end"
def copy_vector_code(vector_filename):
    header = []
    body = []
    file = open(vector_filename, "r")
    state = VectorParseState.SIFTING
    for l in file.readlines():
        if(state == VectorParseState.SIFTING):
            if(header_block_start in l):
                state = VectorParseState.HEADER
            elif(vector_body_start in l):
                state = VectorParseState.BODY
        elif(state == VectorParseState.HEADER):
            if(header_block_end in l):
                state = VectorParseState.SIFTING
            else:
                header.append(l)
        elif(state == VectorParseState.BODY):
            if(vector_body_end in l):
                state = VectorParseState.SIFTING
            else:
                body.append(l)
    return "\n".join(header), "\n".join(body)



vector_header_label = "vector_header_label"
vector_body_label = "vector_body_label"
def combine_with_scalar(combined_filename, scalar_filename, header, body):
    scalar_file = open(scalar_filename, "r")
    combined_file = open(combined_filename, "w+")
    for l in scalar_file.readlines():
        if vector_header_label in l:
            combined_file.write(header)
        elif vector_body_label in l:
            combined_file.write(body)
        else:
            combined_file.write(l)
    return combined_file

if __name__ == "__main__":
    header, body = copy_vector_code("vvadd_vector.s")
    combine_with_scalar("vvadd_combined.s", "vvadd_scalar.s", header, body)
