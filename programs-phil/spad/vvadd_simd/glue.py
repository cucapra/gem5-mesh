import itertools
from enum import Enum, auto

def read_file(filename):
    file = open(filename, "r")
    lines = []
    for line in file.readlines():
       lines.append(line)
    return lines

header_block_start = "header_block_start"
header_block_end = "header_block_end"
vector_body_start = "vector_body_start"
vector_body_end = "vector_body_end"

class ParseState(Enum):
    SIFTING = auto()
    HEADER = auto()
    BODY = auto()

def copy_vector_code(vector_filename):
    header = []
    body = []
    file = open(vector_filename, "r")
    state = ParseState.SIFTING
    for l in file.readlines():
        if(state == ParseState.SIFTING):
            if(header_block_start in l):
                state = ParseState.HEADER
            elif(vector_body_start in l):
                state = ParseState.BODY
        elif(state == ParseState.HEADER):
            if(header_block_end in l):
                state = ParseState.SIFTING
            else:
                header.append(l)
        elif(state == ParseState.BODY):
            if(vector_body_end in l):
                state = ParseState.SIFTING
            else:
                body.append(l)
    return "\n".join(header), "\n".join(body)


# def combine_with_scalar(combined_filename, scalar_filename, header, body):
#     file = open(scalar_filename, "r")
#     combined_file = open(combined_filename, "w+")


header, body = vector_code("vvadd_vector.s")

print("found header:\n{}".format(header))
print("found body:\n{}".format(body))
