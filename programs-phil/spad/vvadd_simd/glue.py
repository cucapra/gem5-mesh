import itertools

def read_file(filename):
    file = open(filename, "r")
    lines = []
    for line in file.readlines():
       lines.append(line)
    return lines

lines = read_file("vvadd_vector.s")
def vissue_kernel_body(kernel_start):
    while_start = next(l for l in lines[kernel_start:]
                        if is_while_start(l))
    print(while_start)

def is_vissue(line):
    return ".insn" in line and "0x77" in line

def is_while_start(line):
    return 

vissue_kernel_body(7)
