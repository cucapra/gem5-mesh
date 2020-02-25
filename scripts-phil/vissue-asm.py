# script to take a .S assembly file and insert the correct length for the vissue instructions

import os, subprocess, time, argparse, re, sys

# cmd line arguments
# parser = argparse.ArgumentParser(description='Analyze stats file in a given directory')
# parser.add_argument('--infile', default='/home/pbb59/hammerblade/gem5/programs-phil/spad/vvadd/vvadd.s', help='Path of .S file you want to modify')
# parser.add_argument('--outfile', default='', help='Path of modified .S file. Defaults to <infile>.S (removes .s)')
# args = parser.parse_args()

# infile = args.infile
# if (args.outfile == ''):
#     outfile = args.infile[0:-2] + '.S'
# else:
#     outfile = args.outfile

# figure out the file names
infile = sys.argv[1]
outfile = sys.argv[2]

# the assemble won't accept a number as the register value, so have to put in 
# a riscv register corresponding to the number
# later riscv will interpret the cnt based on this register
# regs = [  
#     "zero", "ra", "sp", "gp",
#     "tp", "t0", "t1", "t2",
#     "s0", "s1", "a0", "a1",
#     "a2", "a3", "a4", "a5",
#     "a6", "a7", "s2", "s3",
#     "s4", "s5", "s6", "s7",
#     "s8", "s9", "s10", "s11",
#     "t3", "t4", "t5", "t6"
# ]
def get_reg(num):
    assert(num < 32)
    return 'x' + str(num)

# define how to modify and what we are looking for
vissue_regex = re.compile('.insn uj 0x6b, x0, ([a-zA-Z0-9_.]+)')

# create replacement for old vissue inst
def construct_vissue(count, label):
    return '\t.insn uj 0x6b, {0}, {1}\n'.format(get_reg(count), label)

# parse the file again to find where the label is and count how many instructions
# are in the basic block for that label
def find_vissue_count(label):
    print("Find instructions for label " + label)
    label_regex = re.compile('(' + label + ':)')
    anylabel_regex = re.compile('[:]')
    comment_regex = re.compile('[#]')
    anychar_regex = re.compile('[a-z]')
    start_record = False
    cnt = 0
    with open(infile, 'r') as fin:
        for line in fin:
            # if we are recording increment count until next label
            if (start_record):
                # check if we should terminate b/c another label
                match = anylabel_regex.search(line)
                if (match):
                    print("Stopping at label " + line[0:-1] + " w/ cnt " + str(cnt))
                    return cnt
                # ignore comments and blank lines
                elif (not comment_regex.search(line) and anychar_regex.search(line)):
                    print(line[0:-1])
                    cnt += 1
            # if we're haven't start yet see if we can start due to the desired label
            else:
                match = label_regex.search(line)
                if (match):
                    start_record = True
    return 0

            

# check if the line is a vissue inst
def check_vissue(line):
    # check match
    match = vissue_regex.search(line)
    if (match):
        # get label to analyze
        val = match.group(1)
        return (True, val)
    else:
        return (False, '')

# check if inst is vissue and either replace or keep
def try_replace_inst(line):
    # first check if we should replace the line
    (is_vissue, label) = check_vissue(line)
    # if this is a vissue then we need to replace
    if (is_vissue):
        issue_len = find_vissue_count(label)
        return construct_vissue(issue_len, label)
    # if not a vissue just pass along
    else:
        return line


# open the input and output asm files
#

# foreach line copy each line to the output file
# unless we find a vissue, in which case we modify that line
with open(outfile, 'w+') as fout:
    with open(infile, 'r') as fin:
        for line in fin:
            new_line = try_replace_inst(line)
            fout.write(new_line)
        