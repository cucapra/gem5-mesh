# script to take a .S assembly file and figure out sync cnt

import os, subprocess, time, argparse, re, sys, math

# cmd line arguments
parser = argparse.ArgumentParser(description='Analyze stats file in a given directory')
parser.add_argument('infile')
parser.add_argument('--num-hardware-cntrs', default=5, help='Number of frame ctnrs in the hardware')
# parser.add_argument('--vec-dim', default=2, help='Vector dimension for square group')
parser.add_argument('--mesh-queue-slots', default=2, help='Size of queues in mesh')
parser.add_argument('--pipe-queue-slots', default=2, help='Size of queues between pipe stages')
parser.add_argument('--rob-slots', default=8, help='ROB size')
parser.add_argument('--pipe-stages-before-commit', default=4, help='Number of stages before instruction reserved in ROB (i.e. up to and including issue)')
args = parser.parse_args()

# opcodes of vissue type instructions
vissue_opcode = '0x6b'
devec_opcode = '0x2b'

# figure out the file names
infile = args.infile


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
    if (num >= 32):
        print('exceed issue len of 32. have ' + str(num))
        assert(False)
    return 'x' + str(num)

# define how to modify and what we are looking for
label_iden = '[a-zA-Z0-9_.]+'
vissue_regex = re.compile('.insn uj {0}, x0, ({1})'.format(vissue_opcode, label_iden))
devec_regex = re.compile('.insn uj {0}, x0, ({1})'.format(devec_opcode, label_iden))
anylabel_regex = re.compile('({0})[:]'.format(label_iden))
jump_regex = re.compile('j[\t\s]({0})'.format(label_iden))
comment_regex = re.compile('[#]')
call_regex = re.compile('call\t({0})'.format(label_iden))
remem_regex = re.compile('.insn i 0x1b, 0x2, x0, [a-z0-9]+, 0')
ret_regex = re.compile('ret')
beqz_regex = re.compile('beqz\t[a-z0-9]+,({})'.format(label_iden))
cond_branch_regexes = [
    re.compile('beq\t[a-z0-9]+,[a-z0-9]+,({})'.format(label_iden)),
    re.compile('bne\t[a-z0-9]+,[a-z0-9]+,({})'.format(label_iden)),
    re.compile('blt\t[a-z0-9]+,[a-z0-9]+,({})'.format(label_iden)),
    re.compile('bge\t[a-z0-9]+,[a-z0-9]+,({})'.format(label_iden)),
    re.compile('bltu\t[a-z0-9]+,[a-z0-9]+,({})'.format(label_iden)),
    re.compile('bgeu\t[a-z0-9]+,[a-z0-9]+,({})'.format(label_iden))
]
term_regex = re.compile('.insn i 0x1b, 0x7, x0, x0, 0')

cached_src_file = []

# key is orig line
# each element is { <orig_label>, <curr_label>, <count>}
vissue_table = {}

# load the file into lines
def load_file(fileName):
    lines = []
    with open(infile, 'r') as fin:
        for line in fin:
            lines.append(line)
    return lines

# find all vissues in the program and store in a table
def build_vissue_table():
    table = {}
    for line in cached_src_file:
        # check if line is vissue
        (is_vissue, label) = check_vissue(line)
        if (is_vissue):
            table[line] = { 'orig_label': label, 'label': label, 'count': 0 }
    return table

# check if line is a comment
def is_comment(line):
    match = comment_regex.search(line)
    if (match):
        return True
    else:
        return False

# check if the line is a vissue inst
def check_vissue(line):
    if (is_comment(line)):
        return (False, '')
    # check match
    match = vissue_regex.search(line)
    if (match):
        # get label to analyze
        val = match.group(1)
        return (True, val)
    else:
        return (False, '')

# check if the line is a devec inst
def check_devec(line):
    if (is_comment(line)):
        return (False, '')
    # check match
    match = devec_regex.search(line)
    if (match):
        # get label to analyze
        val = match.group(1)
        return (True, val)
    else:
        return (False, '')

# check if this is a label
def check_label(line):
    if (is_comment(line)):
        return (False, '')
    # check match
    match = anylabel_regex.search(line)
    if (match):
        # get label to analyze
        val = match.group(1)
        return (True, val)
    else:
        return (False, '')

# check if the line is a call pseudo-inst
def check_call(line):
    if (is_comment(line)):
        return (False, '')

    # check match
    match = call_regex.search(line)
    if (match):
        # get label to analyze
        val = match.group(1)
        return (True, val)
    else:
        return (False, '')

# check if the line is a ret pseudo-inst
def check_ret(line):
    if (is_comment(line)):
        return False

    # check match
    match = ret_regex.search(line)
    if (match):
        return True
    else:
        return False

def check_term(line):
    if (is_comment(line)):
        return False

    # check match
    match = term_regex.search(line)
    if (match):
        return True
    else:
        return False

def check_remem(line):
    if (is_comment(line)):
        return False

    # check match
    match = remem_regex.search(line)
    if (match):
        return True
    else:
        return False


# parse the file again to find where the label is and count how many instructions
# are in the basic block for that label
def find_vissue_count(label):
    label_regex = re.compile('(' + label + ':)')
    anychar_regex = re.compile('[a-z]')
    start_record = False
    cnt = 0
    for line in cached_src_file:
        # if we are recording increment count until next label
        if (start_record):
            # check if we should terminate b/c another label (or another jump if we hit that)
            (is_label, matched_label) = check_label(line)
            (is_jump, jump_label) = check_jump(line)
            (is_call, call_label) = check_call(line)
            (is_cond_branch, branch_label) = check_cond_branch(line)
            is_term = check_term(line)
            is_ret = check_ret(line)
            # recursion to explore function call count
            if (is_call):
                # print('call label: ' + call_label)
                cnt += 1 + find_vissue_count(call_label)
            # elif (is_label and has_valid_cond_branch(matched_label)):
            #     cnt += 1
            elif (is_term or is_ret):
                # print("Stopping at label " + line[0:-1] + " w/ cnt " + str(cnt))
                # cnt ret
                if (is_ret):
                    cnt += 1
                return cnt
            # ignore comments and blank lines (if the flag is set)
            elif (not is_label and not is_comment(line) and anychar_regex.search(line)):
                # print(line[0:-1])
                cnt += 1
        # if we're haven't start yet see if we can start due to the desired label
        else:
            match = label_regex.search(line)
            if (match):
                start_record = True
    return 0

# want to find blocks that use frames.
# don't care about other blocks b/c they can only REDUCE the number
# of open frames in flight (i.e., won't change the maximum)
def block_uses_frames(label):
    label_regex = re.compile('(' + label + ':)')
    anychar_regex = re.compile('[a-z]')
    start_record = False
    cnt = 0
    for line in cached_src_file:
        # start looking here
        if (start_record):
            if (check_remem(line)):
                return True
            elif(check_term(line)):
                return False
            
        # if we're haven't start yet see if we can start due to the desired label
        else:
            match = label_regex.search(line)
            if (match):
                start_record = True
    return 0

# check if any backedges to the given label
def has_backedge(label):
    # once we've found the label, if there are any jumps then it has a backedge
    found_label = False

    # depending on whether find jump label or this line first
    for line in cached_src_file:
        # if this line is a label check, if label comes
        if (not found_label):
            (is_label, line_label) = check_label(line)
            if (is_label and line_label == label):
                found_label = True

        # check if any jumps come after
        (is_jump, jump_label) = check_jump(line)
        if (is_jump and jump_label == label):
            # back edge
            if (found_label):
                return True
            # case if the jump is a forward edge??
            else:
                pass

    return False

# there is a valid conditional branch to this label
def has_valid_cond_branch(label):
    for line in cached_src_file:
        (is_branch, line_label) = check_cond_branch(line)
        if (is_branch and line_label == label):
            return True
    return False

# # find if any jumps to the label are backwards
# def has_back_jumps_to_label(label_line, label)
#     for line in cached_src_file:
#         if (line == label_line):
#             return True
#         (is_jump, jlabel) = check_jump(line)
#         if (is_jump and jlabel == label):


# check if this line is a jump
# ret (is_jump, is_forward, label)
def check_jump(line):
    if (is_comment(line)):
        return (False, '')
    match = jump_regex.search(line)
    if (match):
        jlabel = match.group(1)
        # check if forward or back branch
        return (True, jlabel)

    else:
        return (False, '')

# check if conditional branch from predication faking
# ret (is_cond_branch, branch_label)
def check_pred_branch(line):
    if (is_comment(line)):
        return (False, '')
    match = beqz_regex.search(line)
    if (match):
        jlabel = match.group(1)
        return (True, jlabel)
    else:
        return (False, '')

# check if normal conditional branch that we actually want to use
def check_cond_branch(line):
    if (is_comment(line)):
        return (False, '')

    for r in cond_branch_regexes:
        match = r.search(line)
        if (match):
            jlabel = match.group(1)
            return (True, jlabel)
    return (False, '')


# check if label is pointed to by a vissue
def is_vissue_label(label):
    for k,v in vissue_table.items():
        if (v['label'] == label):
            return True
    return False

# check if a label was originally point to by a vissue
def was_vissue_label(label):
    for k,v in vissue_table.items():
        if (v['orig_label'] == label):
            return True
    return False

# replacement for removed line, keep curr instructions as reference
def removed_line(line):
    return '\t# removed: {}'.format(line)

def count_vissue(vissue_line):
    label = vissue_table[vissue_line]['label']
    vissue_table[vissue_line]['count'] = find_vissue_count(label)

def ceiledDiv(a, b):
    return int(math.ceil(float(a) / float(b)))

# determines how many prefetch frame can be initially fetch to guarentee worst case sync
# output
#   returns number of frames
# inputs
#   vec_dim - single side dimension of the vector group (assumes square)
#   mesh_queue_slots - how big the mesh queue is between cores
#   rob_slots - how big the rob is
#   pipe_slots - number of queue spots between pipeline stages
#   pipe_stages_up_to_commit - how many stages there are before we insert into rob (also includes commit stage currently)
#   instructions_per_frame - number of instructions in the smallest vissue block using a prefetched frame
#   num_hardware_cntrs - number of seperate frames we can count for at the same time
def det_sync_frames(vec_dim, mesh_queue_slots, rob_slots, pipe_slots, pipe_stages_up_to_commit, instructions_per_frame, num_hardware_cntrs):
    sum_pipe_stages = pipe_stages_up_to_commit * pipe_slots + rob_slots

    queue_spots = (2 * vec_dim - 1) * mesh_queue_slots + sum_pipe_stages
    nr = ceiledDiv(queue_spots, instructions_per_frame)
    print('frames possible in group {0} = {1} / {2}'.format(nr, queue_spots, instructions_per_frame))

    init_frames = num_hardware_cntrs - mesh_queue_slots - nr

    return init_frames


# open the input and output asm files
#

# cache the asm file
cached_src_file = load_file(infile)

# find all vissue instructions
vissue_table = build_vissue_table()

# if there are no vissues in this file then just end
if len(vissue_table.items()) == 0:
    exit()

# count the number instructions for vissue (basic block)
# only care about vissue blocks with FRAME_START/REMEMs
for k,v in vissue_table.items():
    count_vissue(k)

# # just print all
# for k,v in vissue_table.items():
#     print(k)
#     print(vissue_table[k]['count'])

# find minimum size of a vissue block that contains frames -> maximum number of open frames
# TODO a real compiler would be able to figure out per prefetch/vissue block pair, but this prob good for now
min_frame_size = 10000000
for k,v in vissue_table.items():
    if (block_uses_frames(v['label'])):
        if (min_frame_size > v['count']):
            min_frame_size = v['count']

# print("min frame size: " + str(min_frame_size))

# do for vlen4 vlen16 squares

vlen_side = [ 2 , 4 ]

print("---------------------------------------------------------------------------\n")

for sl in vlen_side:

    # do sync calculation using mesh size, frame size, etc..
    init_frames = det_sync_frames(
        int(sl),
        int(args.mesh_queue_slots),
        int(args.rob_slots),
        int(args.pipe_queue_slots),
        int(args.pipe_stages_before_commit),
        min_frame_size,
        int(args.num_hardware_cntrs)
        )

    print("Vlen{}: Guaranteed Synced if Init Frames <= {} ... or your money back\n".format(str(sl*sl), str(init_frames)))

print("---------------------------------------------------------------------------")
