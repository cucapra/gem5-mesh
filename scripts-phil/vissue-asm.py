# script to take a .S assembly file and insert the correct length for the vissue instructions

import os, subprocess, time, argparse, re, sys

# opcodes of vissue type instructions
vissue_opcode = '0x6b'
devec_opcode = '0x2b'

# figure out the file names
infile = sys.argv[1]
outfile = sys.argv[2]
try:
    use_term = sys.argv[3]
    if (int(use_term) == 1):
        use_term = True
except:
    use_term = False

do_fancy_opt = True
if '_combined.s' in infile:
    do_fancy_opt = False
    print("Switching off most optimizations --Neil")

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
ret_regex = re.compile('ret')
beqz_regex = re.compile('beqz\t[a-z0-9]+,({})'.format(label_iden))

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

# create replacement for old vissue inst
def construct_vissue(opcode, count, label):
    return '\t.insn uj {0}, {1}, {2}\n'.format(opcode, get_reg(count), label)

def construct_terminator():
    return '\t.insn i 0x1b, 0x7, x0, x0, 0\n'

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

# parse the file again to find where the label is and count how many instructions
# are in the basic block for that label
def find_vissue_count(label, term_count = False):
    print("Find instructions for label " + label)
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
            is_ret = check_ret(line)
            # recursion to explore function call count
            if (is_call and not term_count):
                print('call label: ' + call_label)
                cnt += 1 + find_vissue_count(call_label)
            elif (is_label or is_jump or is_ret):
                print("Stopping at label " + line[0:-1] + " w/ cnt " + str(cnt))
                # cnt ret
                if (is_ret):
                    cnt += 1
                return cnt
            # ignore comments and blank lines (if the flag is set)
            elif (term_count or (not is_comment(line) and anychar_regex.search(line))):
                print(line[0:-1])
                cnt += 1
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

# check if conditional branch
# ret (is_cond_branch, branch_label)
def check_cond_branch(line):
    if (is_comment(line)):
        return (False, '')
    match = beqz_regex.search(line)
    if (match):
        jlabel = match.group(1)
        return (True, jlabel)
    else:
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

# try to merge blocks following a vissue label
# keep going until encounter a backedge or another vissue label
# IMPORTANT that gcc does not reorder blocks otherwise this won't work
def flatten_vissue(vissue_line):
    # find the label we are point to
    found_label = False
    for i in range(0, len(cached_src_file)):
        line = cached_src_file[i]
        # TODO doing str comparison would be better if each line had unique pointer could use to compare like LLVM!
        (is_label, line_label) = check_label(line)
        if (line_label == vissue_table[vissue_line]['label']):
            found_label = True
            print('found start label {}'.format(line))
            continue

        # remove labels and jumps unless backwards
        if (found_label):
            # handle if the instruction is a jump
            (is_jump, jlabel) = check_jump(line)
            if (is_jump):
                # important to remove line afterwards
                backedge_to_label = has_backedge(jlabel)
                if (backedge_to_label):
                    print('has backedge ' + line)
                    return
                else:
                    cached_src_file[i] = removed_line(line)
                    print('no backedge ' + line)
                    continue

            # handle if this line is for a label
            (is_label, label) = check_label(line)
            if (is_label):
                if (not is_vissue_label(label) and not has_backedge(label)):
                    cached_src_file[i] = removed_line(line)
                    print('found unused label {0}'.format(line))
                    continue
                else:
                    print('found terminating label ' + line)
                    return

# keep bumping label until either a vissue instruction is in front
# or at a label with a backedge
# also don't bump to a label that has already been explored
def adjust_vissue_label(vissue_line):
    # TODO maybe can define a nice iterator for this
    # where can start iteration from provided line rather than having to wait for label like this
    found_label = False
    for i in range(0, len(cached_src_file)):
        line = cached_src_file[i]
        # TODO doing str comparison would be better if each line had unique pointer could use to compare like LLVM!
        (is_label, line_label) = check_label(line)
        if (line_label == vissue_table[vissue_line]['label']):
            found_label = True
            print('found start adjust {}'.format(line))

            # if this label already has a backedge, stay there
            if (has_backedge(line_label)):
                print('stay at label b/c already backedge {}'.format(line))
                return

            continue

        if (found_label):
            # bump to next label
            (is_label, label) = check_label(line)
            if (is_label):
                if (is_vissue_label(label) or was_vissue_label(label)):
                    print('cant move to label because already taken ' + line)
                    return
                elif (has_backedge(label)):
                    print('move to new label ' + line)
                    vissue_table[vissue_line]['label'] = label
                    return

# insert lines into cache source files at the end of the block
def add_terminator(vissue_line):
    # determine which block we're looking for
    label = vissue_table[vissue_line]['label']
    # how many instruction are currently in the block
    # will put the terminator after this
    cnt = find_vissue_count(label, True)

    # find where the label starts in the file and insert after count
    for i in range(0, len(cached_src_file)):
        line = cached_src_file[i]
        (is_label, line_label) = check_label(line)
        if (line_label == vissue_table[vissue_line]['label']):
            # insert an terminator instruction after cnt and return
            print('inserting terminator @ cnt ' + str(cnt + 1))
            cached_src_file.insert(i + cnt + 1, construct_terminator())
            return

# start at vissue label and remove branches and associated labels
def remove_branches_from_block(vissue_line):
    # find the label we are point to
    found_label = False
    for i in range(0, len(cached_src_file)):
        line = cached_src_file[i]
        # try to find label, if we've already found a label and encounter another then we need to stop!
        (is_label, line_label) = check_label(line)
        if (line_label == vissue_table[vissue_line]['label']):
            found_label = True
            continue
        elif (is_label and found_label):
            print('stopping at label ' + line_label)
            return

        # remote branches in this block and assocaited labels
        if (found_label):
            (is_branch, branch_label) = check_cond_branch(line)
            if (is_branch):
                # remote this line and then search future lines for the label
                cached_src_file[i] = removed_line(line)
                print('remove branch ' + line)
                # for j in range(i + 1, len(cached_src_file)):
                #     line = cached_src_file[j]
                #     (is_label, label) = check_label(line)
                #     if (is_label and label == branch_label):
                #         print('remove branch ' + line)
                #         cached_src_file[j] = removed_line(line)
                #         break

def count_vissue(vissue_line):
    label = vissue_table[vissue_line]['label']
    vissue_table[vissue_line]['count'] = find_vissue_count(label)

# check if inst is vissue and either replace or keep
def try_replace_inst(line):
    # if have a vissue need to update
    if (line in vissue_table):
        return construct_vissue(vissue_opcode, vissue_table[line]['count'], vissue_table[line]['label'])
    else:
        # if it's a devec then replace count with one
        (is_devec, devec_label) = check_devec(line)
        if (is_devec):
            return construct_vissue(devec_opcode, 1, devec_label)
        else:
            # if not a vissue just pass along
            return line


# open the input and output asm files
#

# cache the asm file
cached_src_file = load_file(infile)

# find all vissue instructions
vissue_table = build_vissue_table()

# first need to resolve back loops. Want the behavior below
# L0:
#   init code
# L1:
#   more init code
# L2:
#   loop body
# ===========================================
# L0:
#   init code/more init code
# L2:
#   loop body

if do_fancy_opt:
    for k,v in vissue_table.items():
        adjust_vissue_label(k)

    # TODO this step produces a nice objdump, but gem5 not liking it
    # we need to modify code after the labels from the vissue table
    for k, v in vissue_table.items():
        flatten_vissue(k)

    # remove any branches in the block and associated labels
    # assume conditional branches never go up in our case
    for k,v in vissue_table.items():
        remove_branches_from_block(k)

# add terminators at the end of each block
if (use_term):
    for k,v in vissue_table.items():
        add_terminator(k)

# if not using terminator then eed to count
else:
    # count the number instructions for vissue (basic block)
    for k,v in vissue_table.items():
        count_vissue(k)

# foreach line copy each line to the output file
# unless we find a vissue, in which case we modify that line
with open(outfile, 'w+') as fout:
    for line in cached_src_file:
        new_line = try_replace_inst(line)
        fout.write(new_line)
