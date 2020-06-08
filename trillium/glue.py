from collections import OrderedDict
from glue_util import *
import itertools, argparse
from enum import Enum, auto

# 3 kinds of delimiters demarcate the boundaries of vissue blocks in vector code
# `until_next`: boundary immediately extends until the next delimiter is found (of any type)
#               the compiler will explode if no other `until_next` delimiter is found
#   Example: asm("trillium vissue_delim until_next vector_init");
#             ... vissue boundary of "vector_init" block ...
#             asm("trillium vissue_delim until_next vector_body");
# `begin`, `end`: explicitly marks the beginning (`begin`) and end (`end`) of the boundaries of a vissue block
# `return`: this delimiter MUST ALWAYS be placed before the `return` statements of both scalar and vector code.
#           they mark the boundary of a special vissue block whose boundary extends until the next "jump to return address"-ish instruction (e.g., `ret`, `jl ra`, etc)

class VectorParseState(Enum):
    START = auto()
    # INIT = auto()
    UNTIL_NEXT = auto()
    BEGIN_END = auto()
    RETURN = auto()
    JUNK = auto()

# NOTE: kernel_fun_name MUST equal the name of the trillium-asm Vector-SIMD function
def read_vector_bbs(kernel_fun_name, raw_vector_code):
    vector_code = vector_preprocess(raw_vector_code)

    # dissects vector assembly into the following:
    # # init cfg
    # init = []
    # # body cfg
    # body = []
    # # return stack manipulation cfg
    # ret_block = []

    # `trillium_init` is an implicit block containing stack setup code
    # it must be attached to another block, since it's not explicitly
    # handled at the language level.
    # we'll attach it the first block, i.e., the first user-specified block
    # OrderedDict lets us remember which block was read first
    curr_vissue_key = "trillium_init"
    blocks = OrderedDict([(curr_vissue_key, [])])

    # After parsing a `begin/end` block, we discard code until finding another delimiter.
    # For debugging purposes, we collect the code into "junk" blocks
    junk_prefix = "trillium_junk"
    junk_postfix = 0

    state = VectorParseState.START
    for (line_no, l) in vector_code:
        # this is the first state. Here, we look for the start label (using the function name),
        # and parse any assembly generated before the first delimiter as an `until_next` block,
        # called `trillium_init`.
        if state == VectorParseState.START:
            if l == kernel_fun_name + ":":
                print("found start label {} at line {}".format(kernel_fun_name, line_no))
                state = VectorParseState.UNTIL_NEXT

        # in this state, we've already seen an `until_next` delimiter,
        # so we're looking for any other delimiter.
        elif state == VectorParseState.UNTIL_NEXT:
            delim_parse = parse_delim(l)
            if delim_parse != None:
                print("parsed 'until_next'-delimited block: {}".format(curr_vissue_key))
                delim, new_vissue_key = delim_parse
                curr_vissue_key = new_vissue_key
                blocks[new_vissue_key] = []

                if delim == TrilliumAsmDelim.UNTIL_NEXT:
                    state = VectorParseState.UNTIL_NEXT
                elif delim == TrilliumAsmDelim.BEGIN:
                    state = VectorParseState.BEGIN_END
                elif delim == TrilliumAsmDelim.RETURN:
                    state = VectorParseState.RETURN
                else: raise Exception("unrecognized delim found: check parse_delim function")
            else:
                blocks[curr_vissue_key].append(l)

        # in this state, we've just seen the `begin` delimiter,
        # so we're looking for a matching `end`
        elif state == VectorParseState.BEGIN_END:
            delim_parse = parse_delim(l)
            if delim_parse == TrilliumAsmDelim.END:
                print("parsed `begin/end`-delimited block: {}".format(curr_vissue_key))

                # setup collection of "junk code" after `end` delim and before next delim,
                # for debugging purposes
                junk_vissue_key = junk_prefix + str(junk_postfix)
                blocks[junk_vissue_key] = []
                state = VectorParseState.JUNK
            elif delim_parse != None:
                raise Exception("expected `end` delimiter to match `begin` in line {}".format(line_no))
            else:
                blocks[curr_vissue_key].append(l)

        # in this state, we've just finished a begin/end block,
        # so we look for any delimiter, discarding code in the meantime.
        # (we actually collect this "junk code" instead for debugging purposes)
        elif state == VectorParseState.JUNK:
            delim_parse = parse_delim(l)

            if delim_parse != None:
                print("parsed junk block")
                junk_postfix += 1

                delim, new_vissue_key = delim_parse
                curr_vissue_key = new_vissue_key
                blocks[new_vissue_key] = []

                if delim == TrilliumAsmDelim.BEGIN:
                    state = VectorParseState.BEGIN_END
                elif delim == TrilliumAsmDelim.UNTIL_NEXT:
                    state = VectorParseState.UNTIL_NEXT
                elif delim == TrilliumAsmDelim.RETURN:
                    state = VectorParseState.RETURN

            else:
                junk_vissue_key = junk_prefix + str(junk_postfix)
                blocks[junk_vissue_key].append(l)



        # in this state, we've just seen a `return` delimiter,
        # so we're looking for a "return-like" assembly.
        elif state == VectorParseState.RETURN:
            if is_return_inst(l):
                print("parsed return block {}".format(curr_vissue_key))
                break #return block should be the last one
            else:
                blocks[curr_vissue_key].append(l)


    # prepend trillium_init block to the first block
    trillium_init_block = blocks["trillium_init"]
    first_vissue_key = list(blocks.keys())[1] #0 corresponds to "trillium_init"
    first_vissue_block = blocks[first_vissue_key]
    blocks[first_vissue_key] = trillium_init_block + first_vissue_block

    # insert terminator in each block
    terminator = ".insn i 0x1b, 0x7, x0, x0, 0"
    for b in blocks.values():
        b.append(terminator)

    return blocks


class ScalarParseState(Enum):
    HEADER = auto()
    BEFORE_VECTOR_EPOCH = auto()
    AFTER_VECTOR_EPOCH = auto()
    AFTER_DEVEC = auto()
    AFTER_RETURN_DELIM = auto()
    GLUE = auto()
    INDIRECT_SCALAR_RET_FOUND = auto()
    #RETURN_STACK_MANIP = auto()
    #GET_BBS = auto()
    #REPLACE_BB_PLACEHOLDERS = auto()
    #GET_NONVEC_BBS = auto()
    FOOTER = auto()
    # SIFT_BB = auto()
    # NON_VECTOR_BB = auto()

def glue(kernel_fun_name, raw_scalar_code, vector_bbs):

    scalar_code = scalar_preprocess(raw_scalar_code)

    # dissects scalar assembly into the following non-overlapping components:
    # interval notation: open, closed, or half-open intervals

    # [start of file, kernel launch label]
    header = []

    # (kernel launch label, first VECTOR_EPOCH call)
    before_VECTOR_EPOCH = []

    # [first VECTOR_EPOCH call, first DEVEC call)
    after_VECTOR_EPOCH_before_DEVEC = []

    # [first DEVEC call, scalar return delimiter]
    after_DEVEC_before_RET_DELIM = []

    # Now things gets conditionally non-contiguous.
    # scalar return cleanup assembly consists of the following potential assembly locations:
    # - all code immediately following the scalar `return` delimiter and before a jump instruction
    #   is scalar cleanup code (We assume no branching is emitted in that interval)
    # - if a jump to a label is found (instead of to the return address), the block under that label,
    #   excluding the return address jump, is scalar cleanup code 
    scalar_cleanup = []

    # All cores ultimately return to the scalar return address.
    # This can be found at the end of scalar cleanup code, described in the two cases above
    scalar_ret_inst = None

    # labels pointing to vector blocks
    labeled_vector_bbs = OrderedDict()
    labels = [] #stack of labels

    # labels pointing to scalar auxiliary blocks (not including scalar block that returns to jump address, if any)
    # (Note: OrderedDict helps us keep track of the last inserted label)
    aux_bbs = OrderedDict([("trillium_anon_aux_bb",[])])

    # non-instruction lines after all labels/blocks
    footer = []

    def glue_pieces():
        def dict_to_code(d):
            bbs = []
            for label, bb in d.items():
                bbs.append(".{}:".format(label))
                bbs.extend(bb)
            return bbs

        return (
            header +
            [after_VECTOR_EPOCH_before_DEVEC[0]] +
            before_VECTOR_EPOCH +
            after_VECTOR_EPOCH_before_DEVEC[1:] +
            ["# trillium: scalar stack cleanup begin"] +
            scalar_cleanup +
            ["# trillium: scalar stack cleanup end"] +
            after_DEVEC_before_RET_DELIM +
            ["# trillium: auxiliary blocks begin"] +
            dict_to_code(aux_bbs) +
            ["# trillium: auxiliary blocks end"] +
            ["# trillium: vector vissue blocks begin"] +
            dict_to_code(labeled_vector_bbs) +
            ["# trillium: vector vissue blocks end"] +
            ["# trillium: footer begin"] +
            footer +
            ["# trillium: footer end"])


    state = ScalarParseState.HEADER
    for (line_no, l) in scalar_code:

        if state == ScalarParseState.HEADER:
          if l.strip() == kernel_fun_name + ":":
            header.append(l)
            state = ScalarParseState.BEFORE_VECTOR_EPOCH
          else:
            header.append(l)



        elif state == ScalarParseState.BEFORE_VECTOR_EPOCH:
            if is_VECTOR_EPOCH_inst(l):
                after_VECTOR_EPOCH_before_DEVEC.append(l);
                state = ScalarParseState.AFTER_VECTOR_EPOCH
            else:
                before_VECTOR_EPOCH.append(l)



        elif state == ScalarParseState.AFTER_VECTOR_EPOCH:
            if is_DEVEC(l):
                after_DEVEC_before_RET_DELIM.append(l)
                state = ScalarParseState.AFTER_DEVEC
            else:
                after_VECTOR_EPOCH_before_DEVEC.append(l)



        elif state == ScalarParseState.AFTER_DEVEC:
            delim = parse_delim(l)
            if delim != None and delim[0] == TrilliumAsmDelim.RETURN:
                state = ScalarParseState.AFTER_RETURN_DELIM
            else:
                after_DEVEC_before_RET_DELIM.append(l)



        elif state == ScalarParseState.AFTER_RETURN_DELIM:
            parsed_inst = parse_jump_inst(l)
            parsed_label = parse_label(l)

            if is_return_inst(l):
                scalar_ret_inst = l
                scalar_ret_label = None
                state = ScalarParseState.GLUE

            elif parsed_inst != None and parsed_inst[0] == RV_Inst.JUMP:
                scalar_ret_label = parsed_inst[1]
                state = ScalarParseState.GLUE

            elif parsed_label != None:
                scalar_ret_label = None
                labels = [parsed_label]
                state = ScalarParseState.GLUE

            else:
                scalar_cleanup.append(l)


        # ASSUMPTION: after return or "indirect return", label follows immediately
        elif state == ScalarParseState.GLUE:
            #input for this state: scalar_ret_label
            #if not None, we search for scalar cleanup and return at that label
            assert('scalar_ret_label' in locals())

            parsed_label = parse_label(l)
            vissue_key = parse_gluepoint(l)
            footer_parse = parse_footer(l)

            if parsed_label != None:
                if parsed_label == scalar_ret_label:
                    state = ScalarParseState.INDIRECT_SCALAR_RET_FOUND
                else:
                    labels.append(parsed_label)

            elif vissue_key != None:
                latest_label = labels.pop()
                labeled_vector_bbs[latest_label] = vector_bbs[vissue_key]

            elif footer_parse != None:
                footer.append(l)
                state = ScalarParseState.FOOTER

            else:
                if len(labels) > 1:
                    print("warning, appending empty auxiliary blocks: {}".format(str(labels[1:])))
                    print("Is it ok that your scalar assembly code contains empty labels?")
                if len(labels) > 0:
                    for _ in labels:
                        aux_bbs[labels.pop()] = []
                latest_aux_bb = list(aux_bbs.values())[-1]
                latest_aux_bb.append(l)


        elif state == ScalarParseState.INDIRECT_SCALAR_RET_FOUND:
            parsed_label = parse_label(l)

            if is_return_inst(l):
                scalar_ret_inst = l
                state = ScalarParseState.GLUE

            elif parsed_label != None:
                raise Exception(
                        "I was hoping the indirect scalar return block would end in a jump to return address :(\n" ++
                        "Should I generalize my search for this jump across multiple blocks?")

            else:
                scalar_cleanup.append(l)

        elif state == ScalarParseState.FOOTER:
            footer.append(l)

    return glue_pieces()



if __name__ == "__main__":
    import sys
    arg_string = " ".join(sys.argv[1:])
    usage_regex = "(\w+)\s+(\w+.s)\s+(\w+.s)\s+-o\s+(\w+.s)"
    match = re.compile(usage_regex).match(arg_string)
    if not match:
      print("Gluer Usage: [kernel_fun_name] [input vector.s filename] [input scalar.s filename] -o [output combined.s filename]")
      print("expected match with regex: {}".format(usage_regex))
      print("but found: {}".format(arg_string))
      exit(1)

    #TODO: rather than receive as CL arg, parse kernel_fun_name from a sensible location instead
    kernel_fun_name, vector_filename, scalar_filename, combined_filename = match.groups()
    vector_file = open(vector_filename, "r")
    scalar_file = open(scalar_filename, "r")
    combined_file = open(combined_filename, "w")

    vector_code = vector_file.readlines()
    scalar_code = scalar_file.readlines()

    vector_blocks = read_vector_bbs(kernel_fun_name, vector_code)
    for block_name in vector_blocks.keys():
        block = vector_blocks[block_name]
        print("printing {} CFG of length {}".format(block_name, len(block)))
        print(pretty(block))

    combined_code = glue(kernel_fun_name, scalar_code, vector_blocks)
    combined_file.write(pretty(combined_code))

    print("Finished.")
