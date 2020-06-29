from collections import OrderedDict
from glue_util import *
from glue_log import *
import argparse
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
    IF_BEGIN_END = auto()
    RETURN = auto()
    JUNK = auto()


def extract_vector_blocks(raw_vector_code):
    vector_code = vector_preprocess(raw_vector_code)

    blocks = {}
    curr_vissue_key = None
    curr_func = None

    # After parsing a `begin/end` block, we discard code until finding another delimiter.
    # For debugging purposes, we collect the code into "junk" blocks
    junk_prefix = "trillium_junk"
    junk_postfix = 0

    state = VectorParseState.START
    for (line_no, l) in vector_code:
        if state == VectorParseState.START:
            func_name = is_kernel_func_label(l)
            if func_name:
                log.info("found Trilliasm Kernel label {} at line {}".format(l, line_no))

                # `trillium_init` is an implicit block containing stack setup code
                # it must be attached to another block, since it's not explicitly
                # handled at the language level.
                # we'll attach it the first block, i.e., the first user-specified block
                # OrderedDict lets us easily find the block immediately after trilium_init
                curr_vissue_key = "trillium_init"
                curr_func = func_name

                # initialize empty dictionary for this function, then extract the blocks
                blocks[curr_func] = OrderedDict([(curr_vissue_key, [])])

                state = VectorParseState.UNTIL_NEXT
            else:
                log.info("dropping '{}' at line {} in between functions...".format(l, line_no))
                continue

        # in this state, we've already seen an `until_next` delimiter,
        # so we're looking for any other delimiter.
        elif state == VectorParseState.UNTIL_NEXT:
            delim_parse = parse_delim(l)
            if delim_parse != None:
                log.info("parsed 'until_next'-delimited block: {}".format(curr_vissue_key))
                delim, new_vissue_key = delim_parse
                curr_vissue_key = new_vissue_key
                blocks[curr_func][new_vissue_key] = []

                if delim == TrilliumAsmDelim.UNTIL_NEXT:
                    state = VectorParseState.UNTIL_NEXT
                elif delim == TrilliumAsmDelim.BEGIN:
                    state = VectorParseState.BEGIN_END
                elif delim == TrilliumAsmDelim.IF_BEGIN:
                    state = VectorParseState.IF_BEGIN_END
                elif delim == TrilliumAsmDelim.RETURN:
                    state = VectorParseState.RETURN
                else:
                    raise ParseError("unrecognized delim found: check parse_delim function")
            elif not is_jump(l):
                blocks[curr_func][curr_vissue_key].append(l)

        # in this state, we've just seen the `begin` delimiter,
        # so we're looking for a matching `end`
        elif state == VectorParseState.BEGIN_END:
            delim_parse = parse_delim(l)
            if delim_parse == TrilliumAsmDelim.END:
                log.info("parsed `begin/end`-delimited block: {}".format(curr_vissue_key))

                # setup collection of "junk code" after `end` delim and before next delim,
                # for debugging purposes
                junk_vissue_key = junk_prefix + str(junk_postfix)
                blocks[curr_func][junk_vissue_key] = []
                state = VectorParseState.JUNK
            elif delim_parse != None:
                raise ParseError(
                    "expected `end` delimiter to match `begin` in line {}".format(line_no)
                )
            elif not is_jump(l):
                blocks[curr_func][curr_vissue_key].append(l)

        # in this state, we've just seen the `if-begin` delimiter,
        # so we're looking for a matching `if-end`
        elif state == VectorParseState.IF_BEGIN_END:
            delim_parse = parse_delim(l)
            if delim_parse == TrilliumAsmDelim.IF_END:
                log.info("parsed `if-begin/end`-delimited block: {}".format(curr_vissue_key))

                # setup collection of "junk code" after `end` delim and before next delim,
                # for debugging purposes
                junk_vissue_key = junk_prefix + str(junk_postfix)
                blocks[curr_func][junk_vissue_key] = []
                state = VectorParseState.JUNK
            elif delim_parse != None:
                raise ParseError(
                    "expected `if-end` delimiter to match `if-begin` in line {}".format(line_no)
                )
            else:
                blocks[curr_func][curr_vissue_key].append(l)

        # in this state, we've just finished a begin/end block,
        # so we look for any delimiter, discarding code in the meantime.
        # (we actually collect this "junk code" instead for debugging purposes)
        elif state == VectorParseState.JUNK:
            delim_parse = parse_delim(l)

            if delim_parse != None:
                log.info("parsed junk block")
                junk_postfix += 1

                delim, new_vissue_key = delim_parse
                curr_vissue_key = new_vissue_key
                blocks[curr_func][new_vissue_key] = []

                if delim == TrilliumAsmDelim.BEGIN:
                    state = VectorParseState.BEGIN_END
                elif delim == TrilliumAsmDelim.UNTIL_NEXT:
                    state = VectorParseState.UNTIL_NEXT
                elif delim == TrilliumAsmDelim.RETURN:
                    state = VectorParseState.RETURN

            else:
                junk_vissue_key = junk_prefix + str(junk_postfix)
                blocks[curr_func][junk_vissue_key].append(l)


        # in this state, we've just seen a `return` delimiter,
        # so we're looking for a "return-like" assembly.
        # Once we find it, we've completed a Trillism kernel parse
        elif state == VectorParseState.RETURN:
            if is_return_inst(l):
                log.info("parsed return block {}".format(curr_vissue_key))

                # prepend trillium_init block to the first block
                trillium_init_block = blocks[curr_func]["trillium_init"]
                trillium_init_block.insert(0, "#prepended trillium_init block here (See docs for more info)")
                trillium_init_block.insert(1, "#trillium_init begin")
                trillium_init_block.append("#trillium_init end")
                vissue_keys = list(blocks[curr_func].keys())
                if len(vissue_keys) >= 2:
                    first_vissue_key = list(blocks[curr_func].keys())[1] #0 corresponds to "trillium_init"
                    first_vissue_block = blocks[curr_func][first_vissue_key]
                    blocks[curr_func][first_vissue_key] = trillium_init_block + first_vissue_block
                else:
                    log.warn("couldn't append trillium_init to another block, since no other blocks were found.")

                # insert terminator in each block
                terminator = ".insn i 0x1b, 0x7, x0, x0, 0"
                for b in blocks[curr_func].values():
                    b.append(terminator)
                state = VectorParseState.START #get the next kernel, if any
            else:
                #TODO: this assumes the only branch/jump instruction has return address as target.
                #      Should this error out otherwise?
                blocks[curr_func][curr_vissue_key].append(l)

    # After the state machine finishes, we should end up in the RETURN
    # state at the end.
    if state != VectorParseState.START:
        raise ParseError(
            "ended in intermediate parse state {}".format(state.name)
        )


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
    # SIFT_BB = auto()
    # NON_VECTOR_BB = auto()


def glue(raw_scalar_code, all_vector_bbs):
    """Paste vector blocks from `all_vector_bbs`, which is a dict of dicts
    mapping functions to blocks to code, into `raw_scalar_code`, which is an
    assembly string.
    """
    log.info("GLUING VECTOR CODE TO SCALAR...")
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

    # `glue_points` maps labels to function name/vissue key pairs. vissue keys
    # can be used to index into `all_vector_bbs` to get vector block code.
    glue_points = {}
    labels = [] #stack of labels

    # labels pointing to scalar auxiliary blocks (not including scalar block that returns to jump address, if any)
    # (Note: OrderedDict helps us keep track of the last inserted label)
    aux_bbs = OrderedDict([("trillium_anon_aux_bb",[])])

    # non-instruction lines after all labels/blocks
    footer = []

    # The name of the current kernel function we're parsing (or None if we're
    # not in any kernel function).
    cur_kernel_func = None

    # The overall output from the gluer.
    out_lines = []

    def glue_pieces():
        aux_bbs_as_list = []
        for label, bb in aux_bbs.items():
            aux_bbs_as_list.append(".{}:".format(label))
            aux_bbs_as_list.extend(bb)

        labeled_vector_bbs = []
        for label, (func_name, vissue_key) in glue_points.items():
            commented_label = ".{}:  # {} vissue block".format(label, vissue_key)
            labeled_vector_bbs.append(commented_label)

            labeled_vector_bbs.extend(all_vector_bbs[func_name][vissue_key])

        return (
            header +
            [after_VECTOR_EPOCH_before_DEVEC[0]] +
            before_VECTOR_EPOCH +
            after_VECTOR_EPOCH_before_DEVEC[1:-1] + #there's always a label before DEVEC
            ["# trillium: scalar stack cleanup begin"] +
            scalar_cleanup +
            ["# trillium: scalar stack cleanup end"] +
            [after_VECTOR_EPOCH_before_DEVEC[-1]] +
            after_DEVEC_before_RET_DELIM +
            [scalar_ret_inst if scalar_ret_inst else "ret" + "# relocated return instruction"] +
            ["# trillium: auxiliary blocks begin"] +
            aux_bbs_as_list +
            ["# trillium: auxiliary blocks end"] +
            ["# trillium: vector vissue blocks begin"] +
            labeled_vector_bbs +
            ["# trillium: vector vissue blocks end"] +
            ["# trillium: footer begin"] +
            footer +
            ["# trillium: footer end"]
        )


    state = ScalarParseState.HEADER
    for (line_no, l) in scalar_code:

        if state == ScalarParseState.HEADER:
          # Is this a Trillium function (indicated by the naming convention)?
          func_name = is_kernel_func_label(l)
          if func_name:
            header.append(l)
            state = ScalarParseState.BEFORE_VECTOR_EPOCH
            cur_kernel_func = func_name
          else:
            header.append(l)



        elif state == ScalarParseState.BEFORE_VECTOR_EPOCH:
            if is_VECTOR_EPOCH_inst(l):
                after_VECTOR_EPOCH_before_DEVEC.append(l)
                state = ScalarParseState.AFTER_VECTOR_EPOCH
            else:
                before_VECTOR_EPOCH.append(l)



        elif state == ScalarParseState.AFTER_VECTOR_EPOCH:
            if is_DEVEC(l):
                if parse_label(after_VECTOR_EPOCH_before_DEVEC[-1]) == None:
                    raise ParseError("Expected label immediately before DEVEC")
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
                log.info("found return instr after return delimiter")
                scalar_ret_inst = l
                scalar_ret_label = None
                state = ScalarParseState.GLUE

            elif parsed_inst != None and parsed_inst[0] == RV_Inst.JUMP:
                log.info("found jump instr after return delimiter: {}".format(l))
                scalar_ret_label = parsed_inst[1]
                state = ScalarParseState.GLUE

            elif parsed_label != None:
                log.info("found label after return delimiter, and before return instr")
                scalar_ret_label = None
                labels.append(parsed_label)
                state = ScalarParseState.GLUE

            else:
                scalar_cleanup.append(l)


        #input for this state: scalar_ret_label
        #if not None, we search for scalar cleanup and return at that label
        elif state == ScalarParseState.GLUE:
            assert('scalar_ret_label' in locals())

            parsed_label = parse_label(l)
            vissue_key = parse_gluepoint(l)
            footer_parse = parse_footer(l)

            if parsed_label != None:
                if parsed_label == scalar_ret_label:
                    log.info("found scalar return jump label")
                    state = ScalarParseState.INDIRECT_SCALAR_RET_FOUND
                else:
                    labels.append(parsed_label)

            elif vissue_key != None:
                # Save the glue point for later code insertion.
                latest_label = labels.pop()
                glue_points[latest_label] = cur_kernel_func, vissue_key

            elif footer_parse != None:
                footer.append(l)

                # The kernel function has ended!! Let's emit everything we have
                # and take it to the top, starting to look for another kernel.
                log.info('finished gluing kernel {}'.format(cur_kernel_func))
                out_lines += glue_pieces()

                # Reset the state. Don't love this copypasta; should clean it up.
                header = []
                before_VECTOR_EPOCH = []
                after_VECTOR_EPOCH_before_DEVEC = []
                after_DEVEC_before_RET_DELIM = []
                scalar_cleanup = []
                scalar_ret_inst = None
                glue_points = {}
                labels = []
                aux_bbs = OrderedDict([("trillium_anon_aux_bb", [])])
                footer = []

                cur_kernel_func = None
                state = ScalarParseState.HEADER

            else:
                if len(labels) > 1:
                    log.warn(
                        "appending empty auxiliary blocks: {}\n".format(labels[1:]) +
                        "Is it ok that your scalar assembly code contains empty labels?"
                    )
                if len(labels) > 0:
                    for _ in labels:
                        aux_bbs[labels.pop()] = []
                latest_aux_bb = list(aux_bbs.values())[-1]
                latest_aux_bb.append(l)


        elif state == ScalarParseState.INDIRECT_SCALAR_RET_FOUND:
            parsed_label = parse_label(l)
            log.info("indirect return jump found")

            if is_return_inst(l):
                scalar_ret_inst = l
                state = ScalarParseState.GLUE

            elif parsed_label != None:
                raise ParseError(
                        "I was hoping the indirect scalar return block would end in a jump to return address :(\n" +
                        "Should I generalize my search for this jump across multiple blocks?")

            else:
                log.info("adding line to scalar cleanup: {}".format(l))
                scalar_cleanup.append(l)

    # At the end, we will have accumulated the final chunk of code, below the
    # last kernel, as the "header" of the next (nonexistent) kernel. Add these
    # lines unchanged to the output.
    out_lines += header

    return out_lines



if __name__ == "__main__":
    import sys
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("vector", help="name of vector assembly")
    parser.add_argument("scalar", help="name of scalar assembly")
    parser.add_argument("-o", "--output", help="name of output combined assembly")
    args = parser.parse_args()

    vector_file = open(args.vector, "r")
    scalar_file = open(args.scalar, "r")
    if args.output:
        combined_file = open(args.output, "w")
    else:
        combined_file = sys.stdout

    vector_code = vector_file.readlines()
    scalar_code = scalar_file.readlines()

    try:
        # Parse the vector assembly and extract the vector blocks.
        vector_blocks = extract_vector_blocks(vector_code)
        log.info("Extracted the following Trilliasm Kernel vector blocks:")
        for func_name in vector_blocks.keys():
            log.info("For function {}:".format(func_name))
            for vissue_key in vector_blocks[func_name].keys():
                block = vector_blocks[func_name][vissue_key]
                log.info("Block {} length {}".format(vissue_key, len(block)))
                log.info(pretty(block))

        # Splice the vector blocks into the scalar assembly.
        combined_code = glue(scalar_code, vector_blocks)
    except ParseError as exc:
        log.critical(exc)
        sys.exit(1)

    # Print out the combined assembly.
    log.info("Done gluing; ready to print.")
    combined_file.write(pretty(combined_code))
