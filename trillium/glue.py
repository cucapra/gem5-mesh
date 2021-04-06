from collections import OrderedDict
from glue_util import *
from glue_log import *
import argparse
import itertools
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
    END_AT_JUMP = auto()
    RODATA = auto()


def extract_vector_blocks(raw_vector_code):
    vector_code = vector_preprocess(raw_vector_code)

    blocks = {}
    rodata_chunks = []
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

            elif parse_rodata_section(l):
                # A read-only data (constant) section.
                log.info('starting read-only data section: {}'.format(l))
                rodata_chunks.append([l])
                state = VectorParseState.RODATA

            else:
                log.info("dropping '{}' at line {} in between functions...".format(l, line_no))
                continue

        # in this state, we've already seen an `until_next` delimiter,
        # so we're looking for any other delimiter.
        elif state == VectorParseState.UNTIL_NEXT:
            delim_parse = parse_delim(l)

            if delim_parse and delim_parse[0] == TrilliumAsmDelim.END:
                raise ParseError(
                    'until_next block {} terminated with end'.format(
                        curr_vissue_key
                    )
                )

            if delim_parse:
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
        elif state in (VectorParseState.BEGIN_END,
                       VectorParseState.IF_BEGIN_END):
            delim_parse = parse_delim(l)
            if delim_parse and delim_parse[0] == TrilliumAsmDelim.END:
                log.info("parsed `begin/end`-delimited block: {}".format(
                    curr_vissue_key
                ))

                # Support `end at_jump` delimiters, which continue adding
                # instructions until we hit the next jump!
                _, qualifier = delim_parse
                if qualifier == 'at_jump':
                    log.info("end at_jump: continuing for now")
                    state = VectorParseState.END_AT_JUMP
                else:
                    # Set up collection of "junk code" after `end` delim and
                    # before next delim, for debugging purposes.
                    junk_vissue_key = junk_prefix + str(junk_postfix)
                    blocks[curr_func][junk_vissue_key] = []
                    state = VectorParseState.JUNK

            elif delim_parse is not None:
                raise ParseError(
                    "expected `end` delimiter to match `begin` or `if_begin` in line {}".format(line_no)
                )

            else:
                # A normal line of code. Drop jumps in "normal"
                # begin/end blocks, but include jumps in "if" begin/end
                # blocks.
                if state == VectorParseState.IF_BEGIN_END or not is_jump(l):
                    blocks[curr_func][curr_vissue_key].append(l)

        # This state continues vacuuming up a few instructions *after*
        # the "end" delimiter---until we hit a branch/jump instruction.
        elif state == VectorParseState.END_AT_JUMP:
            if is_jump(l):
                log.info("end at_jump: jump found; ending {}".format(
                    curr_vissue_key
                ))
                junk_vissue_key = junk_prefix + str(junk_postfix)
                blocks[curr_func][junk_vissue_key] = []
                state = VectorParseState.JUNK

            elif parse_delim(l):
                raise ParseError("hit delimiter in END_AT_JUMP state")

            elif parse_label(l):
                raise ParseError("hit label in END_AT_JUMP state")

            else:
                blocks[curr_func][curr_vissue_key].append(l)

        # in this state, we've just finished a begin/end block,
        # so we look for any delimiter, discarding code in the meantime.
        # (we actually collect this "junk code" instead for debugging purposes)
        elif state == VectorParseState.JUNK:
            # Check for the end of the function.
            if is_func_end(l):
                log.info('kernel function {} ended'.format(curr_func))

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

                # Return to looking for a new kernel function.
                state = VectorParseState.START

            else:
                delim_parse = parse_delim(l)

                if delim_parse != None:
                    log.info("parsed junk block")
                    junk_postfix += 1

                    delim, new_vissue_key = delim_parse
                    curr_vissue_key = new_vissue_key
                    blocks[curr_func][new_vissue_key] = []

                    if delim == TrilliumAsmDelim.BEGIN:
                        state = VectorParseState.BEGIN_END
                    elif delim == TrilliumAsmDelim.IF_BEGIN:
                        state = VectorParseState.IF_BEGIN_END
                    elif delim == TrilliumAsmDelim.UNTIL_NEXT:
                        state = VectorParseState.UNTIL_NEXT
                    elif delim == TrilliumAsmDelim.RETURN:
                        state = VectorParseState.RETURN
                    else:
                        raise ParseError(
                            'unhandled delimiter after junk: {}'.format(l)
                        )

                else:
                    junk_vissue_key = junk_prefix + str(junk_postfix)
                    blocks[curr_func][junk_vissue_key].append(l)

        # in this state, we've just seen a `return` delimiter,
        # so we're looking for a "return-like" assembly.
        # Once we find it, we've completed a Trillism kernel parse
        elif state == VectorParseState.RETURN:
            if is_return_inst(l):
                log.info("parsed return block {}".format(curr_vissue_key))
                junk_vissue_key = junk_prefix + str(junk_postfix)
                blocks[curr_func][junk_vissue_key] = []
                state = VectorParseState.JUNK

            else:
                #TODO: this assumes the only branch/jump instruction has return address as target.
                #      Should this error out otherwise?
                blocks[curr_func][curr_vissue_key].append(l)

        # Gather up read-only data (constants).
        elif state == VectorParseState.RODATA:
            if is_ident(l):
                log.info('rodata section ended')
                state = VectorParseState.START
            elif parse_rodata_section(l):
                log.info('starting another rodata section: {}'.format(l))
                rodata_chunks.append([l])
            else:
                rodata_chunks[-1].append(l)


    # After the state machine finishes, we should end up in the RETURN
    # state at the end.
    if state != VectorParseState.START:
        raise ParseError(
            "ended in intermediate parse state {}".format(state.name)
        )

    return blocks, rodata_chunks


def make_devec_label(devec):
    """Given a `devec` instruction line, manufacture a label for the
    instruction and return a list of lines to be inserted into the file
    in its place.
    """
    if not is_DEVEC(devec):
        raise ParseError("devec not found where expected")

    # Get the current label the devec is using, and append a marker to
    # make it unique.
    rest, label = devec.rsplit(',', 1)
    new_label = '{}DEVEC'.format(label.strip())

    return [
        '{}:'.format(new_label),
        '{}, {}'.format(rest, new_label),
    ]


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


def glue(raw_scalar_code, all_vector_bbs, rodata_chunks):
    """Paste vector blocks from `all_vector_bbs`, which is a dict of
    dicts mapping functions to blocks to code, into `raw_scalar_code`,
    which is an assembly string. `rodata_chunks` is a list of lists
    containing lines to be inserted containing constant data sections.
    """
    log.info("GLUING VECTOR CODE TO SCALAR...")
    scalar_code = scalar_preprocess(raw_scalar_code)

    # The header consists of the interval [start of file, kernel launch
    # label] and will accumulate all the lines of assembly *before* we
    # encounter the first kernel function.
    header = []

    # The name of the current kernel function we're parsing (or None if
    # we're not in any kernel function).
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

            if func_name not in all_vector_bbs:
                raise ParseError(
                    'attempting to glue in function missing from vector: {}'
                    .format(func_name)
                )
            if vissue_key not in all_vector_bbs[func_name]:
                raise ParseError(
                    'attempt to glue missing vector block: {}'
                    .format(vissue_key)
                )

            labeled_vector_bbs.extend(all_vector_bbs[func_name][vissue_key])

        # Manufacture a new, special label *just* for the devec instruction.
        devec = after_DEVEC_before_RET_DELIM[0]
        devec_lines = make_devec_label(devec)

        return (
            header +
            [after_VECTOR_EPOCH_before_DEVEC[0]] +
            before_VECTOR_EPOCH +
            after_VECTOR_EPOCH_before_DEVEC[1:] +
            ["# trillium: scalar stack cleanup begin"] +
            scalar_cleanup +
            ["# trillium: scalar stack cleanup end"] +
            devec_lines +
            after_DEVEC_before_RET_DELIM[1:] +
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

                # Initialize the storage for all the bits of the function we
                # will extract. We dissect the scalar assembly into the
                # following non-overlapping components: interval
                # notation: open, closed, or half-open intervals.

                # (kernel launch label, first VECTOR_EPOCH call)
                before_VECTOR_EPOCH = []

                # [first VECTOR_EPOCH call, first DEVEC call)
                after_VECTOR_EPOCH_before_DEVEC = []

                # [first DEVEC call, scalar return delimiter]
                after_DEVEC_before_RET_DELIM = []

                # Now things gets conditionally non-contiguous. Scalar
                # return cleanup assembly consists of the following
                # potential assembly locations:
                # - all code immediately following the scalar `return`
                #   delimiter and before a jump instruction is scalar
                #   cleanup code (We assume no branching is emitted in
                #   that interval)
                # - if a jump to a label is found (instead of to the
                #   return address), the block under that label, excluding
                #   the return address jump, is scalar cleanup code
                scalar_cleanup = []

                # All cores ultimately return to the scalar return address.
                # This can be found at the end of scalar cleanup code,
                # described in the two cases above.
                scalar_ret_inst = None

                # `glue_points` maps labels to function name/vissue key pairs.
                # vissue keys can be used to index into `all_vector_bbs` to
                # get vector block code.
                glue_points = {}
                labels = [] #stack of labels

                # "Auxiliary blocks" are code that appears *after* and
                # *among* glue points (not including any scalar block
                # that jumps to return address). We move these before
                # the vector block stuff.
                aux_bbs = OrderedDict()

                # Non-instruction lines after all labels/blocks.
                footer = []

            else:
                # Not a kernel function label; just keep accumulating
                # the non-function "header."
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
                if not labels:
                    raise ParseError(
                        'glue point {} is missing a label'.format(vissue_key),
                    )
                latest_label = labels.pop()
                glue_points[latest_label] = cur_kernel_func, vissue_key

            elif footer_parse != None:
                footer.append(l)

                # The kernel function has ended!! Let's emit everything we have
                # and take it to the top, starting to look for another kernel.
                log.info('finished gluing kernel {}'.format(cur_kernel_func))
                out_lines += glue_pieces()

                # Reset the state.
                header = []
                cur_kernel_func = None
                state = ScalarParseState.HEADER

            else:
                if len(labels) > 1:
                    log.warning(
                        "appending empty auxiliary blocks: {}\n".format(labels[1:]) +
                        "Is it ok that your scalar assembly code contains empty labels?"
                    )

                if labels:
                    for _ in labels:
                        aux_bbs[labels.pop()] = []

                if aux_bbs:
                    latest_aux_bb = list(aux_bbs.values())[-1]
                    latest_aux_bb.append(l)
                else:
                    # No label has occurred yet. We ignore lines that
                    # happen after the glue points but before any label.
                    log.warning(
                        'Ignoring post-glue-point line: {}\n'
                        'Make sure this is not important!'.format(l)
                    )

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

    # At the end, we will have accumulated the final chunk of code,
    # below the last kernel, as the "header" of the next (nonexistent)
    # kernel. We stitch in the rodata lines and dump the combination to
    # the output.
    # Insert before ' .ident  "GCC: (GNU) 10.1.0" ' near the end
    if rodata_chunks:
        for i, l in enumerate(header):
            if '.ident' in l:
                header[i:i] = (
                    ["# trillium: vector constants begin"] +
                    list(itertools.chain.from_iterable(rodata_chunks)) +
                    ["# trillium: vector constants end"]
                )
                break
        else:
            raise ParseError('expected .ident to insert rodata constants')
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

    vector_code = vector_file.readlines()
    scalar_code = scalar_file.readlines()

    try:
        # Parse the vector assembly and extract the vector blocks.
        vector_blocks, rodata_chunks = extract_vector_blocks(vector_code)
        log.info("Extracted the following Trilliasm Kernel vector blocks:")
        for func_name in vector_blocks.keys():
            log.info("For function {}:".format(func_name))
            for vissue_key in vector_blocks[func_name].keys():
                block = vector_blocks[func_name][vissue_key]
                log.info("Block {} length {}".format(vissue_key, len(block)))
                log.info(pretty(block))
        log.info("Extracted these constant blocks:")
        for chunk in rodata_chunks:
            for line in chunk:
                log.info("  {}".format(line))

        # Splice the vector blocks into the scalar assembly.
        combined_code = glue(scalar_code, vector_blocks, rodata_chunks)
    except ParseError as exc:
        log.critical(exc)
        sys.exit(1)

    # Print out the combined assembly.
    log.info("Done gluing; ready to print.")
    out_asm = pretty(combined_code) + '\n'

    if args.output:
        with open(args.output, "w") as f:
            f.write(out_asm)
    else:
        sys.stdout.write(out_asm)
