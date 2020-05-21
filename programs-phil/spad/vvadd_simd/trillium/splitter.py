import re
import quasiquotes
from enum import Enum, auto

# utility to emit C code
class C_Emitter():
    @staticmethod
    def asm(instr, volatile=False):
        return "asm{}({})".format(" volatile" if volatile else "", instr)

    @staticmethod
    def declare(typed_var):
        qualifiers, type, var = typed_var
        qualifier_string = " ".join(qualifiers) + " " if qualifiers else ""
        return "{}{} {}".format(
                    qualifier_string,
                    type,
                    var)

    @staticmethod
    def fun_sig(ret_type, fun_name, typed_args):
        arg_list = ",".join(list(map(C_Emitter.declare, typed_args)))
        return "{} {}({})".format(ret_type, fun_name, arg_list)


    @staticmethod
    def fun(sig, body):
        print(C_Emitter.to_string(C_Emitter.indent(body)))
        return "{} {{\n{}\n}}".format(sig, C_Emitter.to_string(C_Emitter.indent(body)))

    @staticmethod
    def to_string(code):
        return "\n".join([line + ";" for line in code])

    @staticmethod
    def indent(code, n=1):
        return [" "*(2*n) + line for line in code]


# utility to emit Vector_SIMCv1 code
class Vector_SIMCv1_Emitter():
    blackhole_num = 0
    vissue_key_num = 0

    class ReservedKeywords(Enum):
        MASK = "trillium_codegen_mask"
        DEVEC_ARG = "devec_0"
        BLACKHOLE_VAR = "trillium_codegen_bh_"
        VISSUE_KEY = "trillium_codegen_vissue_key_"

    @staticmethod
    def kernel_sig(kernel_name):
        return "void {}(int {})".format(kernel_name, Vector_SIMCv1_Emitter.ReservedKeywords.MASK.value)

    @staticmethod
    def VECTOR_EPOCH():
        return "VECTOR_EPOCH({})".format(Vector_SIMCv1_Emitter.ReservedKeywords.MASK.value)

    @staticmethod
    def DEVEC():
        return "DEVEC({})".format(Vector_SIMCv1_Emitter.ReservedKeywords.DEVEC_ARG.value)

    @staticmethod
    def boilerplate_intro(vector_variables):
        return Vector_SIMCv1_Emitter.intermingled(
            scalar=[Vector_SIMCv1_Emitter.VECTOR_EPOCH()],
            vector=list(map(C_Emitter.declare,vector_variables)))

    @staticmethod
    def boilerplate_outro():
        return Vector_SIMCv1_Emitter.intermingled(
            scalar=
                [Vector_SIMCv1_Emitter.DEVEC(),
                 C_Emitter.asm("fence", volatile=True),
                 C_Emitter.asm("scalar return")],
            vector=
                [C_Emitter.asm("vector return"),
                 "return"])

    @staticmethod
    def intermingled(scalar=[], vector=[]):
        return ("#ifdef SCALAR\n" + C_Emitter.to_string(scalar) +
               "\n#elif defined VECTOR\n" + C_Emitter.to_string(vector))

    @staticmethod
    def blackhole_var():
        blackhole_num += 1
        return BLACKHOLE_VAR + string(blackhole_num)

    @staticmethod
    def intermingled_emit(kernel_name,
                          vector_variables,
                          vec_simcv1_kernel,
                          vissue_blocks):
        kernel_sig = C_Emitter.fun_sig(
                "void",
                kernel_name,
                [([], "int", Vector_SIMCv1_Emitter.ReservedKeywords.MASK.value)])

        return C_Emitter.fun(kernel_sig,
                [Vector_SIMCv1_Emitter.boilerplate_intro(vector_variables),
                 Vector_SIMCv1_Emitter.boilerplate_outro()])




#utility to parse trillium pragmas
class PragmaKind(Enum):
    VEC_SIMD_START = "#pragma trillium vec_simd (\S*) (\d*) start"
    VEC_SIMD_END = "#pragma trillium vec_simd end"
    VEC_BLK_BEGIN = "#pragma trillium vector begin"
    VEC_BLK_END = "#pragma trillium vector end"

def pragma_parse(pragma_kind, line):
    regex = re.compile(pragma_kind.value)
    match = regex.match(line)
    return match.groups() if match else None


class SplitterState(Enum):
    START = auto()
    KERNEL = auto()
    VEC_BLK = auto()

def split(vec_simcv2):
    # kernel start pragma info: 
    # contains kernel name and core count
    kernel_start_pragma_match = None
    # variables to declare in vector
    vector_variables = []
    # intermingled vec_simcv1 kernel 
    vec_simcv1_kernel = []
    # vissue key/block pairs
    vissue_blocks = {}
    # kernel end pragma
    kernel_end_pragma_match = None

    state = SplitterState.START
    for (line_no, line) in preprocess(vec_simcv2):
        if state == SplitterState.START:
            kernel_start_pragma_match = pragma_parse(PragmaKind.VEC_SIMD_START, line)
            if kernel_start_pragma_match != None:
                print("found kernel start")
                state = SplitterState.KERNEL

        elif state == SplitterState.KERNEL:
            # return after reading exactly one kernel
            # can extend later to read many kernels in a file
            kernel_end_pragma_match = pragma_parse(PragmaKind.VEC_SIMD_END, line)
            if kernel_end_pragma_match != None:
                print("found kernel end")
                break

    if kernel_start_pragma_match == None:
        raise(Exception("did not find a pragma indicating the start of a kernel"))
    elif kernel_end_pragma_match == None:
        raise(Exception("did not find a pragma indicating the end of a kernel"))
    else:
        kernel_name, core_count = kernel_start_pragma_match
        return Vector_SIMCv1_Emitter.intermingled_emit(kernel_name, vector_variables, vec_simcv1_kernel, vissue_blocks)



# utility for preprocessing Vector-SIMCv2 code
def preprocess(vec_simcv2_source):
    return list(enumerate(vec_simcv2_source))

def pretty(vec_simcv1_code):
    return "\n".join(vec_simcv1_code)

if __name__ == "__main__":
    vec_simcv2_file = open("test.c", "r")
    vec_simcv1_file= open("splitter_out.c", "w+")

    vec_simcv2_code = vec_simcv2_file.readlines()

    vec_simcv1_code = split(vec_simcv2_code)

    vec_simcv1_file.write(vec_simcv1_code)
    print("Vector-SIMCv1 code emitted:")
    print(vec_simcv1_code)

    print("Finished.")
