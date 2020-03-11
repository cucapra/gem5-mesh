"""

  Run a gem5 simulation and create a stats file reflecting relevant info
  about the sim.
  
  This script does not do any analysis on the stats file, that is left
  to another script. (so can do new analysis without running again)
  
  Might consider dropping a json in the results dir with run info rather than in dir name?
  Also need to be able to recompile the program when want to add more cores or disable/enable vector
"""

import os, subprocess, time, argparse, re, random

import multiprocessing

# cmd line arguments
parser = argparse.ArgumentParser(
    description="Run gem5 simulation and output informative stats files"
)
parser.add_argument(
    "--build",
    default="/home/na469/phil/gem5-mesh/build/RVSP/gem5.opt",
    help="Path to gem5 build",
)
parser.add_argument(
    "--config",
    default="/home/na469/phil/gem5-mesh/configs/phil/brg_hammerblade.py",
    help="Path to gem5 build",
)
parser.add_argument(
    "--results",
    default="/home/na469/phil/gem5-mesh/results",
    help="Path to place to store results",
)
args = parser.parse_args()

# specify programs. with the path to the program, the executible name, the default options, and string to check to see if successful
progDir0 = "/home/na469/phil/gem5-mesh/programs-phil/spad/"
programs = {
    "vvadd": {
        "name": "vvadd",
        "path": progDir0 + "vvadd/vvadd",
        "options": lambda argv: str(argv[0]),
        "serialize": lambda argv: "-size{0}".format(str(argv[0])),
        "success": "\[\[SUCCESS\]\]",
    },
    "gemm": {
        "name": "gemm",
        "path": progDir0 + "gemm_neil/gemm",
        "options": lambda argv: "{0} {0} {0}".format(str(argv[0])),
        "serialize": lambda argv: "-size_mnt{0}".format(str(argv[0])),
        "success": "\[\[SUCCESS\]\]",
    },
    "synth": {
        "name": "synth",
        "path": progDir0 + "synth-diverge/synth",
        "options": lambda argv: "{0} {1} {2}".format(
            str(argv[0]), str(argv[1]), str(argv[2])
        ),
        "serialize": lambda argv: "-size{0}-frac{1}-run{2}".format(
            str(argv[0]), str(argv[1]), str(argv[3])
        ),
        "success": "\[\[SUCCESS\]\]",
    },
}

# create a template for the gem5 command line
# --debug-flags=PipelinePrint
# --debug-flags=gemm_d
gem5_cmd = lambda program, options, result, cpus, vec: '{} -d {}/{} {}  --cmd={} --options="{}" --num-cpus={} {}'.format(
    args.build,
    args.results,
    result,
    args.config,
    program,
    options,
    str(cpus),
    "--vector" if vec else "",
)

# compile command that chooses whether to use scratchpad optimizations
# how many cores/sps are present, and whether to use vector mode
def compile_cmd(program_dir, cpus, use_sp, use_vec):
    cmd = "make clean -C {}".format(program_dir)
    cmd += " && "
    if not use_sp:
        cmd += "ENV_NO_SP=1 "
    if not use_vec:
        cmd += "ENV_NO_VEC=1 "

    cmd += "ENV_N_SPS={} make -C {}".format(cpus, program_dir)
    return cmd


# just compile with args needed for makefile (#cores, and whether vec enabled...etc)
def compile_prog(numCpus, use_vec, use_sps, prog_name, extra_flags):
    print(extra_flags)
    cmplCmd = (
        compile_cmd(
            os.path.dirname(programs[prog_name]["path"]), numCpus, use_sps, use_vec
        )
        + " "
        + extra_flags
    )
    result = subprocess.check_output(cmplCmd, shell=True)
    print(result)


def run_prog(numCpus, use_vec, use_sps, prog_name, argv, extra_info):

    # check if the success flag was asserted using regex checking on the gem5 output
    success_regex = re.compile(programs[prog_name]["success"])

    # run with currSize
    optionsStr = programs[prog_name]["options"](argv)

    # serialize arguments
    serialArgs = programs[prog_name]["serialize"](argv)
    resultsAnno = "-vec" + str(int(use_vec)) + "-" + extra_info + serialArgs
    resultsDir = programs[prog_name]["name"] + resultsAnno
    cmd = gem5_cmd(
        programs[prog_name]["path"], optionsStr, resultsDir, numCpus, use_vec
    )
    print(cmd)
    result = subprocess.check_output(cmd, shell=True)
    print(result)

    # make sure that the run passed
    success = success_regex.search(result)
    if success:
        return True
    else:
        return False


# choose which programs to run with diff parameters

# fixed parameters for the run, compile the binary for these
numCpus = 4  # 16
use_sps = True

size = 32  # 65536  # 32768 #8192
# not sure gem5 se would produce diff ranodm seed each time so do here
random.seed()
# seed = random.randint(1,2**20)
seed = 566925  # when detect a bug can look at causual seed

# run different fractions for synth
runs = 1
# in case want to run the same multiple times w/ diff random seed (not working yet)
run_id = 1
# whether to use vector or not
use_vec_arr = [True, False]

# make_flags = ['NO_VEC','VEC_16','VEC_16_UNROLL','VEC_4','VEC_4_UNROLL','VEC_4_DA', \
#   'VEC_16_UNROLL_SERIAL','VEC_4_DA_SMALL_FRAME','NO_VEC_DA','NO_VEC_W_VLOAD','SIM_DA_VLOAD_SIZE_1', \
#   'VEC_4_NORM_LOAD', 'VEC_16_NORM_LOAD']

make_flags_vvadd = [
    "NO_VEC",
    "VEC_16",
    "VEC_16_UNROLL",
    "VEC_4",
    "VEC_4_UNROLL",
    "VEC_4_DA",
    "VEC_4_DA_SMALL_FRAME",
    "NO_VEC_DA",
    "NO_VEC_W_VLOAD",
    "SIM_DA_VLOAD_SIZE_1",
]

# make_flags_gemm = ["UNBLOCKED_INNER", "BLOCKED", "INTERLEAVED", "UNBLOCKED_OUTER"]
# make_flags_gemm = ["VPF", "SP", "DRAM"]
make_flags_gemm = ["VPF"]
# program = "vvadd"
program = "gemm"

# TODO need a struct describing the experiment. Not all settings match idenpendently


def pack_and_run(numCpus, use_vec, use_sps, prog, i, extra_info):
    frac = 1.0 - float(i) / 10.0
    argv = [size, frac, seed]
    return run_prog(numCpus, use_vec, use_sps, program, argv, extra_info)


pool = multiprocessing.Pool(processes=4)

for use_vec in use_vec_arr:
    for make_flag in make_flags_gemm:
        # use_vec = False
        print(make_flag)
        # run a program from the list above with different parameters
        compile_prog(
            numCpus, use_vec, use_sps, program, "ENV_EXTRA_MAKE_FLAGS=-D" + make_flag
        )

        jobs = []

        for i in range(runs):
            # pack_and_run(numCpus, use_vec, use_sps, 'synth', i)
            # the new file will have the same name as the old file, but also specify the new dir
            proc = pool.apply_async(
                pack_and_run, args=(numCpus, use_vec, use_sps, program, i, make_flag)
            )
            jobs.append(proc)
            pass

        # Wait for jobs to complete before exiting
        while not all([p.ready() for p in jobs]):
            time.sleep(5)

        # Check if any jobs failed
        failed_runs = 0
        for p in jobs:
            if p.get() == False:
                failed_runs += 1

        if failed_runs > 0:
            print("{} runs failed".format(failed_runs))
            assert False

# Safely terminate the pool
pool.close()
pool.join()
