'''

  Run a gem5 simulation and create a stats file reflecting relevant info
  about the sim.
  
  This script does not do any analysis on the stats file, that is left
  to another script. (so can do new analysis without running again)
  
  Might consider dropping a json in the results dir with run info rather than in dir name?
  Also need to be able to recompile the program when want to add more cores or disable/enable vector
'''

import os, subprocess, time, argparse, re, random

import multiprocessing

# cmd line arguments
parser = argparse.ArgumentParser(description='Run gem5 simulation and output informative stats files')
parser.add_argument('--build', default='../../build/RVSP/gem5.opt', help='Path to gem5 build')
parser.add_argument('--config', default='../../configs/phil/brg_hammerblade.py', help='Path to gem5 build')
parser.add_argument('--results', default='../../results', help='Path to place to store results')
args = parser.parse_args()

# specify programs. with the path to the program, the executible name, the default options, and string to check to see if successful
progDir0 = '../../programs-phil/spad/'
programs = {

  'vvadd' : { 'name': 'vvadd', 'path' : progDir0 + 'vvadd/vvadd', 
    'options' : lambda argv: str(argv[0]), 
    'serialize' : lambda argv: '-size{0}'.format(str(argv[0])),
    'success' : '\[\[SUCCESS\]\]'},
    
  'gemm'  : { 'name': 'gemm',  'path' : progDir0 + 'gemm_neil/gemm', 
    'options' : lambda argv: '{0} {0} {0}'.format(str(argv[0])),
    'success' : '\[\[SUCCESS\]\]'},
    
  'synth' : { 'name': 'synth', 'path' : progDir0 + 'synth-diverge/synth', 
    'options' : lambda argv: '{0} {1} {2}'.format(str(argv[0]), str(argv[1]), str(argv[2])),
    'serialize' : lambda argv: '-size{0}-frac{1}'.format(str(argv[0]), str(argv[1])),
    'success' : '\[\[SUCCESS\]\]'},

  'stencil' : { 'name': 'stencil', 'path' : progDir0 + 'stencil/stencil', 
    'options' : lambda argv: str(argv[0]) + " " + str(argv[1]), 
    'serialize' : lambda argv: '-cols{0}-rows{1}'.format(str(argv[0]), str(argv[1])),
    'success' : '\[\[SUCCESS\]\]'},

}

# create a template for the gem5 command line
gem5_cmd = lambda program, options, result, cpus, vec: \
  '{} --remote-gdb-port=0 -d {}/{} {} --cmd={} --options=\"{}\" --num-cpus={} {}'.format(
      args.build, args.results, result, args.config, program, options, str(cpus), '--vector' if vec else '')
  
# compile command that chooses whether to use scratchpad optimizations
# how many cores/sps are present, and whether to use vector mode
def compile_cmd(program_dir, cpus, use_sp, use_vec, extra_flags):
  cmd = 'make clean -C {}'.format(program_dir)
  cmd += ' && '
  if (not use_sp):
    cmd += 'ENV_NO_SP=1 '
  if (not use_vec):
    cmd += 'ENV_NO_VEC=1 '
    
  cmd += extra_flags + ' ENV_N_SPS={} make -C {}'.format(cpus, program_dir)
  return cmd


# just compile with args needed for makefile (#cores, and whether vec enabled...etc)
def compile_prog(numCpus, use_vec, use_sps, prog_name, extra_flags):
  cmplCmd = compile_cmd(os.path.dirname(programs[prog_name]['path']), numCpus, use_sps, use_vec, extra_flags)
  result = subprocess.check_output(cmplCmd, shell=True)
  print(result)

def run_prog(numCpus, use_vec, use_sps, prog_name, argv, extra_info):
  
  # check if the success flag was asserted using regex checking on the gem5 output
  success_regex = re.compile(programs[prog_name]['success'])

  # run with currSize
  optionsStr = programs[prog_name]['options'](argv)
  
  # serialize arguments
  serialArgs = programs[prog_name]['serialize'](argv)
  resultsAnno = '-vec' + str(int(use_vec)) + "-" + extra_info + serialArgs
  resultsDir = programs[prog_name]['name'] + resultsAnno
  cmd = gem5_cmd(programs[prog_name]['path'], optionsStr, resultsDir, numCpus, use_vec)
  print(cmd)
  result = subprocess.check_output(cmd, shell=True)
  print(result)

  # make sure that the run passed
  success = success_regex.search(result)
  if (success):
    return True
  else:
    return False

# consturct a valid config from the args
# filters out configurations that don't make sense
# only works for vec configs
def vvadd_merge_args(vec_size, prefetch_len, load_type):
  config = []

  # vec size flag
  if (vec_size == 1):
    assert(0)
  if (vec_size > 1):
    config.append('VECTOR_LEN=' + str(vec_size))

  # load type flag
  if (load_type == 'SPATIAL'):
    pass
  elif (load_type == 'VERTICAL'):
    config.append('VERTICAL_LOADS')
  elif (load_type == 'SPATIAL_UNROLL'):
    config.append('SPATIAL_UNROLL')
  elif (load_type == 'REUSE'):
    config.append('REUSE')
    config.append('VERTICAL_LOADS')

  # vertical only works with prefetch 16
  if (load_type == 'VERTICAL' or load_type == 'REUSE'):
    if (prefetch_len != 16):
      return (False, config)
    else:
      config.append('PF=16')
      return (True, config)

  # remove configs where vec size exceeds prefetch size
  else:
    if (vec_size < prefetch_len):
      return (False, config)
    else:
      config.append('PF=' + str(prefetch_len))
      return (True, config)

# either array or single string
def strings_to_make_args(args):
  cmd_line = 'ENV_EXTRA_MAKE_FLAGS=\''
  if (isinstance(args, list)):
    for a in args:
      cmd_line += '-D' + a + ' '
    cmd_line += '\''
  else:
    cmd_line += '-D' + args + '\''
  return cmd_line

# turn config into metadata to make which run was used
def strings_to_metadata(args):
  meta = ''
  if (isinstance(args, list)):
    for a in args:
      # special interpretations
      arg = a
      if (a == 'VERTICAL_LOADS'):
        arg = 'V'
      elif (a == 'SPATIAL_UNROLL'):
        arg = 'S'
      elif (a == 'REUSE'):
        arg = 'R'
      elif (a[0:11] == 'VECTOR_LEN='):
        arg = a[11:len(a)]
      elif (a[0:3] == 'PF='):
        arg = a[3:len(a)]
      meta += arg + '_'
  else:
    meta = args
  return meta

  
# choose which programs to run with diff parameters

# fixed parameters for the run, compile the binary for these
numCpus = 64
use_sps = True

# size = 8402 #131072 #8192 #32768

# good for spatial verions
# ncols = 1730
# good for reuse versions
# ncols = 1842  # n * 460 + 2
# ncols = 5522
ncols = 1730
nrows = 60
# not sure gem5 se would produce diff ranodm seed each time so do here
random.seed()
#seed = random.randint(1,2**20) 
seed = 566925 # when detect a bug can look at causual seed

# run different fractions for synth
runs = 1
# in case want to run the same multiple times w/ diff random seed (not working yet)
run_id = 1
# whether to use vector or not
use_vec_arr = [True]

# make_flags = [ 'NO_VEC', 'VEC_4_SIMD', 'VEC_4_SIMD_VERTICAL', 'VEC_4_SIMD_SPATIAL_UNROLLED', 'VEC_16_SIMD', 'VEC_16_SIMD_VERTICAL', 'VEC_16_SIMD_SPATIAL_UNROLLED' ]
# make_flags = [ 'VEC_16_SIMD', 'VEC_4_SIMD' ]

make_flags = []
# vec_sizes = [ 4, 16 ]
# load_types = [ 'SPATIAL', 'VERTICAL', 'SPATIAL_UNROLL', 'REUSE ]
# prefetch_sizes = [ 1, 2, 4, 8, 16 ]
vec_sizes = [ 4 ]
load_types = [ 'VERTICAL' ]
prefetch_sizes = [ 16 ]
for v in vec_sizes:
  for l in load_types:
    for p in prefetch_sizes:
      (is_valid, config) = vvadd_merge_args(v, p, l)
      if is_valid:
        make_flags.append(config)

# add no vec config as well
# make_flags.append('NO_VEC')

# make_flags_gemm = ["SIMD_PRIVATE", "SIMD_SHARING", "NO_VEC"]
# program = "gemm"
program = 'stencil'

# TODO need a struct describing the experiment. Not all settings match idenpendently

def pack_and_run(numCpus, use_vec, use_sps, prog, i, extra_info):
  frac = 1.0 - float(i) / 10.0
  # argv = [ size, frac, seed ]
  argv = [ ncols, nrows, frac, seed ]
  return run_prog(numCpus, use_vec, use_sps, program, argv, extra_info)

pool = multiprocessing.Pool(processes=4)

# for use_vec in use_vec_arr:
for make_flag in make_flags:
  use_vec = True
  # run a program from the list above with different parameters
  compile_prog(numCpus, use_vec, use_sps, program, strings_to_make_args(make_flag))

  jobs = []
  
  for i in range(runs):
    #pack_and_run(numCpus, use_vec, use_sps, program, i, make_flag)
    # the new file will have the same name as the old file, but also specify the new dir
    proc = pool.apply_async(pack_and_run, args=(numCpus, use_vec, use_sps, program, i, strings_to_metadata(make_flag) ))
    jobs.append(proc)
    pass

  # Wait for jobs to complete before exiting
  while(not all([p.ready() for p in jobs])):
    time.sleep(5)

  # Check if any jobs failed
  failed_runs = 0
  for p in jobs:
    if (p.get() == False):
      failed_runs += 1

  if (failed_runs > 0):
    print('{} runs failed'.format(failed_runs))
    assert(False)

# Safely terminate the pool
pool.close()
pool.join()



