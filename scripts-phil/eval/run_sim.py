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

# include configs we want to run
import sim_list

# cmd line arguments
parser = argparse.ArgumentParser(description='Run gem5 simulation and output informative stats files')
parser.add_argument('--build', default='../../build/RVSP/gem5.opt', help='Path to gem5 build')
parser.add_argument('--config', default='../../configs/phil/brg_hammerblade.py', help='Path to gem5 build')
parser.add_argument('--results', default='../../results', help='Path to place to store results')
args = parser.parse_args()

# create a template for the gem5 command line
gem5_cmd = lambda program, options, result, cpus, vec: \
  '{} --remote-gdb-port=0 -d {}/{} {} --cmd={} --options=\"{}\" --num-cpus={} {}'.format(
      args.build, args.results, result, args.config, program, options, str(cpus), '--vector' if vec else '')
  
# compile command that chooses whether to use scratchpad optimizations
# how many cores/sps are present, and whether to use vector mode
def compile_cmd(program_dir, cpus, extra_flags):
  cmd = 'make clean -C {}'.format(program_dir)
  cmd += ' && '
  cmd += extra_flags + ' ENV_N_SPS={} make -C {}'.format(cpus, program_dir)
  return cmd

# just compile with args needed for makefile (#cores, and whether vec enabled...etc)
def compile_prog(numCpus, program, extra_flags):
  cmplCmd = compile_cmd(os.path.dirname(program['path']), numCpus, extra_flags)
  result = subprocess.check_output(cmplCmd, shell=True)
  print(result)

def run_prog(numCpus, program, argv, extra_info):
  
  # check if the success flag was asserted using regex checking on the gem5 output
  if (not 'success' in program['success']):
    program['success'] = '\[\[SUCCESS\]\]'
  success_regex = re.compile(program['success'])

  # run with currSize
  optionsStr = program['options'](argv)
  
  # serialize arguments
  serialArgs = program['serialize'](argv)
  resultsAnno = '-' + extra_info + serialArgs
  resultsDir = program['name'] + resultsAnno
  cmd = gem5_cmd(program['path'], optionsStr, resultsDir, numCpus, True)
  print(cmd)
  result = subprocess.check_output(cmd, shell=True)
  print(result)

  # make sure that the run passed
  success = success_regex.search(result)
  if (success):
    return True
  else:
    return False

# # consturct a valid config from the args
# # filters out configurations that don't make sense
# # only works for vec configs
# def vvadd_merge_args(vec_size, prefetch_len, load_type):
#   config = []

#   # vec size flag
#   if (vec_size == 1):
#     assert(0)
#   if (vec_size > 1):
#     config.append('VECTOR_LEN=' + str(vec_size))

#   # load type flag
#   if (load_type == 'SPATIAL'):
#     pass
#   elif (load_type == 'VERTICAL'):
#     config.append('VERTICAL_LOADS')
#   elif (load_type == 'SPATIAL_UNROLL'):
#     config.append('SPATIAL_UNROLL')
#   elif (load_type == 'REUSE'):
#     config.append('REUSE')
#     config.append('VERTICAL_LOADS')

#   # vertical only works with prefetch 16
#   if (load_type == 'VERTICAL' or load_type == 'REUSE'):
#     if (prefetch_len != 16):
#       return (False, config)
#     else:
#       config.append('PF=16')
#       return (True, config)

#   # remove configs where vec size exceeds prefetch size
#   else:
#     if (vec_size < prefetch_len):
#       return (False, config)
#     else:
#       config.append('PF=' + str(prefetch_len))
#       return (True, config)

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

pool = multiprocessing.Pool(processes=4)

# for use_vec in use_vec_arr:
for sim_config in sim_list.sim_configs.values():
  prog_name = sim_config['prog']
  prog_def  = sim_list.programs[prog_name]
  argv      = sim_config['argv']

  for vec_config in sim_config['vec']:
    # compile program with the specificed vec config
    compile_prog(numCpus, prog_def, strings_to_make_args(vec_config))

    run_prog(numCpus, prog_def, argv, strings_to_metadata(vec_config))

    # jobs = []

    # # the new file will have the same name as the old file, but also specify the new dir
    # proc = pool.apply_async(run_prog, args=(numCpus, prog_def, argv, strings_to_metadata(vec_config) ))
    # jobs.append(proc)

    # # Wait for jobs to complete before exiting
    # while(not all([p.ready() for p in jobs])):
    #   time.sleep(5)

    # # Check if any jobs failed
    # failed_runs = 0
    # for p in jobs:
    #   if (p.get() == False):
    #     failed_runs += 1

    # if (failed_runs > 0):
    #   print('{} runs failed'.format(failed_runs))
    #   assert(False)

# Safely terminate the pool
pool.close()
pool.join()



