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
parser.add_argument('--num-cpus', default=64, help='Number of cpus to use in simulation')
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
  cmd += extra_flags + ' N_SPS={} make -C {}'.format(cpus, program_dir)
  return cmd

# just compile with args needed for makefile (#cores, and whether vec enabled...etc)
def compile_prog(numCpus, prog_key, extra_flags):
  program = sim_list.programs[prog_key]
  cmplCmd = compile_cmd(os.path.dirname(program['path']), numCpus, extra_flags)
  result = subprocess.check_output(cmplCmd, shell=True, stderr=subprocess.STDOUT)
  # print(result)

def run_prog(numCpus, prog_key, argv, extra_info):
  program = sim_list.programs[prog_key]
  # check if the success flag was asserted using regex checking on the gem5 output
  if (not 'success' in program):
    program['success'] = '\[\[SUCCESS\]\]'
  success_regex = re.compile(program['success'])

  # run with currSize
  optionsStr = program['options'].format(*argv)
  
  # serialize arguments
  serialArgs = program['serialize'].format(*argv)
  resultsAnno = '-' + extra_info + serialArgs
  resultsDir = program['name'] + resultsAnno
  cmd = gem5_cmd(program['path'], optionsStr, resultsDir, numCpus, True)
  print(cmd)
  result = subprocess.check_output(cmd, shell=True, stderr=subprocess.STDOUT)
  # print(result)

  # make sure that the run passed
  success = success_regex.search(result)
  if (success):
    return True
  else:
    return False

# either array or single string
def strings_to_make_args(args):
  # cmd_line = 'ENV_EXTRA_MAKE_FLAGS=\''
  cmd_line = 'EXTRA_FLAGS=\''
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
      arg = sim_list.abbreviate_config(a)
      meta += arg + '_'
  else:
    meta = sim_list.abbreviate_config(args)
  return meta

# run all configuration for a single benchmark
# this must be done serially due to recompiling benchmark
# (but can parallize across benchmarks)
def run_all_configs(vec_configs, num_cpus, prog_key, argv):
  all_pass = True
  for vec_config in vec_configs:
    # compile program with the specificed vec config
    compile_prog(num_cpus, prog_key, strings_to_make_args(vec_config))

    ret = run_prog(num_cpus, prog_key, argv, strings_to_metadata(vec_config))
    if (not ret):
      print('-------------------------------------------------------------')
      print('{} failed w/ config {}'.format(prog_key, vec_config))
      print('-------------------------------------------------------------')
      all_pass = False

  return all_pass

# MAIN
# 

num_cpus = args.num_cpus

# limit to 16 threads, each benchmark in parallel, but configs are serial
pool = multiprocessing.Pool(processes=16)
jobs = []

for k,v in sim_list.sim_configs.items():
  prog_key = k
  sim_config = v
  # prog_def  = sim_list.programs[prog_name] # TODO can't pass dicts?
  argv      = sim_config['argv']
  vec_configs = sim_config['vec']

  # the new file will have the same name as the old file, but also specify the new dir
  proc = pool.apply_async(run_all_configs, args=(vec_configs, num_cpus, prog_key, argv, ))
  jobs.append(proc)

# Wait for jobs to complete before exiting
while(not all([p.ready() for p in jobs])):
  time.sleep(5)

# Check if any jobs failed
failed_runs = 0
for p in jobs:
  try:
    if (p.get() == False):
      failed_runs += 1
  except:
    failed_runs += 1

if (failed_runs > 0):
  print('{} runs failed'.format(failed_runs))
  assert(False)

# Safely terminate the pool
pool.close()
pool.join()



