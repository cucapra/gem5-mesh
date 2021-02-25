'''

  Run a gem5 simulation and create a stats file reflecting relevant info
  about the sim.
  
  This script does not do any analysis on the stats file, that is left
  to another script. (so can do new analysis without running again)
  
  Might consider dropping a json in the results dir with run info rather than in dir name?
  Also need to be able to recompile the program when want to add more cores or disable/enable vector
'''

import os, subprocess, time, argparse, re, random

import multiprocessing, time

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
gem5_cmd = lambda program, options, result, cpus, vec, hw_opts: \
  '{} --remote-gdb-port=0 -d {}/{} {} {} --cmd={} --options=\"{}\" --num-cpus={} {}'.format(
      args.build, args.results, result, args.config, hw_opts, program, options, str(cpus), '--vector' if vec else '')
  
# compile command that chooses whether to use scratchpad optimizations
# how many cores/sps are present, and whether to use vector mode
def compile_cmd(program_dir, cpus, extra_flags):
  cmd = extra_flags + ' make clean -C {}'.format(program_dir)
  cmd += ' && '
  cmd += extra_flags + ' N_SPS={} make -C {}'.format(cpus, program_dir)
  return cmd

# just compile with args needed for makefile (#cores, and whether vec enabled...etc)
def compile_prog(numCpus, prog_key, vec_config):
  extra_flags = sim_list.strings_to_make_args(vec_config)
  binary_name = sim_list.get_binary_name(prog_key, vec_config)
  extra_flags += ' BINARY_NAME=' + binary_name
  program = sim_list.programs[prog_key]
  cmplCmd = compile_cmd(os.path.dirname(program['path']), numCpus, extra_flags)
  result = subprocess.check_output(cmplCmd, shell=True, stderr=subprocess.STDOUT)
  # result = subprocess.check_output(cmplCmd, shell=True)
  # print(result)

def run_prog(numCpus, prog_key, argv, vec_config, hw_opts):
  program = sim_list.programs[prog_key]

  # check if the success flag was asserted using regex checking on the gem5 output
  if (not 'success' in program):
    program['success'] = '\[\[SUCCESS\]\]'
  success_regex = re.compile(program['success'])

  # run with currSize
  optionsStr = program['options'].format(*argv)
  
  # serialize arguments
  extra_info = sim_list.preprocess_and_convert_to_metadata(vec_config)
  hw_info = sim_list.preprocess_and_convert_to_metadata(hw_opts)
  serialArgs = program['serialize'].format(*argv)
  resultsAnno = '-' + extra_info + hw_info + serialArgs
  resultsDir = program['name'] + resultsAnno
  progName = os.path.join(os.path.dirname(program['path']),sim_list.get_binary_name(prog_key, vec_config))
  
  # add hw line size to args
  hw_opts += ' ' + sim_list.get_cacheline_opt(vec_config)
  
  cmd = gem5_cmd(progName, optionsStr, resultsDir, numCpus, True, hw_opts)
  print(cmd)
  try:
    result = subprocess.check_output(cmd, shell=True, stderr=subprocess.STDOUT)
  except:
    try:
      print(result)
    except:
      pass
    return False
  # print(result)

  # make sure that the run passed
  success = success_regex.search(result)
  if (success):
    return True
  else:
    print(result)
    return False

# run all configuration for a single benchmark
# this must be done serially due to recompiling benchmark
# (but can parallize across benchmarks)
def run_config(vec_config, num_cpus, prog_key, argv, hw_opts):
  all_pass = True

  ret = run_prog(num_cpus, prog_key, argv, vec_config, hw_opts)
  if (not ret):
    print('-------------------------------------------------------------')
    print('{} failed w/ config {} hw {}'.format(prog_key, vec_config, hw_opts))
    print('-------------------------------------------------------------')
    all_pass = False


# MAIN
# 

num_cpus = args.num_cpus

# limit to 16 threads, each benchmark in parallel, but configs are serial
pool = multiprocessing.Pool(processes=60)
jobs = []

for k,v in sim_list.sim_configs.items():
  prog_key = k
  sim_config = v
  # prog_def  = sim_list.programs[prog_name] # TODO can't pass dicts?
  argv      = sim_config['argv']
  vec_configs = sim_config['vec']
  if ('hw_opts' in sim_config):
    hw_opts = sim_config['hw_opts']
  else:
    hw_opts = ['']
  for vec_config in vec_configs:

    # compile serially so can launch job with overwritting binary
    compile_prog(num_cpus, prog_key, vec_config)

    for hw_opt in hw_opts:
      # the new file will have the same name as the old file, but also specify the new dir
      proc = pool.apply_async(run_config, args=(vec_config, num_cpus, prog_key, argv, hw_opt, ))
      jobs.append(proc)

      # sleep for some time to give time for gem5 to load the binary
      # time.sleep(11)
    

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



