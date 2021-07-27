'''

  Run a gem5 simulation and create a stats file reflecting relevant info
  about the sim.
  
  This script does not do any analysis on the stats file, that is left
  to another script. (so can do new analysis without running again)
  
  Might consider dropping a json in the results dir with run info rather than in dir name?
  Also need to be able to recompile the program when want to add more cores or disable/enable vector

  Authors: Philip Bedoukian
'''

import os, subprocess, time, argparse, re, random

import multiprocessing, time

# include configs we want to run
import sim_list
from timer import Timer

# cmd line arguments
parser = argparse.ArgumentParser(description='Run gem5 simulation and output informative stats files')
parser.add_argument('--build', default='../../build/RVSP/gem5.opt', help='Path to gem5 build')
parser.add_argument('--config', default='../../configs/phil/brg_hammerblade.py', help='Path to gem5 build')
parser.add_argument('--results', default='../../results', help='Path to place to store results')
parser.add_argument('--num-cpus', default=64, help='Number of cpus to use in simulation')
parser.add_argument('--sim-list', default='./experiments/test.json', help='Json file describing sim configs')
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
  try:
    result = subprocess.check_output(cmplCmd, shell=True, stderr=subprocess.STDOUT)
    return True
  except:
    print('Failed to compile ' + binary_name)
    return False
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
  # add hardware vlen to args
  hw_opts += ' ' + sim_list.get_hardware_vlen(vec_config)
  
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

  timer = Timer()
  with timer.time('{} sw {} hw {}'.format(prog_key, vec_config, hw_opts)):
    ret = run_prog(num_cpus, prog_key, argv, vec_config, hw_opts)
    if (not ret):
      print('-------------------------------------------------------------')
      print('{} failed w/ config {} hw {}'.format(prog_key, vec_config, hw_opts))
      print('-------------------------------------------------------------')


# MAIN
# 

# limit parallel threads, each benchmark in parallel, but configs are serial
pool = multiprocessing.Pool(processes=70)
jobs = []
compiled_binaries = []

sim_configs = sim_list.generate_sim_config_from_file(args.sim_list)
for config in sim_configs:
  prog_key   = config['program']
  argv       = config['argv']
  sw_configs = config['software']
  hw_configs = config['hardware']

  for sw in sw_configs:
    num_cpus = sim_list.get_num_cpus(sw, args.num_cpus)

    # compile if havent compiled in this run
    binary_name = sim_list.get_binary_name(prog_key, sw)
    if not binary_name in compiled_binaries:
      # compile serially so can launch job with overwritting binary
      # if fails, kill all jobs and exit
      if (not compile_prog(num_cpus, prog_key, sw)):
        pool.terminate()
        quit()
    else:
      print('[[INFO]] Skip recompilation of ' + binary_name + ' for ' + str(sw) + ' ' + str(hw_configs))

    # remember that we compiled the program so dont do it again
    compiled_binaries.append(binary_name)

    for hw in hw_configs:
      # the new file will have the same name as the old file, but also specify the new dir
      proc = pool.apply_async(run_config, args=(sw, num_cpus, prog_key, argv, hw, ))
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



