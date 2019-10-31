'''

  Run a gem5 simulation and create a stats file reflecting relevant info
  about the sim.
  
  This script does not do any analysis on the stats file, that is left
  to another script. (so can do new analysis without running again)
  
  Might consider dropping a json in the results dir with run info rather than in dir name?
  Also need to be able to recompile the program when want to add more cores or disable/enable vector
'''

import os, subprocess, time, argparse, re

# cmd line arguments
parser = argparse.ArgumentParser(description='Run gem5 simulation and output informative stats files')
parser.add_argument('--build', default='/home/pbb59/hammerblade/gem5/build/RVSP/gem5.opt', help='Path to gem5 build')
parser.add_argument('--config', default='/home/pbb59/hammerblade/gem5/configs/phil/brg_hammerblade.py', help='Path to gem5 build')
parser.add_argument('--results', default='/home/pbb59/hammerblade/gem5/results', help='Path to place to store results')
args = parser.parse_args()

# specify programs. with the path to the program, the executible name, the default options, and string to check to see if successful
progDir0 = '/home/pbb59/hammerblade/gem5/programs-phil/spad/'
programs = {

  'vvadd' : { 'name': 'vvadd', 'path' : progDir0 + 'vvadd-big/vvadd', 
    'options' : lambda psize: str(psize), 
    'success' : '\[\[SUCCESS\]\]'},
    
  'gemm'  : { 'name': 'gemm',  'path' : progDir0 + 'gemm/gemm', 
    'options' : lambda psize: '{0} {0} {0}'.format(str(psize)),
    'success' : '\[\[SUCCESS\]\]'}

}

# create a template for the gem5 command line
gem5_cmd = lambda program, options, result, cpus, vec: \
  '{} -d {}/{} {} --cmd={} --options=\"{}\" --num-cpus={} {}'.format(
      args.build, args.results, result, args.config, program, options, str(cpus), '--vector' if vec else '')
  
# compile command that chooses whether to use scratchpad optimizations
# how many cores/sps are present, and whether to use vector mode
def compile_cmd(program_dir, cpus, use_sp, use_vec):
  cmd = 'make clean -C {}'.format(program_dir)
  cmd += ' && '
  if (not use_sp):
    cmd += 'ENV_NO_SP=1 '
  if (not use_vec):
    cmd += 'ENV_NO_VEC=1 '
    
  cmd += 'ENV_N_SPS={} make -C {}'.format(cpus, program_dir)
  return cmd

def run_prog(numCpus, use_vec, use_sps, prog_name):
  
  # check if the success flag was asserted using regex checking on the gem5 output
  success_regex = re.compile(programs[prog_name]['success'])
  
  cmplCmd = compile_cmd(os.path.dirname(programs[prog_name]['path']), numCpus, use_sps, use_vec)
  result = subprocess.check_output(cmplCmd, shell=True)
  print(result)

  # run program with increasing problem sizes in factors of 2
  minSize = 16
  maxSize = 64 # 256
  currSize = minSize
  while currSize <= maxSize:
    # run with currSize
    optionsStr = programs[prog_name]['options'](currSize)
    resultsAnno = '-vec' + str(int(use_vec)) + '-sp' + str(int(use_sps)) + '-size' + str(currSize)
    resultsDir = programs[prog_name]['name'] + resultsAnno
    cmd = gem5_cmd(programs[prog_name]['path'], optionsStr, resultsDir, numCpus, use_vec)
    print(cmd)
    result = subprocess.check_output(cmd, shell=True)
    print(result)
  
    # double the size for future runs
    currSize *= 2
  
    # make sure that the run passed
    success = success_regex.search(result)
    assert(success)

  
# choose which programs to run with diff parameters

# fixed parameters for the run, compile the binary for these
numCpus = 4
use_vec = True
use_sps = True

# run a program from the list above
run_prog(numCpus, use_vec, use_sps, 'gemm')

