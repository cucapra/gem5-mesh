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
  'vvadd' : { 'name': 'vvadd', 'path' : progDir0 + 'vvadd/vvadd', 'options' : '16', 'success' : '\[\[SUCCESS\]\]'}

}

# create a template for the gem5 command line
gem5_cmd = lambda program, options, result, cpus: \
  '{0} -d {1}/{2} {3} --cmd={4} --options=\"{5}\" --num-cpus={6}'.format(
      args.build, args.results, result, args.config, program, options, str(cpus))
      
# check if the success flag was asserted using regex checking on the gem5 output
success_regex = re.compile(programs['vvadd']['success'])

# run vvadd with multiple sizes in factors of 2
numCpus = 4
minSize = 16
maxSize = 32
currSize = minSize
while currSize <= maxSize:
  # run with currSize
  cmd = gem5_cmd(programs['vvadd']['path'], str(currSize), programs['vvadd']['name'] + '-size' + str(currSize), numCpus)
  print(cmd)
  result = subprocess.check_output(cmd, shell=True)
  print(result)
  
  # make sure that the run passed
  success = success_regex.search(result)
  assert(success)

  # double the size
  currSize *= 2


