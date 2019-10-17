'''

  Extract a collection of stats files from completed simulations and write a csv

'''

import os, subprocess, time, argparse, re

# cmd line arguments
parser = argparse.ArgumentParser(description='Analyze stats file in a given directory')
parser.add_argument('--sims', default='/home/pbb59/hammerblade/gem5/results', help='Path with results you want to analyze')
parser.add_argument('--outfile', default='/home/pbb59/hammerblade/gem5/results/extract.csv', help='CSV Path where extracted data should go')
args = parser.parse_args()

#
# Identify data directories and how to extract parameters about program from them

dirPaths = []

# created by top/eval/run_sim.py
nameConv = 'gemm([-a-zA-Z0-9]+)'
annoConv = '-([a-zA-Z]+)([0-9]+)'
dirRegex = re.compile(nameConv)
annoRegex = re.compile(annoConv)

#
# Get avg of each of these stats

floatRegexStr = '([+-]?([0-9]*[.])?[0-9]+)'
intRegexStr = '([0-9]+)'

stats = { 
  'cycles' : { 'name' : 'cycles', 'regex' : re.compile('system.cpu0+.numCycles\s*' + intRegexStr), 'avg' : 0, 'count' : 0 },
  'icache' : { 'name' : 'icache_access', 'regex' : re.compile('system.icaches[0-9]+.L1cache.demand_accesses\s*' + intRegexStr), 'avg' : 0, 'count' : 0 }, 
}


#
# Function defs

# parse results directory name
def parse_dir_name(dirName):
  # parse the name of the file
  nameMatch = dirRegex.search(dirName)
  if (nameMatch):
    annotation = nameMatch.group(1)
    print(annotation)
  else:
    assert(False)
    
  # parse annotation
  annos = {}
  annos['prog'] = 'gemm'
  
  for match in annoRegex.finditer(annotation):
    field = match.group(1)
    value = int(match.group(2))
    
    annos[field] = value
    
  return annos

# parse stats file
def parse_file(fileName):
  # reset stat table
  for k, v in stats.items():
    v['avg'] = 0
    v['count'] = 0
  
  with open(fileName, 'r') as fin:
    # foreach line search for each regex
    for line in fin:
      for k, v in stats.items():
        match = v['regex'].search(line)
        if (match):
          # get value (always int?)
          val = match.group(1)
          v['avg'] += int(val)
          v['count'] += 1
          
          # no reason to search for other values
          break
          
  # get avg
  for k, v in stats.items():
    if (v['count'] > 0):
      v['avg'] /= v['count']
      

#
# find which directories contain legit data

# https://www.tutorialspoint.com/python/os_walk.htm
for root, dirs, files in os.walk(args.sims):
  #for name in files:
  #  print(os.path.join(root, name))
  for name in dirs:
    path = os.path.join(root, name)
    
    # check that the name is in a format we can read
    dirMatch = dirRegex.search(name)
    if (not dirMatch):
      continue
      
    # check that this directory has a stats file
    foundStats = False
    for root_, dirs_, files_ in os.walk(path):
      # only look one level down
      if (root_ == path):
        for fname in files_:
          if (fname == 'stats.txt'):
            foundStats = True
    
    if (not foundStats):
      continue
      
    # check if stats file is empty or not (empty is sign of failed run)
    numLines = 0
    with open(os.path.join(path, 'stats.txt')) as fin:
      for line in fin:
        numLines += 1
    
    if (numLines == 0):
      continue
    
    dirPaths.append(path)

# 
# figure out where each parameter should be displayed

parameters = []
for dirPath in dirPaths:
  annos = parse_dir_name(os.path.basename(dirPath))
  for k, v in annos.items():
    exists = False
    for param in parameters:
      if (param == k):
        exists = True
    if (not exists):
      parameters.append(k)

dataCSV = ''

#
# Extract data from directories 

for dirPath in dirPaths:
  #
  # parse dir and stats
  
  # get size of file, TODO should try to do on first check and insert into a dict
  annos = parse_dir_name(os.path.basename(dirPath))
  
  # get path to stats
  statsFile = os.path.join(dirPath, 'stats.txt')
  parse_file(statsFile)

  #
  # print extracted stats
  
  print(statsFile)
  for param in parameters:
    if (param in annos):
      val = annos[param]
    else:
      val = 'N/A'
    print('\t{0}: {1}'.format(param, val))
  for k, v in stats.items():
    print('\t{0}: {1}'.format(v['name'], v['avg']))
    
  # 
  # serialize parameters and data into string row by row
  
  # parameters (might not have been annotated with parameter)
  for param in parameters:
    if (param in annos):
      dataCSV += str(annos[param])
    dataCSV += ', '
  
  # data
  for k, v in stats.items():
    dataCSV += '{0}, '.format(str(v['avg']))
    
  dataCSV += '\n'
  
#
# write output to a csv

with open(args.outfile, 'w+') as fout:
  # header line
  for param in parameters:
    fout.write(param + ', ')
  for k, v in stats.items():
    fout.write('{0}, '.format(v['name']))
      
  fout.write('\n')
  
  # add all of the data
  fout.write(dataCSV)
  
  
  
  

