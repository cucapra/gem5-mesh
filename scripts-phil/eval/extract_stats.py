'''

  Extract a collection of stats files from completed simulations and write a csv

'''

import os, subprocess, time, argparse, re
from stat_list import stats
import get_energy

# cmd line arguments
parser = argparse.ArgumentParser(description='Analyze stats file in a given directory')
parser.add_argument('--sims', default='../../results', help='Path with results you want to analyze')
parser.add_argument('--outfile', default='../../results/extract.csv', help='CSV Path where extracted data should go')
parser.add_argument('--prefix', default='', help='prefix of directory name to parse, could be program for example')
args = parser.parse_args()

#
# Identify data directories and how to extract parameters about program from them

dirPaths = []

prefix = args.prefix

# created by top/eval/run_sim.py
nameConv = r'^' + prefix + r'(.*)$'
annoConv = r'-([a-zA-Z]+)(\d+\.?\d*)'
metaConv = r'-([a-zA-Z0-9_]+)'
prefixRegex = re.compile(nameConv)
annoRegex = re.compile(annoConv)
metaRegex = re.compile(metaConv)

# try to parse as prog-vec-othermeta
mc = '[a-zA-Z0-9_]+'
mcdash = '[a-zA-Z0-9_-]+'
dirConv = '({})-({})-({})'.format(mc, mc, mcdash)
dirRegex = re.compile(dirConv)


#
# Function defs

# parse results directory name
def parse_dir_name(dirName):
  annos = {}
  dirMatch = dirRegex.search(dirName)
  if (dirMatch):
    annos['prog'] = dirMatch.group(1)
    annos['config'] = dirMatch.group(2)
    annos['meta'] = dirMatch.group(3)
  else:
    annos['prog'] = dirName
    annos['config'] = 'unknown'
    annos['meta'] = 'na'
    
  return annos

def is_hist_stat(v):
  return ('hist' in v) and v['hist']

def is_formula_stat(v):
  return ('formula' in v)

def find_max_buckets():
  return 20
  # max_buckets = 0
  # for k,v in stats.items():
  #   if (is_hist_stat(v)):
  #     cur_len = len(v['buckets'])
  #     if (cur_len > max_buckets):
  #       max_buckets = cur_len
  # return max_buckets

def can_normal_write(v):
  return (not is_hist_stat(v) or 'energy' in v) # TODO assume energy is merged into one value

# parse stats file
def parse_file(fileName):
  # reset stat table
  for k, v in stats.items():
    if (is_formula_stat(v)):
      pass
    elif (is_hist_stat(v)):
      v['buckets'] = {}
      v['avg'] = 0
    else:
      v['avg'] = 0
      v['count'] = 0
  
  with open(fileName, 'r') as fin:
    # foreach line search for each regex
    for line in fin:
      for k, v in stats.items():
        if (is_formula_stat(v)):
          break

        match = v['regex'].search(line)
        if (match):
          # TODO assumes only one copy of this hist
          if (is_hist_stat(v)):
            bucket_range = match.group(1)
            val          = match.group(2)
          else:
            # get value
            val = match.group(1)

          # setting in stat to ignore in avg when value is 0
          # generally this means this condition is not possible (like in vector config)
          if ('ignore-zero' in v):
            ignore_zero = v['ignore-zero']
          else:
            ignore_zero = False

          if ('upper-bound' in v):
            upper_bound = v['upper-bound']
            has_upper_bound = True
          else:
            has_upper_bound = False
          
          try:
            arith_val = int(val)
          except:
            arith_val = float(val)
          
          if ((not (ignore_zero and (arith_val == 0))) and (not has_upper_bound or arith_val < upper_bound)):
              if (is_hist_stat(v)):
                if (not bucket_range in v['buckets']):
                  v['buckets'][bucket_range] = 0
                v['buckets'][bucket_range] += arith_val
              else:
                v['avg'] += arith_val
                v['count'] += 1
          
          # no reason to search for other values
          #break
          
  # get avg or just keep as sum
  for k, v in stats.items():
    if (is_formula_stat(v)):
      continue

    average = True
    if ('average' in v):
      average = v['average']

    if (average):
      if (not is_hist_stat(v)):
        if (v['count'] > 0):
          v['avg'] /= v['count']

    # check if should do energy analysis
    if ('energy' in v):
      if v['energy'] == 'inst':
        v['avg'] = get_energy.get_instruction_energy(v['buckets'])
      else:
        # just use read energy for all accesses
        v['avg'] = get_energy.get_read_energy(v['energy'], v['avg'])

  for k, v in stats.items():
    if (is_formula_stat(v)):
      v['avg'] = 0
      for s in v['formula']:
        v['avg'] += stats[s]['avg']
#
# find which directories contain legit data

# https://www.tutorialspoint.com/python/os_walk.htm
for root, dirs, files in os.walk(args.sims):
  #for name in files:
  #  print(os.path.join(root, name))
  for name in dirs:
    path = os.path.join(root, name)
    
    # check that the name is in a format we can read
    dirMatch = prefixRegex.search(name)
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

# collect data between all runs together
dataCSV = ''
# write all histograms, don't really fit nicely between diff runs so each run
# going to be printed seperately
histCSV = []
for i in range(find_max_buckets()):
  histCSV.append('')

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
    if (not can_normal_write(v)):
      for b,d in v['buckets'].items():
        print('\t{0}::{1}: {2}'.format(v['name'], b, str(d)))
    else:
      print('\t{0}: {1}'.format(v['name'], v['avg']))
    
  # 
  # serialize parameters and data into string row by row
  
  dataCSV += '\n'
  
  # add file name
  dataCSV += os.path.basename(dirPath) + ', '

  # parameters (might not have been annotated with parameter)
  for param in parameters:
    if (param in annos):
      dataCSV += str(annos[param])
    dataCSV += ', '
  
  # data
  for k, v in stats.items():
    if (can_normal_write(v)):
      dataCSV += '{0}, '.format(str(v['avg']))
    
  #dataCSV += '\n'

  # hist data. from right to left base on run
  # TODO don't put into extract stats currently
  # for k, v in stats.items():
  #   if (is_hist_stat(v)):
  #     buckets = v['buckets']
  #     buck_len = len(buckets)
  #     for i in range(0, find_max_buckets()):
  #       if (i < buck_len):
  #         (b, d) = buckets[i]
  #         if (not 'meta' in annos):
  #           annos['meta'] = ''
  #         histCSV[i] += '{0}:{1},{2},{3},,'.format(annos['meta'], v['name'], str(b), str(d))
  #       else:
  #         histCSV[i] += ',,,,'
  
#
# write output to a csv

with open(args.outfile, 'w+') as fout:
  # header line
  fout.write('run' + ', ')
  for param in parameters:
    fout.write(param + ', ')
  for k, v in stats.items():
    if (can_normal_write(v)):
      fout.write('{0}, '.format(v['name']))
      
  #fout.write('\n')

  # add all of the data
  fout.write(dataCSV)

  # write hist
  fout.write('\n\n')
  for i in range(len(histCSV)):
    fout.write(histCSV[i])
    fout.write('\n')
  
  
  
  

