'''

  Extract a collection of stats files from completed simulations and write a csv

'''

import os, subprocess, time, argparse, re
from stat_list import cpu_stats, gpu_stats
import get_energy
import graph_king

from collections import OrderedDict

# cmd line arguments
parser = argparse.ArgumentParser(description='Analyze stats file in a given directory')
parser.add_argument('--cpu', action="store_true", help='Want to analyze cpu/manycore results')
parser.add_argument('--gpu', action="store_true", help='Want to analyze gpu results')
parser.add_argument('--cpu-sims', default='../../results', help='Path with manycore results you want to analyze')
parser.add_argument('--gpu-sims', default='../../../gem5-gcn3/results/', help='Path with gpu results you want to analyze')
parser.add_argument('--outfile', default='./extract.csv', help='CSV Path where extracted data should go')
parser.add_argument('--prefix', default='', help='prefix of directory name to parse, could be program for example')
get_energy.define_options(parser)
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


# setup energy model
get_energy.setup_energy_model(args)

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
def parse_file(fileName, stat_info):
  # extract stat data
  stat_data = OrderedDict()

  # reset stat table
  for k, v in stat_info.items():
    stat_data[k] = {}

    if (is_formula_stat(v)):
      pass
    elif (is_hist_stat(v)):
      stat_data[k]['buckets'] = {}
      stat_data[k]['avg'] = 0
    else:
      stat_data[k]['avg'] = 0
      stat_data[k]['count'] = 0
  
  with open(fileName, 'r') as fin:
    # foreach line search for each regex
    for line in fin:
      for k, v in stat_info.items():
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
                if (not bucket_range in stat_data[k]['buckets']):
                  stat_data[k]['buckets'][bucket_range] = 0
                stat_data[k]['buckets'][bucket_range] += arith_val
              else:
                if ('max' in v and v['max']):
                  if (arith_val > stat_data[k]['avg']):
                    stat_data[k]['avg'] = arith_val
                else:
                  stat_data[k]['avg'] += arith_val
                  stat_data[k]['count'] += 1
          
          # no reason to search for other values
          #break
          
  # get avg or just keep as sum
  for k, v in stat_info.items():
    if (is_formula_stat(v)):
      continue

    average = True
    if ('average' in v):
      average = v['average']

    if (average):
      if (not is_hist_stat(v)):
        if (stat_data[k]['count'] > 0):
          stat_data[k]['avg'] /= stat_data[k]['count']

    # check if should do energy analysis
    if ('energy' in v):
      if v['energy'] == 'inst':
        stat_data[k]['avg'] = get_energy.get_instruction_energy(stat_data[k]['buckets'])
      else:
        # just use read energy for all accesses
        stat_data[k]['avg'] = get_energy.get_read_energy(v['energy'], stat_data[k]['avg'])

  for k, v in stat_info.items():
    if (is_formula_stat(v)):
      stat_data[k]['avg'] = 0
      for s in v['formula']:
        stat_data[k]['avg'] += stat_data[s]['avg']

  return stat_data

# parse a results directory, and return all data in a dict
def parse_results_dir(results_dir_name, stats_info):
  #
  # find which directories contain legit data
  dirPaths = []

  # https://www.tutorialspoint.com/python/os_walk.htm
  for root, dirs, files in os.walk(results_dir_name):
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

  all_data = []
  for dirPath in dirPaths:
    #
    # parse dir and stats
    
    # get size of file, TODO should try to do on first check and insert into a dict
    annos = parse_dir_name(os.path.basename(dirPath))
    
    # get path to stats
    statsFile = os.path.join(dirPath, 'stats.txt')
    rawStats = parse_file(statsFile, stats_info)

    # cleanup raw data a little bit to just include avg
    stat_dict = OrderedDict()
    for k,v in annos.items():
      stat_dict[k] = v
    for k,v in rawStats.items():
      stat_dict[k] = v['avg']

    stat_dict['_path_'] = dirPath

    all_data.append(stat_dict)

  return all_data


#
# Extract data from directories

# if neither asserted default to cpu
if (not args.cpu and not args.gpu):
  args.cpu = True

manycore_data = []
gpu_data = []

if (args.cpu):
  manycore_data = parse_results_dir(args.cpu_sims, cpu_stats)
if (args.gpu):
  gpu_data      = parse_results_dir(args.gpu_sims, gpu_stats)

all_data = manycore_data + gpu_data

# determine all keys
keys = []
for data in all_data:
  for k in data.keys():
    if (not k in keys):
      keys.append(k)

#
# Backend outputs

# collect data between all runs together
dataCSV = ''

# write header row
dataCSV += 'name, '
for k in keys:
  dataCSV += '{}, '.format(k)


# TODO currently no hist support
# write all histograms, don't really fit nicely between diff runs so each run
# going to be printed seperately
# histCSV = []
# for i in range(find_max_buckets()):
#   histCSV.append('')

#
# serialize and print extracted stats

for data in all_data:
  dirPath = data['_path_']

  print(dirPath)

  for k in keys:
    if (k == '_path_'):
      continue
    # if (not can_normal_write(v)):
    #   for b,d in d[k]['buckets'].items():
    #     print('\t{0}::{1}: {2}'.format(k, b, str(d)))
    # else:
    if (k in data):
      dat = data[k]
    else:
      dat = 0

    print('\t{0}: {1}'.format(k, dat))
    
  # 
  # serialize parameters and data into string row by row

  dataCSV += '\n'

  # add file name
  dataCSV += os.path.basename(dirPath) + ', '

  # data
  for k in keys:
    # if (can_normal_write(v)):

    if (k in data):
      dat = data[k]
    else:
      dat = 0

    dataCSV += '{}, '.format(str(dat))
  
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
  # fout.write('run' + ', ')
  # for k, v in stats.items():
  #   if (can_normal_write(v)):
  #     fout.write('{0}, '.format(v['name']))
      
  #fout.write('\n')

  # add all of the data
  fout.write(dataCSV)

  # # write hist
  # fout.write('\n\n')
  # for i in range(len(histCSV)):
  #   fout.write(histCSV[i])
  #   fout.write('\n')
  

# plot data to graphs
graph_king.plot_speedup(all_data)
graph_king.plot_energy(all_data)
graph_king.plot_inst_energy(all_data)
graph_king.plot_icache_energy(all_data)
graph_king.plot_icache_energy(all_data, True)
graph_king.plot_dmem_energy(all_data)
graph_king.plot_llc_energy(all_data)
graph_king.plot_first_frame_rdy(all_data)
graph_king.plot_cpi(all_data)
  
  

