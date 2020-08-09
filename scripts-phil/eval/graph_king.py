'''
  Takes data created by extract_stats.py and makes graphs
'''

import matplotlib as mpl
# switch away from display backend to Agg backend
mpl.use('Agg')
import matplotlib.pyplot as plt
from cycler import cycler
import numpy as np
from math import floor, ceil
from copy import deepcopy

from layout_helper import get_mesh_dist_sequence

default_prop_cycle = mpl.rcParams['axes.prop_cycle']

# extract all data for a specific field across all benchmarks
# only get the yaxis data, assumed xaxis is trivial to get (i.e., arange or core_id)
def group_line_data(data, desired_field):
  labels = []
  configs = []
  values = []

  for row in data:
    if (not desired_field in row):
      continue
    labels.append('{}_{}'.format(row['prog'], row['config']))
    configs.append(row['config'])
    values.append(row[desired_field])

  return (labels, configs, values)

def avg_by_hops(labels, configs, values, include_v4, include_v16):
  # figure out xaxis (#hops) depending on config. 
  # remove certain values if scalar or inactive core
  v4_hops = get_mesh_dist_sequence('V4')
  v16_hops = get_mesh_dist_sequence('V16')

  # average together series with the same number of hops

  # print(v4_hops)
  # print(v16_hops)
  xaxes = []

  v4_configs = [
    'V4',
    'VEC_4_SIMD_VERTICAL',
    'VEC_4_REUSE_VERTICAL',
    'VEC_4_SIMD_UNROLL'
  ]
  v16_configs = [
    'V16'
  ]

  i = 0
  while (i < len(configs)):
    if (((configs[i] in v4_configs) and include_v4) or
        ((configs[i] in v16_configs) and include_v16)):
      hop_avgs = []
      hop_cnts = []
      max_hops = 7 if configs[i] in v16_configs else 3
      for j in range(1, max_hops+1):
        hop_avgs.append(0)
        hop_cnts.append(0)

      xaxes.append(np.arange(1, max_hops+1))

      hop_list = v16_hops if (configs[i] in v16_configs) else v4_hops
      for j in range(len(hop_list)):
        # avg >0
        hops = hop_list[j]
        if (hops > 0):
          hop_avgs[hops - 1] += values[i][j]
          hop_cnts[hops - 1] += 1

      for j in range(len(hop_avgs)):
        hop_avgs[j] = float(hop_avgs[j]) / float(hop_cnts[j])

      values[i] = hop_avgs

    # delete non vector series
    else:
      labels.pop(i)
      configs.pop(i)
      values.pop(i)
      i -= 1
    i += 1

  return (labels, configs, values, xaxes)


# group together similar series and get preferred field
# expects 3 meta fields(prog, config, meta) along with desired_field
def group_bar_data(data, desired_field):
  # hash of hash
  values = {}

  # extra data
  for row in data:
    # first check if we have something to match. both prog and meta should match
    label_str = row['prog'] + row['meta']
    if (not label_str in values):
      values[label_str] = {}
      values[label_str]['_meta_'] = {}


    if (desired_field in row):
      single_val = row[desired_field]
    else:
      single_val = 0

    values[label_str][row['config']] = single_val
    values[label_str]['_meta_']['prog_name'] = row['prog']

  # flatten labels and values for display
  labels = []
  flat_values = [] # array of arrays

  # assign array position to each config
  config_map = []
  for k,v in values.items():
    for c in v.keys():
      if (not c in config_map and c != '_meta_'):
        config_map.append(c)

  for i in range(len(config_map)):
    flat_values.append([])

  # create flat data to graph
  for k,v in values.items():
    # just use progname
    labels.append(v['_meta_']['prog_name'])
    for i in range(len(config_map)):
      c = config_map[i]
      if (c in v):
        flat_values[i].append(v[c])
      else:
        flat_values[i].append(0)

  return (labels, config_map, flat_values)

# try to normalize to NV otherwise do from first value
def normalize(sub_labels, values, pref_base='NV'):
  # figure out what to normalize with
  norm_idx = 0
  for i in range(len(sub_labels)):
    if (pref_base == sub_labels[i]):
      norm_idx = i

  base_values = deepcopy(values[norm_idx])

  for j in range(len(values)):
    for i in range(len(values[j])):
      try:
        values[j][i] = float(values[j][i]) / float(base_values[i])
      except:
        print('failed to normalize ' + sub_labels[j])
        values[j][i] = 0
      
def inverse(values):
  for j in range(len(values)):
    for i in range(len(values[j])):
      try:
        values[j][i] = 1.0 / float(values[j][i])
      except:
        values[j][i] = 0

def add_average(labels, values):
  # add an average column
  labels.append("Average")
  for config in values:
    summ = 0
    nnz = 0
    for v in config:
      summ += v
      if (v != 0):
        nnz += 1

    if (nnz > 0):
      config.append(summ / nnz)
    else:
      config.append(0)


# plot speedup
# group together same benchmark (if metadata the same)
def plot_speedup(data):
  (labels, sub_labels, values) = group_bar_data(data, 'cycles')

  # flip from cycles to speedup normalized to NV
  normalize(sub_labels, values)
  inverse(values)
  add_average(labels, values)

  bar_plot(labels, sub_labels, values, 'Speedup Relative to Baseline Manycore (NV)', 'Speedup')

def plot_energy(data):
  (labels, sub_labels, values) = group_bar_data(data, 'energy-sum(nJ)')
  normalize(sub_labels, values)
  add_average(labels, values)
  bar_plot(labels, sub_labels, values, 'Energy', 'Energy', True)
  # need to have a buttom=[] when define bar. where bottom is sum of prev

def plot_inst_energy(data):
  (labels, sub_labels, values) = group_bar_data(data, 'inst-cnts-energy(nJ)')
  normalize(sub_labels, values)
  add_average(labels, values)
  bar_plot(labels, sub_labels, values, 'InstEnergy relative to Baseline Manycore', 'Instruction_Energy', True)

def plot_icache_energy(data, norm = False):
  (labels, sub_labels, values) = group_bar_data(data, 'icache-access-energy(nJ)')
  if (norm):
    normalize(sub_labels, values)
  add_average(labels, values)
  name = 'ICache_Energy'
  yaxis = 'Icache Energy(nJ)'
  if (norm):
    name = 'ICache_Energy_Norm'
    yaxis = 'Icache Energy relative to Baseline Manycore'
  bar_plot(labels, sub_labels, values, yaxis, name, norm)

def plot_dmem_energy(data):
  (labels, sub_labels, values) = group_bar_data(data, 'dmem-access-energy(nJ)')
  # normalize(sub_labels, values)
  add_average(labels, values)
  bar_plot(labels, sub_labels, values, 'DmemEnergy relative to Baseline Manycore', 'DMem_Energy', False)  

def plot_llc_energy(data):
  (labels, sub_labels, values) = group_bar_data(data, 'llc-access-energy(nJ)')
  # normalize(sub_labels, values)
  add_average(labels, values)
  bar_plot(labels, sub_labels, values, 'LLC Energy', 'LLC_Energy', False) 

def plot_first_frame_rdy(data):
  (labels, sub_labels, values) = group_bar_data(data, 'frame-occupancy1')
  add_average(labels, values)
  bar_plot(labels, sub_labels, values, 'Next frame ready on remem fraction', 'NextFrameRdy', False) 

def plot_cpi(data):
  (labels, sub_labels, values) = group_bar_data(data, 'active-cpi')
  add_average(labels, values)
  bar_plot(labels, sub_labels, values, 'CPI (Active Cores)', 'CPI', False) 

def plot_inet_stalls(data, includeV4, includeV16):
  (labels, configs, values) = group_line_data(data, 'frac_mesh_stall_sep')
  (labels, configs, values, xaxes) = avg_by_hops(labels, configs, values, includeV4, includeV16)
  title = 'Stalls_{}{}'.format('v4' if includeV4 else '', 'v16' if includeV16 else '')
  line_plot(xaxes, values, labels, 'Hops', 'INET stalls relative to total vector cycles', title)

def plot_frame_stalls(data, includeV4, includeV16):
  (labels, configs, values) = group_line_data(data, 'frac_token_stall_sep')
  (labels, configs, values, xaxes) = avg_by_hops(labels, configs, values, includeV4, includeV16)
  title = 'Frame_Stalls_{}{}'.format('v4' if includeV4 else '', 'v16' if includeV16 else '')
  line_plot(xaxes, values, labels, 'Hops', 'Frame stalls relative to total vector cycles', title)

# create specified barplot and write to file
def bar_plot(labels, sub_labels, values, ylabel, title, annotate=True):
  mpl.rcParams['axes.prop_cycle'] = default_prop_cycle
  # labels = ['G1', 'G2', 'G3', 'G4', 'G5']
  # men_means = [20, 34, 30, 35, 27]
  # women_means = [25, 32, 34, 20, 25]
  x = np.arange(len(labels))  # the label locations

  # figure out bar dimensions
  slack = 0.3
  num_sub_bars = len(sub_labels)
  width = (1 - slack) / num_sub_bars # the width of a single bar

  first_bar_offset = width / -2 * (num_sub_bars-1)

  fig, ax = plt.subplots()
  rects = []
  for i in range(num_sub_bars):
    new_rect = ax.bar(x + first_bar_offset + i * width, values[i], width, label=sub_labels[i])
    rects.append(new_rect)
  # rects1 = ax.bar(x - width/2, men_means, width, label='Men')
  # rects2 = ax.bar(x + width/2, women_means, width, label='Women')
  # Add some text for labels, title and custom x-axis tick labels, etc.
  ax.set_ylabel(ylabel)
  ax.set_title(title)
  ax.set_xticks(x)
  ax.set_xticklabels(labels, rotation=45, ha='right')
  ax.legend()
  def autolabel(rects):
      """Attach a text label above each bar in *rects*, displaying its height."""
      for rect in rects:
          height = rect.get_height()
          if (height == 0.0):
            continue
          ax.annotate('{:.1f}'.format(height),
                      xy=(rect.get_x() + rect.get_width() / 2, height),
                      xytext=(0, 3),  # 3 points vertical offset
                      textcoords="offset points",
                      ha='center', va='bottom')
                
  if (annotate):
    for r in rects:
      autolabel(r)

  fig.tight_layout()
  # plt.show()

  plt.savefig(str(title) + '.png')

# create specified lineplot and write to file
# provide all y_axes and either a single or multiple x_axes
def line_plot(x_axes, y_axes, labels, xlabel, ylabel, title, duplicate_x=False):
  mpl.rcParams['axes.prop_cycle'] = cycler(linestyle=['-', '--', '-.']) * default_prop_cycle
  fig, ax = plt.subplots()

  for i in range(len(y_axes)):
    if (duplicate_x):
      x_axis = x_axes
    else:
      x_axis = x_axes[i]

    y_axis = y_axes[i]
    ax.plot(x_axis, y_axis)

  ax.set_xlabel(xlabel)
  ax.set_ylabel(ylabel)
  ax.set_title(title)
  ax.legend(labels)

  fig.tight_layout()

  plt.savefig(str(title) + '.png')