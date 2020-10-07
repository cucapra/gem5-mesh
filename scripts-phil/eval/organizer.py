'''
  Analyze and organize data in interesting ways
'''

import numpy as np
from math import floor, ceil, isnan
from copy import deepcopy
from graph_king import bar_plot, line_plot, heatmap
from table_king import make_table
from layout_helper import get_mesh_dist_sequence
from scipy.stats.mstats import gmean

# sort data according to specified order (for bar data)
def sort_by_sub_label(cur_order, values, desired_order):
  sorted_values = []
  for i in range(len(desired_order)):
    sorted_values.append([])

  for i in range(len(desired_order)):
    d = desired_order[i]
    # find d
    d_idx = -1
    for j in range(len(cur_order)):
      if cur_order[j] == d:
        d_idx = j

    if (d_idx >= 0):
      sorted_values[i] = values[d_idx]

  return (desired_order, sorted_values)

def format_bar_series(labels, sub_labels, values, desired_order):
  labels = deepcopy(labels)
  sub_labels = deepcopy(sub_labels)
  desired_order = deepcopy(desired_order)

  # remove any labels that have no fields
  j = 0
  while (j < len(desired_order)):
    l = desired_order[j]
    idx = -1
    for i in range(len(sub_labels)):
      if sub_labels[i] == l:
        idx = i

    has_nz = False
    if (idx >= 0):
      for v in values[idx]:
        if (v != 0 and not isnan(v)):
          has_nz = True

    if (not has_nz):
      desired_order.remove(l)
      j -= 1

    j += 1

  # sort sub bar series
  if len(desired_order) > 0:
    (sub_labels, values) = sort_by_sub_label(sub_labels, values, desired_order)
  else:
    sub_labels = []
    values = []

  # sort benchmarks alphabetically with python built-in
  old_labels = deepcopy(labels)
  labels = sorted(labels)
  
  # figure out where each bench went
  label_map = []
  for l in labels:
    for i in range(len(old_labels)):
      if (l == old_labels[i]):
        label_map.append(i)
        break

  # move fields based on sorting map
  old_values = deepcopy(values)
  for j in range(len(values)):
    for i in range(len(values[j])):
      values[j][i] = old_values[j][label_map[i]]


  return (labels, sub_labels, values)

# extract all data for a specific field across all benchmarks
# only get the yaxis data, assumed xaxis is trivial to get (i.e., arange or core_id)
def group_line_data(data, desired_field, desired_configs=[], label_with_config=False):
  labels = []
  configs = []
  values = []

  for row in data:
    if (not desired_field in row):
      continue
    if (not (len(desired_configs) == 0 or row['config'] in desired_configs)):
      continue
    if (label_with_config):
      labels.append('{}_{}'.format(row['prog'], row['config']))
    else:
      labels.append(row['prog'])
    configs.append(row['config'])
    values.append(row[desired_field])

  # alphabetize!
  # TODO factor the nexst two sections out b/c shared with bargraph sort
  # sort benchmarks alphabetically with python built-in
  old_labels = deepcopy(labels)
  old_configs = deepcopy(configs)
  labels = sorted(labels)
  
  # figure out where each bench went
  label_map = []
  for l in labels:
    for i in range(len(old_labels)):
      if (l == old_labels[i]):
        label_map.append(i)
        break

  # move fields based on sorting map
  old_values = deepcopy(values)
  for i in range(len(values)):
    values[i] = old_values[label_map[i]]

  for i in range(len(configs)):
    configs[i] = old_configs[label_map[i]]

  return (labels, configs, values)

def avg_by_hops(labels, configs, values, include_v4, include_v16, include_scalar=False):
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
      min_cnt = 1
      max_cnt = max_hops+1
      if include_scalar:
        max_hops += 1
        min_cnt = 0
        max_cnt = max_hops
      for j in range(min_cnt, max_cnt):
        hop_avgs.append(0)
        hop_cnts.append(0)

      xaxes.append(np.arange(min_cnt, max_cnt))

      hop_list = v16_hops if (configs[i] in v16_configs) else v4_hops
      for j in range(len(hop_list)):
        # avg >0
        hops = hop_list[j]
        hop_idx = hops - 1
        if (include_scalar):
          hop_idx = hops
        if (hops > 0 or include_scalar):
          hop_avgs[hop_idx] += values[i][j]
          hop_cnts[hop_idx] += 1

      for j in range(len(hop_avgs)):
        hop_avgs[j] = float(hop_avgs[j]) / float(hop_cnts[j])

      values[i] = hop_avgs

    # TODO should have been sorted already befoer this so prob dont need
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
def group_bar_data(data, desired_field, desired_config_order= [ 'NV', 'NV_PF', 'V4', 'V16', 'GPU' ]):
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

  # format
  (labels, sub_labels, values) = format_bar_series(labels, config_map, flat_values, desired_config_order)

  return (labels, sub_labels, values)

# get the matrix data for a single config for a certain field, specifiying xdim
def group_heatmap_data(data, desired_config, desired_field, desired_prog, ncols):
  matrix_data = []
  for row in data:
    if (not desired_field in row):
      continue
    if (row['config'] != desired_config):
      continue
    if (row['prog'] != desired_prog):
      continue

    # make 2d array of data
    c = 0
    r = 0
    matrix_row = []
    for d in row[desired_field]:
      # needs to be float for heatmap
      matrix_row.append(d)
      c += 1
      if (c == ncols):
        r += 1
        c = 0
        matrix_data.append(matrix_row)
        matrix_row = []

  xlabels = np.arange(ncols)
  ylabels = np.arange(len(matrix_data))



  return (matrix_data, xlabels, ylabels)



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

def add_arith_mean(labels, values, include_zeros=False):
  # add an average column
  labels.append("ArithMean")
  for config in values:
    summ = 0
    nnz = 0
    for v in config:
      summ += v
      if (v != 0 or include_zeros):
        nnz += 1

    if (nnz > 0):
      config.append(summ / nnz)
    else:
      config.append(0)

def add_geo_mean(labels, values):
  # add an average column
  labels.append("GeoMean")
  for config in values:
    flatVals = []
    for v in config:
      if (v > 0):
        flatVals.append(v)

    geomean = gmean(flatVals)
    config.append(geomean)


# plot speedup
# group together same benchmark (if metadata the same)
def plot_speedup(data):
  (labels, sub_labels, values) = group_bar_data(data, 'cycles')

  # flip from cycles to speedup normalized to NV
  normalize(sub_labels, values)
  inverse(values)
  add_geo_mean(labels, values)

  bar_plot(labels, sub_labels, values, 'Speedup Relative to Baseline (NV)', 'Speedup', ylim=[0, 15], horiz_line=1)

def plot_energy(data):
  (labels, sub_labels, values) = group_bar_data(data, 'energy-sum(nJ)')
  normalize(sub_labels, values)
  add_geo_mean(labels, values)
  bar_plot(labels, sub_labels, values, 'Total On-Chip Energy Relative to Baseline (NV)', 'Energy', horiz_line=1)
  # need to have a buttom=[] when define bar. where bottom is sum of prev

def plot_inst_energy(data):
  (labels, sub_labels, values) = group_bar_data(data, 'inst-cnts-energy(nJ)')
  normalize(sub_labels, values)
  add_geo_mean(labels, values)
  bar_plot(labels, sub_labels, values, 'InstEnergy relative to Baseline Manycore', 'Instruction_Energy', True)

def plot_icache_energy(data, norm = False):
  (labels, sub_labels, values) = group_bar_data(data, 'icache-access-energy(nJ)')
  if (norm):
    normalize(sub_labels, values)
  add_geo_mean(labels, values)
  name = 'ICache_Energy'
  yaxis = 'I-Cache Energy(nJ)'
  if (norm):
    name = 'ICache_Energy_Norm'
    yaxis = 'Icache Energy relative to Baseline Manycore'
  bar_plot(labels, sub_labels, values, yaxis, name, horiz_line=1)

def plot_icache_access(data):
  (labels, sub_labels, values) = group_bar_data(data, 'icache_access')
  normalize(sub_labels, values)
  add_geo_mean(labels, values)
  bar_plot(labels, sub_labels, values, 'I-Cache Accesses Relative to Baseline (NV)', 'icache_accesses', horiz_line=1)

def plot_dmem_energy(data):
  (labels, sub_labels, values) = group_bar_data(data, 'dmem-access-energy(nJ)')
  # normalize(sub_labels, values)
  add_geo_mean(labels, values)
  bar_plot(labels, sub_labels, values, 'DmemEnergy relative to Baseline Manycore', 'DMem_Energy', False)  

def plot_llc_energy(data):
  (labels, sub_labels, values) = group_bar_data(data, 'llc-access-energy(nJ)')
  # normalize(sub_labels, values)
  add_geo_mean(labels, values)
  bar_plot(labels, sub_labels, values, 'LLC Energy', 'LLC_Energy', False) 

def plot_first_frame_rdy(data):
  (labels, sub_labels, values) = group_bar_data(data, 'frame-occupancy1')
  add_arith_mean(labels, values)
  bar_plot(labels, sub_labels, values, 'Next frame ready on remem fraction', 'NextFrameRdy', False) 

def plot_cpi(data):
  (labels, sub_labels, values) = group_bar_data(data, 'active-cpi')
  add_geo_mean(labels, values)
  bar_plot(labels, sub_labels, values, 'CPI (Active Cores)', 'CPI', False) 

def plot_inet_stalls(data):
  # # generate v4 plot
  # (labels, configs, values) = group_line_data(data, 'frac_mesh_stall_sep', desired_configs=['V4'])
  # (labels, configs, values, xaxes) = avg_by_hops(labels, configs, values, True, False)
  # line_plot(xaxes, values, labels, 'Hops', 'INET stalls relative to total vector cycles', 'Stalls_v4', False)
  # # generate v16 plot
  # (labels, configs, values) = group_line_data(data, 'frac_mesh_stall_sep', desired_configs=['V16'])
  # (labels, configs, values, xaxes) = avg_by_hops(labels, configs, values, False, True)
  # line_plot(xaxes, values, labels, 'Hops', 'INET stalls relative to total vector cycles', 'Stalls_v16', False)

  # generate v4 plot
  (labels, configs, values) = group_line_data(data, 'frac_mesh_stall_sep', desired_configs=['V4'])
  (labels_v4, configs_v4, values_v4, xaxes_v4) = avg_by_hops(labels, configs, values, True, False)
  # generate v16 plot
  (labels, configs, values) = group_line_data(data, 'frac_mesh_stall_sep', desired_configs=['V16'])
  (labels_v16, configs_v16, values_v16, xaxes_v16) = avg_by_hops(labels, configs, values, False, True)

  # merge for multiple plots
  xaxes  = [ xaxes_v4 , xaxes_v16 ]
  values = [ values_v4, values_v16 ]
  # use the same labels for both, the above function gaurentee alphabetical so will be the same
  labels = labels_v4
  xlabels = ['V4 Hops', 'V16 Hops']

  line_plot(xaxes, values, labels, xlabels, 'Input inet stalls relative to total vector cycles', 'inet_stalls', False, sub_plots_x=2, bbox=(0.925, 0.15), legend_loc='lower right', width_ratio=[1, 3])


def plot_backpressure(data):
  # generate v4 plot
  (labels, configs, values) = group_line_data(data, 'frac_backpressure_stall_sep', desired_configs=['V4'])
  (labels_v4, configs_v4, values_v4, xaxes_v4) = avg_by_hops(labels, configs, values, True, False, True)
  # generate v16 plot
  (labels, configs, values) = group_line_data(data, 'frac_backpressure_stall_sep', desired_configs=['V16'])
  (labels_v16, configs_v16, values_v16, xaxes_v16) = avg_by_hops(labels, configs, values, False, True, True)

  # merge for multiple plots
  xaxes  = [ xaxes_v4 , xaxes_v16 ]
  values = [ values_v4, values_v16 ]
  # use the same labels for both, the above function gaurentee alphabetical so will be the same
  labels = labels_v4
  xlabels = ['V4 Hops', 'V16 Hops']

  line_plot(xaxes, values, labels, xlabels, 'Backpressure stalls relative to total vector cycles', 'backpressure_stalls', False, sub_plots_x=2, bbox=(0.925, 0.5), legend_loc='lower right', width_ratio=[1, 2.3333333])

def plot_frame_stalls(data):
  # (labels, configs, values) = group_line_data(data, 'frac_token_stall_sep', desired_configs=['V4', 'V16'])
  # (labels, configs, values, xaxes) = avg_by_hops(labels, configs, values, includeV4, includeV16)
  # title = 'Frame_Stalls_{}{}'.format('v4' if includeV4 else '', 'v16' if includeV16 else '')
  # line_plot(xaxes, values, labels, 'Hops', 'Frame stalls relative to total vector cycles', title, False)

  (labels, sub_labels, values) = group_bar_data(data, 'frac_token_stalls', desired_config_order=['V4'])
  # dont do geomean b/c some values are 0
  add_arith_mean(labels, values)

  bar_plot(labels, sub_labels, values, 'Fraction of Vector Cycles Waiting for a Frame', 'Frame_Stalls_v4')


def plot_prefetch_coverage(data):
  (labels, sub_labels, values_v) = group_bar_data(data, 'vertical_pfs')
  (labels, sub_labels, values_h) = group_bar_data(data, 'horizontal_pfs')
  (labels, sub_labels, values_s) = group_bar_data(data, 'scalar_pfs')

  # TODO stack the bars
  # for now just show for v4 config
  v_pfs = []
  h_pfs = []
  s_pfs = []
  for j in range(len(labels)):
    for i in range(len(sub_labels)):
      if (sub_labels[i] == 'V4'):
        v_pf = values_v[i][j]
        h_pf = values_h[i][j]
        s_pf = values_s[i][j]
        total = v_pf + h_pf + s_pf
        if (total > 0):
          v_pf = float(v_pf) / float(total)
          h_pf = float(h_pf) / float(total)
          s_pf = float(s_pf) / float(total)
        v_pfs.append(v_pf)
        h_pfs.append(h_pf)
        s_pfs.append(s_pf)

  sub_labels = [ 'Vertical', 'Horizontal', 'Scalar' ]
  values = [ v_pfs, h_pfs, s_pfs ]

  add_arith_mean(labels, values, True)

  bar_plot(labels, sub_labels, values, 'Number of Decoupled Access Insts. Relative to Total', 'coverage_v4') 

def plot_init_frames(data):
  (labels, sub_labels, values) = group_bar_data(data, 'cycles', desired_config_order=['V4', 'V4_I0'])

  if not 'V4_I0' in sub_labels:
    return

  # flip from cycles to speedup normalized to NV
  normalize(sub_labels, values, pref_base='V4_I0')
  inverse(values)
  add_geo_mean(labels, values)

  bar_plot(labels, sub_labels, values, 'Speedup Relative to V4', 'Init_Frame_Speedup', horiz_line=1)

# substitue field in extracted data (do before analyses above)
def substitute_field(data, prog, from_config, to_config):

  # find field to move
  move_idx = -1
  for i in range(len(data)):
    row = data[i]
    if row['prog'] == prog and row['config'] == from_config:
      move_idx = i

  if (move_idx < 0):
    return

  # find field to replace
  replace_idx = -1
  for i in range(len(data)):
    row = data[i]
    if row['prog'] == prog and row['config'] == to_config:
      replace_idx = i

  # if cant replace just change the config
  if (replace_idx < 0):
    data[move_idx]['config'] = to_config
    print('rename {} {}->{}'.format(prog, from_config, to_config))
    return

  print('replaced {} {}->{}'.format(prog, from_config, to_config))

  # do replacement for bench
  data[replace_idx] = data[move_idx]
  data[replace_idx]['config'] = to_config

  # delete moved
  data.pop(move_idx)

# delete field in extracted data (do before analyses above)
def remove_field(data, prog, config):
  # find field to delete
  i = 0
  while (i < len(data)):
    row = data[i]
    if row['prog'] == prog and row['config'] == config:
      data.pop(i)
      i -= 1
    i += 1

# rename program names
def rename_prog(data, prog_name, new_name):
  for row in data:
    if row['prog'] == prog_name:
      print('rename {} -> {}'.format(row['prog'], new_name))
      row['prog'] = new_name

def plot_llc_request_stalls(data):
  (labels, sub_labels, values) = group_bar_data(data, 'llcRequestStallTime')
  add_geo_mean(labels, values)
  bar_plot(labels, sub_labels, values, 'reqstall', 'reqstall', False) 

def plot_llc_response_stalls(data):
  (labels, sub_labels, values) = group_bar_data(data, 'llcResponseStallTime')
  add_geo_mean(labels, values)
  bar_plot(labels, sub_labels, values, 'respstall', 'respstall', False) 

def plot_mem_response_stalls(data):
  (labels, sub_labels, values) = group_bar_data(data, 'memResponseStallTime')
  add_geo_mean(labels, values)
  bar_plot(labels, sub_labels, values, 'memrespstall', 'memrespstall', False) 

def plot_llc_busy_cycles(data):
  (labels, sub_labels, values) = group_bar_data(data, 'frac_LLC_Busy_Cycles')
  add_geo_mean(labels, values)
  bar_plot(labels, sub_labels, values, 'fracllcbusy', 'fracllcbusy', False) 

def plot_llc_miss_rate(data):
  (labels, sub_labels, values) = group_bar_data(data, 'llcMissRate')
  # normalize(sub_labels, values)
  add_geo_mean(labels, values)
  bar_plot(labels, sub_labels, values, 'LLC Misses', 'LLC_Misses', False) 

def plot_llc_access_rate(data):
  (labels, sub_labels, values) = group_bar_data(data, 'llcAccessRate')
  # normalize(sub_labels, values)
  add_geo_mean(labels, values)
  bar_plot(labels, sub_labels, values, 'LLC Access Rate', 'LLC_Access_Rate', False)


def plot_router_in_heatmap(data):
  (mdata, xlabel, ylabel) = group_heatmap_data(data, 'V4', 'router_in_stalls', 'bicg', 8)

  heatmap(mdata, xlabel, ylabel, 'routerin')


# top level for analysis passes. generates all plots sequentially
def make_plots_and_tables(all_data):
  print("Removing unwanted series")
  # use vertical for conv2d
  substitute_field(all_data, 'conv2d', 'VEC_16_SIMD_VERTICAL', 'V16')
  substitute_field(all_data, 'conv2d', 'VEC_4_SIMD_VERTICAL', 'V4')
  substitute_field(all_data, 'conv2d', 'VEC_4_SIMD_VERTICAL_I0', 'V4_I0')

  # delete reuse
  remove_field(all_data, 'conv2d', 'VEC_4_REUSE_VERTICAL')

  # rename programs to fancier name
  rename_prog(all_data, 'conv2d', '2dconv')
  rename_prog(all_data, 'conv3d', '3dconv')
  rename_prog(all_data, 'fdtd', 'fdtd-2d')
  rename_prog(all_data, 'gram_schmidt', 'gramschm')


  print("Plot speedup")
  plot_speedup(all_data)
  print("Plot energy")
  plot_energy(all_data)
  print("Plot Inst Energy")
  plot_inst_energy(all_data)
  print("Plot icache energy")
  plot_icache_energy(all_data)
  print("Plot icache access")
  plot_icache_access(all_data)
  print("plot dmen energy")
  plot_dmem_energy(all_data)
  print("Plot llc energy")
  plot_llc_energy(all_data)
  print("Plot frame rdy")
  plot_first_frame_rdy(all_data)
  print("Plot cpi")
  plot_cpi(all_data)
  print("Plot inet stalls")
  plot_inet_stalls(all_data)
  print("Plot frame stalls")
  plot_frame_stalls(all_data)
  print("Plot prefetch coverage")
  plot_prefetch_coverage(all_data)
  print("Plot init frames")
  plot_init_frames(all_data)
  print("Plot backpressure")
  plot_backpressure(all_data)
  print("Plot llc stalls")
  plot_llc_request_stalls(all_data)
  plot_llc_response_stalls(all_data)
  plot_mem_response_stalls(all_data)
  plot_llc_busy_cycles(all_data)
  plot_llc_miss_rate(all_data)
  plot_llc_access_rate(all_data)
  plot_router_in_heatmap(all_data)