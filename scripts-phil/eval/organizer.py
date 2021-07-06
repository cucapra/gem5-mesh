'''
  Analyze and organize data in interesting ways
'''

import numpy as np
from math import floor, ceil, isnan
from copy import deepcopy
from graph_king import bar_plot, line_plot, heatmap
from table_king import make_table
from layout_helper import get_mesh_dist_sequence, is_vec_4_config, is_vec_16_config, is_vec_config
from scipy.stats.mstats import gmean

import argparse, pickle, re

# check if has the required series to make the graph
def require_configs(data, plot_name, configs=[]):
  configs = deepcopy(configs)
  for row in data:
    if (row['config'] in configs):
      configs.remove(row['config'])

  if len(configs) == 0:
    return True
  else:
    print('Skipping plot of ' + plot_name + ' because missing required series ' + str(configs))
    return False

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

  # if no desired order just show everything
  if len(desired_order) == 0:
    desired_order = deepcopy(sub_labels)
  else:
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

# takes data and return only the best categories per defined groups
def filter_best_data(data, deciding_field, minimize=True, category_renames = ['BV'], 
    category_configs=[['V4', 'V4_LL', 'V16', 'V16_LL']]):
  # going to paste the best configs for each category
  best_data = []

  # helper to check in same category
  def get_category_num(config):
    # check in configs
    for i in range(0, len(category_configs)):
      if (config in category_configs[i]):
        return i
      # also consider renames
      if (config == category_renames[i]):
        return i
    return -1

  for row in data:
    this_cat = get_category_num(row['config'])
    # check for match in best data
    isNewCat = True
    for b in range(len(best_data)):
      brow = best_data[b]
      other_cat = get_category_num(brow['config'])
      # check if in the same category
      if (brow['prog'] == row['prog'] and this_cat != -1 and other_cat != -1 and this_cat == other_cat):
        # do comparison based on deciding field
        if ((minimize and row[deciding_field] < brow[deciding_field]) or 
              (not minimize and row[deciding_field] > brow[deciding_field])):
          # replace
          new_row = deepcopy(row)
          new_row['config'] = category_renames[this_cat]
          best_data[b] = new_row
        
        # mark that nothing else should be done with this row
        isNewCat = False

    # Insert if cat doesn't exist
    if (isNewCat):
      new_row = deepcopy(row)
      # change name if belongs to group
      if (this_cat >= 0):
        new_row['config'] = category_renames[this_cat]
      best_data.append(new_row)

  return best_data

# only keep or remove the specified programs
def filter_progs(data, progs = [''], filter_out = True):
  ret_data = []

  for row in data:
    if ((row['prog'] in progs and not filter_out) or (not row['prog'] in progs and filter_out)):
      ret_data.append(deepcopy(row))

  return ret_data

def avg_by_hops(labels, configs, values, include_v4, include_v16, include_scalar=False):
  # figure out xaxis (#hops) depending on config. 
  # remove certain values if scalar or inactive core
  v4_hops = get_mesh_dist_sequence('V4')
  v16_hops = get_mesh_dist_sequence('V16')

  # average together series with the same number of hops

  # print(v4_hops)
  # print(v16_hops)
  xaxes = []

  i = 0
  while (i < len(configs)):
    if (((is_vec_4_config(configs[i])) and include_v4) or
        ((is_vec_16_config(configs[i])) and include_v16)):
      hop_avgs = []
      hop_cnts = []
      max_hops = 7 if is_vec_16_config(configs[i]) else 3
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

      hop_list = v16_hops if (is_vec_16_config(configs[i])) else v4_hops
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
def group_bar_data(data, desired_field, desired_config_order= [ 'NV', 'NV_PF', 'PACKED_SIMD', 'V4', 'V16', 'GPU' ], do_single_config=''):
  # hash of hash
  values = {}

  process_stat_dict = do_single_config != ''

  # extra data
  for row in data:
    if (process_stat_dict):
      if row['config'] != do_single_config:
        continue

    # first check if we have something to match. both prog and meta should match
    label_str = row['prog'] + row['meta']
    if (not label_str in values):
      values[label_str] = {}
      values[label_str]['_meta_'] = {}

    # if process stat dict then look at dict of stats for one config
    if (process_stat_dict):
      
      if (desired_field in row):
        sub_vals = row[desired_field]
      else:
        sub_vals = {}

      # turn sub stats into seperate series
      for k,v in sub_vals.items():
        values[label_str][k] = v
      values[label_str]['_meta_']['prog_name'] = row['prog']
    # if processing a single stat, then can look at multiple configs
    else:
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

# find cpu count from config
def find_core_count(config):
  core_regex = re.compile('NCPUS_([0-9]+)')
  match = core_regex.search(config)
  if (match):
    return float(match.group(1))
  else:
    return 64.0


def normalize_by_core_count(sub_labels, values):
  for i in range(len(sub_labels)):
    config = sub_labels[i]
    ncpus = find_core_count(config)
    for j in range(len(values[i])):
      values[i][j] /= ncpus


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
def plot_speedup(data, desired_configs=[], yaxis_name='Speedup Relative to Baseline (NV)', graph_name='Speedup'):
  (labels, sub_labels, values) = group_bar_data(data, 'cycles', desired_config_order=desired_configs)

  # flip from cycles to speedup normalized to NV
  normalize(sub_labels, values)
  inverse(values)
  add_geo_mean(labels, values)

  bar_plot(labels, sub_labels, values, yaxis_name, graph_name, ylim=[0, 15], horiz_line=1)

def plot_energy(data, desired_configs=[], yaxis_name='Total On-Chip Energy Relative to Baseline (NV)', graph_name='Energy'):
  (labels, sub_labels, values) = group_bar_data(data, 'energy-sum(nJ)', desired_config_order=desired_configs)
  normalize(sub_labels, values)
  add_geo_mean(labels, values)
  bar_plot(labels, sub_labels, values, yaxis_name, graph_name, horiz_line=1)
  # need to have a buttom=[] when define bar. where bottom is sum of prev

def plot_inst_energy(data):
  (labels, sub_labels, values) = group_bar_data(data, 'inst-cnts-energy(nJ)', desired_config_order=[])
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

def plot_icache_access(data, desired_configs=[], yaxis_name='I-Cache Accesses Relative to Baseline (NV)', graph_name='icache_accesses'):
  (labels, sub_labels, values) = group_bar_data(data, 'icache_access', desired_config_order=desired_configs)
  normalize(sub_labels, values)
  add_geo_mean(labels, values)
  bar_plot(labels, sub_labels, values, yaxis_name, graph_name, horiz_line=1)

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

def plot_inet_stalls(data, prog_subset=['3dconv', 'gemm', '2dconv', 'bicg', 'syr2k']):
  # only do a subset
  data = filter_progs(data, prog_subset, False)

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


def plot_backpressure(data, prog_subset=['3dconv', 'gemm', '2dconv', 'bicg', 'syr2k']):
  # only do a subset
  data = filter_progs(data, prog_subset, False)

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

def plot_scalability_avg(data, stat_name='cycles', normalize_result=True, inverse_result=True, graph_name='line_scalability', yaxis_name='Avg Rel Speedup', shared_base=False, slope=float('nan'), mul_factor=1.0):

  if (shared_base):
    ncpu_io_cat_names = [ 'NV_NCPUS_1', 'NV_NCPUS_4', 'NV_NCPUS_16', 'NV_NCPUS_64' ]
    ncpu_io_cat_actual = [ ['NV_NCPUS_1'], ['NV_NCPUS_4'], ['NV_NCPUS_16'], ['NV_NCPUS_64'] ]
    
    ncpu_nvpf_cat_names = [ 'NV_NCPUS_1', 'NV_PF_NCPUS_4', 'NV_PF_NCPUS_16', 'NV_PF_NCPUS_64' ]
    ncpu_nvpf_cat_actual = [ ['NV_NCPUS_1'], ['NV_PF_NCPUS_4'], ['NV_PF_NCPUS_16'], ['NV_PF_NCPUS_64'] ]

    ncpu_o3_cat_names = [ 'NV_NCPUS_1', 'NV_NCPUS_4_O3', 'NV_NCPUS_16_O3' ]
    ncpu_o3_cat_actual = [ ['NV_NCPUS_1'], ['NV_NCPUS_4_O3'], ['NV_NCPUS_16_O3'] ]
  else:
    ncpu_io_cat_names = [ 'NV_NCPUS_1', 'NV_NCPUS_4', 'NV_NCPUS_16', 'NV_NCPUS_64' ]
    ncpu_io_cat_actual = [ ['NV_NCPUS_1'], ['NV_NCPUS_4'], ['NV_NCPUS_16'], ['NV_NCPUS_64'] ]
    
    ncpu_nvpf_cat_names = [ 'NV_PF_NCPUS_1', 'NV_PF_NCPUS_4', 'NV_PF_NCPUS_16', 'NV_PF_NCPUS_64' ]
    ncpu_nvpf_cat_actual = [ ['NV_PF_NCPUS_1'], ['NV_PF_NCPUS_4'], ['NV_PF_NCPUS_16'], ['NV_PF_NCPUS_64'] ]

    ncpu_o3_cat_names = [ 'NV_NCPUS_1_O3', 'NV_NCPUS_4_O3', 'NV_NCPUS_16_O3' ]
    ncpu_o3_cat_actual = [ ['NV_NCPUS_1_O3'], ['NV_NCPUS_4_O3'], ['NV_NCPUS_16_O3'] ]


  cat_names_arr = [ ncpu_io_cat_names, ncpu_nvpf_cat_names, ncpu_o3_cat_names ]
  cat_actual_arr = [ ncpu_io_cat_actual, ncpu_nvpf_cat_actual, ncpu_o3_cat_actual ]


  if (not require_configs(data, graph_name, [ 'NV_NCPUS_1', 'NV_PF_NCPUS_1', 'NV_NCPUS_1_O3' ])):
    return

  series_data = []

  for i in range(len(cat_names_arr)):

    inst_data = filter_best_data(data, stat_name, 
      category_renames=cat_names_arr[i],
      category_configs=cat_actual_arr[i])

    (labels, sub_labels, values) = group_bar_data(inst_data, stat_name, desired_config_order=cat_names_arr[i])

    # multiply by a factor
    for v in values:
      for vv in range(len(v)):
        v[vv] *= mul_factor

    # flip from cycles to speedup normalized to NV
    if (normalize_result):
      normalize(sub_labels, values, pref_base=cat_names_arr[i][0])
    
    if (inverse_result):
      inverse(values)
    
    add_geo_mean(labels, values)

    # only take the avg
    series = []
    for v in values:
      series.append(v[-1])
    series_data.append(series)

  # merge for multiple plots
  xaxes  = [ [1, 4, 16, 64 ] ]

  # reformat line data so that organized by x rather than series
  line_values = []
  for x in range(len(xaxes[0])):
    xvals = []
    for s in series_data:
      if (x < len(s)):
        xvals.append(s[x])
      else:
        xvals.append(float('nan'))
    line_values.append(xvals)

  values = [ line_values ]

  labels = [ 'NV', 'NV_PF', 'O3', 'Ideal' ]
  xlabels = 'Num Cores'

  line_plot(xaxes, values, labels, xlabels, yaxis_name, graph_name, False, 
    sub_plots_x=1, bbox=(0.925, 0.5), legend_loc='lower right', width_ratio=[1, 2.3333333],
    slope=slope)

def plot_cpi_stack(data, config_name='NV_PF_NCPUS_64__dram_bw_32', graph_name='cpi_stack_nvpf', yaxis_name='CPI Stack'):

  if (not require_configs(data, graph_name, [config_name])):
    return

  series_name = 'cpi-stack'

  # determine if vector config, in which case want to just look at for expander cores IMO
  if (is_vec_config(config_name)):
    if (is_vec_4_config(config_name)):
      hops = get_mesh_dist_sequence('V4')
    else:
      hops = get_mesh_dist_sequence('V16')

    # average everything at hop 1
    new_series_name = 'cpi-stack-expander'
    data = deepcopy(data)
    for row in data:
      if (row['config'] != config_name):
        continue

      row[new_series_name] = {}

      for k,v in row['cpi-stack-sep'].items():
        avg = 0
        cnt = 0
        # look at each cores and check where it is in a group
        for h_idx in range(len(hops)):
          # only consider hop 1 cores (expander)
          if (hops[h_idx] == 1):
            # print(str(h_idx) + ' ' + str(v[h_idx]))
            avg += v[h_idx]
            cnt += 1
        row[new_series_name][k] = avg / cnt

    series_name = new_series_name


  # TODO prob want to get a field for everything recorded here - totalCycles!
  (labels, sub_labels, values) = group_bar_data(data, series_name, 
    ['Issued', 'Stallon_Fetch', 'Stallon_INET_Pull', 'Stallon_Load', 'Stallon_DepOther', 'Stallon_Frame', 'Stallon_ExeUnit', 'Stallon_ROB_Other'], 
    config_name)
  
  normalize(sub_labels, values, pref_base='Issued')
  add_arith_mean(labels, values) # geomean doesnt make sense
  
  bar_plot(labels, sub_labels, values, yaxis_name, graph_name, stacked=True)
  # (labels, sub_labels, values) = group_bar_data(data, disp_stat, desired_config_order=category_renames)

def plot_frame_stalls(data):
  # (labels, configs, values) = group_line_data(data, 'frac_token_stall_sep', desired_configs=['V4', 'V16'])
  # (labels, configs, values, xaxes) = avg_by_hops(labels, configs, values, includeV4, includeV16)
  # title = 'Frame_Stalls_{}{}'.format('v4' if includeV4 else '', 'v16' if includeV16 else '')
  # line_plot(xaxes, values, labels, 'Hops', 'Frame stalls relative to total vector cycles', title, False)

  (labels, sub_labels, values) = group_bar_data(data, 'frac_token_stalls', desired_config_order=['NV_PF','V4'])
  # dont do geomean b/c some values are 0
  add_arith_mean(labels, values)

  bar_plot(labels, sub_labels, values, 'Fraction of Total Cycles Waiting for a Frame', 'Frame_Stalls_v4')


# def plot_prefetch_coverage(data):
#   (labels, sub_labels, values_v) = group_bar_data(data, 'vertical_pfs')
#   (labels, sub_labels, values_h) = group_bar_data(data, 'horizontal_pfs')
#   (labels, sub_labels, values_s) = group_bar_data(data, 'scalar_pfs')

#   # TODO stack the bars
#   # for now just show for v4 config
#   v_pfs = []
#   h_pfs = []
#   s_pfs = []
#   for j in range(len(labels)):
#     for i in range(len(sub_labels)):
#       if (sub_labels[i] == 'V4'):
#         v_pf = values_v[i][j]
#         h_pf = values_h[i][j]
#         s_pf = values_s[i][j]
#         total = v_pf + h_pf + s_pf
#         if (total > 0):
#           v_pf = float(v_pf) / float(total)
#           h_pf = float(h_pf) / float(total)
#           s_pf = float(s_pf) / float(total)
#         v_pfs.append(v_pf)
#         h_pfs.append(h_pf)
#         s_pfs.append(s_pf)

#   sub_labels = [ 'Vertical', 'Horizontal', 'Scalar' ]
#   values = [ v_pfs, h_pfs, s_pfs ]

#   add_arith_mean(labels, values, True)

#   bar_plot(labels, sub_labels, values, 'Number of Decoupled Access Insts. Relative to Total', 'coverage_v4') 

def plot_init_frames(data):
  (labels, sub_labels, values) = group_bar_data(data, 'cycles', desired_config_order=['V4', 'V4_I0'])

  if not 'V4_I0' in sub_labels:
    return

  # flip from cycles to speedup normalized to NV
  normalize(sub_labels, values, pref_base='V4_I0')
  inverse(values)
  add_geo_mean(labels, values)

  bar_plot(labels, sub_labels, values, 'Speedup Relative to V4', 'Init_Frame_Speedup', horiz_line=1)

# substitute field in extracted data (do before analyses above)
def substitute_field(data, prog, from_config, to_config, remove_src=True):

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
    if (remove_src):
      data[move_idx]['config'] = to_config
      print('rename {} {}->{}'.format(prog, from_config, to_config))
      return
    else:
      copied_row = deepcopy(data[move_idx])
      copied_row['config'] = to_config
      data.append(copied_row)
      print('copy {} {}->{}'.format(prog, from_config, to_config))
      return

  print('replaced {} {}->{}'.format(prog, from_config, to_config))

  # do replacement for bench
  data[replace_idx] = data[move_idx]
  data[replace_idx]['config'] = to_config

  # delete moved
  if (remove_src):
    data.pop(move_idx)

# to the replacement for every program
def substitute_field_for_all_progs(data, from_config, to_config):
  # figure out which programs exist
  prog_list = []
  for row in data:
    if (row['prog'] not in prog_list):
      prog_list.append(row['prog'])

  # do sub for each field
  for prog in prog_list:
    substitute_field(data, prog, from_config, to_config)

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

def plot_llc_miss_rate(data, desired_configs=[], yaxis_name='LLC Miss Rate', graph_name='LLC_Misses'):
  (labels, sub_labels, values) = group_bar_data(data, 'llcMissRate', desired_config_order=desired_configs)
  # normalize(sub_labels, values)
  add_geo_mean(labels, values)
  bar_plot(labels, sub_labels, values, yaxis_name, graph_name, False) 

def plot_llc_access_rate(data):
  (labels, sub_labels, values) = group_bar_data(data, 'llcAccessRate')
  # normalize(sub_labels, values)
  add_geo_mean(labels, values)
  bar_plot(labels, sub_labels, values, 'LLC Access Rate', 'LLC_Access_Rate', False)


def plot_router_in_heatmap(data):
  (mdata, xlabel, ylabel) = group_heatmap_data(data, 'V4', 'router_in_stalls', 'bicg', 8)
  heatmap(mdata, xlabel, ylabel, 'routerin')
  (mdata, xlabel, ylabel) = group_heatmap_data(data, 'NV', 'router_in_stalls', 'bicg', 8)
  heatmap(mdata, xlabel, ylabel, 'routerin_nv')

def plot_router_out_heatmap(data):
  (mdata, xlabel, ylabel) = group_heatmap_data(data, 'V4', 'router_out_stalls', 'bicg', 8)
  heatmap(mdata, xlabel, ylabel, 'routerout')
  (mdata, xlabel, ylabel) = group_heatmap_data(data, 'NV', 'router_out_stalls', 'bicg', 8)
  heatmap(mdata, xlabel, ylabel, 'routerout_nv')

def plot_all_router_stalls(data):
  (labels, sub_labels, values) = group_bar_data(data, 'router_in_stalls_all')
  bar_plot(labels, sub_labels, values, 'RouteVCSstalls', 'RouteVCSstalls', False)

  (labels, sub_labels, values) = group_bar_data(data, 'router_in_no_req_stalls_all')
  bar_plot(labels, sub_labels, values, 'RouteReqStall', 'RouteReqStall', False)

  (labels, sub_labels, values) = group_bar_data(data, 'router_out_stalls_all')
  bar_plot(labels, sub_labels, values, 'RouteOutStall', 'RouteOutStall', False)

def plot_best_speedup(data, category_renames, category_configs, yaxis_name, graph_name, ylim=[0, 15], do_norm_to_cores=False, cmp_stat='cycles', disp_stat='cycles', normalize_result=True, inverse_result=True):
  # filter by min cycles

  data = filter_best_data(data, cmp_stat, 
    category_renames=category_renames,
    category_configs=category_configs)

  if (not require_configs(data, graph_name, [ category_renames[0] ])):
    return

  (labels, sub_labels, values) = group_bar_data(data, disp_stat, desired_config_order=category_renames)

  # flip from cycles to speedup normalized to NV
  if (normalize_result):
    normalize(sub_labels, values, pref_base=category_renames[0])
  if (inverse_result):
    inverse(values)

  if (do_norm_to_cores):
    normalize_by_core_count(sub_labels, values)

  add_geo_mean(labels, values)

  bar_plot(labels, sub_labels, values, yaxis_name, graph_name, ylim=ylim, horiz_line=1)


def plot_best_energy(data, category_renames, category_configs, yaxis_name='Total On-Chip Energy Relative to Baseline (NV)', graph_name='Energy'):
  data = filter_best_data(data, 'cycles', 
    category_renames=category_renames,
    category_configs=category_configs)
  
  (labels, sub_labels, values) = group_bar_data(data, 'energy-sum(nJ)', desired_config_order=category_renames)
  normalize(sub_labels, values, pref_base=category_renames[0])
  add_geo_mean(labels, values)
  bar_plot(labels, sub_labels, values, yaxis_name, graph_name, horiz_line=1)

def plot_best_icache_access(data, category_renames, category_configs, yaxis_name='I-Cache Accesses Relative to Baseline (NV)', graph_name='icache_accesses'):
  data = filter_best_data(data, 'cycles', 
    category_renames=category_renames,
    category_configs=category_configs)
   
  (labels, sub_labels, values) = group_bar_data(data, 'icache_access', desired_config_order=category_renames)
  normalize(sub_labels, values, pref_base=category_renames[0])
  add_geo_mean(labels, values)
  bar_plot(labels, sub_labels, values, yaxis_name, graph_name, horiz_line=1)

def plot_best_llc_miss_rate(data, category_renames, category_configs, yaxis_name='LLC Miss Rate', graph_name='LLC_Misses'):
  data = filter_best_data(data, 'cycles', 
    category_renames=category_renames,
    category_configs=category_configs)

  (labels, sub_labels, values) = group_bar_data(data, 'llcMissRate', desired_config_order=category_renames)
  add_geo_mean(labels, values)
  bar_plot(labels, sub_labels, values, yaxis_name, graph_name, False) 

# top level for analysis passes. generates all plots sequentially
def make_plots_and_tables(all_data):

  # rename programs to fancier name
  rename_prog(all_data, 'conv2d', '2dconv')
  rename_prog(all_data, 'conv3d', '3dconv')
  rename_prog(all_data, 'fdtd', 'fdtd-2d')
  rename_prog(all_data, 'gram_schmidt', 'gramschm')

  # gram schmidt doesnt have V4_PCV or V16_PCV, just copy from V4 for completion
  substitute_field(all_data, 'gramschm', 'V4',  'V4_PCV',  False)
  substitute_field(all_data, 'gramschm', 'V16', 'V16_PCV', False)

  # rename config data to something better
  substitute_field_for_all_progs(all_data, 'V4__net_width_1', 'V4_NW1')
  substitute_field_for_all_progs(all_data, 'V16__net_width_1', 'V16_NW1')
  substitute_field_for_all_progs(all_data, 'NV__net_width_1', 'NV_NW1')
  substitute_field_for_all_progs(all_data, 'NV_PF__net_width_1', 'NV_PF_NW1')
  substitute_field_for_all_progs(all_data, 'PCV_PF__net_width_1', 'PCV_PF_NW1')
  substitute_field_for_all_progs(all_data, 'PCV__net_width_1', 'PCV_NW1')
  substitute_field_for_all_progs(all_data, 'V4_PCV__net_width_1', 'V4_PCV_NW1')
  substitute_field_for_all_progs(all_data, 'V16_PCV__net_width_1', 'V16_PCV_NW1')
  substitute_field_for_all_progs(all_data, 'V4_LL_PCV_CL1024__net_width_1', 'V4_LL_PCV_NW1')
  substitute_field_for_all_progs(all_data, 'V16_LL_PCV_CL1024__net_width_1', 'V16_LL_PCV_NW1')
  substitute_field_for_all_progs(all_data, 'V16_LL_CL1024__net_width_1', 'V16_LL_NW1')

  substitute_field_for_all_progs(all_data, 'V4__llc_max_size_32kB', 'V4_32kB')
  substitute_field_for_all_progs(all_data, 'V16__llc_max_size_32kB', 'V16_32kB')
  substitute_field_for_all_progs(all_data, 'NV__llc_max_size_32kB', 'NV_32kB')
  substitute_field_for_all_progs(all_data, 'NV_PF__llc_max_size_32kB', 'NV_PF_32kB')
  substitute_field_for_all_progs(all_data, 'PCV_PF__llc_max_size_32kB', 'PCV_PF_32kB')
  substitute_field_for_all_progs(all_data, 'PCV__llc_max_size_32kB', 'PCV_32kB')
  substitute_field_for_all_progs(all_data, 'V4_PCV__llc_max_size_32kB', 'V4_PCV_32kB')
  substitute_field_for_all_progs(all_data, 'V16_PCV__llc_max_size_32kB', 'V16_PCV_32kB')
  substitute_field_for_all_progs(all_data, 'V4_LL_PCV_CL1024__llc_max_size_32kB', 'V4_LL_PCV_32kB')
  substitute_field_for_all_progs(all_data, 'V16_LL_PCV_CL1024__llc_max_size_32kB', 'V16_LL_PCV_32kB')
  substitute_field_for_all_progs(all_data, 'V16_LL_CL1024__llc_max_size_32kB', 'V16_LL_32kB')

  substitute_field_for_all_progs(all_data, 'V4_LL_PCV_CL1024', 'V4_LL_PCV')
  substitute_field_for_all_progs(all_data, 'V16_LL_PCV_CL1024', 'V16_LL_PCV')
  substitute_field_for_all_progs(all_data, 'V16_LL_CL1024', 'V16_LL')


  # plot ncpus graphs
  # should prob plot speedup/ncpus
  ncpu_io_cat_names = [ 'NV_NCPUS_1', 'NV_NCPUS_4', 'NV_NCPUS_16', 'NV_NCPUS_64' ]
  ncpu_io_cat_actual = [ ['NV_NCPUS_1'], ['NV_NCPUS_4'], ['NV_NCPUS_16'], ['NV_NCPUS_64'] ]
  
  # good scalability up to 16. 64 has very sublinear. Also better scalabilty than NV
  ncpu_nvpf_cat_names = [ 'NV_PF_NCPUS_1', 'NV_PF_NCPUS_4', 'NV_PF_NCPUS_16', 'NV_PF_NCPUS_64' ]
  ncpu_nvpf_cat_actual = [ ['NV_PF_NCPUS_1'], ['NV_PF_NCPUS_4'], ['NV_PF_NCPUS_16'], ['NV_PF_NCPUS_64'] ]

  ncpu_o3_cat_names = [ 'NV_NCPUS_1_O3', 'NV_NCPUS_4_O3', 'NV_NCPUS_16_O3' ]
  ncpu_o3_cat_actual = [ ['NV_NCPUS_1_O3'], ['NV_NCPUS_4_O3'], ['NV_NCPUS_16_O3'] ]

  ncpu_varied_names = ['NV_NCPUS_1', 'NV_NCPUS_64', 'NV_PF_NCPUS_16', 'NV_PF_NCPUS_64', 'NV_NCPUS_1_O3', 'NV_NCPUS_4_O3', 'NV_NCPUS_16_O3']
  ncpu_varied_actual = [ ['NV_NCPUS_1'], ['NV_NCPUS_64'], ['NV_PF_NCPUS_16'], ['NV_PF_NCPUS_64'], ['NV_NCPUS_1_O3'], ['NV_NCPUS_4_O3'], ['NV_NCPUS_16_O3']]

  plot_best_speedup(all_data,
    category_renames=ncpu_io_cat_names,
    category_configs=ncpu_io_cat_actual,
    yaxis_name = 'Scalability ( Rel Speedup to NV_1 / NCORES )',
    graph_name = 'speedup_io_cpus',
    ylim = [0, 30],
    do_norm_to_cores=True)

  plot_best_speedup(all_data,
    category_renames=ncpu_nvpf_cat_names,
    category_configs=ncpu_nvpf_cat_actual,
    yaxis_name = 'Scalability ( Rel Speedup to NVPF_1 / NCORES )',
    graph_name = 'speedup_iopf_cpus',
    ylim = [0, 30],
    do_norm_to_cores=True)

  plot_best_speedup(all_data,
    category_renames=ncpu_o3_cat_names,
    category_configs=ncpu_o3_cat_actual,
    yaxis_name = 'Scalability ( Rel Speedup to O3_1 / NCORES )',
    graph_name = 'speedup_o3_cpus',
    ylim = [0, 30],
    do_norm_to_cores=True)

  plot_best_speedup(all_data,
    category_renames=ncpu_varied_names,
    category_configs=ncpu_varied_actual,
    yaxis_name = 'Speedup Relative to Baseline (NV_NCPUS_1)',
    graph_name = 'speedup_nv_cpus',
    ylim = [0, 40])


  plot_scalability_avg(all_data, slope=1)
  plot_scalability_avg(all_data, graph_name='line_scale_nv1', shared_base=True, slope=1)
  plot_scalability_avg(all_data, stat_name='dram_bw_used', normalize_result=False, inverse_result=False, graph_name='dram_scale', yaxis_name='DRAM BW')

  plot_scalability_avg(all_data, stat_name='llcAccessRate', normalize_result=False, inverse_result=False, graph_name='llcAccessRate_scale', yaxis_name='LLC Access / Cycle')
  plot_scalability_avg(all_data, stat_name='llcMissRate', normalize_result=False, inverse_result=False, graph_name='llcMissRate_scale', yaxis_name='LLC Miss Rate')

  # divide by 1000 because in ticks not cycles. see MessageBuffer.cc:249
  plot_scalability_avg(all_data, stat_name='llcRequestStallTime', normalize_result=False, inverse_result=False, graph_name='llcRequestStallTime_scale', yaxis_name='LLC Avg Request Stall Cycles', mul_factor=1.0/1000.0)
  plot_scalability_avg(all_data, stat_name='llcResponseStallTime', normalize_result=False, inverse_result=False, graph_name='llcResponseStallTime_scale', yaxis_name='LLC Avg Response Cycles', mul_factor=1.0/1000.0)
  plot_scalability_avg(all_data, stat_name='frac_LLC_Busy_Cycles', normalize_result=False, inverse_result=False, graph_name='llcBusy_scale', yaxis_name='LLC Busy Cycles / Cycles')

  plot_scalability_avg(all_data, stat_name='dram_reads', normalize_result=True, inverse_result=False, graph_name='dramReads', yaxis_name='DRAM Reads Rel to 1core')


  plot_best_speedup(all_data,
    category_renames=ncpu_nvpf_cat_names,
    category_configs=ncpu_nvpf_cat_actual,
    yaxis_name = 'DRAM BW',
    graph_name = 'iopf_dram',
    ylim = [0, float('inf')],
    do_norm_to_cores=False,
    inverse_result=False,
    normalize_result=False,
    disp_stat='dram_bw_used')

  plot_cpi_stack(all_data)
  plot_cpi_stack(all_data, config_name='V4', graph_name='v4_cpi')

  exit()

  # do graphs with bfs before remove it
  print("Plot inet stalls")
  plot_inet_stalls(all_data)
  print("Plot backpressure")
  plot_backpressure(all_data)


  # completely remove bfs for now
  all_data = filter_progs(all_data, ['bfs'], True)

  # config groups
  main_cat_names  = ['NV', 'NV_PF', 'BEST_V']
  main_cat_groups = [['NV'], ['NV_PF'], ['V4', 'V16', 'V16_LL']]

  fixed_vec_cat_names = ['NV_PF', 'PCV_PF', 'BEST_V', 'BEST_V_PCV', 'GPU']
  fixed_vec_cat_groups = [
    ['NV_PF'], 
    ['PCV_PF', 'PCV'], 
    ['V4', 'V16', 'V16_LL'], 
    ['V4_PCV', 'V16_PCV', 'V4_LL_PCV', 'V16_LL_PCV'], 
    ['GPU']
  ]

  flexible_cat_names = ['V4', 'V4_LL_PCV', 'V16', 'V16_LL_PCV']

  # going to do names changes with best speedup (kinda hacky!)
  netwidth_cat_names = ['NV_PF_NW1', 'NV_PF_NW4', 'V4_NW1', 'V4_NW4', 'V16_LL_NW1', 'V16_LL_NW4']
  netwidth_cat_actual = [['NV_PF_NW1'], ['NV_PF'], ['V4_NW1'], ['V4'], ['V16_LL_NW1'], ['V16_LL']]

  cache_cat_names = ['NV_PF_16kB', 'NV_PF_32kB', 'V4_16kB', 'V4_32kB', 'V16_LL_16kB', 'V16_LL_32kB']
  cache_cat_actual = [['NV_PF'], ['NV_PF_32kB'], ['V4'], ['V4_32kB'], ['V16_LL'], ['V16_LL_32kB']]

  print("Plot speedup")
  plot_speedup(all_data)
  plot_best_speedup(all_data,
    category_renames=main_cat_names,
    category_configs=main_cat_groups,
    yaxis_name = 'Speedup Relative to Baseline (NV)',
    graph_name = 'speedup_nv_best_tril')
  plot_best_speedup(all_data,
    category_renames=fixed_vec_cat_names,
    category_configs=fixed_vec_cat_groups,
    yaxis_name = 'Speedup Relative to Baseline (NV_PF)',
    graph_name = 'speedup_fixed_vec',
    ylim=[0, 5.5])
  plot_speedup(all_data,
    desired_configs=flexible_cat_names,
    yaxis_name = 'Speedup Relative to V4',
    graph_name = 'speedup_between_vecs')

  # compare hardware configs speedup
  plot_best_speedup(all_data,
    category_renames=netwidth_cat_names,
    category_configs=netwidth_cat_actual,
    yaxis_name = 'Speedup Relative to NV_PF_NW1',
    graph_name = 'speedup_netwidth')

  plot_best_speedup(all_data,
    category_renames=cache_cat_names,
    category_configs=cache_cat_actual,
    yaxis_name = 'Speedup Relative to NV_PF_32kB',
    graph_name = 'speedup_llc_size')

  print("Plot energy")
  plot_energy(all_data)
  plot_best_energy(all_data,
    category_renames=main_cat_names,
    category_configs=main_cat_groups,
    yaxis_name = 'Total On-Chip Energy Relative to Baseline (NV)',
    graph_name = 'energy_nv_best_tril')
  plot_best_energy(all_data,
    category_renames=fixed_vec_cat_names,
    category_configs=fixed_vec_cat_groups,
    yaxis_name = 'Total On-Chip Energy Relative to Baseline (NV_PF)',
    graph_name = 'energy_fixed_vec')
  plot_energy(all_data,
    desired_configs=flexible_cat_names,
    yaxis_name = 'Total On-Chip Energy Relative to V4',
    graph_name = 'energy_between_vecs')

  print("Plot icache access")
  plot_icache_access(all_data)
  plot_best_icache_access(all_data,
    category_renames=main_cat_names,
    category_configs=main_cat_groups,
    yaxis_name = 'I-Cache Accesses Relative to Baseline (NV)',
    graph_name = 'icache_nv_best_tril')
  plot_best_icache_access(all_data,
    category_renames=fixed_vec_cat_names,
    category_configs=fixed_vec_cat_groups,
    yaxis_name = 'I-Cache Accesses Relative to Baseline (NV_PF)',
    graph_name = 'icache_fixed_vec')
  plot_icache_access(all_data,
    desired_configs=flexible_cat_names,
    yaxis_name = 'I-Cache Accesses Relative to V4',
    graph_name = 'icache_between_vecs')

  print("Plot frame stalls")
  plot_frame_stalls(all_data)

  print("Plot Inst Energy")
  plot_inst_energy(all_data)
  print("Plot icache energy")
  plot_icache_energy(all_data)
  print("Plot dmem energy")
  plot_dmem_energy(all_data)
  print("Plot llc energy")
  plot_llc_energy(all_data)

  print("Plot llc miss rate")
  plot_llc_miss_rate(all_data)
  main_llc_cat_names  = ['NV', 'NV_PF', 'BEST_V', 'V16_LL']
  main_llc_cat_groups = [['NV'], ['NV_PF'], ['V4', 'V16'], ['V16_LL']]
  plot_best_llc_miss_rate(all_data,
    category_renames=main_llc_cat_names,
    category_configs=main_llc_cat_groups,
    yaxis_name = 'LLC Miss Rate',
    graph_name = 'llc_nv_best_tril')
  plot_best_llc_miss_rate(all_data,
    category_renames=fixed_vec_cat_names,
    category_configs=fixed_vec_cat_groups,
    yaxis_name = 'LLC Miss Rate',
    graph_name = 'llc_fixed_vec')
  plot_llc_miss_rate(all_data,
    desired_configs=flexible_cat_names,
    yaxis_name = 'LLC Miss Rate',
    graph_name = 'llc_between_vecs')


  plot_best_llc_miss_rate(all_data,
    category_renames=cache_cat_names,
    category_configs=cache_cat_actual,
    yaxis_name = 'LLC miss to NV_PF_32kB',
    graph_name = 'llc_miss_llc_size')

  # print("Plot cpi")
  # plot_cpi(all_data)
  # print("Plot frame rdy")
  # plot_first_frame_rdy(all_data)
  # print("Plot init frames")
  # plot_init_frames(all_data)
  # print("Plot llc stalls")
  # plot_llc_request_stalls(all_data)
  # plot_llc_response_stalls(all_data)
  # plot_mem_response_stalls(all_data)
  # plot_llc_busy_cycles(all_data)
  # plot_llc_access_rate(all_data)

  # need to pick specifc bench for this, so only use when needed
  # print("Plot router stalls")
  # plot_router_in_heatmap(all_data)
  # plot_router_out_heatmap(all_data)
  # plot_all_router_stalls(all_data)


# if run standalone
if __name__ == "__main__":
  parser = argparse.ArgumentParser(description='Sorts and plots data from file generated by extract_stats')
  parser.add_argument('--infile', default='./extract.csv.pickle', help='Pickle Path where extracted data is')
  args = parser.parse_args()

  # read infile
  with open(args.infile, 'rb') as f:
    all_data = pickle.load(f)

  make_plots_and_tables(all_data)