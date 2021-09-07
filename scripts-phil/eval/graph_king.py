'''
  Takes data created by extract_stats.py and makes graphs

  Authors: Philip Bedoukian
           Neil Adit
'''

import matplotlib as mpl
# switch away from display backend to Agg backend
mpl.use('Agg')
import matplotlib.pyplot as plt
from cycler import cycler
import numpy as np
from math import floor, ceil, isnan
from copy import deepcopy
import json
from matplotlib.patches import Rectangle

COLOR_JSON = "color_engine.json"
USE_COLOR = True

default_prop_cycle = mpl.rcParams['axes.prop_cycle']

def is_number(obj):
  return (isinstance(obj, (int, float)) and not isinstance(obj, bool))

# normalize string lens
def equalize_str_len(annotations):
  # figure out max handle length
  max_handle_len = 0
  for anno_str in annotations:
    if (len(anno_str) > max_handle_len):
      max_handle_len = len(anno_str)

  adjusted = []
  for anno_str in annotations:
    while (len(anno_str) < max_handle_len):
      anno_str = '\hspace{0.7}' + anno_str
    adjusted.append(anno_str)

  return adjusted

# create specified barplot and write to file
# Labels are the x axis names
# Sub labels are clustered/stacked field names
# Values is the data formatted as SubLabel_Data -> Label_Data (?)
# Ylabel is y axis title
# Title is graph name
# Annotate will display value of field is clips the graph dimensinos
# Ylim is the bound of y axis
# Horiz_line will insert a line at specified point
# Stack is whether to sub label bars instead of cluster
# Stacknum is how many to stack before creating another sub field in cluster. -1 means all stack
# TODO for stacked clustered might want have config labels. Could do with second axis place below or do annotations below each bar (like what do with clipping)
def bar_plot(labels, sub_labels, values, ylabel, title, annotate=False, ylim=[], horiz_line='', stacked=False, stacknum=-1, sub_sub_labels=[], annotations = ['B', '1', '2', '3']):
  mpl.rcParams['axes.prop_cycle'] = default_prop_cycle

  x = np.arange(len(labels))  # the label locations

  # figure out bar dimensions
  slack = 0.3
  num_sub_bars = len(sub_labels)

  # how many clustred bars should have (not stacked)
  if (stacked):
    if (stacknum <= 0):
      num_cluster_bars = 1
    else:
      num_cluster_bars = num_sub_bars / stacknum
  else:
    num_cluster_bars = num_sub_bars

  is_stacked_clusted = stacked and num_cluster_bars > 1
  width = (1 - slack) / num_cluster_bars # the width of a single bar
  first_bar_offset = (width / -2) * (num_cluster_bars-1)
  stack_count = ( num_sub_bars / num_cluster_bars)

  if USE_COLOR:
    with open(COLOR_JSON) as f:
      all_colors = json.load(f)

    # need to be able to force to the same color when stacked and clustered
    if (is_stacked_clusted):
      assert(str(stack_count) in all_colors.keys())

    config_color = True
    # if too many color dont use
    if stacked and not str(stack_count) in all_colors.keys():
      config_color = False
    if not stacked and not str(num_cluster_bars) in all_colors.keys():
      config_color = False
  else:
    config_color = False

  # if both stacked and clustered then increase figsize over default of figsize=(6.4,4.8)
  if (is_stacked_clusted):
    fig, ax = plt.subplots(figsize=(12.8,4.8))
    edgecolor='black'
  else:
    fig, ax = plt.subplots()
    edgecolor='none'
  rects = []
  for i in range(num_sub_bars):
    cluster_idx = i / stack_count
    stack_idx = i % stack_count
    x_pos = x + first_bar_offset + cluster_idx * width
    y_pos = np.zeros(len(labels))
    
    if (stacked):
      bar_label = sub_labels[stack_idx]
      # dont double label if stacked and clustered
      if cluster_idx > 0:
        bar_label = ''

      # loop over each val in the same stack
      for rcontain_idx in range(cluster_idx*stack_count, len(rects)):
        rcontain = rects[rcontain_idx]
        # loop over all series
        for rect_idx in range(len(rcontain)):
          prev_rect = rcontain[rect_idx]
          y_pos[rect_idx] += prev_rect.get_height()
    else:
      bar_label = sub_labels[cluster_idx]

    if config_color:
      if (stacked):
        color = all_colors[str(stack_count)][stack_idx]
      else:
        color = all_colors[str(num_cluster_bars)][cluster_idx]
      new_rect = ax.bar(x_pos, values[i], width, color=color, label=bar_label, bottom=y_pos, edgecolor=edgecolor)
    else:
      new_rect = ax.bar(x_pos, values[i], width, label=bar_label, bottom=y_pos)
    rects.append(new_rect)

  # Add some text for labels, title and custom x-axis tick labels, etc.
  ax.set_ylabel(ylabel)
  # ax.set_title(title)
  ax.set_xticks(x)
  ax.set_xticklabels(labels, rotation=45, ha='right')
  orig_legend = ax.legend()
  def autolabel(rects, height=''):
      """Attach a text label above each bar in *rects*, displaying its height."""
      for rect in rects:
          orig_height = rect.get_height()
          if (not is_number(height)):
            height = orig_height
          if (height == 0.0):
            continue
          ax.annotate('{:.0f}'.format(orig_height),
                      xy=(rect.get_x() + rect.get_width() / 2, height),
                      xytext=(0, 3),  # 3 points vertical offset
                      textcoords="offset points",
                      ha='center', va='bottom')

  if (annotate):
    for r in rects:
      autolabel(r)

  if (len(ylim) > 0):
    (cur_bot, cur_top) = ax.get_ylim()
    max_top = ylim[1]
    if (cur_top > max_top):

      # either all annotated or there is a bound that is cutting off
      if (not annotate):
        for r in rects:
          for rect in r:
            if (rect.get_height() > max_top):
              autolabel([rect], max_top)

      ax.set_ylim(top=max_top)

  # add horizontal line if requested
  if (is_number(horiz_line)):
    ax.axhline(y=horiz_line, linewidth=1, color='black', linestyle='dashed')

  # for grouped and stacked add cluster bar labels as annoation?
  if (is_stacked_clusted):
    for r_idx in range(len(rects)):
      if (r_idx % stack_count != 0):
        continue
      for rect in rects[r_idx]:
        height = rect.get_height()
        ax.annotate(annotations[r_idx / stack_count],
                    xy=(rect.get_x() + rect.get_width() / 2, 0),
                      xytext=(0, -9),  # 3 points vertical offset
                      textcoords="offset points",
                      ha='center', va='bottom',
                      fontsize=7)

    # add text explaining cluster axis (put below current legend)
    # need to manually add first legend otherwise will be deleted
    fig.gca().add_artist(orig_legend)

    annotations = equalize_str_len(annotations)

    dummy_handle = Rectangle((0, 0), 1, 1, fc='w', fill=False, edgecolor='none', linewidth=0)
    handles = []
    handle_names = []
    for l_idx in range(len(sub_sub_labels)):
      handles.append(dummy_handle)
      # use latex rendering mode r"$...$" to get bold 
      handle_str = r"$\bf{" + annotations[l_idx] + r":}$ " + sub_sub_labels[l_idx]
      handle_names.append(handle_str)
    cluster_legend = ax.legend(handles, handle_names, loc='upper left', handlelength=0, handletextpad=0)

  fig.tight_layout()
  # plt.show()

  if (is_stacked_clusted):
    ax.xaxis.set_tick_params(color='w', length=6)

  plt.savefig(str(title) + '.png')
  plt.savefig(str(title) + '.pdf')

  plt.close()


def single_plot(x_axes, y_axes, duplicate_x, ax, plot_id=-1):
  if USE_COLOR:
    with open(COLOR_JSON) as f:
      all_colors = json.load(f)

  for i in range(len(y_axes)):
    if (duplicate_x):
      x_axis = x_axes
    else:
      x_axis = x_axes[i]

    y_axis = y_axes[i]

    if (plot_id >= 0):
      ax[plot_id].plot(x_axis, y_axis,  color= all_colors[str(5)][i])
    else:
      ax.plot(x_axis, y_axis)

def single_label(x_axes, infer_ticks, xlabel, ylabel, ax, plot_id=-1):
  if (plot_id >= 0):
    a = ax[plot_id]
  else:
    a = ax

  # find max number of ticks and set that
  if (not infer_ticks):
    xticks = []
    for xax in x_axes:
      if (len(xax) > len(xticks)):
        xticks = xax
    a.set_xticks(xticks)

  a.set_xlabel(xlabel)
  a.set_ylabel(ylabel)

# create specified lineplot and write to file
# provide all y_axes and either a single or multiple x_axes
def line_plot(x_axes, y_axes, labels, xlabel, ylabel, title, infer_ticks=True, duplicate_x=False, sub_plots_x=1, bbox='', legend_loc='', width_ratio=[1,1], slope=float('nan')):
  mpl.rcParams['axes.prop_cycle'] = cycler(linestyle=['-', '--', '-.']) * default_prop_cycle

  if (sub_plots_x == 1):
    fig, ax = plt.subplots()
  else:
    # todo fixing relative sizes
    fig, ax = plt.subplots(1, sub_plots_x, sharey='row', gridspec_kw={'width_ratios' : width_ratio})

  # make sure list of list for subplots
  if (sub_plots_x == 1):
    single_plot(x_axes, y_axes, duplicate_x, ax)

    single_label(x_axes, infer_ticks, xlabel, ylabel, ax)
  else:
    for i in range(len(ax.flat)):
      single_plot(x_axes[i], y_axes[i], duplicate_x, ax, i)

    for i in range(len(ax.flat)):
      single_label(x_axes[i], infer_ticks, xlabel[i], ylabel, ax, i)

    for a in ax.flat:
      a.label_outer()
      

  # add a line with m=1
  if (not isnan(slope)):
    ylines = []
    for x in x_axes[0]:
      ylines.append(x * slope)
    ax.plot(x_axes[0], ylines, linestyle='dashed', color='black')

  # ax.set_title(title)
  # if bbox(x,y) included then loc specifies which corner your moving
  if (legend_loc == '' or bbox == ''):
    fig.legend(labels)
  else:
    fig.legend(labels, loc=legend_loc, ncol = 2, bbox_to_anchor=bbox)

  fig.tight_layout()

  plt.savefig(str(title) + '.png')
  plt.savefig(str(title) + '.pdf')

  plt.close()


def heatmap(data_matrix, x_labels, y_labels, title):
  fig, ax = plt.subplots()

  # load data into heatmap
  im = ax.imshow(data_matrix)

  # set dim of heatmap
  ax.set_xticks(np.arange(len(x_labels)))
  ax.set_yticks(np.arange(len(y_labels)))

  # label
  ax.set_xticklabels(x_labels)
  ax.set_yticklabels(y_labels)

  # add legend bar
  cbar = ax.figure.colorbar(im, ax=ax)
  # cbar.ax.set_ylabel

  fig.tight_layout()

  plt.savefig(str(title) + '.png')

  plt.close()

