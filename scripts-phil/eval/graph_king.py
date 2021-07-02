'''
  Takes data created by extract_stats.py and makes graphs
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

COLOR_JSON = "color_engine.json"
USE_COLOR = True

default_prop_cycle = mpl.rcParams['axes.prop_cycle']

def is_number(obj):
  return (isinstance(obj, (int, float)) and not isinstance(obj, bool))

# create specified barplot and write to file
def bar_plot(labels, sub_labels, values, ylabel, title, annotate=False, ylim=[], horiz_line='', stacked=False):
  mpl.rcParams['axes.prop_cycle'] = default_prop_cycle
  # labels = ['G1', 'G2', 'G3', 'G4', 'G5']
  # men_means = [20, 34, 30, 35, 27]
  # women_means = [25, 32, 34, 20, 25]
  x = np.arange(len(labels))  # the label locations

  # figure out bar dimensions
  slack = 0.3
  num_sub_bars = len(sub_labels)
  if (stacked):
    width = (1 - slack)
    first_bar_offset = 0
  else:
    width = (1 - slack) / num_sub_bars # the width of a single bar
    first_bar_offset = width / -2 * (num_sub_bars-1)

  if USE_COLOR:
    with open(COLOR_JSON) as f:
      all_colors = json.load(f)

  fig, ax = plt.subplots()
  rects = []
  for i in range(num_sub_bars):
    x_pos = x + first_bar_offset
    y_pos = np.zeros(len(labels))
    # print('start subbar ' + str(i))
    if (stacked):
      # loop over each val in cluster
      for rcontain in rects:
        # print('start rcontain')
        # loop over all series
        for rect_idx in range(len(rcontain)):
          prev_rect = rcontain[rect_idx]
          # print(str(y_pos) + ' += ' + str(prev_rect.get_height()))
          y_pos[rect_idx] += prev_rect.get_height()
    else:
      x_pos += i * width
    if USE_COLOR and str(num_sub_bars) in all_colors.keys():
      new_rect = ax.bar(x_pos, values[i], width, color= all_colors[str(num_sub_bars)][i], label=sub_labels[i], bottom=y_pos)
    else:
      new_rect = ax.bar(x_pos, values[i], width, label=sub_labels[i], bottom=y_pos)
    rects.append(new_rect)
  # rects1 = ax.bar(x - width/2, men_means, width, label='Men')
  # rects2 = ax.bar(x + width/2, women_means, width, label='Women')
  # Add some text for labels, title and custom x-axis tick labels, etc.
  ax.set_ylabel(ylabel)
  # ax.set_title(title)
  ax.set_xticks(x)
  ax.set_xticklabels(labels, rotation=45, ha='right')
  ax.legend()
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

  fig.tight_layout()
  # plt.show()

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

