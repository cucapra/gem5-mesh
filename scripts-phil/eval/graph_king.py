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

default_prop_cycle = mpl.rcParams['axes.prop_cycle']

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
def line_plot(x_axes, y_axes, labels, xlabel, ylabel, title, infer_ticks=True, duplicate_x=False):
  mpl.rcParams['axes.prop_cycle'] = cycler(linestyle=['-', '--', '-.']) * default_prop_cycle
  fig, ax = plt.subplots()

  for i in range(len(y_axes)):
    if (duplicate_x):
      x_axis = x_axes
    else:
      x_axis = x_axes[i]

    y_axis = y_axes[i]
    ax.plot(x_axis, y_axis)

  # find max number of ticks and set that
  if (not infer_ticks):
    xticks = []
    for xax in x_axes:
      if (len(xax) > len(xticks)):
        xticks = xax
    ax.set_xticks(xticks)

  ax.set_xlabel(xlabel)
  ax.set_ylabel(ylabel)
  ax.set_title(title)
  ax.legend(labels)

  fig.tight_layout()

  plt.savefig(str(title) + '.png')