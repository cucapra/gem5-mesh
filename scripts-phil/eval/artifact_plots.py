'''
    Generates plots for the artifact
'''

from organizer import rename_prog, substitute_field_for_all_progs, plot_best_speedup, plot_best_icache_access, plot_best_energy
import pickle, argparse

parser = argparse.ArgumentParser(description='Plot graphs for the artifact evaluation')
parser.add_argument('--data-path', default='./extract.csv.pickle', help='Path to pickled data file')
parser.add_argument('--experiment', default='small', help='Experiment: either small, medium, or large')
args = parser.parse_args()

with open(args.data_path, 'rb') as f:
    all_data = pickle.load(f)

# rename programs to fancier name
rename_prog(all_data, 'conv2d', '2dconv')
rename_prog(all_data, 'conv3d', '3dconv')
rename_prog(all_data, 'fdtd', 'fdtd-2d')
rename_prog(all_data, 'gram_schmidt', 'gramschm')
substitute_field_for_all_progs(all_data, 'V16_LL_CL1024', 'V16_LL')

# do plots depending on experiment
if (args.experiment == 'small' or args.experiment == 'medium'):
    plotted_names = ['NV', 'V4']
    plotted_series = [['NV'], ['V4']]
else:
    plotted_names = ['NV', 'NV_PF', 'BEST_V']
    plotted_series = [['NV'], ['NV_PF'], ['V4', 'V16', 'V16_LL']]

plot_best_speedup(all_data,
    category_renames = plotted_names,
    category_configs = plotted_series,
    yaxis_name = 'Speedup Relative to Baseline ({})'.format(plotted_names[0]),
    graph_name = 'artifact_speedup')
plot_best_energy(all_data,
    category_renames=plotted_names,
    category_configs=plotted_series,
    yaxis_name = 'Total On-Chip Energy Relative to Baseline ({})'.format(plotted_names[0]),
    graph_name = 'artifact_energy')
plot_best_icache_access(all_data,
    category_renames=plotted_names,
    category_configs=plotted_series,
    yaxis_name = 'I-Cache Accesses Relative to Baseline ({})'.format(plotted_names[0]),
    graph_name = 'artifact_icache')