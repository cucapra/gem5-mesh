'''
    Script for MICRO'21 Artifact Evaluation

    Three options -- small, medium, and large with differing number of CPU hours

    Should be run in configured environment (either docker or self configured)
    Invokes the follwing scripts
        run_sim.py --> run simulations
        extract_stats.py --> extract stats
        organizer.py functions --> do final graphing

    Should be run from ./gem5-mesh/scripts-phil/eval
'''

import argparse, pickle, subprocess
from organizer import rename_prog, plot_best_speedup, plot_best_icache_access, plot_best_energy

# helper function to run scripts
def run_cmd(cmd):
    print(cmd)
    try:
        result = subprocess.check_output(cmd, shell=True, stderr=subprocess.STDOUT)
        print(result)
        return True
    except:
        print('[[Failed]]')
        return False

# cmd line arguments
parser = argparse.ArgumentParser(description='Run gem5 simulation and output informative stats files')
parser.add_argument('--experiment', default='small', help='Experiment: either small (~24 CPU hours), medium (~100 CPU hours), or large (~300 CPU hours)')
args = parser.parse_args()

# get experiment file
if (args.experiment == 'small'):
    exp_file = './experiments/small.json'
elif (args.experiment == 'medium'):
    exp_file = './experiments/medium.json'
elif (args.experiment == 'large'):
    exp_file = './experiments/large.json'
else:
    print('[[FAILED]] Invalid experiment type')
    assert(False)

# run the simulations (will take a while, but will be faster the more CPUs available)
print('Start simulations.')
run_sim_cmd = 'python {}/run_sim.py --sim-list={} --results=./artifact-{}'.format('.', exp_file, args.experiment)
run_cmd(run_sim_cmd)

# run data extractor on the simulations
print('Start extract stats.')
ext_sim_cmd = 'python {}/extract_stats.py --cpu-sims=./artifact-{} --skip-graph'.format('.', args.experiment)
run_cmd(ext_sim_cmd)

# do graphing stuff
print('Start graphing.')
with open('./extract.csv.pickle', 'rb') as f:
    all_data = pickle.load(f)

# rename programs to fancier name
rename_prog(all_data, 'conv2d', '2dconv')
rename_prog(all_data, 'conv3d', '3dconv')
rename_prog(all_data, 'fdtd', 'fdtd-2d')
rename_prog(all_data, 'gram_schmidt', 'gramschm')

# do plots depending on experiment
if (args.experiment == 'small' or args.experiment == 'medium'):
    plotted_names = ['NV_PF', 'V4']
    plotted_series = [['NV_PF'], ['V4']]
else:
    plotted_names = ['NV', 'NV_PF', 'BEST_V']
    plotted_series = [['NV'], ['NV_PF'], ['V4', 'V16']]

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

print('Done.')