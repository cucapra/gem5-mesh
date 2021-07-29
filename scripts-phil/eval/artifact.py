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

import argparse, subprocess


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
parser = argparse.ArgumentParser(description='Run simulations, extract data, and plot graphs')
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

# sim commands
run_sim_cmd = 'python {}/run_sim.py --sim-list={} --results=./artifact-{}'.format('.', exp_file, args.experiment)
ext_sim_cmd = 'python {}/extract_stats.py --cpu-sims=./artifact-{} --skip-graph'.format('.', args.experiment)
plot_sim_cmd = 'python {}/artifact_plots.py --experiment={}'.format('.', args.experiment)

# print commands that will be run in case
print('Start Artifact Evaluation.\n\nThe following commands will be run. If a later command fails, you can restart from an intermediate point by manually entering them:')
print('1. ' + run_sim_cmd)
print('2. ' + ext_sim_cmd)
print('3. ' + plot_sim_cmd)
print('\n')

# run the simulations (will take a while, but will be faster the more CPUs available)
print('Start simulations.')
run_cmd(run_sim_cmd)

# run data extractor on the simulations
print('Start extract stats.')
run_cmd(ext_sim_cmd)

# do graphing stuff
print('Start graphing.')
run_cmd(plot_sim_cmd)

# finish
print('Done.')