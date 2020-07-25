'''
  Estimates energy based on gem5 sim.

  Meant to be called from extract_stats with counts
'''

import argparse, re, json

parser = argparse.ArgumentParser(description='Estimate energy based on gem5 stats file')
parser.add_argument('--inst-cost-file', default='../data/inst/inst_energy_28nm.json', help='Path to json containing instruction costs')
parser.add_argument('--inst-cost-node', default='28', help='What technology node the costs were measured at in nm')
parser.add_argument('--used-node', default='32', help='Desired technology node to scale to')
parser.add_argument('--icache-file', default='../data/memory/4kB_icache.out', help='Path to file containing params for icache')
parser.add_argument('--dmem-file', default='../data/memory/4kB_spad.out', help='Path to file containing params for local data mem')
parser.add_argument('--llc-file', default='../data/memory/32kb_llc.out', help='Path to file containing params for llc')
args = parser.parse_args()

# globals that are set in main

cost_dicts = {
  'inst' : {},
  'icache' : {},
  'dmem' : {},
  'llc' : {},
}

# scale value
def linear_scale(val, from_val, to_val):
  ratio =  to_val / from_val
  return val * ratio

# return dict containing info about instruction cost file, scaled to des_node
def parse_inst_cost_file(file_name, cur_node, des_node):
  # load data
  with open(file_name, 'r') as f:
    data = json.load(f)

  # scale data to target tech node
  for k,v in data.items():
    data[k] = linear_scale(float(v), float(cur_node), float(des_node))

  return data

# return dict containing info about memory cost file
def parse_memory_file(file_name):
  pass

# get cost for specified name
def get_cost(name, field):
  return cost_dicts[name][field]

# give name of memory ('icache', 'dmem', or 'llc') along with count
def get_read_energy(memory_name, num_reads):
  pass

def get_write_energy(memory_name, num_writes):
  pass

def get_memory_energy(memory_name, num_reads, num_writes):
  pass

# get instruction cost
def get_instruction_energy(num_int_alu, num_float_add, num_float_mul, num_mem):
  pass


# do parsing into member/global variables

cost_dicts['inst'] = parse_inst_cost_file(args.inst_cost_file, args.inst_cost_node, args.used_node)
cost_dicts['icache'] = parse_memory_file(args.icache_file)
cost_dicts['dmem'] = parse_memory_file(args.dmem_file)
cost_dicts['llc'] = parse_memory_file(args.llc_file)


# MAIN
# 

for k,v in cost_dicts['inst'].items():
  print('{} {}'.format(k, v))



