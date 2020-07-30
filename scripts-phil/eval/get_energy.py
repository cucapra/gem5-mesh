'''
  Estimates energy based on gem5 sim.

  Meant to be called from extract_stats with counts

  Everything is converted and output as nJ
'''

import argparse, re, json

# parser = argparse.ArgumentParser(description='Estimate energy based on gem5 stats file')
# parser.add_argument('--inst-cost-file', default='../data/inst/inst_energy_28nm.json', help='Path to json containing instruction costs')
# parser.add_argument('--inst-cost-node', default='28', help='What technology node the costs were measured at in nm')
# parser.add_argument('--used-node', default='32', help='Desired technology node to scale to')
# parser.add_argument('--icache-file', default='../data/memory/4kB_icache.out', help='Path to file containing params for icache')
# parser.add_argument('--dmem-file', default='../data/memory/4kB_spad.out', help='Path to file containing params for local data mem')
# parser.add_argument('--llc-file', default='../data/memory/32kb_llc.out', help='Path to file containing params for llc')
# args = parser.parse_args()

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
    # if (k != 'DRAM IO/bit'):
    data[k] = linear_scale(float(v), float(cur_node), float(des_node))

    # scale from pJ to nJ
    data[k] *= 0.001

    # also subtract icache cost (make sure already parsed) b/c applied seperately
    # TODO this is somewhat hand wavy!
    data[k] -= get_read_energy('icache', 1)

  return data

# return dict containing info about memory cost file
def parse_memory_file(file_name):
  # is this r/w or just read?
  floatRegexStr = '([+-]?([0-9]*[.])?[0-9]+)'
  energy_regex = re.compile('Total dynamic read energy per access \(nJ\): ' + floatRegexStr)

  data = {}

  with open(file_name, 'r') as f:
    for line in f:
      match = energy_regex.search(line)
      if (match):
        # TODO is read energy the same as write energy??
        data['read_energy'] = float(match.group(1))

  return data
      

# get cost for specified name
def get_cost(name, field):
  if (not name in cost_dicts or not field in cost_dicts[name]):
    return 0
  return cost_dicts[name][field]

# give name of memory ('icache', 'dmem', or 'llc') along with count
def get_read_energy(memory_name, num_reads):
  return get_cost(memory_name, 'read_energy') * num_reads

def get_write_energy(memory_name, num_writes):
  # TODO using read energy (all cacti reports)
  # looks like pretty similar based on paper I saw
  return get_cost(memory_name, 'read_energy') * num_writes

def get_memory_energy(memory_name, num_reads, num_writes):
  return get_read_energy(memory_name, num_reads) + get_write_energy(memory_name, num_writes)

# get instruction cost
# expect a dict with total counts for relevant OpClasses from gem5 stats
def get_instruction_energy(cnts):
  if (len(cnts) == 0):
    return 0

  # aggregate gem5 op types into counts we know how to map
  intAlu = \
    cnts['No_OpClass'] + \
    cnts['IntAlu']

  intMult = \
    cnts['IntMult']

  intDiv = \
    cnts['IntDiv'] + \
    cnts['FloatDiv']

  floatAdd = \
    cnts['FloatAdd'] + \
    cnts['FloatCmp']
    # cnts['FloatMultAcc'] # double cnt this for add and mult. TODO but then counts non-op non-icache overhead

  floatMult = \
    cnts['FloatMult'] + \
    cnts['FloatMultAcc']

  # just the core energy to do (AFAIK)
  memRead = \
    cnts['MemRead'] + \
    cnts['FloatMemRead']

  memWrite = \
    cnts['MemWrite'] + \
    cnts['FloatMemWrite']

  # ignoring the follwoing
    # branch T/NT (just using NT for now)
    # cnts['FloatCvt']
    # cnts['FloatSqrt']

  # merge counts with costs
  return \
    get_cost('inst', 'IntAlu')    * intAlu    + \
    get_cost('inst', 'IntMult')   * intMult   + \
    get_cost('inst', 'IntDiv')    * intDiv    + \
    get_cost('inst', 'FloatAdd')  * floatAdd  + \
    get_cost('inst', 'FloatMult') * floatMult + \
    get_cost('inst', 'MemRead')   * memRead   + \
    get_cost('inst', 'MemWrite')  * memWrite

# define options
def define_options(parser):
  parser.add_argument('--inst-cost-file', default='../data/inst/inst_energy_28nm.json', help='Path to json containing instruction costs')
  parser.add_argument('--inst-cost-node', default='28', help='What technology node the costs were measured at in nm')
  parser.add_argument('--used-node', default='32', help='Desired technology node to scale to')
  parser.add_argument('--icache-file', default='../data/memory/4kB_icache.out', help='Path to file containing params for icache')
  parser.add_argument('--dmem-file', default='../data/memory/4kB_spad.out', help='Path to file containing params for local data mem')
  parser.add_argument('--llc-file', default='../data/memory/32kb_llc.out', help='Path to file containing params for llc')

# do parsing into member/global variables
def setup_energy_model(args):
  cost_dicts['icache'] = parse_memory_file(args.icache_file)
  cost_dicts['dmem'] = parse_memory_file(args.dmem_file)
  cost_dicts['llc'] = parse_memory_file(args.llc_file)
  cost_dicts['inst'] = parse_inst_cost_file(args.inst_cost_file, args.inst_cost_node, args.used_node)

  # TODO DRAM energy. pj/bit = 7
  # Access prob 64bytes (line) * 8bits/byte * 7pj/bit = 3584pJ


  # MAIN
  # 

  # for k,v in cost_dicts['inst'].items():
  #   print('{} {}'.format(k, v))

  # print('{} {}'.format('icache', cost_dicts['icache']['read_energy']))
  # print('{} {}'.format('dmem', cost_dicts['dmem']['read_energy']))
  # print('{} {}'.format('llc', cost_dicts['llc']['read_energy']))