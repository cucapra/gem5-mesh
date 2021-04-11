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

# return dict containing info about instruction cost file, scaled to target_node
def parse_inst_cost_file(file_name, target_node):
  # load data
  with open(file_name, 'r') as f:
    data = json.load(f)

  cur_node = data['Node']
  assert(data['Units'] == 'pJ')

  # scale data to target tech node
  for inst_key, i in data['Costs'].items():
    for k,v in i.items():
      if k == 'Ex_Cycles':
        continue
      i[k] = linear_scale(float(v), float(cur_node), float(target_node))

      # scale from pJ to nJ
      i[k] *= 0.001

    # take relevant fields to calculate total energy cost
    # Add most fields. For mul and div that have multiple cycle executions 
    #   incorporate by increasing mul cost by that factor
    # excluding the following
    #   I$
    #   VM
    #   D$
    #   CTS - i assume this is counters
    # # i['PC'] + i['IF_Rest'] + \
    # data[inst_key] = \
    #   i['PC'] + i['IF_Rest'] + \
    #   i['DEC'] + i['ID_Rest'] + \
    #   i['IS'] + \
    #   i['LS'] + (i['MUL'] * i['Ex_Cycles']) + i['ALU'] + i['EX_Rest'] + \
    #   i['WB'] + i['CSR'] + i['Rest']

  return data

# cache instruction costs
def update_inst_cost():
  data = cost_dicts['inst']
  for inst_key, i in data['Costs'].items():
    # scalar instruction
    data[inst_key] = get_inst_cost(i, 1)

    # cache some vector versions are well
    data['EstVec4' + inst_key] =  get_inst_cost(i, 4)

# calculate cost for just doing instruction
# For estimated vector, multiply MUL, ALU, EX_Rest, and WB by vlen for total
# can't scale register read b/c not clear how to do, but shouldn't have huge impact
def get_inst_cost(i, vlen):
  # take relevant fields to calculate total energy cost
  # Add most fields. For mul and div that have multiple cycle executions 
  #   incorporate by increasing mul cost by that factor
  # excluding the following
  #   I$
  #   VM
  #   D$
  #   CTS - i assume this is counters
  # # i['PC'] + i['IF_Rest'] + \
  total_cost = \
      i['DEC'] + i['ID_Rest'] + \
      i['IS'] + \
      i['LS'] + ((i['MUL'] * i['Ex_Cycles']) + i['ALU'] + i['EX_Rest']) * vlen + \
      (i['WB'] * vlen) + i['CSR'] + i['Rest']

  return total_cost


def update_icache_cost():
  # bake in energy from fetch stage into this icache access
  # just take IntALU cost pc and if cost, 
  # these dont vary much between instruction types (div a lil low but its weird)
  cost_dicts['icache']['read_energy'] += \
    cost_dicts['inst']['Costs']['IntAlu']['PC'] + \
    cost_dicts['inst']['Costs']['IntAlu']['IF_Rest']

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
    assert(False)
    return 0
  else:
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
    cnts['FloatCmp'] + \
    cnts['FloatCvt']
    # cnts['FloatMultAcc'] # double cnt this for add and mult. TODO but then counts non-op non-icache overhead

  floatMult = \
    cnts['FloatMult'] + \
    cnts['FloatMultAcc'] + \
    cnts['FloatSqrt']

  # just the core energy to do (AFAIK)
  memRead = \
    cnts['MemRead'] + \
    cnts['FloatMemRead']

  memWrite = \
    cnts['MemWrite'] + \
    cnts['FloatMemWrite']

  # ignoring the follwoing
    # branch T/NT (just using NT for now)

  vecAdd = \
    cnts['SimdAdd'] + \
    cnts['SimdAlu'] + \
    cnts['SimdCmp'] + \
    cnts['SimdMisc'] + \
    cnts['SimdShift'] + \
    cnts['SimdFloatAdd'] + \
    cnts['SimdFloatAlu'] + \
    cnts['SimdFloatCmp'] + \
    cnts['SimdReduceAdd'] + \
    cnts['SimdFloatReduceAdd']

  vecMul = \
    cnts['SimdMult'] + \
    cnts['SimdFloatMult'] + \
    cnts['SimdFloatSqrt']

  vecDiv = \
    cnts['SimdFloatDiv']


  # merge counts with costs
  # SIMD costs assum vlen of 4!!
  return \
    get_cost('inst', 'IntAlu')    * intAlu    + \
    get_cost('inst', 'IntMult')   * intMult   + \
    get_cost('inst', 'IntDiv')    * intDiv    + \
    get_cost('inst', 'IntAlu')    * floatAdd  + \
    get_cost('inst', 'IntMult')   * floatMult + \
    get_cost('inst', 'MemOp')     * memRead   + \
    get_cost('inst', 'MemOp')     * memWrite  + \
    get_cost('inst', 'EstVec4IntAlu')   * vecAdd + \
    get_cost('inst', 'EstVec4IntMult')  * vecMul + \
    get_cost('inst', 'EstVec4IntDiv')   * vecDiv

# define options
def define_options(parser):
  parser.add_argument('--inst-cost-file', default='../data/inst/inst_energy_28nm.json', help='Path to json containing instruction costs')
  parser.add_argument('--used-node', default='32', help='Desired technology node to scale to')
  parser.add_argument('--icache-file', default='../data/memory/4kB_icache.out', help='Path to file containing params for icache')
  parser.add_argument('--dmem-file', default='../data/memory/4kB_spad.out', help='Path to file containing params for local data mem')
  parser.add_argument('--llc-file', default='../data/memory/32kb_llc.out', help='Path to file containing params for llc')

# do parsing into member/global variables
def setup_energy_model(args):
  cost_dicts['inst'] = parse_inst_cost_file(args.inst_cost_file, args.used_node)
  cost_dicts['icache'] = parse_memory_file(args.icache_file)
  cost_dicts['dmem'] = parse_memory_file(args.dmem_file)
  cost_dicts['llc'] = parse_memory_file(args.llc_file)

  # figure out totals
  update_inst_cost()
  update_icache_cost()


  # TODO DRAM energy. pj/bit = 7
  # Access prob 64bytes (line) * 8bits/byte * 7pj/bit = 3584pJ


  # MAIN
  # 

  # for k,v in cost_dicts['inst'].items():
  #   print('{} {}'.format(k, v))

  # print('{} {}'.format('icache', cost_dicts['icache']['read_energy']))
  # print('{} {}'.format('dmem', cost_dicts['dmem']['read_energy']))
  # print('{} {}'.format('llc', cost_dicts['llc']['read_energy']))