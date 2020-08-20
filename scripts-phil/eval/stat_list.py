import re
from collections import OrderedDict

#
# Get avg of each of these stats

floatRegexStr = '([+-]?([0-9]*[.])?[0-9]+)'
intRegexStr = '([0-9]+)'

cpu_stats = OrderedDict([ 
  ('cycles' , { 
    'regex' : re.compile('system.cpu0+.numCycles\s*' + intRegexStr),
    'ignore-zero' : False,
  }), 
  ('icache_access' , { 
    'regex' : re.compile('system.cpu[0-9]+.fetch.icache_word_accesses\s*' + intRegexStr),
    'ignore-zero' : False,
    'average' : False,
  }), 
  ('local_sp_access'  , { 
    'regex' : re.compile('system.scratchpads[0-9]+.local_accesses\s*' + intRegexStr),
    'ignore-zero' : False,
  }), 
  ('remote_sp_access'  , { 
    'regex' : re.compile('system.scratchpads[0-9]+.remote_accesses\s*' + intRegexStr),
    'ignore-zero' : False, 
  }),
  ('llc_access'     , { 
    'regex' : re.compile('system.l2_cntrls[0-9]+.cacheMemory.demand_accesses\s*' + intRegexStr),
    'ignore-zero' : False,
  }),
  ('dram_reads' , { 
    'regex' : re.compile('system.mem_ctrl.num_reads::total\s*' + intRegexStr),
    'ignore-zero' : False,
  }),
  ('dram_writes' , { 
    'regex' : re.compile('system.mem_ctrl.num_writes::total\s*' + intRegexStr),
    'ignore-zero' : False,
  }),
#   ('inMesh' , { 
#     'name' : 'mesh_stall', 
#     'regex' : re.compile('system.cpu[0-9]+.vector.mesh_input_stalls\s*' + intRegexStr), 
#     'ignore-zero' : False,
#   }),
  ('slave_mesh_stall' , { 
    'regex' : re.compile('system.cpu[0-9]+.vector.mesh_input_stalls\s*' + intRegexStr), 
    'ignore-zero' : True,
  }),
  ('pipe_stall' , {  
    'regex' : re.compile('system.cpu[0-9]+.vector.pipe_input_stalls\s*' + intRegexStr), 
    'ignore-zero' : False,
  }),
  # ('inPipeRoot' , { 
  #   'name' : 'root_pipe_stall', 
  #   'regex' : re.compile('system.cpu0.vector.pipe_input_stalls\s*' + intRegexStr), 
  #   'ignore-zero' : False,
  # }),
  # backpressure
  ('master_backpress_stall' , { 
    'regex' : re.compile('system.cpu[0-9]+.late_vector.backpressure_stalls\s*' + intRegexStr), 
    'ignore-zero' : True,
  }),
  # ('revec' ,  { 
  #   'name' : 'slave_revec_stall', 
  #   'regex' : re.compile('system.cpu[0-9]+.vector.revec_stalls\s*' + intRegexStr), 
  #   'ignore-zero' : True,
  # }),
  ('active-cpi' ,  { 
    'regex' : re.compile('system.cpu[0-9]+.cpi\s*' + floatRegexStr), 
    'ignore-zero' : False,
    # cpi of inactive cores is really high, ignore for stat reporting
    'upper-bound' : 200
  }),
  ('llc-misses' ,  { 
    'regex' : re.compile('system.l2_cntrls[0-9]+.cacheMemory.demand_misses\s*' + intRegexStr), 
    'ignore-zero' : False,
  }),
  # ('prefetch-queue-size' ,  { 
  #   'name' : 'prefetch-queue-size', 
  #   'regex' : re.compile('system.scratchpads[0-9]+.max_queue_size\s*' + intRegexStr), 
  #   'ignore-zero' : False,
  # }),
  #
  ('frame-occupancy1' ,  { 
    'regex' : re.compile('system.scratchpads[0-9]+.occupancy::1\s+' + floatRegexStr), 
    'ignore-zero' : True,
    'upper-bound' : 1.000001,
  }),
  ('frame-occupancy2' ,  { 
    'regex' : re.compile('system.scratchpads[0-9]+.occupancy::2\s+' + floatRegexStr), 
    'ignore-zero' : True,
    'upper-bound' : 1.000001,
  }),
  ('frame-occupancy3' ,  { 
    'regex' : re.compile('system.scratchpads[0-9]+.occupancy::3\s+' + floatRegexStr), 
    'ignore-zero' : True,
    'upper-bound' : 1.000001,
  }),
  # ('frame-occupancy4' ,  { 
  #   'name' : '4', 
  #   'regex' : re.compile('system.scratchpads[0-9]+.occupancy::4\s+' + floatRegexStr), 
  #   'ignore-zero' : True,
  # }),
  #
  # ('prefetch-latency' ,  { 
  #   'name' : 'prefetch-latency', 
  #   'regex' : re.compile('system.prefetch_latencies::([0-9\-]+)\s+' + intRegexStr), 
  #   'hist' : True,
  # }),
  ('inst-cnts-energy(nJ)' ,  { 
    'regex' : re.compile('system.cpu[0-9]+.iew.executed_insts_0::([a-zA-Z_]+)\s+' + intRegexStr),
    'hist' : True,
    'average' : False,
    'energy' : 'inst',
  }),
  ('icache-access-energy(nJ)' ,  { 
    'regex' : re.compile('system.cpu[0-9]+.fetch.icache_word_accesses\s*' + intRegexStr), 
    'energy' : 'icache',
    'average' : False,
  }),
  ('dmem-access-energy(nJ)' ,  { 
    'regex' : re.compile('system.scratchpads[0-9]+.total_accesses\s*' + intRegexStr), 
    'energy' : 'dmem',
    'average' : False,
  }),
  ('llc-access-energy(nJ)' ,  {  
    'regex' : re.compile('system.l2_cntrls[0-9]+.cacheMemory.demand_accesses\s*' + intRegexStr), 
    'energy' : 'llc',
    'average' : False,
  }),
  ('energy-sum(nJ)' ,  { 
    'formula' : ['inst-cnts-energy(nJ)', 'icache-access-energy(nJ)', 'dmem-access-energy(nJ)', 'llc-access-energy(nJ)'],
    'formula_op' : lambda args: args[0] + args[1] + args[2] + args[3] 
  }),

  ('token_stalls' , {
    'regex' : re.compile('system.cpu[0-9]+.iew.stall_on_tokens\s*' + intRegexStr), 
  }),

  ('mesh_stall_sep' , {
    'regex' : re.compile('system.cpu[0-9]+.vector.mesh_input_stalls\s*' + intRegexStr), 
    'seperate-cores' : True,
  }),
  ('vec_cycles_sep' , {
    'regex' : re.compile('system.cpu[0-9]+.vector.vec_cycles\s*' + intRegexStr), 
    'seperate-cores' : True,
  }),
  ('vec_cycles' , {
    'regex' : re.compile('system.cpu[0-9]+.vector.vec_cycles\s*' + intRegexStr),
  }),
  ('frac_mesh_stall_sep' , {
    'formula' : ['mesh_stall_sep', 'vec_cycles_sep'],
    'formula_op' : lambda args: float(args[0]) / float(args[1]) if args[1] > 0 else 0,
    'seperate-cores' : True,
  }),
  ('frac_token_stalls' , {
    'formula' : ['token_stalls', 'vec_cycles'],
    'formula_op' : lambda args: float(args[0]) / float(args[1]) if args[1] > 0 else 0,
  }),

  ('vertical_pfs' ,  {  
    'regex' : re.compile('system.cpu[0-9]+.iew.Mem.vertical_prefetches\s*' + intRegexStr), 
    'average' : False,
  }),
  ('horizontal_pfs' ,  {  
    'regex' : re.compile('system.cpu[0-9]+.iew.Mem.horizontal_prefetches\s*' + intRegexStr), 
    'average' : False,
  }),
  ('scalar_pfs' ,  {  
    'regex' : re.compile('system.cpu[0-9]+.iew.Mem.scalar_prefetches\s*' + intRegexStr), 
    'average' : False,
  }),

])


gpu_stats = OrderedDict([ 
  ('cycles' , { 
    'regex' : re.compile('system.cpu2.CUs[0-9]+.num_total_cycles\s*' + intRegexStr),
    'max'   : True,
  }), 
])
