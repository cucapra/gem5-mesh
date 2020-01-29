import re
from collections import OrderedDict

#
# Get avg of each of these stats

floatRegexStr = '([+-]?([0-9]*[.])?[0-9]+)'
intRegexStr = '([0-9]+)'

stats = OrderedDict([ 
  ('cycles' , { 
    'name' : 'cycles',
    'regex' : re.compile('system.cpu0+.numCycles\s*' + intRegexStr),
    'ignore-zero' : False,
  }), 
  ('icache' , { 
    'name' : 'icache_access',
    'regex' : re.compile('system.icaches[0-9]+.L1cache.demand_accesses\s*' + intRegexStr),
    'ignore-zero' : False,
  }), 
  ('locsp'  , { 
    'name' : 'local_sp_access',
    'regex' : re.compile('system.scratchpads[0-9]+.local_accesses\s*' + intRegexStr),
    'ignore-zero' : False,
  }), 
  ('remsp'  , { 
    'name' : 'remote_sp_access',
    'regex' : re.compile('system.scratchpads[0-9]+.remote_accesses\s*' + intRegexStr),
    'ignore-zero' : False, 
  }),
  ('l2'     , { 
    'name' : 'llc_access',
    'regex' : re.compile('system.l2_cntrls[0-9]+.cacheMemory.demand_accesses\s*' + intRegexStr),
    'ignore-zero' : False,
  }),
  ('dramrd' , { 
    'name' : 'dram_reads',
    'regex' : re.compile('system.mem_ctrl.num_reads::total\s*' + intRegexStr),
    'ignore-zero' : False,
  }),
  ('dramwr' , { 
    'name' : 'dram_writes',
    'regex' : re.compile('system.mem_ctrl.num_writes::total\s*' + intRegexStr),
    'ignore-zero' : False,
  }),
#   ('inMesh' , { 
#     'name' : 'mesh_stall', 
#     'regex' : re.compile('system.cpu[0-9]+.vector.mesh_input_stalls\s*' + intRegexStr), 
#     'ignore-zero' : False,
#   }),
  ('slaveMesh' , { 
    'name' : 'slave_mesh_stall', 
    'regex' : re.compile('system.cpu[0-9]+.vector.mesh_input_stalls\s*' + intRegexStr), 
    'ignore-zero' : True,
  }),
  ('inPipe' , { 
    'name' : 'pipe_stall', 
    'regex' : re.compile('system.cpu[0-9]+.vector.pipe_input_stalls\s*' + intRegexStr), 
    'ignore-zero' : False,
  }),
  ('inPipeRoot' , { 
    'name' : 'root_pipe_stall', 
    'regex' : re.compile('system.cpu0.vector.pipe_input_stalls\s*' + intRegexStr), 
    'ignore-zero' : False,
  }),
  # backpressure
  ('backHg' , { 
    'name' : 'master_backpress_stall',
    'regex' : re.compile('system.cpu[0-9]+.late_vector.backpressure_stalls\s*' + intRegexStr), 
    'ignore-zero' : True,
  }),
  ('revec' ,  { 
    'name' : 'slave_revec_stall', 
    'regex' : re.compile('system.cpu[0-9]+.vector.revec_stalls\s*' + intRegexStr), 
    'ignore-zero' : True,
  }),
  ('cpi' ,  { 
    'name' : 'cpi', 
    'regex' : re.compile('system.cpu[0-9]+.cpi\s*' + floatRegexStr), 
    'ignore-zero' : False,
  }),
  ('llc-misses' ,  { 
    'name' : 'llc-misses', 
    'regex' : re.compile('system.l2_cntrls[0-9]+.cacheMemory.demand_misses\s*' + intRegexStr), 
    'ignore-zero' : False,
  }),
  ('prefetch-queue-size' ,  { 
    'name' : 'prefetch-queue-size', 
    'regex' : re.compile('system.scratchpads[0-9]+.max_queue_size\s*' + intRegexStr), 
    'ignore-zero' : False,
  }),

])