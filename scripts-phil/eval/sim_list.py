'''
  Sim configurations

  Authors: Philip Bedoukian
'''
from copy import deepcopy
import json

# get configs (resolves config groups)
def get_configs(config_list, config_info, config_groups):
  flat_configs = []
  for config_name in config_list:
    # if a config group add all sub config_infos
    if config_name in config_groups:
      for c in config_groups[config_name]:
        if (c in config_info):
          flat_configs.append(c)
    # otherwise just add config
    elif config_name in config_info:
      flat_configs.append(config_name)

  return flat_configs

# get simconfig from json
def generate_sim_config_from_file(filename):
  with open(filename) as fin:
    sim_json = json.load(fin)

  sim_configs = []

  config_groups = {}

  benchmarks = sim_json['Benchmarks']
  config_info = sim_json['ConfigInfo']
  if 'ConfigGroups' in sim_json:
    config_groups = sim_json['ConfigGroups']

  # generate run description for each program and hw/sw configurations
  for k,v in benchmarks.items():
    # get configurations (if diff config, then make a new entry in sim_configs)
    flat_configs = get_configs(v['Configs'], config_info, config_groups)
    for config_name in flat_configs:
      config = config_info[config_name]

      # get hardware and software info (all combinations applied)
      soft_configs = []
      hard_configs = []
      for s in config['Software']:
        soft_configs.append(s)
      for h in config['Hardware']:
        hard_configs.append(h)

      # run descriptions is:
      #    bench
      #    argv
      #    SoftwareConfig
      #    HardwareConfig
      run_desc = {
        'program' : k,
        'argv'    : v['Argv'],
        'software': soft_configs,
        'hardware': hard_configs
      }

      sim_configs.append(run_desc)

  return sim_configs

def string_to_cacheline_arg(a):
  if (a[0:16] == 'CACHE_LINE_SIZE='):
    return int(a[16:len(a)])
  elif (a[0:16] == 'CACHE_LINE_SIZE_'):
    return int(a[16:len(a)])
  else:
    return -1

def string_to_hwvlen_arg(a):
  if (a[0:20] == 'HARDWARE_VECTOR_LEN='):
    return int(a[20:len(a)])
  elif (a[0:20] == 'HARDWARE_VECTOR_LEN_'):
    return int(a[20:len(a)])
  else:
    return -1

def string_to_num_cpus(a):
  if (a[0:6] == 'NCPUS='):
    return int(a[6:len(a)])
  elif (a[0:6] == 'NCPUS_'):
    return int(a[6:len(a)])
  else:
    return -1

# find an appropriate hardware cachelines size
# if cant find defined in software, then default to 64 
def get_cacheline_opt(args):
  found = False
  if (isinstance(args, list)):
    for a in args:
      cl_sw_size = string_to_cacheline_arg(a)
      if (cl_sw_size > 0):
        cl_hw_size = cl_sw_size
        found = True
  else:
    cl_sw_size = string_to_cacheline_arg(args)
    if (cl_sw_size > 0):
      cl_hw_size = cl_sw_size
      found = True

  if (found):
    return '--cacheline_size=' + str(cl_hw_size)
  else:
    return ''

def get_hardware_vlen(args):
  found = False
  if (isinstance(args, list)):
    for a in args:
      vl_sw_size = string_to_hwvlen_arg(a)
      if (vl_sw_size > 0):
        vl_hw_size = vl_sw_size
        found = True
  else:
    vl_sw_size = string_to_hwvlen_arg(args)
    if (vl_sw_size > 0):
      vl_hw_size = vl_sw_size
      found = True
  
  if (found):
    return '--hw-vlen=' + str(vl_hw_size)
  else:
    return ''

def get_num_cpus(args, default_cpus=64):
  found = False
  found_num_cpus = 0
  if (isinstance(args, list)):
    for a in args:
      num_cpus = string_to_num_cpus(a)
      if (num_cpus > 0):
        found_num_cpus = num_cpus
        found = True
  else:
    num_cpus = string_to_num_cpus(args)
    if (num_cpus > 0):
      found_num_cpus = num_cpus
      found = True

  if (found):
    return found_num_cpus
  else:
    return default_cpus

# make a shorthand to represent the config output name
# note replace '=' with '_' because converted when called
def abbreviate_config(config):
  if (config == 'VEC_4_SIMD' or config == 'VEC_LEN_4' or config == 'VECTOR_LEN_4' ):
    return 'V4'
  elif (config == 'VEC_16_SIMD' or config == 'VEC_LEN_16' or config == 'VECTOR_LEN_16' ):
    return 'V16'
  elif (config == 'NO_VEC'):
    return 'NV'
  elif (config == 'REUSE'):
    return 'R'
  elif (config == 'MANYCORE_PREFETCH'):
    return 'PF'
  elif (config == 'INIT_FRAMES_0'):
    return 'I0'
  elif (config == 'LONGLINES'):
    return 'LL'
  elif (config == 'PER_CORE_SIMD'):
    return 'PCV'
  elif (config == 'CACHE_LINE_SIZE_1024'):
    return 'CL1024'
  elif (config == 'CACHE_LINE_SIZE_256'):
    return 'CL256'
  elif (config == '__cpu_type_DerivO3CPU'):
    return '_O3'
  elif (config[0:11] == 'VECTOR_LEN='):
    return config[11:len(config)]
  elif (config[0:3] == 'PF='):
    return config[3:len(config)]
  else:
    return config

# either array or single string
def strings_to_make_args(args):
  # cmd_line = 'ENV_EXTRA_MAKE_FLAGS=\''
  cmd_line = 'EXTRA_FLAGS=\''
  if (isinstance(args, list)):
    for a in args:
      cmd_line += '-D' + a + ' '
    cmd_line += '\''
  else:
    cmd_line += '-D' + args + '\''

  # DONE in makefile currently
  # # add which compiler to use
  # uses_tril_vec = False
  # if (isinstance(args, list)):
  #   for a in args:
  #     if (is_tril_vec_config(a)):
  #       uses_tril_vec = True
  # else:
  #   if (is_tril_vec_config(args)):
  #     uses_tril_vec = True

  # if (uses_tril_vec):
  #   cmd_line += ' ENV_GCC_VER=8'
  # else:
  #   cmd_line += ' ENV_GCC_VER=10'

  return cmd_line

# remove dashes from hardware args so annoation is nicer to read and parse
# then call strings_to_metadata after transformation
def preprocess_metadata(arg):
  out_str = ''
  for l in arg:
    # swap - and = with _
    if (l == '-' or l == '=' or l == ' '):
      next_char = '_'
    else:
      next_char = l
    out_str += next_char
  return out_str

# turn hardware config command line into part of run annoation
# similar to strings_to_metadata() but first does some processing on format
def preprocess_and_convert_to_metadata(args):
  converted_args = deepcopy(args)
  if (isinstance(args, list)):
    for i in range(0, len(converted_args)):
      converted_args[i] = preprocess_metadata(converted_args[i])
  else:
    converted_args = preprocess_metadata(converted_args)

  return strings_to_metadata(converted_args)

# turn config into metadata to make which run was used
def strings_to_metadata(args):
  meta = ''
  if (isinstance(args, list)):
    for a in args:
      # special interpretations
      arg = abbreviate_config(a)
      meta += arg
      if (a != args[-1]):
        meta += '_'
  else:
    meta = abbreviate_config(args)
  return meta

def get_binary_name(prog_key, vec_config):
  return prog_key + '_' + preprocess_and_convert_to_metadata(vec_config)


# specify programs. with the path to the program, the executible name, the default options, and string to check to see if successful (opt)
progDir0 = '../../programs-spad/'
programs = {

  'vvadd' : { 'name': 'vvadd', 'path' : progDir0 + 'vvadd/vvadd', 
    'options' : '{0}',
    'serialize' : '-size{0}',
    'success' : '\[\[SUCCESS\]\]'},
  'stencil' : { 'name': 'stencil', 'path' : progDir0 + 'stencil/stencil', 
    'options' : '{0} {1}',
    'serialize' : '-cols{0}-rows{1}',
  },
  'bicg' : { 'name': 'bicg', 'path' : progDir0 + 'bicg/bicg', 
    'options' : "{0} {0}",
    'serialize' : '-NX{0}-NY{0}',
  },
  'gram' : { 'name': 'gram_schmidt', 'path' : progDir0 + 'gramschmidt/gramschmidt', 
    'options' : "{0} {0}",
    'serialize' : '-vecs{0}-len{0}',
  },
  'syrk' : { 'name': 'syrk', 'path' : progDir0 + 'syrk/syrk', 
    'options' : "{0} {0}",
    'serialize' : '-N{0}-M{0}',
  },
  'syr2k' : { 'name': 'syr2k', 'path' : progDir0 + 'syr2k/syr2k', 
    'options' : "{0} {0}",
    'serialize' : '-N{0}-M{0}',
  },
  'atax' : { 'name': 'atax', 'path' : progDir0 + 'atax/atax', 
    'options' : "{0} {0}",
    'serialize' : '-NX{0}-NY{0}',
  },
  'mvt' : { 'name': 'mvt', 'path' : progDir0 + 'mvt/mvt', 
    'options' : "{0}",
    'serialize' : '-N{0}',
  },
  'gemm' : { 'name': 'gemm', 'path' : progDir0 + 'gemm_final/gemm', 
    'options' : "{0} {0} {0}",
    'serialize' : '-N{0}-M{0}-T{0}',
  },
  'gesummv' : { 'name': 'gesummv', 'path' : progDir0 + 'gesummv/gesummv', 
    'options' : "{0}",
    'serialize' : '-N{0}',
  },
  'corr' : { 'name': 'corr', 'path' : progDir0 + 'corr/corr', 
    'options' : "{0} {0}",
    'serialize' : '-M{0}-N{0}',
  },
  'covar' : { 'name': 'covar', 'path' : progDir0 + 'covar/covar', 
    'options' : "{0} {0}",
    'serialize' : '-M{0}-N{0}',
  },
  'conv2d' : { 'name': 'conv2d', 'path' : progDir0 + 'conv2d/conv2d', 
    'options' : "{0} {0}",
    'serialize' : '-NI{0}-NJ{0}',
  },
  'conv3d' : { 'name': 'conv3d', 'path' : progDir0 + 'conv3d/conv3d', 
    'options' : "{0} {0} {0}",
    'serialize' : '-NI{0}-NJ{0}-NK{0}',
  },
  '2mm' : { 'name': '2mm', 'path' : progDir0 + '2mm/2mm', 
    'options' : "{0} {0} {0} {0}",
    'serialize' : '-{0}-{0}-{0}-{0}',
  },
  '3mm' : { 'name': '3mm', 'path' : progDir0 + '3mm/3mm', 
    'options' : "{0} {0} {0} {0} {0}",
    'serialize' : '-{0}-{0}-{0}-{0}-{0}',
  },
  'fdtd' : { 'name': 'fdtd', 'path' : progDir0 + 'fdtd2d/fdtd2d', 
    'options' : "{0} {0} {1}",
    'serialize' : '-NX{0}-NY{0}-t{1}',
  },
  'bfs' : { 'name': 'bfs', 'path' : progDir0 + 'bfs-rodinia/bfs', 
    'options' : "{0}",
    'serialize' : '-4k',
  },
}




