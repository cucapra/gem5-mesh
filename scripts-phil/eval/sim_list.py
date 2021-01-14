'''
  Sim configurations
'''

# helper set of vec configs you can use in a benchmark (assuming it supports)
ALL_CONFIGS = ['NO_VEC', 'PACKED_SIMD', 'VEC_4_SIMD', 'VEC_16_SIMD', [ 'NO_VEC', 'MANYCORE_PREFETCH' ] ]
ALL_NEIL_CONFIGS = [ 'NO_VEC', 'PACKED_SIMD', 'VEC_LEN=4', 'VEC_LEN=16', [ 'NO_VEC', 'MANYCORE_PREFETCH' ]]
INIT0_CONFIGS = [ [ 'VEC_4_SIMD', 'INIT_FRAMES=0' ] ]
INIT0_NEIL_CONFIGS = [ [ 'VEC_LEN=4', 'INIT_FRAMES=0' ] ]

TEST_1_12_CONFIGS = ['PACKED_SIMD', 'VEC_4_SIMD', 'VEC_16_SIMD', [ 'NO_VEC', 'MANYCORE_PREFETCH' ] ]
TEST_1_12_CONFIGS_NEIL = ['PACKED_SIMD', 'VEC_LEN=4', 'VEC_LEN=16', [ 'NO_VEC', 'MANYCORE_PREFETCH' ] ]
HW_OPTS = ['--net-width=1', '--net-width=2', '--net-width=16']
ALL_CONFIGS = TEST_1_12_CONFIGS
ALL_NEIL_CONFIGS = TEST_1_12_CONFIGS_NEIL
INIT0_CONFIGS = []
INIT0_NEIL_CONFIGS = []

# choose which programs to run via script and with what configs
sim_configs = {
  # Test programs, not actual benchmarks

  # 'vvadd': {
  #   'vec'  : ALL_CONFIGS,
  #   'argv' : ['131072'] # ['1024']
  # },
  # 'stencil': {
  #   'vec'  : ['VEC_4_SIMD'],
  #   'argv' : ['1730', '60']
  # },

  # Benchmarks

  # 'bicg'   : {
  #   'vec'  : ALL_CONFIGS + INIT0_CONFIGS,
  #   'argv' : ['2048'],
  #   'hw_opts' : HW_OPTS
  # },
  'gram'   : {
    'vec'  : ALL_CONFIGS + INIT0_CONFIGS,
    'argv' : ['320'],
    'hw_opts' : HW_OPTS
  },
  'syrk'   : {
    'vec'  : ALL_CONFIGS + INIT0_CONFIGS,
    'argv' : ['256'],
    'hw_opts' : HW_OPTS
  },
  'syr2k'  : {
    'vec'  : ALL_CONFIGS + INIT0_CONFIGS,
    'argv' : ['256'],
    'hw_opts' : HW_OPTS
  },
  'covar'   : {
    'vec'  : ALL_CONFIGS + INIT0_CONFIGS,
    'argv' : ['512'],
    'hw_opts' : HW_OPTS
  },
  'conv2d' : {
    # 'vec'  : [ 'NO_VEC', 'PACKED_SIMD', 'VEC_4_SIMD_VERTICAL', 'VEC_16_SIMD_VERTICAL', [ 'NO_VEC', 'MANYCORE_PREFETCH' ], ['VEC_4_SIMD_VERTICAL', 'INIT_FRAMES=0' ] ],
    'vec'  : [ 'PACKED_SIMD', 'VEC_4_SIMD_VERTICAL', 'VEC_16_SIMD_VERTICAL', [ 'NO_VEC', 'MANYCORE_PREFETCH' ] ],
    'argv' : ['2048'],
    'hw_opts' : HW_OPTS
  },
  'conv3d' : {
    'vec'  : ALL_CONFIGS + INIT0_CONFIGS,
    'argv' : ['256'],
    'hw_opts' : HW_OPTS
  },
  'fdtd' : {
    'vec'  : ALL_CONFIGS + INIT0_CONFIGS,
    'argv' : ['512', '30'],
    'hw_opts' : HW_OPTS
  },

  # 'atax'   : {
  #   'vec'  : ALL_NEIL_CONFIGS + INIT0_NEIL_CONFIGS,
  #   'argv' : ['2048'], # ['128']
  #   'hw_opts' : HW_OPTS
  # },
  # 'mvt'    : {
  #   'vec'  : ALL_NEIL_CONFIGS + INIT0_NEIL_CONFIGS,
  #   'argv' : ['4096'], # ['128']
  #   'hw_opts' : HW_OPTS
  # },
  'gemm'   : {
    'vec'  : ALL_NEIL_CONFIGS + INIT0_NEIL_CONFIGS,
    'argv' : ['256'], #['64']
    'hw_opts' : HW_OPTS
  },
  'gesummv'   : {
    'vec'  : ALL_NEIL_CONFIGS + INIT0_NEIL_CONFIGS,
    'argv' : ['4096'], #['128']
    'hw_opts' : HW_OPTS 
  },
  'corr'   : {
    'vec'  : ALL_NEIL_CONFIGS + INIT0_NEIL_CONFIGS,
    'argv' : ['512'], #['64']
    'hw_opts' : HW_OPTS
  },
  # '2mm' : {
  #   'vec'  : ALL_NEIL_CONFIGS + INIT0_NEIL_CONFIGS,
  #   'argv' : ['256'], #['64']
  #   'hw_opts' : HW_OPTS
  # },
  # '3mm' : {
  #   'vec'  : ALL_NEIL_CONFIGS + INIT0_NEIL_CONFIGS,
  #   'argv' : ['256'], #['32']
  #   'hw_opts' : HW_OPTS
  # },

}




# make a shorthand to represent the config output name
def abbreviate_config(config):
  if (config == 'VEC_4_SIMD' or config == 'VEC_LEN=4'):
    return 'V4'
  elif (config == 'VEC_16_SIMD' or config == 'VEC_LEN=16'):
    return 'V16'
  elif (config == 'NO_VEC'):
    return 'NV'
  elif (config == 'REUSE'):
    return 'R'
  elif (config == 'MANYCORE_PREFETCH'):
    return 'PF'
  elif (config == 'INIT_FRAMES=0'):
    return 'I0'
  elif (config[0:11] == 'VECTOR_LEN='):
    return a[11:len(a)]
  elif (config[0:3] == 'PF='):
    return a[3:len(a)]
  else:
    return config

def is_tril_vec_config(config):
  a = abbreviate_config(config)
  if (a == 'V4' or a == 'V16' or a == 'VEC_4_SIMD_VERTICAL' or a == 'VEC_16_SIMD_VERTICAL'):
    return True
  else:
    return False

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
def hw_opt_to_metadata(arg):
  out_str = ''
  for l in arg:
    # swap - and = with _
    if (l == '-' or l == '='):
      next_char = '_'
    else:
      next_char = l
    out_str += next_char
  return out_str

# turn hardware config command line into part of run annoation
# similar to strings_to_metadata() but first does some processing on format
def hw_opts_to_metadata(args):
  if (isinstance(args, list)):
    for i in range(0, len(args)):
      args[i] = hw_opt_to_metadata(args[i])
  else:
    args = hw_opt_to_metadata(args)

  return strings_to_metadata(args)

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
  return prog_key + '_' + strings_to_metadata(vec_config)


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
}




