'''
  Sim configurations
'''

# helper set of vec configs you can use in a benchmark (assuming it supports)
ALL_CONFIGS = ['NO_VEC', 'VEC_4_SIMD']


# choose which programs to run via script and with what configs
sim_configs = {
  # 'stencil': {
  #   'vec'  : ['VEC_4_SIMD'],
  #   'argv' : ['1730', '60']
  # },
  # 'bicg'   : {
  #   'vec'  : ALL_CONFIGS,
  #   'argv' : ['96']
  # },
  # 'gram'   : {
  #   'vec'  : ALL_CONFIGS,
  #   'argv' : ['16']
  # },
  'syrk'   : {
    'vec'  : ALL_CONFIGS,
    'argv' : ['32']
  },
  # 'syr2k'  : {
  #   'vec'  : ALL_CONFIGS,
  #   'argv' : ['32']
  # },
  # 'atax'   : { # TODO 16 CPUs?
  #   'vec'  : ALL_CONFIGS,
  #   'argv' : ['128']
  # },
  # 'mvt'    : {
  #   'vec'  : ALL_CONFIGS,
  #   'argv' : ['128']
  # },
  # 'gemm'   : {
  #   'vec'  : ALL_CONFIGS,
  #   'argv' : ['64']
  # },
  # 'corr'   : {
  #   'vec'  : ALL_CONFIGS,
  #   'argv' : ['64']
  # },
}

# make a shorthand to represent the config output name
def abbreviate_config(config):
  if (config == 'VEC_4_SIMD'):
    return 'V4'
  elif (config == 'VEC_16_SIMD'):
    return 'V16'
  elif (config == 'NO_VEC'):
    return 'NV'
  elif (config == 'REUSE'):
    return 'R'
  elif (config[0:11] == 'VECTOR_LEN='):
    return a[11:len(a)]
  elif (config[0:3] == 'PF='):
    return a[3:len(a)]
  else:
    return config


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
  'corr' : { 'name': 'corr', 'path' : progDir0 + 'corr/corr', 
    'options' : "{0} {0}",
    'serialize' : '-M{0}-N{0}',
  },
}




