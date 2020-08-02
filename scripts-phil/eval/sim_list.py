'''
  Sim configurations
'''

# helper set of vec configs you can use in a benchmark (assuming it supports)
ALL_CONFIGS = ['NO_VEC', 'VEC_4_SIMD']
ALL_NEIL_CONFIGS = ['NO_VEC', 'VEC_LEN=4']


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

  'bicg'   : {
    'vec'  : ALL_CONFIGS,
    'argv' : ['2048']
  },
  'gram'   : {
    'vec'  : ALL_CONFIGS,
    'argv' : ['320']
  },
  'syrk'   : {
    'vec'  : ALL_CONFIGS,
    'argv' : ['512']
  },
  'syr2k'  : {
    'vec'  : ALL_CONFIGS,
    'argv' : ['512']
  },
  'covar'   : {
    'vec'  : ALL_CONFIGS,
    'argv' : ['512']
  },
  'conv2d' : {
    'vec'  : ALL_CONFIGS + [ 'VEC_4_SIMD_VERTICAL', 'VEC_4_REUSE_VERTICAL' ],
    'argv' : ['2048']
  },
  'conv3d' : {
    'vec'  : ALL_CONFIGS,
    'argv' : ['256']
  },
  'fdtd' : {
    'vec'  : ALL_CONFIGS,
    'argv' : ['512', '60']
  },

  'atax'   : {
    'vec'  : ALL_NEIL_CONFIGS,
    'argv' : ['2048'] # ['128']
  },
  'mvt'    : {
    'vec'  : ALL_NEIL_CONFIGS,
    'argv' : ['4096'] # ['128']
  },
  'gemm'   : {
    'vec'  : ALL_NEIL_CONFIGS + [ [ 'NO_VEC', 'MANYCORE_PREFETCH' ] ],
    'argv' : ['256'] #['64']
  },
  'gesummv'   : {
    'vec'  : ALL_NEIL_CONFIGS,
    'argv' : ['4096'] #['128'] 
  },
  'corr'   : {
    'vec'  : ALL_NEIL_CONFIGS,
    'argv' : ['512'] #['64']
  },
  '2mm' : {
    'vec'  : ALL_NEIL_CONFIGS + [ [ 'NO_VEC', 'MANYCORE_PREFETCH' ] ],
    'argv' : ['256'] #['64']
  },
  '3mm' : {
    'vec'  : ALL_NEIL_CONFIGS + [ [ 'NO_VEC', 'MANYCORE_PREFETCH' ] ],
    'argv' : ['256'] #['32']
  },

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




