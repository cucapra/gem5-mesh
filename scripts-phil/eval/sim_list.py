'''
  Sim configurations
'''

# possible vector config enumerators
# V_0      = ['no_vec']
# V_4      = ['vec_4']
# V_0_4    = ['no_vec', 'vec_4']
# V_0_4_16 = ['no_vec', 'vec_4', 'vec_16']

# V_S      = ['spatial']
# V_V      = ['vertical']
# V_S_V    = ['spatial', 'vertical']


# choose which programs to run via script and with what configs
sim_configs = {
  'stencil' : {
    'prog' : 'stencil',
    'vec'  : ["VEC_4_SIMD"],
    'argv' : ['1730', '60']
    # 'style': V_S,
    # 'pf'   : [4,16]
  },
  
}


# specify programs. with the path to the program, the executible name, the default options, and string to check to see if successful
progDir0 = '../../programs-spad/'
programs = {

  'vvadd' : { 'name': 'vvadd', 'path' : progDir0 + 'vvadd/vvadd', 
    'options' : lambda argv: str(argv[0]), 
    'serialize' : lambda argv: '-size{0}'.format(str(argv[0])),
    'success' : '\[\[SUCCESS\]\]'},
  'stencil' : { 'name': 'stencil', 'path' : progDir0 + 'stencil/stencil', 
    'options' : lambda argv: str(argv[0]) + " " + str(argv[1]), 
    'serialize' : lambda argv: '-cols{0}-rows{1}'.format(str(argv[0]), str(argv[1]))
  },
  'bicg' : { 'name': 'bicg', 'path' : progDir0 + 'bicg/bicg', 
    'options' : lambda argv: "{0} {0}".format(str(argv[0])),
    'serialize' : lambda argv: '-NX{0}-NY{0}'.format(str(argv[0])),
  },
  'gram' : { 'name': 'gram-schmidt', 'path' : progDir0 + 'gramschmidt/gramschmidt', 
    'options' : lambda argv: "{0} {0}".format(str(argv[0])), 
    'serialize' : lambda argv: '-vecs{0}-len{0}'.format(str(argv[0])),
  },
  'syrk' : { 'name': 'syrk', 'path' : progDir0 + 'syrk/syrk', 
    'options' : lambda argv: "{0} {0}".format(str(argv[0])), 
    'serialize' : lambda argv: '-N{0}-M{0}'.format(str(argv[0]))
  },
  'syr2k' : { 'name': 'syr2k', 'path' : progDir0 + 'syr2k/syr2k', 
    'options' : lambda argv: "{0} {0}".format(str(argv[0])), 
    'serialize' : lambda argv: '-N{0}-M{0}'.format(str(argv[0]))
  },
  'atax' : { 'name': 'atax', 'path' : progDir0 + 'atax/atax', 
    'options' : lambda argv: "{0} {0}".format(str(argv[0])), 
    'serialize' : lambda argv: '-NX{0}-NY{0}'.format(str(argv[0]))
  },
  'mvt' : { 'name': 'mvt', 'path' : progDir0 + 'mvt/mvt', 
    'options' : lambda argv: "{0}".format(str(argv[0])), 
    'serialize' : lambda argv: '-N{0}'.format(str(argv[0]))
  },
  'gemm' : { 'name': 'gemm', 'path' : progDir0 + 'gemm_final/gemm', 
    'options' : lambda argv: "{0} {0} {0}".format(str(argv[0])), 
    'serialize' : lambda argv: '-N{0}-M{0}-T{0}'.format(str(argv[0]))
  },
  'corr' : { 'name': 'corr', 'path' : progDir0 + 'corr/corr', 
    'options' : lambda argv: "{0} {0}".format(str(argv[0])), 
    'serialize' : lambda argv: '-M{0}-N{0}'.format(str(argv[0]))
  },

}

