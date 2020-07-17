'''
  Sim configurations
'''

import os, subprocess, time, argparse, re, random

import multiprocessing

# cmd line arguments
parser = argparse.ArgumentParser(description='Run gem5 simulation and output informative stats files')
parser.add_argument('--build', default='../../build/RVSP/gem5.opt', help='Path to gem5 build')
parser.add_argument('--config', default='../../configs/phil/brg_hammerblade.py', help='Path to gem5 build')
parser.add_argument('--results', default='../../results', help='Path to place to store results')
args = parser.parse_args()

# specify programs. with the path to the program, the executible name, the default options, and string to check to see if successful
progDir0 = '../../programs-spad/'
programs = {

  'vvadd' : { 'name': 'vvadd', 'path' : progDir0 + 'vvadd/vvadd', 
    'options' : lambda argv: str(argv[0]), 
    'serialize' : lambda argv: '-size{0}'.format(str(argv[0])),
    'success' : '\[\[SUCCESS\]\]'},
    
  'gemm'  : { 'name': 'gemm',  'path' : progDir0 + 'gemm_neil/gemm', 
    'options' : lambda argv: '{0} {0} {0}'.format(str(argv[0])),
    'success' : '\[\[SUCCESS\]\]'},
    
  'synth' : { 'name': 'synth', 'path' : progDir0 + 'synth-diverge/synth', 
    'options' : lambda argv: '{0} {1} {2}'.format(str(argv[0]), str(argv[1]), str(argv[2])),
    'serialize' : lambda argv: '-size{0}-frac{1}'.format(str(argv[0]), str(argv[1])),
    'success' : '\[\[SUCCESS\]\]'},

  'stencil' : { 'name': 'stencil', 'path' : progDir0 + 'stencil/stencil', 
    'options' : lambda argv: str(argv[0]) + " " + str(argv[1]), 
    'serialize' : lambda argv: '-cols{0}-rows{1}'.format(str(argv[0]), str(argv[1])),
    'success' : '\[\[SUCCESS\]\]'},

}

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
  }
}