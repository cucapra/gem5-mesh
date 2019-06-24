# automate https://nitish2112.github.io/post/adding-instruction-riscv/
# to add or remove an instruction to the assembler in riscv that we can use
# in a riscv gem5 simulation

# specify instruction type (from prexisting types for now)
# specify fields of instruction (op-codes / immediates)
# specify gem5 functionality (.isa code)

# give location of riscv-gnu-toolchain, riscv-tools, and gem5 and should
# recompile all with the changes

import os, re, argparse
from enum import Enum
from functools import cmp_to_key
# https://stackoverflow.com/questions/2358045/how-can-i-implement-a-tree-in-python-are-there-any-built-in-data-structures-in
from anytree import Node, RenderTree, PreOrderIter


########################################################################
# Command line parsing
########################################################################

parser = argparse.ArgumentParser(description='Add an instruction to riscv-gcc and sim it')

parser.add_argument('--gem5-dir', 
                    #default='./gem5',
                    default='/home/pbb59/hammerblade/gem5',
                    help='Path to gem5 src')

args = parser.parse_args()

########################################################################
# Important path and file names
########################################################################

gem5_rv_decoder_path = args.gem5_dir + '/src/arch/riscv/isa'
gem5_rv_decoder_file = 'decoder.isa'


########################################################################
# Create internal representation of instructions
########################################################################

# special label (FieldType) that will be used to identify certain fields
class FT(Enum):
  funct7  = 1
  rd      = 2
  rs1     = 3
  rs2     = 4
  rs3     = 5
  funct3  = 6
  opcode  = 7
  imm     = 8
  
# instruction formats we know how to parse
class INST_FORM(Enum):
  r = 1
  i = 2           


class BitField:
  # constructor
  def __init__(self, label, end, begin):
    # special label that will be used to identify certain fields
    self.label      = label
    # bit range
    self.bit_start  = begin
    self.bit_end    = end

  def try_convert(val):
    try:
      return int(val)
    except:
      try:
        return int(val, 16)
      except:
        return val
    

  # overload for how to request information for this field
  def req_input(self):
    if (self.label == FT.opcode):
      self.fixed_val = BitField.try_convert(input('Op code (inst) : '))
    elif (self.label == FT.funct7):
      self.fixed_val = BitField.try_convert(input('Op code (funct7) : '))
    elif (self.label == FT.funct3):
      self.fixed_val = BitField.try_convert(input('Op code (funct3) : '))
    else:
      pass
      
  '''
  # give the flag that it exists as well as to tell if the right one
  def gem5_regex(self):
    if (self.label == FT.opcode):
      return [
        # regex to match, replace if doesn't exist, value to look for
        ( '(decode QUADRANT default Unknown::unknown\(\) {)', '', self.fixed_val )
        ]
    elif (self.label == FT.funct3):
      return [
        ( '(decode FUNCT3 {)', 'decode FUNCT3 {\n', self.fixed_val )
        ] 
    elif (self.label == FT.funct7):
      return [
        ( '(decode FUNCT7 {)', 'decode FUNCT7 {\n', self.fixed_val )
        ]
    else:
      return []
  '''
      
  def __len__(self):
    return self.bit_end - self.bit_start + 1


class Inst:
  def __init__(self, inst_form, bitfields):
    self.inst_form = inst_form
    self.bitfields = bitfields
  
  # get the requested input from everyone
  def req_inputs(self):
    self.inst_name = input('Inst name ? : ')
    
    for bf in self.bitfields:
      bf.req_input()
    
  def find_in_bitfields(self, field_type):
    for bf in self.bitfields:
      if (bf.label == field_type):
        if (hasattr(bf, 'fixed_val')):
          return bf.fixed_val
          
    return -1
  '''
  (0x3, 1, 'decode OPCODE {', '}'), 
      (0x1a, 2, 'decode FUNCT3 {', '}'), # need to remove first two bits
      ('format ROp', 3, '{', '}'),
      (0x0, 4, 'decode FUNCT7 {', '}'),
      (0x1, 5, 'mod({{', 'Rd = Rs1_sd % Rs2_sd;\n}});')
  '''
  # get the nesting, this function puts the individual expectations
  # in the correct order
  def gem5_decoder_insertion(self):
    # create the search list
    # needs have first 2bits as 0x3, can't use .insn assembler hack otherwise
    ins_strs = [(0x3, 1, 'decode OPCODE {', '}')]
    
    opcode = self.find_in_bitfields(FT.opcode)
    # don't include the first 2 bits in opcode
    opcode >>= 2
    
    # find the desired fields for the type
    if (self.inst_form == INST_FORM.r):
      funct3 = self.find_in_bitfields(FT.funct3)
      funct7 = self.find_in_bitfields(FT.funct7)
      
      ins_strs.append((opcode, 2, 'decode FUNCT3 {', '}'))
      ins_strs.append(('format ROp', 3, '{', '}'))
      ins_strs.append((funct3, 4, 'decode FUNCT7 {', '}'))
      ins_strs.append((funct7, 5, 'mod({{', 'Rd = Rs1_sd % Rs2_sd;\n}});'))
    
    return ins_strs
  

  def gen_c_macro(self):
    inst_name = self.inst_name
    opcode    = self.find_in_bitfields(FT.opcode)
    
    
    macro_str = '#define __' + inst_name + '(' 
    
    # TODO only support r type parsing right now
    if (self.inst_form == INST_FORM.r):
      funct3    = self.find_in_bitfields(FT.funct3)
      funct7    = self.find_in_bitfields(FT.funct7)
      
      macro_str += 'c, a, b)                                  \\\n'
      macro_str += 'asm volatile                              \\\n'
      macro_str += '(                                         \\\n'
      macro_str += '  \".insn r ' + hex(opcode) + ', ' + str(funct3) + ', ' + str(funct7) + ', %[z], %[x], %[y]\\n\\t\" \\\n'
      macro_str += '  : [z] "=r" (c)                          \\\n'
      macro_str += '  : [x] "r" (a), [y] "r" (b)              \\\n'
      macro_str += ')'
      
    return macro_str
 
 

# see https://content.riscv.org/wp-content/uploads/2017/05/riscv-spec-v2.2.pdf
rv_formats = {
  'R' : Inst ( INST_FORM.r ,
        [ BitField( FT.funct7, 31, 25 ) , BitField( FT.rs2, 24, 20 ) , 
          BitField( FT.rs1, 19, 15 ) , BitField( FT.funct3, 14, 12 ) ,
          BitField( FT.rd, 11, 7 ) , BitField( FT.opcode, 6, 0) ] 
        ), 
  'I' : Inst ( INST_FORM.i ,
        [ BitField( FT.imm, 31, 20 ), BitField( FT.rs1, 19, 15 ) , 
          BitField( FT.funct3, 14, 12 ), BitField( FT.rd, 11 , 7 ) ,
          BitField( FT.opcode, 6, 0 ) ]
        )
}

########################################################################
# FILE IO
########################################################################



# goal is to read the whole file in, regex where the important struct starts
# add/remove a line within that structure
# write all of the lines of the file out in a similar order

class FileInsert:
  def __init__(self, path, file_name):
    self.path = path
    self.file_name = file_name
    
  def full_path(self):
    return self.path + '/' + self.file_name
    
  def temp_path(self):
    return './' + self.file_name
    
  # parse decoder isa into a tree!
  def parse_isa_tree(self):
    # read the whole file a into a list of lines
    # since line ordering is consisent can cheat a bit on white space issues
    with open(self.full_path()) as fin:
      # read all of the lines in the file
      lines = []
      for line in fin:
        lines.append(line)
    
    new_node_regex = [ 
      re.compile('(0([xX])([0-9a-fA-F]+): ([0-9a-zA-Z :_{\(\)]+))'),
      re.compile('(format ([0-9a-zA-Z :_{]+))'),
      re.compile('(default: ([0-9a-zA-Z :_{\(]+))')
    ]
    #leaf_node_regex = re.compile('([\({{]+)')
    any_string_regex = re.compile('([0-9a-zA-Z :_{}\(\)<>;\/\"\",|&=\-~.!\+\?^*\[\]%]+)')
    root_node_str = 'decode QUADRANT default Unknown::unknown() {'
    root_node_regex = re.compile('(decode QUADRANT default Unknown::unknown\(\) {)')
    
    # keep track of the current nestign level
    nest_level = 0
    nest_regex = re.compile('({)')
    unnest_regex = re.compile('(})')
    paren_level = 0
    paren_regex = re.compile('(\()')
    unparen_regex = re.compile('(\))')
    
    curr_parent = Node('temp', contents=[], nest_level=0)
    
    for i in range(len(lines)):
      line = lines[i]
      # do all regex matching for this line
      
      # check what kind of line this is
      nnr = []
      for nn_regex in new_node_regex:
        nnr.append(nn_regex.search(line))
      
      #lnr = leaf_node_regex.search(line)
      asr = any_string_regex.search(line)
      rnr = root_node_regex.search(line)
      
      # compute the next nesting level
      nest_matches = re.findall(nest_regex, line)
      unnest_matches = re.findall(unnest_regex, line)
      paren_matches = re.findall(paren_regex, line)
      unparen_matches = re.findall(unparen_regex, line)
      
      # save the root node
      if (rnr):
        self.root_node = Node('root', label=root_node_str, nest_level=nest_level, contents=[])
        curr_parent = self.root_node
      
      # we detected a new node
      # create it in the tree and set it to the new parent
      elif (nnr[0] or nnr[1] or nnr[2]):
        if (nnr[0]):
          node_value = int('0x' + nnr[0].group(3), 16)
          node_label = nnr[0].group(4)
          new_node = Node(str(i), parent=curr_parent, value=node_value, label=node_label, contents=[], nest_level=nest_level)

        else:
          if (nnr[1]):
            nnr_ok = nnr[1]
          else:
            nnr_ok = nnr[2]
          node_label = nnr_ok.group(0)
          new_node = Node(str(i), parent=curr_parent, label=node_label, contents=[], nest_level=nest_level)

        curr_parent = new_node
      
      # otherwise lets appending to the current node/parent
      elif (asr):
        str_match = asr.group(0)
        curr_parent.contents.append(str_match)
        
      # update the nest level
      nest_level += len(nest_matches)
      nest_level -= len(unnest_matches)
      
      paren_level += len(paren_matches)
      paren_level -= len(unparen_matches)
      
      # we evaluated all nesting within the current parent node so we should go up a level
      while ((nest_level <= curr_parent.nest_level) and (paren_level == 0) and (curr_parent.parent is not None)):
        curr_parent = curr_parent.parent
  
  def emit_node(self, node, fout):
    # get the number of spaces based on the nest level (x4)
    nest_spaces = ''
    for i in range(node.nest_level * 4):
      nest_spaces += ' '
        
    # print the header line
    new_line = nest_spaces
    if (hasattr(node, 'value')):
      new_line += hex(node.value) + ': '
    new_line += node.label
        
    fout.write(new_line + '\n')
        
    # do recursion into other nodes
    for child_node in node.children:
      self.emit_node(child_node, fout)
        
    # print all of the content lines afterwards (for leaf nodes?)
    for content_line in node.contents:
      fout.write(content_line + '\n')
     
  # output the tree!
  def emit_isa_tree(self):
    with open(self.temp_path(), 'w') as fout:
      #node.is_leaf <label> {(( <contents> ... else <label> { <contents>
      self.emit_node(self.root_node, fout)
    
    # copy to gem5 directory
    os.system('cp ' + self.temp_path() + ' ' + self.full_path())
  
  
  def is_node_match(self, node, desired_val):
    if (isinstance(desired_val, str)):
      if (hasattr(node, 'label')):
        regex = re.compile(desired_val)
        match = regex.search(node.label)
        if (match):
          #print ('found str ' + node.label)
          return True
            
    # handle hex val
    else:
      if (hasattr(node, 'value')):
        if (desired_val == node.value):
          #print ('found ' + hex(desired_val))
          return True
          
    return False
  
  # ret whether found or inserted
  def add_in_children(self, node, desired_tuple):
    # see if we can find the next options
    desired_val, desired_nest, ins_str_pre, ins_str_post = desired_tuple[0]
    
    found_next = False
    
    for child_node in node.children:
      if(self.add_to_tree_level(child_node, desired_tuple)):
        found_next = True
        
    # if there are unfound nodes in all children then we need to add here
    if ((not found_next) and (desired_nest == node.nest_level + 1)):
      print('adding ' + str(desired_val))
      if (isinstance(desired_val, str)):
        new_node = Node('ins' + str(desired_nest) + str(desired_val), 
          parent=node, label=(desired_val + ' ' + ins_str_pre), 
          contents=[ins_str_post], nest_level=desired_nest)
      else:
        new_node = Node('ins' + str(desired_nest) + str(desired_val), 
          parent=node, value=desired_val, label=ins_str_pre, 
          contents=[ins_str_post], nest_level=desired_nest)
        
      # try to traverse this new one
      if (len(desired_tuple) > 1):
        self.add_to_tree_level(new_node, desired_tuple[1:])
    
    return True
      
  # ret whether found
  def add_to_tree_level(self, node, desired_tuple):
    #print ('tuple size: ' + str(len(desired_tuple)))
    
    desired_val, desired_nest, ins_str_pre, ins_str_post = desired_tuple[0]
    
    
    
    # skip if we're not at the right level
    if (desired_nest > node.nest_level):
      #print('pass it')
      return self.add_in_children(node, desired_tuple)
        
    # try to match the level
    elif (desired_nest == node.nest_level):
      # if found try to find the next level with children
      if (self.is_node_match(node, desired_val)):
        #print ('***** tuple size: ' + str(len(desired_tuple)))
        if (len(desired_tuple) > 1):
          #print('call it')
          self.add_in_children(node, desired_tuple[1:])
        return True
      else:
        return False
          
    else:
      return False

  
  # insert an instruction signature into the tree
  def add_to_isa_tree(self, new_inst):
    desired_tuple_list = new_inst.gem5_decoder_insertion()
    self.add_to_tree_level(self.root_node, desired_tuple_list)
  
  def rmv_from_lvl(self, node, regex):
    num_child_pre = len(node.children)
    
    for child_node in node.children:
      self.rmv_from_lvl(child_node, regex)
      
    match = regex.search(node.label)
    # remove these mode by severing ties with children :(
    if (match):
      print ('found del')
      for child_node in node.children:
        child_node.parent = None
      node.parent = None
     
    
    if ((len(node.children) == 0) and num_child_pre > 0):
      node.parent = None
    
          
  
  def del_from_isa_tree(self, rmv_name):
    # search for a node and delete it from the tree if it exists!
    regex = re.compile('(' + rmv_name + ')')
    self.rmv_from_lvl(self.root_node, regex)
    
    
  '''
  #define MOD(c, a, b)                        \
  asm volatile                              \
  (                                         \
    ".insn r 0x6b, 0, 1, %[z], %[x], %[y]\n\t" \
    : [z] "=r" (c)                          \
    : [x] "r" (a), [y] "r" (b)              \
  )
  '''
    
  # generate macro we can call in compile riscv code
  def gen_c_macro(self, inst):
    macro_str = inst.gen_c_macro()
    
    with open('macro_file.c', 'w') as fout:
      fout.write(macro_str)
      

########################################################################
# Interactive shell to add or remove an instruction
########################################################################

def add_instruction():
  # find which format we are dealing with to know which fields are relevant
  avail_formats = ''
  for key, value in rv_formats.items():
    avail_formats += key + ' '
  chosen_format = input('Enter an instruction format ( ' + avail_formats + '): ')

  # go through each relevant field and ask about what value the field should be
  inst = rv_formats[chosen_format]
  inst.req_inputs()

  # print out .insn macros for the instruction

  '''
  fio = FileInsert(toolchain_opcode_path, toolchain_opcode_file)
  regex_strs_and_insert = [
  # regex --- what to replace if not found --- should cont after replacement
    ( '(const struct riscv_opcode riscv_opcodes\[\] =)', '', False ),
    ( '({)' , '', False ),
    ( '(' + inst_str + ')', inst_str, True ),
  ]
  fio.insert_after_regex(regex_strs_and_insert)
  '''
  
  # insert the op and its functionality into gem5
  fio = FileInsert(gem5_rv_decoder_path, gem5_rv_decoder_file)
  '''
  # need to match two in a row
  regex_strs = inst.gem5_decoder_regex()
  # TODO on this
  inst_lambda = 'mod' + '({{\n' + 'Rd = Rs1_sd % Rs2_sd;\n' + '}});\n'
  regex_strs.append(('(' + inst_lambda + ')', inst_lambda, -1))
  fio.insert_after_regex(regex_strs)
  '''
  
  fio.parse_isa_tree()
  fio.add_to_isa_tree(inst)
  fio.emit_isa_tree()
  
  fio.gen_c_macro(inst)
  

def remove_instruction():
  inst_to_rm = input('Enter the name of the instruction you want to remove : ')
  fio = FileInsert(gem5_rv_decoder_path, gem5_rv_decoder_file)
  fio.parse_isa_tree()
  fio.del_from_isa_tree(inst_to_rm)
  fio.emit_isa_tree()


########################################################################
# Main
########################################################################


add_rmv = input('Would you like to \"add\" or \"remove\" an instruction? : ')

if (add_rmv == 'add'):
  add_instruction()
elif (add_rmv == 'remove'):
  remove_instruction()
else:
  print('invalid arg')


'''
# insert the op and its functionality into gem5
fio = FileInsert(gem5_rv_decoder_path, gem5_rv_decoder_file)
  
fio.parse_isa_tree()
fio.add_to_isa_tree(Inst([]))
#fio.del_from_isa_tree('logic1')
fio.emit_isa_tree()


fio.gen_c_macro(Inst([]))
'''





