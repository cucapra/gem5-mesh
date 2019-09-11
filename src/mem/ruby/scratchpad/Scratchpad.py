from m5.params import *
from m5.proxy import *
from Controller import RubyController

class Scratchpad(RubyController):
  type = 'Scratchpad'
  cxx_class = "Scratchpad"
  cxx_header = "mem/ruby/scratchpad/Scratchpad.hh"

  sp_size           = Param.Int(0, "Size of this scratchpad (in bytes)")
  dram_size         = Param.Int(0, "Size of DRAM (in bytes)")

  memReqBuffer      = Param.MessageBuffer("")
  memRespBuffer     = Param.MessageBuffer("")
  remoteReqBuffer   = Param.MessageBuffer("")
  remoteRespBuffer  = Param.MessageBuffer("")

  cpu_port          = SlavePort("cpu_port")

  num_l2s           = Param.Int(1, "Number of L2 banks")
  maxNumPendingReqs = Param.Int(8, "Max number of pending requests")
