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
  grid_dim_x        = Param.Int(2, "XDim of core mesh")
  grid_dim_y        = Param.Int(2, "YDim of core mesh")
  netWidth          = Param.Int(1, "Number of words that can be sent over network")
  maxNumPendingReqs = Param.Int(8, "Max number of pending requests")
  
  prefetchBufSize   = Param.Int(2, "Allowed number of pending prefetches from master core")

  numFrameCntrs     = Param.Int(4, "Number of frames that can be counted at the same time")
  
  cpu               = Param.IOCPU("")
