from m5.params import *
from m5.proxy import *
from Controller import RubyController

class Forwarder(RubyController):
  type = 'Forwarder'
  cxx_class = "Forwarder"
  cxx_header = "mem/ruby/scratchpad/Forwarder.hh"

  netResponseBuffer   = Param.MessageBuffer("")
  cacheForwardBuffer  = Param.MessageBuffer("")