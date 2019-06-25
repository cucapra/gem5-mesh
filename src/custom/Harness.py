from m5.params import *
from m5.proxy import *
from m5.objects.ClockedObject import ClockedObject

class Harness(ClockedObject):
    # association with other python and c++ stuff
    type = 'Harness'
    cxx_header = "custom/harness.hh"

    # Vector port example. Both the instruction and data ports connect to this
    # port which is automatically split out into two ports.
    from_cpu = VectorSlavePort("From CPU, receives requests")
    to_cpu = VectorMasterPort("To CPU, sends requests")

    system = Param.System(Parent.any, "The system this object is part of")
    
    # TODO might want to give a file path to load into a src sink tester
