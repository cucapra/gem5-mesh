#==============================================================================
# brg_scratchpad.py
#==============================================================================
# Python configuration file for BRG scratchpad system
#
# Author: Tuan Ta
# Date  : 19/07/09

import optparse
import sys
import os
import math

import m5
from m5.defines import buildEnv
from m5.objects import *
from m5.util import addToPath, fatal, warn

addToPath('../')

from ruby import Ruby
from topologies.Crossbar import *
from network.Network import *

from common import Options
from common import Simulation
from common import CacheConfig
from common import CpuConfig
from common import MemConfig
from common.Caches import *

from math import log

#------------------------------------------------------------------------------
# Get workload process
#------------------------------------------------------------------------------

def get_processes(options):
    multiprocesses = []
    inputs = []
    outputs = []
    errouts = []
    pargs = []

    workloads = options.cmd.split(';')
    if options.input != "":
        inputs = options.input.split(';')
    if options.output != "":
        outputs = options.output.split(';')
    if options.errout != "":
        errouts = options.errout.split(';')
    if options.options != "":
        pargs = options.options.split(';')

    idx = 0
    for wrkld in workloads:
        process = Process(pid = 100 + idx)
        process.executable = wrkld
        process.cwd = os.getcwd()

        if options.env:
            with open(options.env, 'r') as f:
                process.env = [line.rstrip() for line in f]

        if len(pargs) > idx:
            process.cmd = [wrkld] + pargs[idx].split()
        else:
            process.cmd = [wrkld]

        if len(inputs) > idx:
            process.input = inputs[idx]
        if len(outputs) > idx:
            process.output = outputs[idx]
        if len(errouts) > idx:
            process.errout = errouts[idx]

        multiprocesses.append(process)
        idx += 1

    return multiprocesses

#------------------------------------------------------------------------------
# Make network
#------------------------------------------------------------------------------
# This creates a mesh network n_rows x n_cols
# The first (n_rows - 1) rows are connected to either CPUs and/or xcels
# The last row is connected to L2 banks

def makeMeshTopology(n_rows, n_cols, system, network,
                     IntLink, ExtLink, Router):
  assert(n_rows >= 2)
  assert(n_cols >= 1)

  num_routers = n_rows * n_cols;

  link_latency = 1
  router_latency = 1

  n_cpus = len(system.cpu)
  n_xcels = len(system.xcel)

  # all controllers
  icaches   = system.icaches
  cpu_sps   = system.scratchpads[:n_cpus]
  xcel_sps  = system.scratchpads[n_cpus:]
  l2s       = system.l2_cntrls

  assert(len(icaches) == n_cpus)
  assert(len(cpu_sps) == n_cpus)
  assert(n_cpus <= num_routers)
  assert(n_xcels <= num_routers)
  assert(len(l2s) <= n_cols)

  # create all routers
  routers = [ Router(router_id = i,
                     latency = router_latency) for i in range(num_routers) ]
  network.routers = routers

  # link count used to set unique link IDs
  link_count = 0

  #--------------------------
  # Set up all external links
  #--------------------------

  ext_links = []

  # add all CPU I-caches and SPs to the first few routers
  for i in xrange(n_cpus):
    icache_ext_link = ExtLink(link_id   = link_count,
                              ext_node  = icaches[i],
                              int_node  = routers[i],
                              latency   = link_latency)
    link_count += 1
    ext_links.append(icache_ext_link)

    cpu_sp_ext_link = ExtLink(link_id   = link_count,
                              ext_node  = cpu_sps[i],
                              int_node  = routers[i],
                              latency   = link_latency)
    link_count += 1
    ext_links.append(cpu_sp_ext_link)

  # add all Xcel SPs to all routers
  for i in xrange(n_xcels):
    xcel_ext_link = ExtLink(link_id   = link_count,
                            ext_node  = xcel_sps[i],
                            int_node  = routers[i],
                            latency   = link_latency)
    link_count += 1
    ext_links.append(xcel_ext_link)

  # add all l2s to bottom routers
  l2_idx = 0
  for i in xrange(n_cols * (n_rows - 1), num_routers):
    if l2_idx < len(l2s):
      l2_ext_link = ExtLink(link_id   = link_count,
                            ext_node  = l2s[l2_idx],
                            int_node  = routers[i],
                            latency   = link_latency)
      l2_idx += 1
      link_count += 1
      ext_links.append(l2_ext_link)

  network.ext_links = ext_links

  #--------------------------
  # Set up all internal links
  #--------------------------

  int_links = []

  # East output to West input links (weight = 1)
  for row in xrange(n_rows):
    for col in xrange(n_cols):
      if (col + 1 < n_cols):
        east_out  = col + (row * n_cols)
        west_in   = (col + 1) + (row * n_cols)
        int_links.append(IntLink(link_id      = link_count,
                                 src_node     = routers[east_out],
                                 dst_node     = routers[west_in],
                                 src_outport  = "East",
                                 dst_inport   = "West",
                                 latency      = link_latency,
                                 weight       = 1 ))
        link_count += 1

  # West output to East input links (weight = 1)
  for row in xrange(n_rows):
    for col in xrange(n_cols):
      if (col + 1 < n_cols):
        east_in  = col + (row * n_cols)
        west_out = (col + 1) + (row * n_cols)
        int_links.append(IntLink(link_id      = link_count,
                                 src_node     = routers[west_out],
                                 dst_node     = routers[east_in],
                                 src_outport  = "West",
                                 dst_inport   = "East",
                                 latency      = link_latency,
                                 weight       = 1 ))
        link_count += 1

  # North output to South input links (weight = 2)
  for col in xrange(n_cols):
    for row in xrange(n_rows):
      if (row + 1 < n_rows):
        north_out = col + (row * n_cols)
        south_in = col + ((row + 1) * n_cols)
        int_links.append(IntLink(link_id      = link_count,
                                 src_node     = routers[north_out],
                                 dst_node     = routers[south_in],
                                 src_outport  = "North",
                                 dst_inport   = "South",
                                 latency      = link_latency,
                                 weight       = 2 ))
        link_count += 1

  # South output to North input links (weight = 2)
  for col in xrange(n_cols):
    for row in xrange(n_rows):
      if (row + 1 < n_rows):
        north_in  = col + (row * n_cols)
        south_out = col + ((row + 1) * n_cols)
        int_links.append(IntLink(link_id      = link_count,
                                 src_node     = routers[south_out],
                                 dst_node     = routers[north_in],
                                 src_outport  = "South",
                                 dst_inport   = "North",
                                 latency      = link_latency,
                                 weight       = 2 ))
        link_count += 1

  network.int_links = int_links

#------------------------------------------------------------------------------
# Parse options
#------------------------------------------------------------------------------

parser = optparse.OptionParser()
Options.addCommonOptions(parser)
Options.addSEOptions(parser)
Options.addBRGOptions(parser)
Ruby.define_options(parser)

parser.add_option("--num-xcels", type = "int", default = 16)
parser.add_option("--stream-width", type = "int", default = 8)
parser.add_option("--xcel", type = "string", default = "")

(options, args) = parser.parse_args()

n_cpus  = options.num_cpus
n_xcels = options.num_xcels

n_cols  = int(math.sqrt(n_xcels))
n_rows  = n_cols + 1

n_l2s   = n_cols

# network classes
assert(options.network == "garnet2.0")
NetworkClass = GarnetNetwork
IntLinkClass = GarnetIntLink
ExtLinkClass = GarnetExtLink
RouterClass = GarnetRouter
InterfaceClass = GarnetNetworkInterface

# xcel classes
if options.xcel == "TensorSumXcel":
  XcelClass = TensorSumXcel
elif options.xcel == "EmbeddingXcel":
  XcelClass = EmbeddingXcel
elif options.xcel == "EmbeddingBwXcel":
  XcelClass = EmbeddingBwXcel
else:
  assert(False)

# Do not support multi-process simulation
process = get_processes(options)[0]

#------------------------------------------------------------------------------
# Construct CPUs
#------------------------------------------------------------------------------

# CPU class
CPUClass = TimingSimpleCPU

# Create top-level system
system = System(cpu = [ CPUClass(cpu_id = i) for i in xrange(n_cpus) ],
                        mem_mode = CPUClass.memory_mode(),
                        mem_ranges = [ AddrRange(options.mem_size) ],
                        cache_line_size = 32) #options.cacheline_size)

# Create a top-level voltage domain
system.voltage_domain = VoltageDomain(voltage = options.sys_voltage)

# Create a source clock for the system and set the clock period
system.clk_domain = SrcClockDomain(clock =  options.sys_clock,
                                   voltage_domain = system.voltage_domain)

# Create a CPU voltage domain
system.cpu_voltage_domain = VoltageDomain()

# Create a separate clock domain for the CPUs
system.cpu_clk_domain = SrcClockDomain(clock = options.sys_clock,
                                       voltage_domain =
                                       system.cpu_voltage_domain)

# All cpus belong to a common cpu_clk_domain, therefore running at a common
# frequency.
for cpu in system.cpu:
    cpu.clk_domain = system.cpu_clk_domain

# Assign workload to CPUs
for i in xrange(n_cpus):
  system.cpu[i].workload = process
  system.cpu[i].createThreads()

#------------------------------------------------------------------------------
# Construct Xcels
#------------------------------------------------------------------------------

system.xcel = [ XcelClass(cpu_process = process, \
                          stream_width = options.stream_width) \
                for i in xrange(n_xcels) ]

#------------------------------------------------------------------------------
# Construct Ruby memory system
#------------------------------------------------------------------------------

system.ruby = RubySystem()

# Construct network
network = NetworkClass (ruby_system = system.ruby,
                        routers = [],
                        ext_links = [],
                        int_links = [],
                        netifs = [],
                        number_of_virtual_networks = 2)

# Scratchpads
n_scratchpads = n_cpus + n_xcels
scratchpads = []

for i in xrange(n_scratchpads):
  sp = Scratchpad(version           = i,
                  ruby_system       = system.ruby,
                  sp_size           = AddrRange(options.spm_size).size(),
                  dram_size         = AddrRange(options.mem_size).size(),
                  num_l2s           = n_l2s,
                  maxNumPendingReqs = options.stream_width)

  sp.memReqBuffer             = MessageBuffer(ordered = True)
  sp.memReqBuffer.master      = network.slave

  sp.memRespBuffer            = MessageBuffer(ordered = True)
  sp.memRespBuffer.slave      = network.master

  sp.remoteReqBuffer          = MessageBuffer(ordered = True)
  sp.remoteReqBuffer.slave    = network.master

  sp.remoteRespBuffer         = MessageBuffer(ordered = True)
  sp.remoteRespBuffer.master  = network.slave

  scratchpads.append(sp)

system.scratchpads = scratchpads

# L1-I caches and sequencers
n_icaches = n_cpus
icache_cntrls = []
sequencers = []
for i in xrange(n_icaches):
  icache = RubyCache(size = '32kB', assoc = 2)
  icache_cntrl = L1Cache_Controller(version = i,
                                    L1cache = icache,
                                    transitions_per_cycle = options.ports,
                                    ruby_system = system.ruby)

  icache_cntrl.requestToNetwork          = MessageBuffer(ordered = True)
  icache_cntrl.requestToNetwork.master   = network.slave
  icache_cntrl.responseFromNetwork       = MessageBuffer(ordered = True)
  icache_cntrl.responseFromNetwork.slave = network.master
  icache_cntrl.mandatoryQueue            = MessageBuffer(ordered = True)

  sequencer = RubySequencer()
  sequencer.version = i
  sequencer.icache = icache
  sequencer.dcache = icache
  sequencer.ruby_system = system.ruby
  sequencer.is_cpu_sequencer = True

  icache_cntrl.sequencer = sequencer

  icache_cntrls.append(icache_cntrl)
  sequencers.append(sequencer)

system.icaches = icache_cntrls
system.ruby.num_of_sequencers = len(sequencers)
system.ruby.number_of_virtual_networks = 2

# L2 cache
l2_cntrls = []

if n_l2s == 1:
  l2_size = '256kB'
elif n_l2s == 2:
  l2_size = '128kB'
elif n_l2s == 4:
  l2_size = '64kB'
elif n_l2s == 8:
  l2_size = '32kB'
else:
  fatal("Invalid number of L2 banks")

for i in xrange(n_l2s):
  l2_cache = RubyCache(size = l2_size, assoc = 4)
  l2_cntrl = L2Cache_Controller(version = i,
                                cacheMemory = l2_cache,
                                transitions_per_cycle = options.ports,
                                ruby_system = system.ruby)

  l2_cntrl.requestToLLC           = MessageBuffer(ordered = True)
  l2_cntrl.requestToLLC.slave     = network.master

  l2_cntrl.responseFromLLC        = MessageBuffer(ordered = True)
  l2_cntrl.responseFromLLC.master = network.slave

  l2_cntrl.responseFromMemory     = MessageBuffer(ordered = True)

  l2_cntrls.append(l2_cntrl)

system.l2_cntrls = l2_cntrls

#------------------------------------------------------------------------------
# Connect all controllers to network
#------------------------------------------------------------------------------

#all_cntrls = system.icaches + system.scratchpads + system.l2_cntrls
#topology = Crossbar(all_cntrls)
#topology.makeTopology(options, network,
#                      IntLinkClass, ExtLinkClass, RouterClass)

makeMeshTopology(n_rows, n_cols, system, network,
                 IntLinkClass, ExtLinkClass, RouterClass)

init_network(options, network, InterfaceClass)

system.network = network

system.ruby.sys_port_proxy = RubyPortProxy(ruby_system = system.ruby)
system.system_port = system.ruby.sys_port_proxy.slave

#------------------------------------------------------------------------------
# Construct memory controller
#------------------------------------------------------------------------------

system.mem_mode = 'timing'
system.mem_ranges = [ AddrRange(options.mem_size) ]

system.mem_ctrl = SimpleMemory()
system.mem_ctrl.range = system.mem_ranges[0]
system.mem_ctrl.latency = '60ns'
system.mem_ctrl.bandwidth = '16GB/s'

#------------------------------------------------------------------------------
# Construct a crossbar that connects L2s and mem_ctrl
#------------------------------------------------------------------------------

system.l2_bus = NoncoherentXBar()

# 16 bytes per cycle. This is set to match with the mem_ctrl.bandwidth
system.l2_bus.width = 16
system.l2_bus.frontend_latency = 1
system.l2_bus.forward_latency = 1
system.l2_bus.response_latency = 1
system.l2_bus.clk_domain = system.clk_domain

for i in xrange(n_l2s):
  system.l2_bus.slave = system.l2_cntrls[i].memory

system.l2_bus.master = system.mem_ctrl.port

#------------------------------------------------------------------------------
# Connect memory controller and CPUs to the Ruby system
#------------------------------------------------------------------------------

#system.mem_ctrl.port = l2_cntrl.memory

for i in xrange(n_cpus):
  system.cpu[i].createInterruptController()
  system.cpu[i].icache_port = sequencers[i].slave
  system.cpu[i].dcache_port = system.scratchpads[i].cpu_port

for i in xrange(n_xcels):
  system.xcel[i].mem_port = system.scratchpads[n_cpus + i].cpu_port

#------------------------------------------------------------------------------
# Simulation
#------------------------------------------------------------------------------

# set up the root SimObject and start the simulation
root = Root(full_system = False, system = system)

# instantiate all of the objects we've created above
m5.instantiate()

print("Beginning simulation!")
exit_event = m5.simulate()
print('Exiting @ tick %i because %s' % (m5.curTick(), exit_event.getCause()))
print('Exit code %i' % exit_event.getCode())
