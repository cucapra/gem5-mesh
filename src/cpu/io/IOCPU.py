# Author: Tuan Ta
# Date:   19/08/29

from __future__ import print_function

from m5.defines import buildEnv
from m5.params import *
from m5.proxy import *
from m5.SimObject import SimObject
from BaseCPU import BaseCPU
from BranchPredictor import *

class IOCPU(BaseCPU):
  type = 'IOCPU'
  cxx_header = 'cpu/io/cpu.hh'

  @classmethod
  def memory_mode(cls):
    return 'timing'

  @classmethod
  def require_caches(cls):
    return True

  @classmethod
  def support_take_over(cls):
    return True

  # Number of physical registers
  numPhysIntRegs = Param.Unsigned(48, "Number of physical integer registers")
  numPhysFloatRegs = Param.Unsigned(48, "Number of physical floating point "
                                    "registers")
  _defaultNumPhysCCRegs = 0
  if buildEnv['TARGET_ISA'] in ('arm','x86'):
      # For x86, each CC reg is used to hold only a subset of the
      # flags, so we need 4-5 times the number of CC regs as
      # physical integer regs to be sure we don't run out.  In
      # typical real machines, CC regs are not explicitly renamed
      # (it's a side effect of int reg renaming), so they should
      # never be the bottleneck here.
      _defaultNumPhysCCRegs = Self.numPhysIntRegs * 5

  numPhysVecRegs = Param.Unsigned(256, "Number of physical vector "
                                    "registers")
  numPhysVecPredRegs = Param.Unsigned(32, "Number of physical predicate "
                                    "registers")
  numPhysCCRegs = Param.Unsigned(_defaultNumPhysCCRegs,
                                   "Number of physical cc registers")

  # Cache ports
  numIcachePorts = Param.Unsigned(1, "Number of I-cache ports")
  numDcachePorts = Param.Unsigned(1, "Number of D-cache ports")

  # Stage width
  fetchWidth     = Param.Unsigned(1, "Fetch width")
  decodeWidth    = Param.Unsigned(1, "Decode width")
  renameWidth    = Param.Unsigned(1, "Rename width")
  issueWidth     = Param.Unsigned(1, "Issue width")
  writebackWidth = Param.Unsigned(1, "Writeback width")
  commitWidth    = Param.Unsigned(1, "Commit width")

  # Sizes of input buffers in different stages
  decodeBufferSize = Param.Unsigned(2, "Size of decode's input buffer")
  renameBufferSize = Param.Unsigned(2, "Size of rename's input buffer")
  iewBufferSize    = Param.Unsigned(2, "Size of iew's input buffer")
  commitBufferSize = Param.Unsigned(2, "Size of commit's input buffer")

  # Branch predictor
  branchPred = Param.BranchPredictor(
                                TournamentBP(numThreads = Parent.numThreads),
                                "Branch Predictor")

  # ROB size
  numROBEntries = Param.Unsigned(8, "Number of reorder buffer entries")

  # Exec unit latencies
  intAluOpLatency = Param.Unsigned(1, "");
  intMulOpLatency = Param.Unsigned(3, "");
  divOpLatency    = Param.Unsigned(10, "");
  fpAluOpLatency  = Param.Unsigned(3, "");
  fpMulOpLatency  = Param.Unsigned(5, "");

  # Memory unit params
  numLoadQueueEntries   = Param.Unsigned(2, "");
  numStoreQueueEntries  = Param.Unsigned(2, "");
