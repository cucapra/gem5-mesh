/*
 * Copyright 2014 Google, Inc.
 * Copyright (c) 2010-2013,2015,2017 ARM Limited
 * All rights reserved
 *
 * The license below extends only to copyright in the software and shall
 * not be construed as granting a license to any other intellectual
 * property including but not limited to intellectual property relating
 * to a hardware implementation of the functionality of the software
 * licensed hereunder.  You may use the software subject to the license
 * terms below provided that you ensure that this notice is replicated
 * unmodified and in its entirety in all distributions of the software,
 * modified or unmodified, in source code or in binary form.
 *
 * Copyright (c) 2002-2005 The Regents of The University of Michigan
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met: redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer;
 * redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution;
 * neither the name of the copyright holders nor the names of its
 * contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * Authors: Steve Reinhardt
 */

#include "cpu/simple/timing.hh"

#include "arch/locked_mem.hh"
#include "arch/mmapped_ipr.hh"
#include "arch/utility.hh"
#include "config/the_isa.hh"
#include "cpu/exetrace.hh"
#include "debug/Config.hh"
#include "debug/Drain.hh"
#include "debug/ExecFaulting.hh"
#include "debug/Mwait.hh"
#include "debug/SimpleCPU.hh"
#include "debug/Mesh.hh"
#include "mem/packet.hh"
#include "mem/packet_access.hh"
#include "params/TimingSimpleCPU.hh"
#include "sim/faults.hh"
#include "sim/full_system.hh"
#include "sim/system.hh"

#include "custom/mesh_helper.hh"

using namespace std;
using namespace TheISA;

void
TimingSimpleCPU::init()
{
    BaseSimpleCPU::init();
}

/*----------------------------------------------------------------------
 * Implement base port behavior
 *--------------------------------------------------------------------*/ 

void
TimingSimpleCPU::TimingCPUPort::TickEvent::schedule(PacketPtr _pkt, Tick t)
{
    pkt = _pkt;
    cpu->schedule(this, t);
}

/*----------------------------------------------------------------------
 * Ports: C++ object to python name mappings
 *--------------------------------------------------------------------*/ 

Port &
TimingSimpleCPU::getPort(const string &if_name, PortID idx)
{
    // Get the right port based on name. This applies to all the
    // subclasses of the base CPU and relies on their implementation
    // of getDataPort and getInstPort.
    if (if_name == "dcache_port")
        return getDataPort();
    else if (if_name == "icache_port")
        return getInstPort();
    else if (if_name == "to_mesh_port"  && idx < toMeshPort.size())
        return toMeshPort[idx];
    else if (if_name == "from_mesh_port" && idx < fromMeshPort.size())
        return fromMeshPort[idx];
    else
        return ClockedObject::getPort(if_name, idx);
}

/*----------------------------------------------------------------------
 * Arch support for binds
 *--------------------------------------------------------------------*/

Fault
TimingSimpleCPU::trySendMeshRequest(uint64_t payload) {
  // get direction from the appropriate csr
  SimpleExecContext &t_info = *threadInfo[curThread];
  SimpleThread* thread = t_info.thread;
  uint64_t val = thread->readMiscRegNoEffect(MISCREG_EXE);
  
  // if in default behavior then don't send a mesh packet
  if (MeshHelper::isCSRDefault(val)) return NoFault;
  
  Mesh_Dir dir;
  if (!MeshHelper::csrToRd(val, dir)) return NoFault;
  
  // create a packet to send
  // size is numbytes?
  int size = sizeof(payload);
  
  // TODO getting wrong data from instruction 
  // bypass issue for now
  payload = 2;
  
  // need to break up payload into bytes
  // assume big endian?
  uint8_t *data = new uint8_t[size];
  for (int i = 0; i < size; i++) {
    // shift off byte at a time and truncate
    data[i] = (uint8_t)(payload >> (i * 8));
  }
  
  // create a packet to send
  Addr addr = dir;
  RequestPtr req = std::make_shared<Request>(addr, size, 0, 0);
  PacketPtr new_pkt = new Packet(req, MemCmd::WritebackDirty, size);
  new_pkt->dataDynamic(data);
  
  //if (toMeshPort[dir].checkHandshake()) {
  if (getOutRdy()) {
    DPRINTF(Mesh, "Sending mesh request %#x\n", addr);
    toMeshPort[dir].sendTimingReq(new_pkt);
    
    nextVal = true;
    nextRdy = true;
    schedule(setValRdyEvent, clockEdge(Cycles(1)));
  }
  else {
    DPRINTF(Mesh, "Port not ready so buffering packet %#x\n", addr);
    toMeshPort[dir].failToSend(new_pkt);
    
    nextVal = true;
    nextRdy = false;
    schedule(setValRdyEvent, clockEdge());
  }
  
  // schedule machinetick for the next cycle? if failed to send.
  // then need to remove valid
  // if sucessfully send then need to assert valid next cycle
  //meshMachineTick();
  //schedule(machineTickEvent, clockEdge());
  
  return NoFault;
}

// when a bind is created you need to assert val, rdy and then check that
// others are also val, rdy. Until then cpu status will be !running.
// we'll keep scheduling this to check each time the attach port updates its value
// when all relevant point handshake we'll call advanceInst with _status = running
// to get the next instruction

// TODO only want to do this if sync flag in csr is asserted?
// imagine getting signal from and'ed bind csrs to determine what ports
// are relevant in the handshake protocol

// 1) set val rdy for appropriate ports
// 2) these ports need to be met with response from pair port for handshake
//  -- if val or rdy not assert locally then don't need to check
// foreach port (need (val && rdy) || (!val && !rdy)) --> xnor
Fault
TimingSimpleCPU::setupAndHandshake() {
  DPRINTF(Mesh, "setup handshake and try unblock\n");
  
  //setup required handshake ports
  setupHandshake();
  
  /*
  // set valid and ready locally
  // actually this will be done by the statemachine
  //setValRdy();
  _status = BindSync;
  
  // checkHandshake/try unblock locally
  tryUnblock(); // this should be called every cycle
  
  // call checkHandshake in the external cores now that here's been an upadte
  handshakeNeighbors();
  */
  return NoFault;
}

void
TimingSimpleCPU::setupHandshake() {
  // get each bind csr
  std::vector<int> csrs = MeshHelper::getCSRCodes();
  
  // use the current thread to get the csr values we need
  SimpleExecContext &t_info = *threadInfo[curThread];
  SimpleThread* thread = t_info.thread;
  uint64_t regVal;
  
  numPortsActive = 0;
  
  // foreach bind csr set val or rdy in the apprpriate port
  for (int i = 0; i < csrs.size(); i++) {
    regVal = thread->readMiscRegNoEffect(csrs[i]);
    DPRINTF(Mesh, "regval %d from csr %#x\n", regVal, csrs[i]);
    // assert valid on each port we're sending to
    // TODO only one port supported now
    Mesh_Dir outDir;
    if (MeshHelper::csrToRd(regVal, outDir)) {
      toMeshPort[outDir].setActive(true);
      numPortsActive++;
    }
    
    // try to handshake with recv ports
    Mesh_Dir op1Dir;
    if (MeshHelper::csrToOp1(regVal, op1Dir)) {
      fromMeshPort[op1Dir].setActive(true);
      numPortsActive++;
    }
    
    Mesh_Dir op2Dir;
    if (MeshHelper::csrToOp2(regVal, op2Dir)) {
      fromMeshPort[op2Dir].setActive(true);
      numPortsActive++;
    }
  }
}

bool
TimingSimpleCPU::getOutRdy() {
  bool allRdy = true;
  
  for (int i = 0; i < toMeshPort.size(); i++) {
    if (toMeshPort[i].getActive()) {
      if (!toMeshPort[i].getPairRdy()) allRdy = false;
    }
  }
  
  return allRdy;
}

bool
TimingSimpleCPU::getInVal() {
   bool allVal = true;
  
  for (int i = 0; i < fromMeshPort.size(); i++) {
    if (fromMeshPort[i].getActive()) {
      if (!fromMeshPort[i].getPairVal()) allVal = false;
    }
  }
  
  return allVal;
}

Fault
TimingSimpleCPU::setRdy() {
  if (rdy) return NoFault;
  rdy = true;
  
  for (int i = 0; i < fromMeshPort.size(); i++) {
    fromMeshPort[i].setRdyIfActive();
  }
  
  // inform neighbors of state update so they can potentially unblock
  informNeighbors();
  
  return NoFault;
}

Fault
TimingSimpleCPU::setVal() {
  if (val) return NoFault;
  val = true;
  
  for (int i = 0; i < toMeshPort.size(); i++) {
    toMeshPort[i].setValIfActive();
  }
  
  informNeighbors();
  
  return NoFault;
}

Fault
TimingSimpleCPU::resetRdy() {
  if (!rdy) return NoFault;
  rdy = false;
  
  for (int i = 0; i < fromMeshPort.size(); i++) {
    fromMeshPort[i].setRdy(false);
  }
  
  informNeighbors();
  
  return NoFault;
}

Fault
TimingSimpleCPU::resetVal() {
  
  if (!val) return NoFault;
  val = false;
  
  for (int i = 0; i < toMeshPort.size(); i++) {
    toMeshPort[i].setVal(false);
  }
  
  informNeighbors();
  
  return NoFault;
}

/*Fault
TimingSimpleCPU::resetActive() {
  for (int i = 0; i < fromMeshPort.size(); i++) {
    fromMeshPort[i].setActive(false);
  }
  
  for (int i = 0; i < toMeshPort.size(); i++) {
    toMeshPort[i].setActive(false);
  }
}*/

void
TimingSimpleCPU::meshMachineTick() {
  // update state machine here?
  MeshMachine::MeshStateOutputs outputs = 
    machine.updateMachine(MeshMachine::MeshStateInputs(getOutRdy(), getInVal(), false, (numPortsActive > 0)));
  
  // change things in the core based on statemachine outputs
  if (outputs.selfRdy) {
    DPRINTF(Mesh, "set self rdy\n");
    setRdy();
  }
  else resetRdy();
  
  if (outputs.selfVal) {
    DPRINTF(Mesh, "set self val\n");
    setVal();
  }
  else resetVal();
}

Fault
TimingSimpleCPU::tryUnblock() {
  // TODO integrate into the statemachine instead ... lazy!

  if (numPortsActive > 0) {
  DPRINTF(Mesh, "out:\nactive %d %d %d %d\npair rdy %d %d %d %d\n\n",
    toMeshPort[0].getActive(), toMeshPort[1].getActive(), toMeshPort[2].getActive(), toMeshPort[3].getActive(),
    toMeshPort[0].getPairRdy(), toMeshPort[1].getPairRdy(), toMeshPort[2].getPairRdy(), toMeshPort[3].getPairRdy());

  DPRINTF(Mesh, "in:\nactive %d %d %d %d\npair val %d %d %d %d\n\n",
    fromMeshPort[0].getActive(), fromMeshPort[1].getActive(), fromMeshPort[2].getActive(), fromMeshPort[3].getActive(),
    fromMeshPort[0].getPairVal(), fromMeshPort[1].getPairVal(), fromMeshPort[2].getPairVal(), fromMeshPort[3].getPairVal());
  }
  
  // don't do machine update here?
  // b/c implies send? need another state otherwise
  //meshMachineTick();
  /*setRdy();
  
  bool handshake = true;
  
  // foreach in port make sure that its handshake requirements are met
  for (int i = 0; i < fromMeshPort.size(); i++) {
    bool ok = fromMeshPort[i].checkHandshake();
    if (!ok) handshake = false;
  }
  
  // foreach out port make sure handshake requirements are met
  for (int i = 0; i < toMeshPort.size(); i++) {
    bool ok = toMeshPort[i].checkHandshake();
    if (!ok) handshake = false;
  }
  
  if (handshake) {
    DPRINTF(Mesh, "Handshake!\n");
  }
  else {
    DPRINTF(Mesh, "Failed handshake\n");
  }
  */
  
  /*if (_status != Running && _status != BindSync) {
    DPRINTF(Mesh, "Got status %d\n", _status);
    assert(0);
  }*/
  
  // the processor could be doing something else when this update is requested
  // only change the cpu state if in BindSync state (i.e. waiting for sync)
  
  // unstall the processor (stalled when first call bind)
  if (_status == BindSync) {
    tryInstruction();
  }
  
  return NoFault;
}

// flag deletion
void
TimingSimpleCPU::handshakeNeighbors() {
  DPRINTF(Mesh, "notify neighbors\n");
  // go through mesh ports to get tryUnblock function called in neighbor cores
  for (int i = 0; i < toMeshPort.size(); i++) {
    toMeshPort[i].tryUnblockNeighbor();
  }
}

void
TimingSimpleCPU::informNeighbors() {
  DPRINTF(Mesh, "notify neighbors\n");
  // go through mesh ports to get tryUnblock function called in neighbor cores
  for (int i = 0; i < toMeshPort.size(); i++) {
    toMeshPort[i].tryUnblockNeighbor();
  }
}


uint64_t
TimingSimpleCPU::getMeshPortData(Mesh_Dir dir) {
  return fromMeshPort[dir].getPacketData();
}

/*void
TimingSimpleCPU::setPortPacket(PacketPtr pkt, Mesh_Dir dir) {
  fromMeshPort[dir].setPacket(pkt);
}*/


// check whether val/rdy for src/dest needed by instruction
// ret and the caller should block if not true

// will be 0 if don't care, TODO implement don't cares

bool
TimingSimpleCPU::checkOpsValRdy() {
  //curStaticInst->destRegIdx(0);
  //curStaticInst->srcRegIdx(0);
  
  // figure out which operands matter TODO addi where one taken from outside
  //DPRINTF(Mesh, "%d %d %d\n", curStaticInst->destRegIdx(0), 
  //  curStaticInst->srcRegIdx(0), curStaticInst->srcRegIdx(1));
    
  // see if the ports associated are ready
  bool outRdy = getOutRdy();
  bool inVal = getInVal();
  
  return (outRdy && inVal);
  
}

void
TimingSimpleCPU::setNextValRdy() {
  if (nextVal) setVal();
  else resetVal();
  
  if (nextRdy) setRdy();
  else resetRdy();
}

void
TimingSimpleCPU::tryInstruction() {
  
  SimpleExecContext& t_info = *threadInfo[curThread];
  
  if (curStaticInst && curStaticInst->isMemRef()) {
        // load or store: just send to dcache
        Fault fault = curStaticInst->initiateAcc(&t_info, traceData);

        // If we're not running now the instruction will complete in a dcache
        // response callback or the instruction faulted and has started an
        // ifetch
        if (_status == BaseSimpleCPU::Running) {
          DPRINTF(SimpleCPU, "no block on dcache load\n");
            if (fault != NoFault && traceData) {
                // If there was a fault, we shouldn't trace this instruction.
                delete traceData;
                traceData = NULL;
            }

            postExecute();
            // @todo remove me after debugging with legion done
            if (curStaticInst && (!curStaticInst->isMicroop() ||
                        curStaticInst->isFirstMicroop()))
                instCnt++;
            advanceInst(fault);
        }
        else {
          // always blocks!
          DPRINTF(SimpleCPU, "Block on dcache load\n");
        }
    // pbb handle a bind instruction, advanceInst will be called
    // from within bind
    /*} else if (curStaticInst && curStaticInst->isBind()) {
        Fault fault = curStaticInst->execute(&t_info, traceData);*/
    } else if (curStaticInst) {
      DPRINTF(SimpleCPU, "advance inst on fetch\n");
      // check if the src and dests are rdy (otherwise block)
      
      bool ok = checkOpsValRdy();
      if (curStaticInst->isBind()) {
        DPRINTF(Mesh, "Encounter bind instruction %d\n", ok);
      }
      
      if (ok) _status = Running;
      else _status = BindSync;
      
      if (numPortsActive > 0) {
        setRdy();
      }
      // temp
      //_status = Running;
      
      if (_status == Running) {
      
        // set self as rdy if passed val/rdy check
        //setRdy();
      
        // non-memory instruction: execute completely now
        Fault fault = curStaticInst->execute(&t_info, traceData);

        // keep an instruction count
        if (fault == NoFault)
            countInst();
        else if (traceData && !DTRACE(ExecFaulting)) {
            delete traceData;
            traceData = NULL;
        }

        postExecute();
        // @todo remove me after debugging with legion done
        if (curStaticInst && (!curStaticInst->isMicroop() ||
                curStaticInst->isFirstMicroop()))
            instCnt++;
        advanceInst(fault);
      }
      else {
        // not rdy or val if blocking?
        //setRdy();
        DPRINTF(Mesh, "Mesh blocking\n");
      }
    } else {
      DPRINTF(SimpleCPU, "advance inst on fetch null\n");
        advanceInst(NoFault);
    }
  
}

/*----------------------------------------------------------------------
 * Define normal processor behavior
 *--------------------------------------------------------------------*/ 

TimingSimpleCPU::TimingSimpleCPU(TimingSimpleCPUParams *p)
    : BaseSimpleCPU(p), fetchTranslation(this), icachePort(this),
      dcachePort(this), ifetch_pkt(NULL), dcache_pkt(NULL), previousCycle(0)
      
      // begin additions (needs to be in order declared in .hh)
      , machine(this), numPortsActive(0), 
      machineTickEvent([this] { meshMachineTick(); }, name()),
      
      nextVal(0), nextRdy(0), setValRdyEvent([this] { setNextValRdy(); }, name()),
      val(0), rdy(0),
      // end additions
      
      fetchEvent([this]{ fetch(); }, name())
      
{
    _status = Idle;
    
    // declare vector ports
    for (int i = 0; i < p->port_to_mesh_port_connection_count; ++i) {
        toMeshPort.emplace_back(this, i);
    }
    
    for (int i = 0; i < p->port_from_mesh_port_connection_count; ++i) {
        fromMeshPort.emplace_back(this, i);
    }
    
    // keep in mind that you shouldn't use 'this' within objects that are declared as vec?
    /*for (int i = 0; i < 4; i++) {
      DPRINTF(Mesh, "ptrs %ld\n", (uint64_t)(&(fromMeshPort[i])));
    }*/
}



TimingSimpleCPU::~TimingSimpleCPU()
{
}

DrainState
TimingSimpleCPU::drain()
{
    // Deschedule any power gating event (if any)
    deschedulePowerGatingEvent();

    if (switchedOut())
        return DrainState::Drained;

    if (_status == Idle ||
        (_status == BaseSimpleCPU::Running && isDrained())) {
        DPRINTF(Drain, "No need to drain.\n");
        activeThreads.clear();
        return DrainState::Drained;
    } else {
        DPRINTF(Drain, "Requesting drain.\n");

        // The fetch event can become descheduled if a drain didn't
        // succeed on the first attempt. We need to reschedule it if
        // the CPU is waiting for a microcode routine to complete.
        if (_status == BaseSimpleCPU::Running && !fetchEvent.scheduled())
            schedule(fetchEvent, clockEdge());

        return DrainState::Draining;
    }
}

void
TimingSimpleCPU::drainResume()
{
    assert(!fetchEvent.scheduled());
    if (switchedOut())
        return;

    DPRINTF(SimpleCPU, "Resume\n");
    verifyMemoryMode();

    assert(!threadContexts.empty());

    _status = BaseSimpleCPU::Idle;

    for (ThreadID tid = 0; tid < numThreads; tid++) {
        if (threadInfo[tid]->thread->status() == ThreadContext::Active) {
            threadInfo[tid]->notIdleFraction = 1;

            activeThreads.push_back(tid);

            _status = BaseSimpleCPU::Running;

            // Fetch if any threads active
            if (!fetchEvent.scheduled()) {
                schedule(fetchEvent, nextCycle());
            }
        } else {
            threadInfo[tid]->notIdleFraction = 0;
        }
    }

    // Reschedule any power gating event (if any)
    schedulePowerGatingEvent();

    system->totalNumInsts = 0;
}

bool
TimingSimpleCPU::tryCompleteDrain()
{
    if (drainState() != DrainState::Draining)
        return false;

    DPRINTF(Drain, "tryCompleteDrain.\n");
    if (!isDrained())
        return false;

    DPRINTF(Drain, "CPU done draining, processing drain event\n");
    signalDrainDone();

    return true;
}

void
TimingSimpleCPU::switchOut()
{
    SimpleExecContext& t_info = *threadInfo[curThread];
    M5_VAR_USED SimpleThread* thread = t_info.thread;

    BaseSimpleCPU::switchOut();

    assert(!fetchEvent.scheduled());
    assert(_status == BaseSimpleCPU::Running || _status == Idle);
    assert(!t_info.stayAtPC);
    assert(thread->microPC() == 0);

    updateCycleCounts();
    updateCycleCounters(BaseCPU::CPU_STATE_ON);
}


void
TimingSimpleCPU::takeOverFrom(BaseCPU *oldCPU)
{
    BaseSimpleCPU::takeOverFrom(oldCPU);

    previousCycle = curCycle();
}

void
TimingSimpleCPU::verifyMemoryMode() const
{
    if (!system->isTimingMode()) {
        fatal("The timing CPU requires the memory system to be in "
              "'timing' mode.\n");
    }
}

void
TimingSimpleCPU::activateContext(ThreadID thread_num)
{
    DPRINTF(SimpleCPU, "ActivateContext %d\n", thread_num);

    assert(thread_num < numThreads);

    threadInfo[thread_num]->notIdleFraction = 1;
    if (_status == BaseSimpleCPU::Idle)
        _status = BaseSimpleCPU::Running;

    // kick things off by initiating the fetch of the next instruction
    if (!fetchEvent.scheduled())
        schedule(fetchEvent, clockEdge(Cycles(0)));

    if (std::find(activeThreads.begin(), activeThreads.end(), thread_num)
         == activeThreads.end()) {
        activeThreads.push_back(thread_num);
    }

    BaseCPU::activateContext(thread_num);
}


void
TimingSimpleCPU::suspendContext(ThreadID thread_num)
{
    DPRINTF(SimpleCPU, "SuspendContext %d\n", thread_num);

    assert(thread_num < numThreads);
    activeThreads.remove(thread_num);

    if (_status == Idle)
        return;

    assert(_status == BaseSimpleCPU::Running);

    threadInfo[thread_num]->notIdleFraction = 0;

    if (activeThreads.empty()) {
        _status = Idle;

        if (fetchEvent.scheduled()) {
            deschedule(fetchEvent);
        }
    }

    BaseCPU::suspendContext(thread_num);
}

bool
TimingSimpleCPU::handleReadPacket(PacketPtr pkt)
{
    SimpleExecContext &t_info = *threadInfo[curThread];
    SimpleThread* thread = t_info.thread;

    const RequestPtr &req = pkt->req;

    // We're about the issues a locked load, so tell the monitor
    // to start caring about this address
    if (pkt->isRead() && pkt->req->isLLSC()) {
        TheISA::handleLockedRead(thread, pkt->req);
    }
    if (req->isMmappedIpr()) {
        Cycles delay = TheISA::handleIprRead(thread->getTC(), pkt);
        new IprEvent(pkt, this, clockEdge(delay));
        _status = DcacheWaitResponse;
        dcache_pkt = NULL;
    } else if (!dcachePort.sendTimingReq(pkt)) {
        _status = DcacheRetry;
        dcache_pkt = pkt;
    } else {
        _status = DcacheWaitResponse;
        // memory system takes ownership of packet
        dcache_pkt = NULL;
    }
    return dcache_pkt == NULL;
}

void
TimingSimpleCPU::sendData(const RequestPtr &req, uint8_t *data, uint64_t *res,
                          bool read)
{
    SimpleExecContext &t_info = *threadInfo[curThread];
    SimpleThread* thread = t_info.thread;

    PacketPtr pkt = buildPacket(req, read);
    pkt->dataDynamic<uint8_t>(data);

    if (req->getFlags().isSet(Request::NO_ACCESS)) {
        assert(!dcache_pkt);
        pkt->makeResponse();
        completeDataAccess(pkt);
    } else if (read) {
        handleReadPacket(pkt);
    } else {
        bool do_access = true;  // flag to suppress cache access

        if (req->isLLSC()) {
            do_access = TheISA::handleLockedWrite(thread, req, dcachePort.cacheBlockMask);
        } else if (req->isCondSwap()) {
            assert(res);
            req->setExtraData(*res);
        }

        if (do_access) {
            dcache_pkt = pkt;
            handleWritePacket();
            threadSnoop(pkt, curThread);
        } else {
            _status = DcacheWaitResponse;
            completeDataAccess(pkt);
        }
    }
}

void
TimingSimpleCPU::sendSplitData(const RequestPtr &req1, const RequestPtr &req2,
                               const RequestPtr &req, uint8_t *data, bool read)
{
    PacketPtr pkt1, pkt2;
    buildSplitPacket(pkt1, pkt2, req1, req2, req, data, read);
    if (req->getFlags().isSet(Request::NO_ACCESS)) {
        assert(!dcache_pkt);
        pkt1->makeResponse();
        completeDataAccess(pkt1);
    } else if (read) {
        SplitFragmentSenderState * send_state =
            dynamic_cast<SplitFragmentSenderState *>(pkt1->senderState);
        if (handleReadPacket(pkt1)) {
            send_state->clearFromParent();
            send_state = dynamic_cast<SplitFragmentSenderState *>(
                    pkt2->senderState);
            if (handleReadPacket(pkt2)) {
                send_state->clearFromParent();
            }
        }
    } else {
        dcache_pkt = pkt1;
        SplitFragmentSenderState * send_state =
            dynamic_cast<SplitFragmentSenderState *>(pkt1->senderState);
        if (handleWritePacket()) {
            send_state->clearFromParent();
            dcache_pkt = pkt2;
            send_state = dynamic_cast<SplitFragmentSenderState *>(
                    pkt2->senderState);
            if (handleWritePacket()) {
                send_state->clearFromParent();
            }
        }
    }
}

void
TimingSimpleCPU::translationFault(const Fault &fault)
{
    // fault may be NoFault in cases where a fault is suppressed,
    // for instance prefetches.
    updateCycleCounts();
    updateCycleCounters(BaseCPU::CPU_STATE_ON);

    if (traceData) {
        // Since there was a fault, we shouldn't trace this instruction.
        delete traceData;
        traceData = NULL;
    }

    postExecute();

    advanceInst(fault);
}

PacketPtr
TimingSimpleCPU::buildPacket(const RequestPtr &req, bool read)
{
    return read ? Packet::createRead(req) : Packet::createWrite(req);
}

void
TimingSimpleCPU::buildSplitPacket(PacketPtr &pkt1, PacketPtr &pkt2,
        const RequestPtr &req1, const RequestPtr &req2, const RequestPtr &req,
        uint8_t *data, bool read)
{
    pkt1 = pkt2 = NULL;

    assert(!req1->isMmappedIpr() && !req2->isMmappedIpr());

    if (req->getFlags().isSet(Request::NO_ACCESS)) {
        pkt1 = buildPacket(req, read);
        return;
    }

    pkt1 = buildPacket(req1, read);
    pkt2 = buildPacket(req2, read);

    PacketPtr pkt = new Packet(req, pkt1->cmd.responseCommand());

    pkt->dataDynamic<uint8_t>(data);
    pkt1->dataStatic<uint8_t>(data);
    pkt2->dataStatic<uint8_t>(data + req1->getSize());

    SplitMainSenderState * main_send_state = new SplitMainSenderState;
    pkt->senderState = main_send_state;
    main_send_state->fragments[0] = pkt1;
    main_send_state->fragments[1] = pkt2;
    main_send_state->outstanding = 2;
    pkt1->senderState = new SplitFragmentSenderState(pkt, 0);
    pkt2->senderState = new SplitFragmentSenderState(pkt, 1);
}

Fault
TimingSimpleCPU::initiateMemRead(Addr addr, unsigned size,
                                 Request::Flags flags)
{
    SimpleExecContext &t_info = *threadInfo[curThread];
    SimpleThread* thread = t_info.thread;

    Fault fault;
    const int asid = 0;
    const Addr pc = thread->instAddr();
    unsigned block_size = cacheLineSize();
    BaseTLB::Mode mode = BaseTLB::Read;

    if (traceData)
        traceData->setMem(addr, size, flags);

    RequestPtr req = std::make_shared<Request>(
        asid, addr, size, flags, dataMasterId(), pc,
        thread->contextId());

    req->taskId(taskId());

    Addr split_addr = roundDown(addr + size - 1, block_size);
    assert(split_addr <= addr || split_addr - addr < block_size);

    _status = DTBWaitResponse;
    if (split_addr > addr) {
        RequestPtr req1, req2;
        assert(!req->isLLSC() && !req->isSwap());
        req->splitOnVaddr(split_addr, req1, req2);

        WholeTranslationState *state =
            new WholeTranslationState(req, req1, req2, new uint8_t[size],
                                      NULL, mode);
        DataTranslation<TimingSimpleCPU *> *trans1 =
            new DataTranslation<TimingSimpleCPU *>(this, state, 0);
        DataTranslation<TimingSimpleCPU *> *trans2 =
            new DataTranslation<TimingSimpleCPU *>(this, state, 1);

        thread->dtb->translateTiming(req1, thread->getTC(), trans1, mode);
        thread->dtb->translateTiming(req2, thread->getTC(), trans2, mode);
    } else {
        WholeTranslationState *state =
            new WholeTranslationState(req, new uint8_t[size], NULL, mode);
        DataTranslation<TimingSimpleCPU *> *translation
            = new DataTranslation<TimingSimpleCPU *>(this, state);
        thread->dtb->translateTiming(req, thread->getTC(), translation, mode);
    }

    return NoFault;
}

bool
TimingSimpleCPU::handleWritePacket()
{
    SimpleExecContext &t_info = *threadInfo[curThread];
    SimpleThread* thread = t_info.thread;

    const RequestPtr &req = dcache_pkt->req;
    if (req->isMmappedIpr()) {
        Cycles delay = TheISA::handleIprWrite(thread->getTC(), dcache_pkt);
        new IprEvent(dcache_pkt, this, clockEdge(delay));
        _status = DcacheWaitResponse;
        dcache_pkt = NULL;
    } else if (!dcachePort.sendTimingReq(dcache_pkt)) {
        _status = DcacheRetry;
    } else {
        _status = DcacheWaitResponse;
        // memory system takes ownership of packet
        dcache_pkt = NULL;
    }
    return dcache_pkt == NULL;
}

Fault
TimingSimpleCPU::writeMem(uint8_t *data, unsigned size,
                          Addr addr, Request::Flags flags, uint64_t *res)
{
    SimpleExecContext &t_info = *threadInfo[curThread];
    SimpleThread* thread = t_info.thread;

    uint8_t *newData = new uint8_t[size];
    const int asid = 0;
    const Addr pc = thread->instAddr();
    unsigned block_size = cacheLineSize();
    BaseTLB::Mode mode = BaseTLB::Write;

    if (data == NULL) {
        assert(flags & Request::STORE_NO_DATA);
        // This must be a cache block cleaning request
        memset(newData, 0, size);
    } else {
        memcpy(newData, data, size);
    }

    if (traceData)
        traceData->setMem(addr, size, flags);

    RequestPtr req = std::make_shared<Request>(
        asid, addr, size, flags, dataMasterId(), pc,
        thread->contextId());

    req->taskId(taskId());

    Addr split_addr = roundDown(addr + size - 1, block_size);
    assert(split_addr <= addr || split_addr - addr < block_size);

    _status = DTBWaitResponse;
    if (split_addr > addr) {
        RequestPtr req1, req2;
        assert(!req->isLLSC() && !req->isSwap());
        req->splitOnVaddr(split_addr, req1, req2);

        WholeTranslationState *state =
            new WholeTranslationState(req, req1, req2, newData, res, mode);
        DataTranslation<TimingSimpleCPU *> *trans1 =
            new DataTranslation<TimingSimpleCPU *>(this, state, 0);
        DataTranslation<TimingSimpleCPU *> *trans2 =
            new DataTranslation<TimingSimpleCPU *>(this, state, 1);

        thread->dtb->translateTiming(req1, thread->getTC(), trans1, mode);
        thread->dtb->translateTiming(req2, thread->getTC(), trans2, mode);
    } else {
        WholeTranslationState *state =
            new WholeTranslationState(req, newData, res, mode);
        DataTranslation<TimingSimpleCPU *> *translation =
            new DataTranslation<TimingSimpleCPU *>(this, state);
        thread->dtb->translateTiming(req, thread->getTC(), translation, mode);
    }

    // Translation faults will be returned via finishTranslation()
    return NoFault;
}

Fault
TimingSimpleCPU::initiateMemAMO(Addr addr, unsigned size,
                                Request::Flags flags,
                                AtomicOpFunctor *amo_op)
{
    SimpleExecContext &t_info = *threadInfo[curThread];
    SimpleThread* thread = t_info.thread;

    Fault fault;
    const int asid = 0;
    const Addr pc = thread->instAddr();
    unsigned block_size = cacheLineSize();
    BaseTLB::Mode mode = BaseTLB::Write;

    if (traceData)
        traceData->setMem(addr, size, flags);

    RequestPtr req = make_shared<Request>(asid, addr, size, flags,
                            dataMasterId(), pc, thread->contextId(), amo_op);

    assert(req->hasAtomicOpFunctor());

    req->taskId(taskId());

    Addr split_addr = roundDown(addr + size - 1, block_size);

    // AMO requests that access across a cache line boundary are not
    // allowed since the cache does not guarantee AMO ops to be executed
    // atomically in two cache lines
    // For ISAs such as x86 that requires AMO operations to work on
    // accesses that cross cache-line boundaries, the cache needs to be
    // modified to support locking both cache lines to guarantee the
    // atomicity.
    if (split_addr > addr) {
        panic("AMO requests should not access across a cache line boundary\n");
    }

    _status = DTBWaitResponse;

    WholeTranslationState *state =
        new WholeTranslationState(req, new uint8_t[size], NULL, mode);
    DataTranslation<TimingSimpleCPU *> *translation
        = new DataTranslation<TimingSimpleCPU *>(this, state);
    thread->dtb->translateTiming(req, thread->getTC(), translation, mode);

    return NoFault;
}

void
TimingSimpleCPU::threadSnoop(PacketPtr pkt, ThreadID sender)
{
    for (ThreadID tid = 0; tid < numThreads; tid++) {
        if (tid != sender) {
            if (getCpuAddrMonitor(tid)->doMonitor(pkt)) {
                wakeup(tid);
            }
            TheISA::handleLockedSnoop(threadInfo[tid]->thread, pkt,
                    dcachePort.cacheBlockMask);
        }
    }
}

void
TimingSimpleCPU::finishTranslation(WholeTranslationState *state)
{
    _status = BaseSimpleCPU::Running;

    if (state->getFault() != NoFault) {
        if (state->isPrefetch()) {
            state->setNoFault();
        }
        delete [] state->data;
        state->deleteReqs();
        translationFault(state->getFault());
    } else {
        if (!state->isSplit) {
            sendData(state->mainReq, state->data, state->res,
                     state->mode == BaseTLB::Read);
        } else {
            sendSplitData(state->sreqLow, state->sreqHigh, state->mainReq,
                          state->data, state->mode == BaseTLB::Read);
        }
    }

    delete state;
}


void
TimingSimpleCPU::fetch()
{
    // Change thread if multi-threaded
    swapActiveThread();

    SimpleExecContext &t_info = *threadInfo[curThread];
    SimpleThread* thread = t_info.thread;

    DPRINTF(SimpleCPU, "Fetch\n");

    if (!curStaticInst || !curStaticInst->isDelayedCommit()) {
        checkForInterrupts();
        checkPcEventQueue();
    }

    // We must have just got suspended by a PC event
    if (_status == Idle)
        return;

    TheISA::PCState pcState = thread->pcState();
    bool needToFetch = !isRomMicroPC(pcState.microPC()) &&
                       !curMacroStaticInst;

    if (needToFetch) {
        _status = BaseSimpleCPU::Running;
        RequestPtr ifetch_req = std::make_shared<Request>();
        ifetch_req->taskId(taskId());
        ifetch_req->setContext(thread->contextId());
        setupFetchRequest(ifetch_req);
        DPRINTF(SimpleCPU, "Translating address %#x\n", ifetch_req->getVaddr());
        thread->itb->translateTiming(ifetch_req, thread->getTC(),
                &fetchTranslation, BaseTLB::Execute);
    } else {
        _status = IcacheWaitResponse;
        completeIfetch(NULL);

        updateCycleCounts();
        updateCycleCounters(BaseCPU::CPU_STATE_ON);
    }
}


void
TimingSimpleCPU::sendFetch(const Fault &fault, const RequestPtr &req,
                           ThreadContext *tc)
{
    if (fault == NoFault) {
        DPRINTF(SimpleCPU, "Sending fetch for addr %#x(pa: %#x)\n",
                req->getVaddr(), req->getPaddr());
        ifetch_pkt = new Packet(req, MemCmd::ReadReq);
        ifetch_pkt->dataStatic(&inst);
        DPRINTF(SimpleCPU, " -- pkt addr: %#x\n", ifetch_pkt->getAddr());

        if (!icachePort.sendTimingReq(ifetch_pkt)) {
            // Need to wait for retry
            _status = IcacheRetry;
        } else {
            // Need to wait for cache to respond
            _status = IcacheWaitResponse;
            // ownership of packet transferred to memory system
            ifetch_pkt = NULL;
        }
    } else {
        DPRINTF(SimpleCPU, "Translation of addr %#x faulted\n", req->getVaddr());
        // fetch fault: advance directly to next instruction (fault handler)
        _status = BaseSimpleCPU::Running;
        advanceInst(fault);
    }

    updateCycleCounts();
    updateCycleCounters(BaseCPU::CPU_STATE_ON);
}


void
TimingSimpleCPU::advanceInst(const Fault &fault)
{
    SimpleExecContext &t_info = *threadInfo[curThread];

    if (_status == Faulting)
        return;

    if (fault != NoFault) {
        DPRINTF(SimpleCPU, "Fault occured. Handling the fault\n");

        advancePC(fault);

        // A syscall fault could suspend this CPU (e.g., futex_wait)
        // If the _status is not Idle, schedule an event to fetch the next
        // instruction after 'stall' ticks.
        // If the cpu has been suspended (i.e., _status == Idle), another
        // cpu will wake this cpu up later.
        if (_status != Idle) {
            DPRINTF(SimpleCPU, "Scheduling fetch event after the Fault\n");

            Tick stall = dynamic_pointer_cast<SyscallRetryFault>(fault) ?
                         clockEdge(syscallRetryLatency) : clockEdge();
            reschedule(fetchEvent, stall, true);
            _status = Faulting;
        }

        return;
    }

    if (!t_info.stayAtPC)
        advancePC(fault);

    if (tryCompleteDrain())
        return;

    // pbb we're only going to fetch the next instruction if status
    // is 'running'. Otherwise, we might be in 'IcacheWaitResponse'
    // where we won't try to fetch again.
    
    // 

    if (_status == BaseSimpleCPU::Running) {
        // kick off fetch of next instruction... callback from icache
        // response will cause that instruction to be executed,
        // keeping the CPU running.
        fetch();
    }
}


void
TimingSimpleCPU::completeIfetch(PacketPtr pkt)
{
    //SimpleExecContext& t_info = *threadInfo[curThread];

    DPRINTF(SimpleCPU, "Complete ICache Fetch for addr %#x\n", pkt ?
            pkt->getAddr() : 0);

    // received a response from the icache: execute the received
    // instruction
    assert(!pkt || !pkt->isError());
    assert(_status == IcacheWaitResponse);

    _status = BaseSimpleCPU::Running;

    updateCycleCounts();
    updateCycleCounters(BaseCPU::CPU_STATE_ON);

    if (pkt)
        pkt->req->setAccessLatency();

    // setup curStaticInst based on the received instruction packet
    preExecute();
    
    tryInstruction();
    
    /*if (curStaticInst && curStaticInst->isMemRef()) {
        // load or store: just send to dcache
        Fault fault = curStaticInst->initiateAcc(&t_info, traceData);

        // If we're not running now the instruction will complete in a dcache
        // response callback or the instruction faulted and has started an
        // ifetch
        if (_status == BaseSimpleCPU::Running) {
          DPRINTF(SimpleCPU, "no block on dcache load\n");
            if (fault != NoFault && traceData) {
                // If there was a fault, we shouldn't trace this instruction.
                delete traceData;
                traceData = NULL;
            }

            postExecute();
            // @todo remove me after debugging with legion done
            if (curStaticInst && (!curStaticInst->isMicroop() ||
                        curStaticInst->isFirstMicroop()))
                instCnt++;
            advanceInst(fault);
        }
        else {
          // always blocks!
          DPRINTF(SimpleCPU, "Block on dcache load\n");
        }
    } else if (curStaticInst) {
      DPRINTF(SimpleCPU, "advance inst on fetch\n");
      // check if the src and dests are rdy (otherwise block)
      bool ok = checkOpsValRdy();
      if (ok) _status = Running;
      else _status = BindSync;
      
      if (_status == Running) {
      
        // set self as rdy if passed val/rdy check
        setRdy();
      
        // non-memory instruction: execute completely now
        Fault fault = curStaticInst->execute(&t_info, traceData);

        // keep an instruction count
        if (fault == NoFault)
            countInst();
        else if (traceData && !DTRACE(ExecFaulting)) {
            delete traceData;
            traceData = NULL;
        }

        postExecute();
        // @todo remove me after debugging with legion done
        if (curStaticInst && (!curStaticInst->isMicroop() ||
                curStaticInst->isFirstMicroop()))
            instCnt++;
        advanceInst(fault);
      }
      else {
        // not rdy or val if blocking?
        resetRdy();
        resetVal();
        DPRINTF(Mesh, "Mesh blocking\n");
      }
    } else {
      DPRINTF(SimpleCPU, "advance inst on fetch null\n");
        advanceInst(NoFault);
    }*/
    
    // pbb lets check if the mesh csr is written as it will effect 
    // processor behavior
    /*SimpleThread* thread = t_info.thread;
    uint64_t val = thread->readMiscRegNoEffect(MISCREG_MESHOUT);
    if (curStaticInst && pkt && val == 2) {
    //if (curStaticInst && curStaticInst->isBind()) { 
      int size = 64;
      RequestPtr req = (RequestPtr)new Request(pkt->getAddr(), size, 0, 0);
      PacketPtr new_pkt = new Packet(req, MemCmd::WritebackDirty, size);
      DPRINTF(Mesh, "Sending mesh request %#x\n", pkt->getAddr());
      toMeshPort[0].sendTimingReq(new_pkt);
    }*/

    if (pkt) {
        delete pkt;
    }
}

/*----------------------------------------------------------------------
 * Define icache port behavior
 *--------------------------------------------------------------------*/ 

void
TimingSimpleCPU::IcachePort::ITickEvent::process()
{
    cpu->completeIfetch(pkt);
}

bool
TimingSimpleCPU::IcachePort::recvTimingResp(PacketPtr pkt)
{
    DPRINTF(SimpleCPU, "Received fetch response %#x\n", pkt->getAddr());
    // we should only ever see one response per cycle since we only
    // issue a new request once this response is sunk
    assert(!tickEvent.scheduled());
    // delay processing of returned data until next CPU clock edge
    tickEvent.schedule(pkt, cpu->clockEdge());

    return true;
}

void
TimingSimpleCPU::IcachePort::recvReqRetry()
{
    // we shouldn't get a retry unless we have a packet that we're
    // waiting to transmit
    assert(cpu->ifetch_pkt != NULL);
    assert(cpu->_status == IcacheRetry);
    PacketPtr tmp = cpu->ifetch_pkt;
    if (sendTimingReq(tmp)) {
        cpu->_status = IcacheWaitResponse;
        cpu->ifetch_pkt = NULL;
    }
}

void
TimingSimpleCPU::completeDataAccess(PacketPtr pkt)
{
    // received a response from the dcache: complete the load or store
    // instruction
    assert(!pkt->isError());
    assert(_status == DcacheWaitResponse || _status == DTBWaitResponse ||
           pkt->req->getFlags().isSet(Request::NO_ACCESS));

    pkt->req->setAccessLatency();

    updateCycleCounts();
    updateCycleCounters(BaseCPU::CPU_STATE_ON);

    if (pkt->senderState) {
        SplitFragmentSenderState * send_state =
            dynamic_cast<SplitFragmentSenderState *>(pkt->senderState);
        assert(send_state);
        delete pkt;
        PacketPtr big_pkt = send_state->bigPkt;
        delete send_state;

        SplitMainSenderState * main_send_state =
            dynamic_cast<SplitMainSenderState *>(big_pkt->senderState);
        assert(main_send_state);
        // Record the fact that this packet is no longer outstanding.
        assert(main_send_state->outstanding != 0);
        main_send_state->outstanding--;

        if (main_send_state->outstanding) {
            return;
        } else {
            delete main_send_state;
            big_pkt->senderState = NULL;
            pkt = big_pkt;
        }
    }

    _status = BaseSimpleCPU::Running;

    Fault fault = curStaticInst->completeAcc(pkt, threadInfo[curThread],
                                             traceData);

    // keep an instruction count
    if (fault == NoFault)
        countInst();
    else if (traceData) {
        // If there was a fault, we shouldn't trace this instruction.
        delete traceData;
        traceData = NULL;
    }

    delete pkt;

    postExecute();

    advanceInst(fault);
}

void
TimingSimpleCPU::updateCycleCounts()
{
    const Cycles delta(curCycle() - previousCycle);

    numCycles += delta;

    previousCycle = curCycle();
}

/*----------------------------------------------------------------------
 * Define dcache port behavior
 *--------------------------------------------------------------------*/

void
TimingSimpleCPU::DcachePort::recvTimingSnoopReq(PacketPtr pkt)
{
    for (ThreadID tid = 0; tid < cpu->numThreads; tid++) {
        if (cpu->getCpuAddrMonitor(tid)->doMonitor(pkt)) {
            cpu->wakeup(tid);
        }
    }

    // Making it uniform across all CPUs:
    // The CPUs need to be woken up only on an invalidation packet (when using caches)
    // or on an incoming write packet (when not using caches)
    // It is not necessary to wake up the processor on all incoming packets
    if (pkt->isInvalidate() || pkt->isWrite()) {
        for (auto &t_info : cpu->threadInfo) {
            TheISA::handleLockedSnoop(t_info->thread, pkt, cacheBlockMask);
        }
    }
}

void
TimingSimpleCPU::DcachePort::recvFunctionalSnoop(PacketPtr pkt)
{
    for (ThreadID tid = 0; tid < cpu->numThreads; tid++) {
        if (cpu->getCpuAddrMonitor(tid)->doMonitor(pkt)) {
            cpu->wakeup(tid);
        }
    }
}

bool
TimingSimpleCPU::DcachePort::recvTimingResp(PacketPtr pkt)
{
    DPRINTF(SimpleCPU, "Received load/store response %#x\n", pkt->getAddr());

    // The timing CPU is not really ticked, instead it relies on the
    // memory system (fetch and load/store) to set the pace.
    if (!tickEvent.scheduled()) {
        // Delay processing of returned data until next CPU clock edge
        tickEvent.schedule(pkt, cpu->clockEdge());
        return true;
    } else {
        // In the case of a split transaction and a cache that is
        // faster than a CPU we could get two responses in the
        // same tick, delay the second one
        if (!retryRespEvent.scheduled())
            cpu->schedule(retryRespEvent, cpu->clockEdge(Cycles(1)));
        return false;
    }
}

void
TimingSimpleCPU::DcachePort::DTickEvent::process()
{
    cpu->completeDataAccess(pkt);
}

void
TimingSimpleCPU::DcachePort::recvReqRetry()
{
    // we shouldn't get a retry unless we have a packet that we're
    // waiting to transmit
    assert(cpu->dcache_pkt != NULL);
    assert(cpu->_status == DcacheRetry);
    PacketPtr tmp = cpu->dcache_pkt;
    if (tmp->senderState) {
        // This is a packet from a split access.
        SplitFragmentSenderState * send_state =
            dynamic_cast<SplitFragmentSenderState *>(tmp->senderState);
        assert(send_state);
        PacketPtr big_pkt = send_state->bigPkt;

        SplitMainSenderState * main_send_state =
            dynamic_cast<SplitMainSenderState *>(big_pkt->senderState);
        assert(main_send_state);

        if (sendTimingReq(tmp)) {
            // If we were able to send without retrying, record that fact
            // and try sending the other fragment.
            send_state->clearFromParent();
            int other_index = main_send_state->getPendingFragment();
            if (other_index > 0) {
                tmp = main_send_state->fragments[other_index];
                cpu->dcache_pkt = tmp;
                if ((big_pkt->isRead() && cpu->handleReadPacket(tmp)) ||
                        (big_pkt->isWrite() && cpu->handleWritePacket())) {
                    main_send_state->fragments[other_index] = NULL;
                }
            } else {
                cpu->_status = DcacheWaitResponse;
                // memory system takes ownership of packet
                cpu->dcache_pkt = NULL;
            }
        }
    } else if (sendTimingReq(tmp)) {
        cpu->_status = DcacheWaitResponse;
        // memory system takes ownership of packet
        cpu->dcache_pkt = NULL;
    }
}

TimingSimpleCPU::IprEvent::IprEvent(Packet *_pkt, TimingSimpleCPU *_cpu,
    Tick t)
    : pkt(_pkt), cpu(_cpu)
{
    cpu->schedule(this, t);
}

void
TimingSimpleCPU::IprEvent::process()
{
    cpu->completeDataAccess(pkt);
}

const char *
TimingSimpleCPU::IprEvent::description() const
{
    return "Timing Simple CPU Delay IPR event";
}


void
TimingSimpleCPU::printAddr(Addr a)
{
    dcachePort.printAddr(a);
}


////////////////////////////////////////////////////////////////////////
//
//  TimingSimpleCPU Simulation Object
//
TimingSimpleCPU *
TimingSimpleCPUParams::create()
{
    return new TimingSimpleCPU(this);
}
