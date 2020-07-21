#ifndef __CUSTOM_VEC_INST_SEL_HH__
#define __CUSTOM_VEC_INST_SEL_HH__

/*
 * Handles storage of recv instructions/pcs from the mesh net
 */ 

#include "cpu/io/stage.hh"
#include "custom/mesh_helper.hh"
#include "custom/mesh_ports.hh"
#include <queue>

class IOCPU;

class VecInstSel : public Named {
  public:
    // TODO rename vecCmd
    // information to use to create local IODynInst
    // currenlty cheating and using all possible info
    struct MasterData {
        IODynInstPtr inst;
        bool isInst; // inst or PC
        TheISA::PCState pc;
        int recvCnt;

        // sending an instruction
        MasterData(IODynInstPtr inst) {
          this->inst = inst;
          isInst = true;
        }

        // sending a PC
        MasterData(TheISA::PCState pc) {
          this->pc = pc;
          isInst = false;
        }

        MasterData() {

        }
    };


  public:
    // constructor
    VecInstSel(IOCPU *_cpu_p, IOCPUParams *params);

    // enqueue an instruction or packet from port
    // ret whether there is an open space
    bool enqueueTiming(PacketPtr pkt);

    // whether this can recv a packet this cycle
    bool getRdy();

    // dequeue an instruction (either from icache or mesh, but unknown to caller and frankly does not matter)
    IODynInstPtr dequeueInst();

    // check if has any vecCmd at head
    bool getVal();

    // reset this unit
    void reset();

    // resp from the icache
    void recvIcacheResp(PacketPtr pkt);

    // squash due to a branch, update the local PC and allow to continue fetching
    void doSquash(SquashComm::BaseSquash &squashInfo, StageIdx initiator);

    // register stats for this
    void regStats(std::string parentName);

    // actually do profiling for the cycle, called from vector stage
    void profile();

    void resetStallWait();

  protected:
    // PC gen for uop decomposition
    // I think this would be in the normal fetch PC GEN in real RTL
    // but easier to just replicate in the game5 model
    // don't need to unstall fetch or worry about ordering
    // can also try to do functionalReq to get the icache resp immedietly as if did last cycle?
    // fine as long as assume warmed up cache and not enough microops to ever warrant icache miss <4kB
    void setPCGen(TheISA::PCState issuePC, int cnt);
    bool isPCGenActive();
    void tryReqNextUop();
    void sendICacheReq(int tid, Addr instAddr);
    int extractInstCntFromVissue(IODynInstPtr inst);
    void processHead();
    void enqueueCmd();
    // will have opening in queue by the end of this cycle
    bool willHaveOpening();

    void cleanCurIssueBlock();

  protected:
    // cpu
    IOCPU *m_cpu_p;

    // the current uop PC
    TheISA::PCState _uopPC;
    // number of uops to get before completion (<8 or so, can make hardware small)
    int _uopIssueLen;
    // the current number of uops
    int _uopCnt;

    // queue to store pending vec commands for this core
    std::queue<std::shared_ptr<MasterData>> _vecCmds;

    // max number of vec cmds available
    int _maxVecCmds;

    // current mach_inst (in case we need to build it due to compressed instructions)
    // NOTE we are going to not compile with compressed instructions anymore. 
    // hammerblade doesn't do it and honestly just a pain in the butt
    // RiscvISA::MachInst _curMachAcc;

    // built icache instruction
    IODynInstPtr _lastICacheResp;

    // whether there is a pending icache req
    Addr _pendingICacheReqAddr;
    bool _pendingICacheReq;

    // trasient vec cmd to be enqueue 
    std::shared_ptr<MasterData> _toEnqueue;

    // event to enqueue
    EventFunctionWrapper _enqueueEvent;

    int _tempREMEMS;
    int _tempBlocksRecv;
    int _tempBlocksPopped;

    Tick _lastSendTick;

    // flag to stall until recv stall signal for the next PC
    // this is set by a feedback signal from decode
    bool _stallUntilJumpPC;

    // waiting for terminator
    // currently allow either a count or a terminating instruction to signal the end of a block
    bool _waitingForTerminator;
    // flag to signal the end of the block
    bool _terminatorFound;

    // if we failed to send a req, then save it to send later
    // in a real system don't think would ever fail here though??
    bool _pendingFailedReq;
    Addr _failedReqVirtAddr;

    Stats::Vector2d MeshQueueSize;
    Stats::Scalar   NoFetchedInst;
    Stats::Scalar   TryFetchAgain;
    Stats::Scalar   StallsOnControl;
    Stats::Scalar   AlreadyInstruction;
    Stats::Scalar   DequeueReqs;
    Tick lastReqNoInst;
  
};

#endif
