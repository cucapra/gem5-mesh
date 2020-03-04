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

    // whether we can recv a new packet next cycle?
    // TODO might need port retry interface to get this 0 cycle latency working


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
    void processHead(std::shared_ptr<MasterData> cmd);
    void enqueueCmd();
    // will have opening in queue by the end of this cycle
    bool willHaveOpening();

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
    
  
};

#endif
