#ifndef __CUSTOM_VECTOR_UNIT_HH__
#define __CUSTOM_VECTOR_UNIT_HH__

/*
 * Handles insturction communication over the mesh network:
 * sends, recvs, and syncs with neighbors.
 * 
 * This is a pseudo-pipeline stage in the sense that it fires in a 
 * logical order with the other pipeline stages in the Minor CPU model.
 * 
 * This will fire before the the stage that needs its results so that
 * zero cycle communication can be resolved (as in pipeline.cc)
 */ 

#include <functional>

#include "cpu/io/stage.hh"
#include "custom/mesh_helper.hh"
#include "custom/event_driven_fsm.hh"
#include "custom/mesh_ports.hh"

class Vector : public Stage {
  // inheritance
  public:
    Vector(IOCPU *_cpu_p, IOCPUParams *params);
    ~Vector() = default;

    /** Init (this is called after all CPU structures are created) */
    void init() override;

    /** Return name of this stage object */
    std::string name() const override;

    /** Register stats */
    void regStats() override;

    /** Main tick function */
    void tick() override;
    
    /** Line trace */
    void linetrace(std::stringstream& ss) override;

  // inheritance
  protected:
    /** Place the given instruction into the buffer to the next stage */
    void sendInstToNextStage(IODynInstPtr inst) override;
    
    /** Squash all instructions younger than the given squash instruction */
    void doSquash(SquashComm::BaseSquash &squashInfo, StageIdx initiator) override;

  public:
    
    // setup which mesh ports are active
    void setupConfig(int csrId, RegVal csrVal);
    
    // cpu needs to bind ports to names, so expose them via a reference
    Port &getMeshPort(int idx, bool isOut);
    int getNumMeshPorts();
  
    // check if the output nodes are ready
    bool getOutRdy();
    
    // check if the input nodes are valid
    bool getInVal();
    
    // the fsm is sensitive to status updates in neighboring nodes
    // update it when neccesary
    void neighborUpdate();
    
    // get num ports being used
    int getNumPortsActive();
    
    // inform neighboring nodes we had a status change and should attempt
    // a new state for their local fsms
    void informNeighbors();
    
    // set appropriate mesh node as ready
    void setRdy(bool rdy);
    
    // set appropriate mesh node ports as valid
    void setVal(bool val);
    
    // check if we need to stall due to the internal pipeline
    bool isInternallyStalled();
    
    // get if this is configured beyond default behavior
    bool getConfigured();
    
    // communicate that there was a branch misprediction in fetch, the next
    // instruction transmitted should include a bit to let slaves know
    //void setMispredict();
    
    // if cpu allows idling, need to call this to make sure active when stuff to do
    // currently does nothing in IOCPU
    void signalActivity() {}
    
  protected:
    // information to use to create local IODynInst
    // currenlty cheating and using all possible info
    struct MasterData {
        IODynInstPtr inst;
    };
    
    // create a dynamic instruction
    IODynInstPtr createInstruction(const MasterData &instInfo);
    
    // make a dynamic instruction then push it
    void pushInstToNextStage(const MasterData &instInfo);
    
    // master calls this to broadcast to neighbors
    void forwardInstruction(const MasterData &instInfo);
    
    // pull an instruction from the mesh network (figure out the right dir)
    void pullInstruction(MasterData &instInfo);
    
    // pass the instruction through this stage because not in vec mode
    // wont this add buffer slots in !sequentialMode which is undesirable?
    // need to reflect in credits
    void passInstructions();
    
    // extract (functional decode) the machinst to ExtMachInst
    StaticInstPtr extractInstruction(const TheISA::MachInst inst);
    
    // create a packet to send over the mesh network
    PacketPtr createMeshPacket(RegVal payload);
    // cheat version for experiments
    PacketPtr createMeshPacket(IODynInstPtr inst);
    
    // reset all mesh node port settings
    void resetActive();
    
    // TEMP get an instruction from the master
    IODynInstPtr getMeshPortInst(Mesh_Dir dir);
    
    // get received data from the specified mesh port
    uint64_t getMeshPortData(Mesh_Dir dir);
    
    // get the received packet
    PacketPtr getMeshPortPkt(Mesh_Dir dir);
    
    // get a machinst from the local fetch2
    IODynInstPtr getFetchInput();
    
    // mark the fetch2 input as processed so that it can push more stuff
    //void popFetchInput();
    
    // check if there is a new internal stall this cycle from fetch2 or decode
    void processInternalStalls();
    
    // check if this stage can proceed
    bool shouldStall();
    
    // encode + decode of information over mesh net
    // requires 32 + 1 = 33bits currently
    //Minor::ForwardVectorData decodeMeshData(uint64_t data);
    //uint64_t encodeMeshData(const Minor::ForwardVectorData &instInfo);
    
    //int getStreamSeqNum();
    
    // when setup a configuration, may need to stall or unstall the frontend
    void stallFetchInput(ThreadID tid);
    void unstallFetchInput(ThreadID tid);

    // handle when there is a misprediction forwarded from the master
    //void handleMispredict();
     
  protected:
  
    // define the ports we're going to use for to access the mesh net
    std::vector<ToMeshPort> _toMeshPort;
    std::vector<FromMeshPort> _fromMeshPort;
    
    // number of ports currently actively communicating
    int _numInPortsActive;
    int _numOutPortsActive;

    // the stage this vector unit is representing
    SensitiveStage _stage;
    
    // finite state machine to know when to send an receive
    std::shared_ptr<EventDrivenFSM> _fsm;
  
    // cache the most recently set csr value
    RegVal _curCsrVal;
    
    // remember the last stream seq num. 
    // this is how many branches there has been?
    //InstSeqNum _lastStreamSeqNum;
    
    // if stalled last cycle
    bool _wasStalled;
    
    // do we receive input this cycle, buffer will be cleared before state 
    // machine will check so need to save
    //bool _internalInputThisCycle;
    
    // should not set the misprediction for this cycle b/c the instruction sent
    // would have been fetch without knowing?
    //EventFunctionWrapper _mispredictUpdate;
    
    // TEMP to make all relevant info available
    struct SenderState : public Packet::SenderState
    {
      // important this is shared_ptr so wont be freed while packet 'in-flight'
      std::shared_ptr<IODynInst> master_inst;
      SenderState(IODynInstPtr _master_inst)
        : master_inst(_master_inst)
      { }
    };

    
  public:
    // TEMP?
    // Minor execute stage expects a stream seq number (the number of branches I believe)
    // need to cache this before config starts
    //void updateStreamSeqNum(InstSeqNum seqNum);
    
    // whether we sent anything this cycle, will dictate what state we go into
    // on stall (STALL_VAL or STALL_NOT_VAL)
    //bool sentMsgThisCycle();
  
};

#endif
