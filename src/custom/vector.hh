#ifndef __CUSTOM_VECTOR_UNIT_HH__
#define __CUSTOM_VECTOR_UNIT_HH__

/*
 * Handles instruction communication over the mesh network:
 * sends, recvs, and syncs with neighbors.
 * 
 */ 

#include "cpu/io/stage.hh"
#include "custom/mesh_helper.hh"
#include "custom/mesh_ports.hh"

class Vector : public Stage {
  // inheritance
  public:
    Vector(IOCPU *_cpu_p, IOCPUParams *params, size_t in_size, size_t out_size,
            StageIdx stageType, bool canRootSend, bool canRecv);
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
    //void setRdy(bool rdy);
    
    // set appropriate mesh node ports as valid
    //void setVal(bool val);
    
    // check if we need to stall due to the internal pipeline
    bool isInternallyStalled();
    
    // get if this is configured beyond default behavior
    bool getConfigured();
    
    // if cpu allows idling, need to call this to make sure active when stuff to do
    // currently does nothing in IOCPU
    void signalActivity() {}
    
  protected:
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
    
    // create a dynamic instruction
    IODynInstPtr createInstruction(const MasterData &instInfo);
    
    // make a dynamic instruction then push it
    void pushPipeInstToNextStage(const MasterData &instInfo);
    void pushMeshInstToNextStage(const MasterData &instInfo);
    
    // master calls this to broadcast to neighbors
    void forwardInstruction(const MasterData &instInfo);
    
    // pull an instruction from the mesh or pipe
    void pullPipeInstruction(MasterData &instInfo);
    void pullMeshInstruction(MasterData &instInfo);
    
    // pass the instruction through this stage because not in vec mode
    // wont this add buffer slots in !sequentialMode which is undesirable?
    // need to reflect in credits
    void passInstructions();
    
    // extract (functional decode) the machinst to ExtMachInst
    StaticInstPtr extractInstruction(const TheISA::MachInst inst, TheISA::PCState& cur_pc);
    
    // create a packet to send over the mesh network
    PacketPtr createMeshPacket(RegVal payload);
    // cheat version for experiments
    PacketPtr createMeshPacket(const MasterData &inst);
    
    // reset all mesh node port settings
    void resetActive();
    
    // TEMP get an instruction from the master
    std::shared_ptr<MasterData> getMeshPortInst(Mesh_Dir dir);
    
    // get received data from the specified mesh port
    uint64_t getMeshPortData(Mesh_Dir dir);
    
    // get the received packet
    PacketPtr getMeshPortPkt(Mesh_Dir dir);
    
    // get a machinst from the local fetch2
    IODynInstPtr getFetchInput();
    
    // check if this stage can proceed
    //bool shouldStall();
    
    // is stalled due to mesh
    //bool meshStalled();
    
    // encode + decode of information over mesh net
    // requires 32 + 1 = 33bits currently
    //Minor::ForwardVectorData decodeMeshData(uint64_t data);
    //uint64_t encodeMeshData(const Minor::ForwardVectorData &instInfo);
    
    // take credits from previous stage, stage should have 0 credits after this
    void stealCredits();
    
    // return credits to previous stage that were stolen
    // stage should have m_max_num_credits after this
    void restoreCredits();
    
    // get references to the master and slave ports owned by CPU
    std::vector<ToMeshPort>& getMeshMasterPorts();
    std::vector<FromMeshPort>& getMeshSlavePorts();

    // profile any stalling
    void profile();

    // PC gen for uop decomposition
    // I think this would be in the normal fetch PC GEN in real RTL
    // but easier to just replicate in the game5 model
    // don't need to unstall fetch or worry about ordering
    // can also try to do functionalReq to get the icache resp immedietly as if did last cycle?
    // fine as long as assume warmed up cache and not enough microops to ever warrant icache miss <4kB
    void setPCGen(TheISA::PCState issuePC, int cnt);
    bool isPCGenActive();
    IODynInstPtr nextAtomicInstFetch();
    RiscvISA::MachInst doICacheFuncRead(int tid, Addr instAddr, int fetchSize);
    int extractInstCntFromVissue(IODynInstPtr inst);

   public:
    // helpers to figure out settings of this stage
    bool isRootMaster();
    bool isMaster();
    bool isSlave();
    bool canWriteMesh();
    bool canReadMesh();
    bool isOutPipeStalled();
    bool isInPipeStalled();
    bool isOutMeshStalled();
    bool isInMeshStalled();
    bool isDecoupledAccess() { return MeshHelper::isDecoupledAccess(_curCsrVal); };
    bool hasForwardingPath();
protected:
    // instructions can come from either I$ or mesh and can be send to mesh or pipe (fully connected)
    typedef enum InstSource {
        None = 0,
        Pipeline,
        Mesh
    } InstSource;
    
    InstSource getOutMeshSource();
    InstSource getOutPipeSource();
    
    void handleRevec(const IODynInstPtr pipeInst, const IODynInstPtr meshInst);
    bool pipeHasRevec();
    bool meshHasRevec();
    int getPipeRevec();
    int getMeshRevec();
    void setPipeRevec(int val);
    void setMeshRevec(int val);
    void resetPipeRevec();
    void resetMeshRevec();
    bool getRevecStall();
    
  public:
    bool isCurDiverged();
    
    // get current vector length
    int getXLen();
    int getYLen();

    // get current vector group origin
    int getXOrigin();
    int getYOrigin();
     
  protected:
  
    // define the ports we're going to use for to access the mesh net
    //std::vector<ToMeshPort> _toMeshPort;
    //std::vector<FromMeshPort> _fromMeshPort;
    
    // number of ports currently actively communicating
    int _numInPortsActive;
    int _numOutPortsActive;

    // the stage this vector unit is representing
    SensitiveStage _stage;
  
    // cache the most recently set csr value
    RegVal _curCsrVal;
    
    // need to steal credits to force stall the previous stage
    // effectively the mesh network has these credits
    int _stolenCredits;
    
    // if this core is the root master then it sends
    bool _canRootSend;
    
    // this stage does not recv from mesh net
    bool _canRecv;
    
    // keep track for debug
    int _numInstructions;
    
    // whether pass through mode is enabled where operates are usual, but still sends instructions
    bool _vecPassThrough;
    
    // whether we've seen a revec instruction and whats it's value (?)
    int _meshRevecId;
    int _pipeRevecId;
    
    // count the number of revecs we've recved in the past
    //int _revecCntr;

    // the current uop PC
    TheISA::PCState _uopPC;
    // number of uops to get before completion (<8 or so, can make hardware small)
    int _uopIssueLen;
    // the current number of uops
    int _uopCnt;
    
    // TEMP to make all relevant info available
    struct SenderState : public Packet::SenderState
    {
      // important this is shared_ptr so wont be freed while packet 'in-flight'
      std::shared_ptr<MasterData> master_data;
      SenderState(std::shared_ptr<MasterData> _master_data)
        : master_data(_master_data)
      { }
    };

  private:
    /** stats */
    Stats::Scalar m_revec_stalls;
    Stats::Scalar m_backpressure_stalls;
    Stats::Scalar m_no_mesh_stalls;
    Stats::Scalar m_no_pipe_stalls;
  
};

#endif
