#ifndef __CUSTOM_VECTOR_UNIT_HH__
#define __CUSTOM_VECTOR_UNIT_HH__

/*
 * Handles instruction communication over the mesh network:
 * sends, recvs, and syncs with neighbors.
 * 
 * Authors: Philip Bedoukian
 */ 

#include "cpu/io/stage.hh"
#include "custom/mesh_helper.hh"
#include "custom/mesh_ports.hh"
#include "custom/vec_inst_sel.hh"

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
    
    // check if we need to stall due to the internal pipeline
    bool isInternallyStalled();
    
    // get if this is configured beyond default behavior
    bool getConfigured();
    
    // if cpu allows idling, need to call this to make sure active when stuff to do
    // currently does nothing in IOCPU
    void signalActivity() {}
    
    VecInstSel &getVecInstSel() { return _vecUops; }

  protected:

    // create a dynamic instruction
    IODynInstPtr createInstruction(const IODynInstPtr &instInfo);
    
    // make a dynamic instruction then push it
    void pushPipeInstToNextStage(const IODynInstPtr &instInfo);
    void pushMeshInstToNextStage(const IODynInstPtr &instInfo);
    
    // master calls this to broadcast to neighbors
    void forwardInstruction(const IODynInstPtr &instInfo);
    
    // pull an instruction from the mesh or pipe
    void pullPipeInstruction(IODynInstPtr &instInfo);
    void pullMeshInstruction(IODynInstPtr &instInfo);
    
    // pass the instruction through this stage because not in vec mode
    // wont this add buffer slots in !sequentialMode which is undesirable?
    // need to reflect in credits
    void passInstructions();
    
    // extract (functional decode) the machinst to ExtMachInst
    StaticInstPtr extractInstruction(const TheISA::MachInst inst, TheISA::PCState& cur_pc);
    
    // create a packet to send over the mesh network
    PacketPtr createMeshPacket(RegVal payload);
    // cheat version for experiments
    PacketPtr createMeshPacket(const VecInstSel::MasterData &inst);
    
    // reset all mesh node port settings
    void resetActive();
    
    // get received data from the specified mesh port
    uint64_t getMeshPortData(Mesh_Dir dir);
    
    // get the received packet
    PacketPtr getMeshPortPkt(Mesh_Dir dir);
    
    // get a machinst from the local fetch2
    IODynInstPtr getFetchInput();
    
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

    // if this stage is going to stall in the current cycle
    bool canPullMesh();
    bool canPullPipe();
    bool canPushMesh();
    bool canPushPipe();

    bool canRecvMeshPkt();
    bool enqueueMeshPkt(PacketPtr pkt);
    void recvICacheResp(PacketPtr pkt);
     
  protected:
    
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

    // unit to provide instructions from the mesh either full inst or uop from fetch it did
    VecInstSel _vecUops;
  public:
    // TEMP to make all relevant info available
    struct SenderState : public Packet::SenderState
    {
      // important this is shared_ptr so wont be freed while packet 'in-flight'
      std::shared_ptr<VecInstSel::MasterData> master_data;
      SenderState(std::shared_ptr<VecInstSel::MasterData> _master_data)
        : master_data(_master_data)
      { }
    };

  private:
    /** stats */
    Stats::Scalar m_revec_stalls;
    Stats::Scalar m_backpressure_stalls;
    Stats::Scalar m_no_mesh_stalls;
    Stats::Scalar m_no_pipe_stalls;
    Stats::Scalar m_cycles_in_vec;
  
};

#endif
