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

#include "cpu/minor/buffers.hh"
#include "cpu/minor/cpu.hh"
#include "cpu/minor/pipe_data.hh"

#include "custom/mesh_helper.hh"
#include "custom/event_driven_fsm.hh"
#include "custom/mesh_ports.hh"


class VectorForward : public Named {
  public:
    VectorForward(const std::string &name,
        MinorCPU &cpu,
        MinorCPUParams &params,
        Minor::Latch<Minor::ForwardInstData>::Input out,
        std::vector<Minor::InputBuffer<Minor::ForwardInstData>> &nextStageReserve)
      ;
  
    // MinorDynInstPtr from Fetch2?
      // MachInst only exists in Fetch2
    // MinorDynInstPtr from Decode?
    
    // I think should be called from fetch2 and inject into decode
    
    // master calls this to broadcast to neighbors
    void forwardInstruction(const TheISA::MachInst inst);
    
    // like a pipeline stage, tick this and pass stuff on to the next stage
    // if possible. this should mimic fetch2 somewhat
    void evaluate();
    
    // setup which mesh ports are active
    void setupConfig(RegVal csrVal);
    
    // tells the cpu in fetch2 or decode(?) whether it needs to stall or not
    // before _effectively_ issuing the current instruction
    // either the mesh is configured and active, or the mesh isn't active
    //bool canIssue();
    
    // cpu needs to bind ports to names, so expose them via a reference
    Port &getMeshPort(int idx, bool isOut);
    int getNumMeshPorts();
  
  
    // used just by mesh ports (TODO friend class??)
  
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
  
    // the communication channel between fetch2 and vector
    std::vector<Minor::InputBuffer<Minor::ForwardVectorData>>& getInputBuf();
  protected:
    
    // give the instruction to a slave core once at local node
    // we will write to the input latch between this unit and decode
    // decode will take the instruction in the exact same way as it would
    // from the regular fetch2
    // 
    // the instruction must be decoded internally
    
    // create a dynamic instruction
    Minor::MinorDynInstPtr createInstruction(const TheISA::MachInst instWord);
    
    // push output to the next stage
    void pushToNextStage(const Minor::MinorDynInstPtr);
    
    // call the two things above to make a dynamic instruction then push it
    void pushInstToNextStage(const TheISA::MachInst instWord);
    
    // pull an instruction from the mesh network (figure out the right dir)
    TheISA::MachInst pullInstruction();
    
    // extract (functional decode) the machinst to ExtMachInst
    StaticInstPtr extractInstruction(const TheISA::MachInst inst);
    
    // create a packet to send over the mesh network
    PacketPtr createMeshPacket(RegVal payload);
    
    // reset all mesh node port settings
    void resetActive();
    
    // get received data from the specified mesh port
    uint64_t getMeshPortData(Mesh_Dir dir);
    
    // get the received packet
    PacketPtr getMeshPortPkt(Mesh_Dir dir);
    
    // get a machinst from the local fetch2
    TheISA::MachInst getFetchInput(ThreadID tid);
    
    // mark the fetch2 input as processed so that it can push more stuff
    void popFetchInput(ThreadID tid);
     
  protected:
    
    // Pointer back to the containing CPU
    MinorCPU &_cpu;
  
    // Output port carrying instructions into Decode
    Minor::Latch<Minor::ForwardInstData>::Input _out;
    
    // Interface to reserve space in the next stage // ?
    std::vector<Minor::InputBuffer<Minor::ForwardInstData>> &_nextStageReserve;
    
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
    
    // communication channel between fetch2 and vector
    std::vector<Minor::InputBuffer<Minor::ForwardVectorData>> _inputBuffer;
  
};

#endif
