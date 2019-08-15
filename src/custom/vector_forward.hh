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
        Minor::Latch<Minor::ForwardInstData>::Input out);
  
    // MinorDynInstPtr from Fetch2?
      // MachInst only exists in Fetch2
    // MinorDynInstPtr from Decode?
    
    // I think should be called from fetch2 and inject into decode
    
    // master calls this to broadcast to neighbors
    void pushInstruction(const TheISA::MachInst inst);
    
    // slave calls this to get an instruction from the mesh
    // the instruction must be decoded internally
    StaticInstPtr pullInstruction();
    
    // setup which mesh ports are active
    void setupConfig(RegVal csrVal);
    
    // tells the cpu whether it needs to stall or not
    bool isMeshSynced();
  
  protected:
    // give the instruction to a slave core once at local node
    //void giveInstruction(const TheISA::ExtMachInst inst);
    
    // extract (functional decode) the machinst to ExtMachInst
    StaticInstPtr extractInstruction(const TheISA::MachInst inst);
    
    // create a packet to send over the mesh network
    PacketPtr createMeshPacket(RegVal payload);
    
    // check if the output nodes are ready
    bool getOutRdy();
    
    // check if the input nodes are valid
    bool getInVal();
    
    // set appropriate mesh node as ready
    void setRdy(bool rdy);
    
    // set appropriate mesh node ports as valid
    void setVal(bool val);
    
    // reset all mesh node port settings
    void resetActive();
    
    // the fsm is sensitive to status updates in neighboring nodes
    // update it when neccesary
    void neighborUpdate();
    
    // inform neighboring nodes we had a status change and should attempt
    // a new state for their local fsms
    void informNeighbors();
    
    // get received data from the specified mesh port
    uint64_t getMeshPortData(Mesh_Dir dir);
    
    // get the received packet
    PacketPtr getMeshPortPkt(Mesh_Dir dir);
     
  protected:
    
    // define the ports we're going to use for to access the mesh net
    std::vector<ToMeshPort> _toMeshPort;
    std::vector<FromMeshPort> _fromMeshPort;
    
    // number of ports currently actively communicating
    int _numOutPortsActive;
    int _numInPortsActive;
    
    // finite state machine to know when to send an receive
    std::shared_ptr<EventDrivenFSM> _fsm;
    
    // the stage this vector unit is representing
    SensitiveStage _stage;
    
    // cache the most recently set csr value
    RegVal _curCsrVal;
  
    // Pointer back to the containing CPU
    MinorCPU &_cpu;
  
    // Output port carrying instructions into Decode
    Minor::Latch<Minor::ForwardInstData>::Input _out;
    
    
  
};










#endif
