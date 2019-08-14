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


class VectorForward : public Named {
  public:
    VectorForward(const std::string &name,
        MinorCPU &cpu_,
        MinorCPUParams &params,
        Latch<ForwardInstData>::Input out_);
  
    // MinorDynInstPtr from Fetch2?
      // MachInst only exists in Fetch2
    // MinorDynInstPtr from Decode?
    
    // I think should be called from fetch2 and inject into decode
    
    // master calls this to broadcast to neighbors
    void pushInstruction(const TheISA::MachInst inst);
    
    // setup which mesh ports are active
    void setupConfig(RegVal csrVal);
  
  protected:
    // give the instruction to a slave core once at local node
    void giveInstruction(const TheISA::ExtMachInst inst);
    
    // extract (functional decode) the machinst to ExtMachInst
    TheISA::ExtMachInst extractInstruction(const TheISA::MachInst inst);
    
    // define the ports we're going to use for to access the mesh net
    std::vector<ToMeshPort> _toMeshPort;
    std::vector<FromMeshPort> _fromMeshPort;
    
    // number of ports currently actively communicating
    int numOutPortsActive;
    int numInPortsActive;
    
    // finite state machine to know when to send an receive
    std::shared_ptr<EventDrivenFSM>> _fsm;
  

  
}










#endif
