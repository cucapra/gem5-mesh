//-----------------------------------------------------------------------------
// stage.hh
//-----------------------------------------------------------------------------
// Parent class for all stages in the io model

#ifndef __CPU_IO_STAGE_HH__
#define __CPU_IO_STAGE_HH__

#include <iomanip>
#include <sstream>
#include <queue>

#include "cpu/io/comm.hh"
#include "cpu/io/dyn_inst.hh"
#include "cpu/timebuf.hh"

class IOCPU;

class Stage {
    protected:
        /** Pointer to the main CPU */
        IOCPU* m_cpu_p;
    
        /** Is this stage active? */
        bool m_is_active;

        /** N-entry input instruction buffer */
        std::queue<IODynInstPtr> m_insts;

        /** Input queue's size */
        const size_t m_input_queue_size;

        /** Max number of credits. This is equal to the size of input buffer in the
        * next stage */
        const size_t m_max_num_credits;
        
        /** Number of credits for forward communication */
        size_t m_num_credits;
    
        /** the idx of this stage to lookup what the next stage should be */
        StageIdx m_stage_idx;
        
        /** whether that stage takes one or zero cycles (combinational) */
        bool m_is_sequential;
        
        /**
        * Time buffer interface
        */
        TimeBuffer<InstComm>::wire m_outgoing_inst_wire;     // to next stage
        TimeBuffer<InstComm>::wire m_incoming_inst_wire;     // from prev stage

        TimeBuffer<CreditComm>::wire m_outgoing_credit_wire; // to prev stage
        TimeBuffer<CreditComm>::wire m_incoming_credit_wire; // from next stage
        
        TimeBuffer<SquashComm>::wire m_outgoing_squash_wire; // to any
        TimeBuffer<SquashComm>::wire m_incoming_squash_wire; // from any
        
        TimeBuffer<InfoComm>::wire   m_outgoing_info_wire;  // to any
        TimeBuffer<InfoComm>::wire   m_incoming_info_wire; // from commit?
        
    
        /** Receive input instructions from the prev stage */
        virtual void queueInsts();
        
        /** Send output instructions to the next stage */
        virtual void sendInstToNextStage(IODynInstPtr inst);
    
        /** Consume an instruction and product credit for prev stage */
        virtual void consumeInst();

        /** Read new credits from the next stage */
        virtual void readCredits();
    
        /** check whether we are at ends of pipeline */
        bool hasNextStage();
        bool hasPrevStage();
    
        /** get the input and output instruction buffers */
        std::list<std::shared_ptr<IODynInst>> &inputInst();
        std::list<std::shared_ptr<IODynInst>> &outputInst();
    
        /** get the input and output credit buffers */
        size_t &inputCredit();
        size_t &outputCredit();
        
        /** method to check if we have any credits */
        bool nextStageRdy();
        
    public:
        Stage(IOCPU* _cpu_p, size_t inputBufSize, size_t outputBufSize,
                StageIdx stageIdx, bool isSequential);
        ~Stage() = default;
    
        /** Init (this is called after all CPU structures are created) */
        virtual void init() = 0;

        /** Return name of this stage object */
        virtual std::string name() const = 0;

        /** Register stats */
        virtual void regStats() = 0;
    
        /** Set incoming/outgoing communication wires */
        virtual void setCommBuffers(TimeBuffer<InstComm>& inst_buffer,
                       TimeBuffer<CreditComm>& credit_buffer,
                       TimeBuffer<SquashComm>& squash_buffer,
                       TimeBuffer<InfoComm>& info_buffer);
                       
        /** Main tick function */
        virtual void tick();

        /** Wake up this stage */
        virtual void wakeup();

        /** Suspend this stage */
        virtual void suspend();

        /** Line trace */
        virtual void linetrace(std::stringstream& ss) = 0;
        
};

#endif
