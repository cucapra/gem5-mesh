//-----------------------------------------------------------------------------
// stage.hh
//-----------------------------------------------------------------------------
// Parent class for all stages in the io model
// Authors: Tuan Ta
//          Philip Bedoukian

#ifndef __CPU_IO_STAGE_HH__
#define __CPU_IO_STAGE_HH__

#include <iomanip>
#include <sstream>
#include <queue>
#include <functional>

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
        const int m_max_num_credits;
        
        /** Number of credits for forward communication */
        int m_num_credits;
    
        /** the idx of this stage to lookup what the next stage should be */
        StageIdx m_stage_idx;
       
        /** the idx of the next stage you should send insts to. cached for quick lookup */
        StageIdx m_next_stage_inst_idx;
        
        /** the idx of the next stage to get credits from. cached for quick lookup */
        StageIdx m_next_stage_credit_idx;

        /** stages that can squash this one. cached for quick lookup */
        std::vector<StageIdx> m_squashing_stages;

        /** whether that stage takes one (sequential) or zero cycles (combinational) */
        bool m_is_sequential;
        
        /** this is hack to allow combinational stages to stall previous sequential stages 
         * the flag prevents this stage from reclaiming credits */
        bool m_is_unemployed;

        /**
        * Check whether any sequential stages are ahead
        */
        //bool m_any_seq_stage_after;
        
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
        int &inputCredit();
        int &outputCredit();
        
        /** Check if we have to stall because there are not credits available */
        virtual bool checkStall();
        
        /** Check squash signal (and do squash). Return true if this stage is squashed */
        // TODO can we get a nice refactor on this that checks only future stages in reverse order?
        // will need to incorporate lambdas for diff case, but could be nice!
        virtual bool checkSquash();
        
        // TODO another refactor where checkSquash return who squashed instead of calling this directly
        virtual void doSquash(SquashComm::BaseSquash &squashInfo, StageIdx initiator) = 0;
        
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
        
        /** whether this is an actual sequntial stage or pretend 0 delay combinational stage*/
        bool isSequential() const { return m_is_sequential; }
        
        /** hack to force core to stall. either requires -ve credits if seq stages 
         * or a wire if same stage but diff pseudo stages */
        void setUnemployed(bool val) { m_is_unemployed = val; }
        
};

#endif
