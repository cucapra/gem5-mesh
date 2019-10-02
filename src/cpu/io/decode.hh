//-----------------------------------------------------------------------------
// decode.hh
//-----------------------------------------------------------------------------
// Decode stage for IO CPU
//
// Author: Tuan Ta
// Date:   19/08/20

#ifndef __CPU_IO_DECODE_HH__
#define __CPU_IO_DECODE_HH__

#include <iomanip>
#include <queue>
#include <sstream>

#include "cpu/inst_seq.hh"
#include "cpu/io/comm.hh"
#include "cpu/io/dyn_inst.hh"
#include "cpu/timebuf.hh"
#include "params/IOCPU.hh"

class IOCPU;

class Decode
{
  public:
    Decode(IOCPU* _cpu_p, IOCPUParams* params);
    ~Decode() = default;

    /** Init (this is called after all CPU structures are created) */
    void init();

    /** Return name of this stage object */
    std::string name() const;

    /** Register stats */
    void regStats();

    /** Set incoming/outgoing communication wires */
    void setCommBuffers(TimeBuffer<InstComm>& inst_buffer,
                        TimeBuffer<CreditComm>& credit_buffer,
                        TimeBuffer<SquashComm>& squash_buffer,
                        TimeBuffer<InfoComm>& info_buffer);

    /** Main tick function */
    void tick();

    /** Wake up this stage */
    void wakeup();

    /** Suspend this stage */
    void suspend();

    /** Line trace */
    void linetrace(std::stringstream& ss);

  private:
    enum DecodeStatus {
      Squashed,
      InitSquash,
      Stalled,
      Busy,
      NumStatus
    };

  private:
    /** Do decode */
    void doDecode();

    /** Put all instructions to be processed this cycle into m_insts queue */
    void queueInsts();

    /** Check squash signal. Return true if this stage is squashed */
    bool checkSquash();

    /** Read credit signal */
    void readCredits();

    /** Squash all instructions younger than the squash instruction */
    void doSquash(IODynInstPtr squash_inst);

    /** Initiate a squash signal due to branch misprediction */
    void initiateSquash(const IODynInstPtr& mispred_inst);

    /** Place the given instruction into the buffer to the next stage */
    void sendInstToNextStage(IODynInstPtr inst);

  private:
    /** Pointer to the main CPU */
    IOCPU* m_cpu_p;

    /** Is this stage active? */
    bool m_is_active;

    /** N-entry input instruction buffer */
    std::queue<IODynInstPtr> m_insts;

    /** Input queue's size */
    const size_t m_input_queue_size;

    /** Max number of instructions that can be decoded in 1 cycle */
    size_t m_decode_width;

    /** Max number of credits. This is equal to the size of input buffer in the
     * next stage */
    const size_t m_max_num_credits;

    /** Number of credits for forward communication */
    size_t m_num_credits;

    /**
     * Time buffer interface
     */
    TimeBuffer<InstComm>::wire m_outgoing_inst_wire;     // to Rename
    TimeBuffer<InstComm>::wire m_incoming_inst_wire;     // from Fetch

    TimeBuffer<CreditComm>::wire m_outgoing_credit_wire;   // to Fetch
    TimeBuffer<CreditComm>::wire m_incoming_credit_wire;   // from Rename

    TimeBuffer<SquashComm>::wire m_outgoing_squash_wire; // to Fetch
    TimeBuffer<SquashComm>::wire m_incoming_squash_wire; // from IEW/Commit

#ifdef DEBUG
    /** Stage's status (for line trace) */
    std::bitset<DecodeStatus::NumStatus> m_stage_status;

    /** List of instructions processed in the current cycle (for line trace) */
    std::vector<IODynInstPtr> m_decoded_insts;
#endif
};

#endif // CPU_IO_DECODE_HH
