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
#include "cpu/io/stage.hh"

class Decode : public Stage
{
  public:
    Decode(IOCPU* _cpu_p, IOCPUParams* params);
    ~Decode() = default;

    /** Init (this is called after all CPU structures are created) */
    void init() override;

    /** Return name of this stage object */
    std::string name() const override;

    /** Register stats */
    void regStats() override;

    /** Main tick function */
    void tick() override;

    /** Wake up this stage */
    void wakeup() override;

    /** Suspend this stage */
    void suspend() override;

    /** Line trace */
    void linetrace(std::stringstream& ss) override;

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

    /** Check squash signal. Return true if this stage is squashed */
    bool checkSquash() override;

    /** Squash all instructions younger than the squash instruction */
    void doSquash(IODynInstPtr squash_inst);

    /** Initiate a squash signal due to branch misprediction */
    void initiateSquash(const IODynInstPtr& mispred_inst);

    /** Place the given instruction into the buffer to the next stage */
    void sendInstToNextStage(IODynInstPtr inst) override;

  private:  
    /** Max number of instructions that can be decoded in 1 cycle */
    size_t m_decode_width;
  
#ifdef DEBUG
    /** Stage's status (for line trace) */
    std::bitset<DecodeStatus::NumStatus> m_stage_status;

    /** List of instructions processed in the current cycle (for line trace) */
    std::vector<IODynInstPtr> m_decoded_insts;
#endif
};

#endif // CPU_IO_DECODE_HH
