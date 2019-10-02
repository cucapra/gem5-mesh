//=============================================================================
// rob.hh
//=============================================================================
// Re-order buffer for IO CPU
//
// Author: Tuan Ta
// Date:   19/08/28

#ifndef __CPU_IO_ROB_HH__
#define __CPU_IO_ROB_HH__

#include <list>

#include "cpu/io/dyn_inst.hh"
#include "params/IOCPU.hh"

class ROB
{
  public:
    ROB(IOCPUParams* params);
    ~ROB() = default;

    /** Check if ROB is empty */
    bool isEmpty() const;

    /** Check if ROB is full */
    bool isFull() const;

    /** Is head instruction ready to commit? */
    bool isHeadReady() const;

    /** Push the given instruction to the back of ROB */
    void push(IODynInstPtr inst);

    /** Commit and pop head instruction out of ROB */
    void commitHead();

    /** Squash instructions younger than the given squash_inst */
    void squash(IODynInstPtr squash_inst);

    /** Get head instruction */
    IODynInstPtr getHead();

    /** Return true if the given instruction exists in ROB */
    bool hasInst(IODynInstPtr inst) const;

    /** Return the number of memory instructions in ROB */
    size_t getMemInstCount() const;

  private:
    /** ROB size */
    size_t m_size;

    /** List of instructions */
    std::list<IODynInstPtr> m_inst_list;
};

#endif // __CPU_IO_ROB_HH__
