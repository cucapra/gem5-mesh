/*
  Increaess forwarding bandwidth intot he network

*/


#ifndef __MEM_RUBY_FORWARDER_HH__
#define __MEM_RUBY_FORWARDER_HH__

#include <deque>
#include <unordered_map>
#include <queue>

#include "mem/packet.hh"
#include "mem/port.hh"
#include "mem/ruby/slicc_interface/AbstractController.hh"
#include "mem/ruby/system/RubySystem.hh"
#include "params/Forwarder.hh"

class Forwarder : public AbstractController
{
  public:
    typedef ForwarderParams Params;
    Forwarder(const Params* p);
    ~Forwarder();

    /**
     * Initialize network queues from/to Ruby network
     */
    void initNetQueues() override;

    /**
     * Wakeup scratchpad when there're incoming messages from network
     */
    void wakeup() override;

    /**
     * Print out network port's name
     */
    void print(std::ostream& out) const override;

    /**
     * setup stats for this scratchpad
     */ 
    void regStats() override;

    /**
     * unused inherited functions
     */

    void resetStats() override { }

    MessageBuffer* getMandatoryQueue() const override
    { return nullptr; }

    MessageBuffer* getMemoryQueue() const override
    { return nullptr; }

    AccessPermission getAccessPermission(const Addr &addr) override
    { return AccessPermission_Invalid; }

    void recordCacheTrace(int cntrl, CacheRecorder* tr) override
    { }

    Sequencer* getCPUSequencer() const override
    { return nullptr; }

    GPUCoalescer* getGPUCoalescer() const override
    { return nullptr; }

    bool functionalRead(const Addr &addr, PacketPtr pkt) override
    { warn("Fowarder does not support functionalRead\n"); return false; }

    int functionalWriteBuffers(PacketPtr& pkt) override;

    int functionalWrite(const Addr &addr, PacketPtr pkt) override
    { warn("Forwarder does not support functionalWrite\n"); return false; }

    void collateStats() override
    { warn("Forwarder does not support collateStats()\n"); }

  private:
    /**
     * Pointer to Ruby system
     */
    RubySystem* m_ruby_system_p;

    /**
     * Ruby network buffers
     */
    MessageBuffer* m_cache_forward_buffer_p;   // cache to this
    MessageBuffer* m_net_response_buffer_p;    // this to network

};

#endif // MEM_RUBY_SCRATCHPAD_HH
