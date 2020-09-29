#include "mem/ruby/scratchpad/Forwarder.hh"

#include <algorithm>
#include <cmath>

#include "mem/protocol/LLCRequestMsg.hh"
#include "mem/protocol/LLCResponseMsg.hh"
#include "mem/ruby/network/Network.hh"
#include "mem/ruby/scratchpad/MemMessage.hh"
#include "arch/registers.hh"
#include "cpu/io/cpu.hh"

#include "debug/Frame.hh"
#include "debug/LoadTrack.hh"
#include "debug/RubyNetwork.hh"

Forwarder::Forwarder(const Params* p)
    : AbstractController(p),
      m_ruby_system_p(p->ruby_system),
      m_cache_forward_buffer_p(p->cacheForwardBuffer),
      m_net_response_buffer_p(p->netResponseBuffer)
{
  // init AbstractController fields
  m_machineID.type = MachineType_Forwarder;
  m_machineID.num = m_version;

  // set consumer objects for incoming buffers
  m_cache_forward_buffer_p->setConsumer(this);
}

Forwarder::~Forwarder()
{
}

void
Forwarder::initNetQueues()
{
  MachineType machine_type = string_to_MachineType("Forwarder");
  int base M5_VAR_USED = MachineType_base_number(machine_type);

  // To-network buffers
  assert(m_cache_forward_buffer_p);
  // m_net_ptr->setToNetQueue(m_version + base,
  //                          m_cache_forward_buffer_p->getOrdered(),
  //                          0,
  //                          "response",
  //                          m_cache_forward_buffer_p);

  assert(m_net_response_buffer_p);
  m_net_ptr->setToNetQueue(m_version + base,
                           m_net_response_buffer_p->getOrdered(),
                           0,
                           "response",
                           m_net_response_buffer_p);
}

// This just places a packet into another queue to be processed later
void
Forwarder::wakeup()
{
  if (m_cache_forward_buffer_p->isReady(clockEdge())) {
    // const LLCResponseMsg* llc_msg_p =
    //   dynamic_cast<const LLCResponseMsg*>(m_cache_forward_buffer_p->peek());
    auto msg_p = m_cache_forward_buffer_p->peekMsgPtr();

    m_net_response_buffer_p->enqueue(msg_p,
                            clockEdge(),
                            cyclesToTicks(Cycles(1)));
    
    m_cache_forward_buffer_p->dequeue(clockEdge());

    // if input buffers are not empty, schedule an event in the next cycle
    if (!m_cache_forward_buffer_p->isEmpty())
      scheduleEvent(Cycles(1));
  }
}

int
Forwarder::functionalWriteBuffers(PacketPtr& pkt)
{
  int num_functional_writes = 0;
  num_functional_writes += m_cache_forward_buffer_p->functionalWrite(pkt);
  num_functional_writes += m_net_response_buffer_p->functionalWrite(pkt);
  return num_functional_writes;
}

void
Forwarder::print(std::ostream& out) const
{
}

void
Forwarder::regStats()
{
  AbstractController::regStats();
}

Forwarder*
ForwarderParams::create()
{
  return new Forwarder(this);
}
