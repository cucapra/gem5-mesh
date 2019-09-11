//=============================================================================
// MemMessage
//=============================================================================
// Memory message exchanged between scratchpad and LLC controllers. This is
// different from the default Ruby message types that work on the cache line
// granularity.
//
// Author: Tuan Ta
// Date  : 19/07/01

#include "mem/ruby/scratchpad/MemMessage.hh"

MemMessage::MemMessage(Tick _cur_time, MachineID _src, MachineID _dest,
                       Packet* _pkt_p)
    : Message(_cur_time),
      m_sender(_src),
      m_message_size(MessageSizeType_Control),
      m_pkt_p(_pkt_p)
{
  m_destinations.add(_dest);
}

MemMessage::MemMessage(const MemMessage& other)
    : Message(other),
      m_sender(other.m_sender),
      m_destinations(other.m_destinations),
      m_message_size(other.m_message_size),
      m_pkt_p(other.m_pkt_p)
{ }

MsgPtr
MemMessage::clone() const
{
  return std::shared_ptr<Message>(new MemMessage(*this));
}

void
MemMessage::print(std::ostream& out) const
{
  out << "[MemMessage: ";
  out << "sender = " << m_sender << " ";
  out << "destinations = " << m_destinations << " ";
  out << "size = " << m_message_size << " ";
  out << "]";
}

const MessageSizeType&
MemMessage::getMessageSize() const
{
  return m_message_size;
}

MessageSizeType&
MemMessage::getMessageSize()
{
  return m_message_size;
}

const NetDest&
MemMessage::getDestination() const
{
  return m_destinations;
}

NetDest&
MemMessage::getDestination()
{
  return m_destinations;
}

Packet*
MemMessage::getPacket() const
{
  return m_pkt_p;
}

MachineID
MemMessage::getSenderID() const
{
  return m_sender;
}
