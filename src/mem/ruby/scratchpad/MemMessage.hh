//=============================================================================
// MemMessage
//=============================================================================
// Memory message exchanged between scratchpad and LLC controllers. This is
// different from the default Ruby message types that work on the cache line
// granularity.
//
// Author: Tuan Ta
// Date  : 19/07/01

#ifndef __MEM_RUBY_MEM_MESSAGE_HH__
#define __MEM_RUBY_MEM_MESSAGE_HH__

#include "mem/packet.hh"
#include "mem/ruby/slicc_interface/Message.hh"

class MemMessage : public Message
{
  public:
    MemMessage(Tick _cur_time, MachineID _src, MachineID _dest,
               Packet* _pkt_p);
    MemMessage(const MemMessage& other);

    MsgPtr clone() const override;

    void print(std::ostream& out) const override;

    const MessageSizeType& getMessageSize() const override;

    MessageSizeType& getMessageSize() override;

    bool functionalRead(Packet* pkt) override
    { panic("MemMessage does not support functionalRead\n"); }

    bool functionalWrite(Packet* pkt) override
    { panic("MemMessage does not support functionalWrite\n"); }

    const NetDest& getDestination() const override;
    NetDest& getDestination() override;

    Packet* getPacket() const;

    MachineID getSenderID() const;

  private:
    MachineID m_sender;
    NetDest m_destinations;
    MessageSizeType m_message_size;
    Packet* m_pkt_p;
};

#endif //MEM_RUBY_MEM_MESSAGE_HH
