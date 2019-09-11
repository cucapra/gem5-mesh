//========================================================================
// Scratchpad
//========================================================================
// This models a scratchpad that supports Ruby network interface and remote
// read/write
//
// +----------------------------------------------------------------+
// |                          RubyNetwork                           |
// +----------------------------------------------------------------+
//     ^              |                     |                 ^
//     |              v                     V                 |
// mem_req_buf   mem_resp_buf        remote_req_buf    remote_resp_buf
//     ^              |                     |                 ^
//     |              v                     V                 |
// +----------------------------------------------------------------+
// |                          Scratchpad                            |
// +----------------------------------------------------------------+
//                                ^
//                                |
//                             cpu_port
//                                |
//                                v
// +----------------------------------------------------------------+
// |                             CPU                                |
// +----------------------------------------------------------------+
//
// Author: Tuan Ta
// Date  : 2019/07/01

#include "mem/ruby/scratchpad/Scratchpad.hh"

#include <algorithm>
#include <cmath>

#include "debug/Scratchpad.hh"
#include "mem/protocol/LLCRequestMsg.hh"
#include "mem/protocol/LLCResponseMsg.hh"
#include "mem/ruby/network/Network.hh"
#include "mem/ruby/scratchpad/MemMessage.hh"

int Scratchpad::m_num_scratchpads = 0;

//-----------------------------------------------------------------------------
// CpuPort
//-----------------------------------------------------------------------------

CpuPort::CpuPort(Scratchpad* _scratchpad_p, const std::string& _name)
    : SlavePort(_name, _scratchpad_p),
      m_scratchpad_p(_scratchpad_p),
      m_need_retry(false)
{ }

bool
CpuPort::recvTimingReq(Packet *pkt)
{
  assert(pkt);

  if (m_scratchpad_p->handleCpuReq(pkt)) {
    assert(!needRetry());
    return true;
  }

  setRetry();
  return false;
}

void
CpuPort::recvFunctional(Packet* pkt)
{
  m_scratchpad_p->handleFunctionalCpuReq(pkt);
}

//-----------------------------------------------------------------------------
// Scratchpad
//-----------------------------------------------------------------------------

Scratchpad::Scratchpad(const Params* p)
    : AbstractController(p),
      m_ruby_system_p(p->ruby_system),
      m_size(p->sp_size),
      m_base_spm_addr(p->dram_size),  // SPM range is right after DRAM range
      m_cpu_port_p(new CpuPort(this, this->name() + ".cpu_port")),
      m_mem_req_buffer_p(p->memReqBuffer),
      m_mem_resp_buffer_p(p->memRespBuffer),
      m_remote_req_buffer_p(p->remoteReqBuffer),
      m_remote_resp_buffer_p(p->remoteRespBuffer),
      m_data_array(new uint8_t[m_size]),
      m_cpu_resp_event([this]{ sendCPUResponse(); },
                       "Send a response to CPU",
                       false),
      m_cur_seq_num(0),
      m_max_num_pending_pkts(p->maxNumPendingReqs),
      m_pending_base_addr_req(MachineID(), nullptr),
      m_pending_go_flag_req(MachineID(), nullptr),
      m_pending_done_flag_req(MachineID(), nullptr),
      m_base_addr_p((uint64_t* const) (m_data_array + SPM_BASE_ADDR_OFFSET)),
      m_go_flag_p((uint32_t* const)(m_data_array + SPM_GO_FLAG_OFFSET)),
      m_done_flag_p((uint32_t* const)(m_data_array + SPM_DONE_FLAG_OFFSET)),
      m_num_l2s(p->num_l2s)
{
  m_num_scratchpads++;

  // init AbstractController fields
  m_machineID.type = MachineType_Scratchpad;
  m_machineID.num = m_version;

  // set consumer objects for incoming buffers
  m_mem_resp_buffer_p->setConsumer(this);
  m_remote_req_buffer_p->setConsumer(this);

  // fill m_data_array with 0s
  std::fill_n(m_data_array, m_size, 0);
}

Scratchpad::~Scratchpad()
{
  delete m_cpu_port_p;
  delete[] m_data_array;
}

BaseSlavePort&
Scratchpad::getSlavePort(const std::string& if_name, PortID idx)
{
  return *m_cpu_port_p;
}

void
Scratchpad::initNetQueues()
{
  MachineType machine_type = string_to_MachineType("Scratchpad");
  int base M5_VAR_USED = MachineType_base_number(machine_type);

  // To-network buffers
  assert(m_mem_req_buffer_p);
  m_net_ptr->setToNetQueue(m_version + base,
                           m_mem_req_buffer_p->getOrdered(),
                           1,
                           "request",
                           m_mem_req_buffer_p);

  assert(m_remote_resp_buffer_p);
  m_net_ptr->setToNetQueue(m_version + base,
                           m_remote_resp_buffer_p->getOrdered(),
                           0,
                           "response",
                           m_remote_resp_buffer_p);

  // From-network buffers
  assert(m_remote_req_buffer_p);
  m_net_ptr->setFromNetQueue(m_version + base,
                             m_remote_req_buffer_p->getOrdered(),
                             1,
                             "request",
                             m_remote_req_buffer_p);

  assert(m_mem_resp_buffer_p);
  m_net_ptr->setFromNetQueue(m_version + base,
                             m_mem_resp_buffer_p->getOrdered(),
                             0,
                             "response",
                             m_mem_resp_buffer_p);
}

void
Scratchpad::wakeup()
{
  // Check if we have any response from the network
  if (m_mem_resp_buffer_p->isReady(clockEdge())) {
    const MemMessage* mem_msg_p =
            dynamic_cast<const MemMessage*>(m_mem_resp_buffer_p->peek());
    const LLCResponseMsg* llc_msg_p =
            dynamic_cast<const LLCResponseMsg*>(m_mem_resp_buffer_p->peek());

    // sanity check: either MemMessage or LLCResponseMsg but not both
    assert(mem_msg_p || llc_msg_p);
    assert(!(mem_msg_p && llc_msg_p));

    if (mem_msg_p) {
      // This is a response msg from a remote scratchpad
      Packet* pkt_p = mem_msg_p->getPacket();
      assert(pkt_p && pkt_p->isResponse());

      DPRINTF(Scratchpad, "Handling mem resp pkt %s from a remote "
                          "scratchpad\n", pkt_p->print());

      // Save the response packet and schedule an event in the next cycle to
      // send it to CPU
      m_cpu_resp_pkts.push_back(pkt_p);
      if (!m_cpu_resp_event.scheduled())
        schedule(m_cpu_resp_event, clockEdge(Cycles(1)));

      // Pop the message from mem_resp_buffer
      m_mem_resp_buffer_p->dequeue(clockEdge());
    } else {
      // sanity check: make sure this is the response we're waiting for
      assert(m_pending_pkt_map.count(llc_msg_p->m_SeqNum) == 1);
      Packet* pending_mem_pkt_p = m_pending_pkt_map[llc_msg_p->m_SeqNum];

      assert(pending_mem_pkt_p &&
             llc_msg_p->m_LineAddress ==
                            makeLineAddress(pending_mem_pkt_p->getAddr()));

      if (llc_msg_p->m_Type == LLCResponseType_DATA) {
        // copy data from DataBlock to pending_mem_pkt_p
        int offset = pending_mem_pkt_p->getAddr() - llc_msg_p->m_LineAddress;
        int len = pending_mem_pkt_p->getSize();
        const uint8_t* data_p = (llc_msg_p->m_DataBlk).getData(offset, len);
        pending_mem_pkt_p->setData(data_p);
      } else if (llc_msg_p->m_Type == LLCResponseType_ACK) {
        if (pending_mem_pkt_p->isLLSC()) {
          assert(pending_mem_pkt_p->req);
          pending_mem_pkt_p->req->
                            setExtraData((uint64_t) llc_msg_p->m_SC_Success);
        }
      } else {
        panic("Received wrong LLCResponseType");
      }

      // turn around the request packet
      pending_mem_pkt_p->makeResponse();

      DPRINTF(Scratchpad, "Handling mem resp pkt %s from LLC seq_num %d\n",
                          pending_mem_pkt_p->print(), llc_msg_p->m_SeqNum);

      // Save the response packet and schedule an event in the next cycle to
      // send it to CPU
      m_cpu_resp_pkts.push_back(pending_mem_pkt_p);
      if (!m_cpu_resp_event.scheduled())
        schedule(m_cpu_resp_event, clockEdge(Cycles(1)));

      // remove pending_mem_pkt_p from the pending pkt map
      m_pending_pkt_map.erase(llc_msg_p->m_SeqNum);

      // Pop the message from mem_resp_buffer
      m_mem_resp_buffer_p->dequeue(clockEdge());
    }
  }

  // Check if we have remote request from the network
  if (m_remote_req_buffer_p->isReady(clockEdge())) {
    const MemMessage* msg_p =
                dynamic_cast<const MemMessage*>(m_remote_req_buffer_p->peek());
    assert(msg_p);

    Packet* pkt_p = msg_p->getPacket();
    assert(pkt_p && pkt_p->isRequest());

    // sanity check: make sure this request is for me
    assert(getScratchpadIdFromAddr(pkt_p->getAddr()) == m_version);

    DPRINTF(Scratchpad, "Handling remote req pkt %s from %s\n",
                        pkt_p->print(), msg_p->getSenderID());

    if (!handleRemoteReq(pkt_p, msg_p->getSenderID())) {
      DPRINTF(Scratchpad, "Not able to handle remote req. \
                          Will retry to handle it later\n");
      scheduleEvent(Cycles(1));
    } else {
      DPRINTF(Scratchpad, "Finished a remote req\n");
      m_remote_req_buffer_p->dequeue(clockEdge());
    }
  }

  // if CPU needs to retry, wake it up
  if (m_cpu_port_p->needRetry()) {
    m_cpu_port_p->sendRetryReq();
    m_cpu_port_p->clearRetry();
  }

  // if input buffers are not empty, schedule an event in the next cycle
  if (!m_mem_resp_buffer_p->isEmpty() || !m_remote_req_buffer_p->isEmpty())
    scheduleEvent(Cycles(1));
}

bool
Scratchpad::handleCpuReq(Packet* pkt_p)
{
  assert(pkt_p->isRequest());

  /**
   * From Xcel, access to base_addr field
   */
  if (pkt_p->isSPM() && pkt_p->getAddr() == SPM_BASE_ADDR_OFFSET) {
    assert(pkt_p->isRead());

    DPRINTF(Scratchpad, "Handling SPM_BASE_ADDR read: pkt %s\n",
                        pkt_p->print());

    if (*m_base_addr_p == 0) {
      m_pending_base_addr_req = std::make_pair(MachineID(), pkt_p);
    } else {
      pkt_p->setData((uint8_t*) m_base_addr_p);
      pkt_p->makeResponse();
      // Save the response packet and schedule an event in the next cycle to
      // send it to CPU
      m_cpu_resp_pkts.push_back(pkt_p);
      if (!m_cpu_resp_event.scheduled())
        schedule(m_cpu_resp_event, clockEdge(Cycles(1)));
    }

    return true;
  }

  /**
   * From Xcel, access to go_flag field
   */
  if (pkt_p->isSPM() && pkt_p->getAddr() == SPM_GO_FLAG_OFFSET) {
    assert(pkt_p->isRead());

    DPRINTF(Scratchpad, "Handling SPM_GO_FLAG read: pkt %s\n",
                        pkt_p->print());

    if (*m_go_flag_p == 0) {
      m_pending_go_flag_req = std::make_pair(m_machineID, pkt_p);
    } else {
      pkt_p->setData((uint8_t *) m_go_flag_p);
      pkt_p->makeResponse();
      *m_go_flag_p = 0;
      // Save the response packet and schedule an event in the next cycle to
      // send it to CPU
      m_cpu_resp_pkts.push_back(pkt_p);
      if (!m_cpu_resp_event.scheduled())
        schedule(m_cpu_resp_event, clockEdge(Cycles(1)));
    }

    return true;
  }

  /**
   * From Xcel, access to done_flag field
   */
  if (pkt_p->isSPM() && pkt_p->getAddr() == SPM_DONE_FLAG_OFFSET) {
    assert(pkt_p->isWrite());

    DPRINTF(Scratchpad, "Handling SPM_DONE_FLAG write: pkt %s\n",
                        pkt_p->print());

    pkt_p->writeData((uint8_t*) m_done_flag_p);

    DPRINTF(Scratchpad, "Done value: %d\n", *m_done_flag_p);

    pkt_p->makeResponse();
    // Save the response packet and schedule an event in the next cycle to
    // send it to CPU
    m_cpu_resp_pkts.push_back(pkt_p);
    if (!m_cpu_resp_event.scheduled())
      schedule(m_cpu_resp_event, clockEdge(Cycles(1)));

    // wake up any remote request reading this flag
    if (m_pending_done_flag_req.second) {
      DPRINTF(Scratchpad, "Waking up a deferred SPM_DONE_FLAG read: pkt %s\n",
                          m_pending_done_flag_req.second);

      assert(handleRemoteReq(m_pending_done_flag_req.second,
                             m_pending_done_flag_req.first));
      m_pending_done_flag_req.first = MachineID();
      m_pending_done_flag_req.second = nullptr;
    }

    return true;
  }

  /**
   * From either CPU or Xcel, access to data
   */

  // This is either a remote access or DRAM access
  // Figure out who will handle this request

  NodeID dst_sp_id = getScratchpadIdFromAddr(pkt_p->getAddr());
  assert(dst_sp_id <= m_num_scratchpads);

  if (dst_sp_id == m_version) {
    // This is a local access
    DPRINTF(Scratchpad, "Doing a local access for pkt %s\n", pkt_p->print());
    accessDataArray(pkt_p);
    // Save the response packet and schedule an event in the next cycle to send
    // it to CPU
    m_cpu_resp_pkts.push_back(pkt_p);
    if (!m_cpu_resp_event.scheduled())
      schedule(m_cpu_resp_event, clockEdge(Cycles(1)));

    return true;
  }

  // Check if we have enough slot in m_mem_req_buffer to queue a new request
  // If not, we need to handle this request later
  if (!m_mem_req_buffer_p->areNSlotsAvailable(1, clockEdge())) {
    DPRINTF(Scratchpad, "mem_req_buffer is busy\n");
    return false;
  }

  MachineID src_port = m_machineID;
  MachineID dst_port;

  if (dst_sp_id == m_num_scratchpads) {
    // This packet will be delivered to LLC
    if (m_pending_pkt_map.size() == m_max_num_pending_pkts) {
      DPRINTF(Scratchpad, "Blocking. Pending pkt buffer is full\n");
      return false;
    } else {
      dst_port = { MachineType_L2Cache, getL2BankFromAddr(pkt_p->getAddr()) };

      // make and queue an LLCRequest message
      std::shared_ptr<LLCRequestMsg> msg_p
                                = std::make_shared<LLCRequestMsg>(clockEdge());
      msg_p->m_LineAddress = makeLineAddress(pkt_p->getAddr());
      msg_p->m_Requestor = m_machineID;
      msg_p->m_MessageSize = MessageSizeType_Request_Control;
      (msg_p->m_Destination).add(dst_port);
      msg_p->m_SeqNum = m_cur_seq_num;

      if (pkt_p->isAtomicOp()) {  // Atomic ops
        msg_p->m_Type = LLCRequestType_ATOMIC;

        int offset = pkt_p->getAddr() - makeLineAddress(pkt_p->getAddr());
        int len = pkt_p->getSize();
        (msg_p->m_writeMask).setMask(offset, len);
        (msg_p->m_writeMask).addAtomicOp(offset, pkt_p->getAtomicOp());
      } else if (pkt_p->isLLSC()) {   // LL/SC ops
        if (pkt_p->isRead()) {
          msg_p->m_Type = LLCRequestType_LL;
        } else if (pkt_p->isWrite()) {
          msg_p->m_Type = LLCRequestType_SC;

          int offset = pkt_p->getAddr() - makeLineAddress(pkt_p->getAddr());
          int len = pkt_p->getSize();
          (msg_p->m_DataBlk).setData(pkt_p->getConstPtr<uint8_t>(), offset,
                                     len);
          (msg_p->m_writeMask).setMask(offset, len);
        } else {
          panic("Invalid LLSC packet\n");
        }
      } else if (pkt_p->isRead()) {   // Read
        assert(!pkt_p->isWrite());
        msg_p->m_Type = LLCRequestType_READ;
      } else if (pkt_p->isWrite()) {  // Write
        msg_p->m_Type = LLCRequestType_WRITE;

        int offset = pkt_p->getAddr() - makeLineAddress(pkt_p->getAddr());
        int len = pkt_p->getSize();
        (msg_p->m_DataBlk).setData(pkt_p->getConstPtr<uint8_t>(), offset, len);
        (msg_p->m_writeMask).setMask(offset, len);
      }

      m_mem_req_buffer_p->enqueue(msg_p,
                                  clockEdge(),
                                  cyclesToTicks(Cycles(1)));

      // set the pending packet
      m_pending_pkt_map[m_cur_seq_num] = pkt_p;
      m_cur_seq_num++;

      DPRINTF(Scratchpad, "Sent pkt %s to LLC seq_num %d\n",
                        pkt_p->print(), m_cur_seq_num - 1);
    }
  } else if (dst_sp_id != m_version) {
    // This packet will be delivered to a remote scratchpad
    dst_port = { MachineType_Scratchpad, dst_sp_id };

    DPRINTF(Scratchpad, "Sending pkt %s to %s\n", pkt_p->print(), dst_port);

    // Make and queue the message
    std::shared_ptr<MemMessage> msg_p =
          std::make_shared<MemMessage>(clockEdge(), src_port, dst_port, pkt_p);

    m_mem_req_buffer_p->enqueue(msg_p,
                                clockEdge(),
                                cyclesToTicks(Cycles(1)));
  }

  return true;
}

bool
Scratchpad::handleRemoteReq(Packet* pkt_p, MachineID remote_sender)
{
  assert(pkt_p->isRequest());

  // Check if we have enough slot in m_remote_resp_buffer to queue a new
  // response message
  if (!m_remote_resp_buffer_p->areNSlotsAvailable(1, clockEdge())) {
    return false;
  }

  MachineID src_port = m_machineID;
  MachineID dst_port = remote_sender;

  bool respond_sender = true;

  /**
   * From a remote CPU, access to base_addr field
   */
  if (getLocalAddr(pkt_p->getAddr()) == SPM_BASE_ADDR_OFFSET) {
    assert(pkt_p->isWrite());

    DPRINTF(Scratchpad, "Handling remote SPM_BASE_ADDR write: pkt %s\n",
                        pkt_p->print());

    pkt_p->writeData((uint8_t*) m_base_addr_p);
    pkt_p->makeResponse();

    // wake up pending request reading base_addr
    if (m_pending_base_addr_req.second) {
      assert(handleCpuReq(m_pending_base_addr_req.second));
      m_pending_base_addr_req.first = MachineID();
      m_pending_base_addr_req.second = nullptr;
    }
  }
  /**
   * From a remote CPU, access to go_flag field
   */
  else if (getLocalAddr(pkt_p->getAddr()) == SPM_GO_FLAG_OFFSET) {
    assert(pkt_p->isWrite());

    DPRINTF(Scratchpad, "Handling remote SPM_GO_FLAG write: pkt %s\n",
                        pkt_p->print());

    pkt_p->writeData((uint8_t*) m_go_flag_p);
    pkt_p->makeResponse();

    // wake up pending request reading go_flag
    if (m_pending_go_flag_req.second) {
      assert(handleCpuReq(m_pending_go_flag_req.second));
      m_pending_go_flag_req.first = MachineID();
      m_pending_go_flag_req.second = nullptr;
    }
  }
  /**
   * From a remote CPU, access to done_flag field
   */
  else if (getLocalAddr(pkt_p->getAddr()) == SPM_DONE_FLAG_OFFSET) {
    assert(pkt_p->isRead());

    DPRINTF(Scratchpad, "Handling remote SPM_DONE_FLAG read: pkt %s\n",
                        pkt_p->print());

    if (*m_done_flag_p == 0) {
      DPRINTF(Scratchpad, "Deferring SPM_GO_FLAG write request: pkt %s\n",
                          pkt_p->print());
      m_pending_done_flag_req = std::make_pair(remote_sender, pkt_p);
      respond_sender = false;
    } else {
      // read and reset
      pkt_p->setData((uint8_t*) m_done_flag_p);
      pkt_p->makeResponse();
      *m_done_flag_p = 0;
    }
  }
  /**
   * From a remote CPU/Xcel, access to data
   */
  else {
    // access data array
    accessDataArray(pkt_p);
  }

  // Make and queue the message
  if (respond_sender) {
    std::shared_ptr<MemMessage> msg_p =
        std::make_shared<MemMessage>(clockEdge(), src_port, dst_port, pkt_p);

    DPRINTF(Scratchpad, "Sending pkt %s to %s\n", pkt_p->print(), dst_port);

    m_remote_resp_buffer_p->enqueue(msg_p,
                                    clockEdge(),
                                    cyclesToTicks(Cycles(1)));
  }

  return true;
}

void
Scratchpad::accessDataArray(Packet* pkt_p)
{
  uint8_t* local_data_p = m_data_array + getLocalAddr(pkt_p->getAddr());

  if (pkt_p->cmd == MemCmd::SwapReq) {
    panic("Scratchpad does not support atomic request\n");
  } else if (pkt_p->isRead()) {
    assert(!pkt_p->isWrite() && !pkt_p->isLLSC());
    pkt_p->setData(local_data_p);
  } else if (pkt_p->isWrite()) {
    assert(!pkt_p->isLLSC());
    pkt_p->writeData(local_data_p);
  }

  // make a response packet
  pkt_p->makeResponse();
}

void
Scratchpad::sendCPUResponse()
{
  assert(!m_cpu_resp_pkts.empty());
  assert(!m_cpu_resp_event.scheduled());

  DPRINTF(Scratchpad, "Sending %s to CPU\n", m_cpu_resp_pkts.front()->print());

  if (!m_cpu_port_p->sendTimingResp(m_cpu_resp_pkts.front())) {
    panic("Failed to send a response to CPU. \
          CPU is assumed to always be ready to accept response packets\n");
  }

  m_cpu_resp_pkts.pop_front();

  if (!m_cpu_resp_pkts.empty())
    schedule(m_cpu_resp_event, clockEdge(Cycles(1)));
}

void
Scratchpad::print(std::ostream& out) const
{
  out << "Scratchpad_" << m_version;
}

NodeID
Scratchpad::getScratchpadIdFromAddr(Addr addr) const
{
  // XXX: assume the scratchpad address range starts right after the DRAM
  // address range
  if (addr < m_base_spm_addr) {
    // out of scratchpad address range, this address belongs to normal DRAM
    // memory region
    return m_num_scratchpads;
  }

  Addr sp_addr = addr - m_base_spm_addr;
  NodeID sp_id = sp_addr / m_size;

  if (sp_id >= m_num_scratchpads) {
    panic("Invalid scratchpad address 0x%x\n", addr);
  }

  return sp_id;
}

Addr
Scratchpad::getLocalAddr(Addr addr) const
{
  // sanity check: make sure this address is a scratchpad address
  assert(getScratchpadIdFromAddr(addr) < m_num_scratchpads);
  return (addr - m_base_spm_addr) % m_size;
}

int
Scratchpad::functionalWriteBuffers(PacketPtr& pkt)
{
  int num_functional_writes = 0;
  num_functional_writes += m_mem_req_buffer_p->functionalWrite(pkt);
  num_functional_writes += m_mem_resp_buffer_p->functionalWrite(pkt);
  num_functional_writes += m_remote_req_buffer_p->functionalWrite(pkt);
  num_functional_writes += m_remote_resp_buffer_p->functionalWrite(pkt);
  return num_functional_writes;
}

void
Scratchpad::handleFunctionalCpuReq(Packet* pkt_p)
{
  DPRINTF(Scratchpad, "Functional access for address: %#x pkt: %s\n",
                      pkt_p->getAddr(), pkt_p->print());

  RubySystem *rs = m_ruby_system_p;

  assert(pkt_p->getAddr() + pkt_p->getSize() <=
         makeLineAddress(pkt_p->getAddr()) + RubySystem::getBlockSizeBytes());

  bool accessSucceeded = false;
  bool needsResponse = pkt_p->needsResponse();

  // Do the functional access on ruby memory
  if (pkt_p->isRead()) {
      accessSucceeded = rs->functionalRead(pkt_p);
  } else if (pkt_p->isWrite()) {
      accessSucceeded = rs->functionalWrite(pkt_p);
  } else {
      panic("Unsupported functional command %s\n", pkt_p->cmdString());
  }

  // Unless the requester explicitly said otherwise, generate an error if
  // the functional request failed
  if (!accessSucceeded && !pkt_p->suppressFuncError()) {
      fatal("Ruby functional %s failed for address %#x\n",
            pkt_p->isWrite() ? "write" : "read", pkt_p->getAddr());
  }

  // turn packet around to go back to requester if response expected
  if (needsResponse) {
      pkt_p->setFunctionalResponseStatus(accessSucceeded);
  }

  DPRINTF(Scratchpad, "Functional access %s!\n",
                      accessSucceeded ? "successful":"failed");

}

NodeID
Scratchpad::getL2BankFromAddr(Addr addr) const
{
  if (m_num_l2s == 1)
    return 0;

  unsigned int low_bit = RubySystem::getBlockSizeBits();
  unsigned int hig_bit = low_bit + floorLog2(m_num_l2s) - 1;
  NodeID l2_node_id = (NodeID) bitSelect(addr, low_bit, hig_bit);
  assert(l2_node_id < m_num_l2s);
  return l2_node_id;
}

Scratchpad*
ScratchpadParams::create()
{
  return new Scratchpad(this);
}
