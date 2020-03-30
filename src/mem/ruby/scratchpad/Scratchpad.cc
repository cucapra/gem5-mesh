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

#include "debug/Mesh.hh"

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

  /*if (m_scratchpad_p->handleCpuReq(pkt)) {
    assert(!needRetry());
    return true;
  }
  
  // TODO better if just clearRetry on cpu squash. doesnt work, need to send somtimes
  // an lwspec (spad specific load) only stalls due to bit not being set
  // possible that no packet will come from llc to this spad will trigger a wakeup for it if squashed 
  // (the typical way to clearRetry())
  // thus we should not check the retry otherwise will have issues, so dont set here
  //if (!pkt->getSpecSpad())
  //  setRetry();
  
  setRetry();
  return false;*/
  
  return m_scratchpad_p->handleCpuReq(pkt);
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
      m_num_l2s(p->num_l2s),
      m_grid_dim_x(p->grid_dim_x),
      m_grid_dim_y(p->grid_dim_y),
      m_cpu_p(p->cpu),
      m_max_pending_sp_prefetches(p->prefetchBufSize),
      m_process_resp_event([this]{ processRespToSpad(); }, "Process a resp to spad", false),
      m_proc_ruby_last(false)
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
  
  // setup cpu touched array to keep track of divergences (TODO not sure how should be implemetned in practice?)
  // maybe use a single bit/byte of data towards this purpose
  // for(int i = 0; i < m_size / sizeof(uint32_t); i++) {
  //   m_fresh_array.push_back(0);
  // }
  m_region_cntr = 0;
  m_cur_prefetch_region = 0;
}

Scratchpad::~Scratchpad()
{
  delete m_cpu_port_p;
  delete[] m_data_array;
}

//BaseSlavePort&
//Scratchpad::getSlavePort(const std::string& if_name, PortID idx)
//{
//  return *m_cpu_port_p;
//}

Port &
Scratchpad::getPort(const std::string &if_name, PortID idx)
{
  if (if_name == "cpu_port") {
    return *m_cpu_port_p; 
  }
  else {
    return AbstractController::getPort(if_name, idx);
  }
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


// event called from both wakeup (ruby) and internally to handle locally buffered pkts
void
Scratchpad::processRespToSpad() {
  // pull a packet off the queue
  // every packet on this queue should be ready as long as only do once
  // per cycle
  
  // pick a resp from either the ruby queue or internal buffer queue to process
  // Aribtration logic here!
  bool hasBufPkt = !m_prefetch_resp_queue.empty();
  bool hasMemPkt = !m_ruby_resp_queue.empty();
  assert(hasBufPkt || hasMemPkt);
  
  PacketPtr pkt_p;
  if (hasMemPkt && (!hasBufPkt || !m_proc_ruby_last)) {
    pkt_p = m_ruby_resp_queue.front();
    m_ruby_resp_queue.pop();
    m_proc_ruby_last = true;
  }
  else {
    pkt_p = m_prefetch_resp_queue.front();
    m_prefetch_resp_queue.pop();
    m_proc_ruby_last = false;
  }
  
  // if either buffer has a packet not used then schedule again for next cycle
  if (!m_prefetch_resp_queue.empty() || !m_ruby_resp_queue.empty()) {
    if (!m_process_resp_event.scheduled())
      schedule(m_process_resp_event, clockEdge(Cycles(1)));
  }
  
  switch (pkt_p->spRespType) {
    case Packet::RespPktType::LLC_Data_Resp:
    case Packet::RespPktType::Remote_Resp: {
      // Save the response packet and schedule an event in the next cycle to
      // send it to CPU. In scratchpads, don't directly store this. Need CPU to explicitly write later on
      m_cpu_resp_pkts.push_back(pkt_p);
      if (!m_cpu_resp_event.scheduled())
        schedule(m_cpu_resp_event, clockEdge(Cycles(1)));
        
      break;
    }
    case Packet::RespPktType::Prefetch_Self_Resp:
    case Packet::RespPktType::Prefetch_Patron_Resp: {
      bool memDiv = memoryDiverged(pkt_p->getAddr()); //setPrefetchFresh(pkt_p);
      
      // bool isSelfResp = pkt_p->spRespType == Packet::RespPktType::Prefetch_Self_Resp; // TODO deprecated
      // throw away if diverged and its not for future epochs
      // TODO not sure if it's the right thing to throw away here
      // but definetly need to update the counter signalling it was recv
      // bool controlDiv = controlDiverged();
      // bool throwAway = controlDiv && !memDiv;
  
      // if (throwAway) {
      //   DPRINTF(Mesh, "[[WARNING]] drop due to control div\n");

      //   // TODO not sure if actually need to drop here?
      //   // def need to update the counter as rdy
      //   if (pkt_p->isRead())
      //     m_remote_loads++;
      //   else if (pkt_p->isWrite())
      //     m_remote_stores++;
      //   // access data array
      //   accessDataArray(pkt_p);
      //   // set the word as ready for future packets
      //   setWordRdy(pkt_p->getAddr());

      //   // just drop the packet if there's divergence and this is from vector prefetch
      //   delete pkt_p;
      //   pkt_p = nullptr;
      // }
      /*else*/ if (memDiv) {
        
        // place packet into buffer to use later
        // assure that this is a very small buffer otherwise actually diverge
        // in wakeup check this buffer too to see if rdy and can place into queue (or maybe somehwere else
        //m_sp_prefetch_buffer.push_back(pkt_p);
        enqueueStallRespToSp(pkt_p);
        
        DPRINTF(Mesh, "[[WARNING]] potential diverge, now %d pending epoch %d core epoch %d addr %#x spadEntry %d\n", 
          m_prefetch_resp_queue.size(), getDesiredRegion(m_prefetch_resp_queue.front()->getAddr()), getCoreEpoch(),
          m_prefetch_resp_queue.front()->getAddr(),
          getLocalAddr(m_prefetch_resp_queue.front()->getAddr()) / sizeof(uint32_t)
          );

        // record the max size ever seen for stats
        if (m_prefetch_resp_queue.size() > (int)m_max_queue_size.total()) {
          m_max_queue_size = m_prefetch_resp_queue.size();
        }
        
        // TODO I don't think it's possible for this to be active? even when there is
        // tons or unhandled reqs?
        // TODO this is limiting number of prefetch requests that can be active in a single
        // epoch.... need to fix. Either remove since never run into problem here??? or 
        // somehow detect which epochs are still active
        if (m_prefetch_resp_queue.size() > m_max_pending_sp_prefetches) {
          DPRINTF(Mesh, "[[WARNING]] must diverge now\n");
          assert(false);
          // clear the pending buffer and inform cpu of divergence, to get squash
          //m_sp_prefetch_buffer.clear();
          
          // TODO inform CPU of divergence
          // should be extremely rare
        }
      }
      else {
        // profile, TODO should classify different than just remote load/store?
        if (pkt_p->isRead())
          m_remote_loads++;
        else if (pkt_p->isWrite())
          m_remote_stores++;
  
        // access data array
        accessDataArray(pkt_p);
        
        DPRINTF(Mesh, "store spec prefetch %#x\n", pkt_p->getAddr());
        
        // set the word as ready for future packets
        setWordRdy(pkt_p->getAddr());
        delete pkt_p;
        pkt_p = nullptr;
      }
      break;
    }
    default: {
      
      break;
    }
        
        
  }
  
  // if CPU needs to retry, wake it up
  if (m_cpu_port_p->needRetry()) {
    m_cpu_port_p->sendRetryReq();
    m_cpu_port_p->clearRetry();
  }
  
  
  
  
}

/*void
Scratchpad::enqueueRespToSp(PacketPtr pkt_p, Packet::RespPktType type) {
  pkt_p->spRespType = type;
  m_resp_val_queue.push(pkt_p);
  
  // schedule for next cycle if not already scheduled
  if (!m_process_resp_event.scheduled())
    schedule(m_process_resp_event, clockEdge(Cycles(1)));
}*/

void
Scratchpad::enqueueRubyRespToSp(PacketPtr pkt_p, Packet::RespPktType type) {
  pkt_p->spRespType = type;
  m_ruby_resp_queue.push(pkt_p);
  
  // schedule for next cycle if not already scheduled
  if (!m_process_resp_event.scheduled())
    schedule(m_process_resp_event, clockEdge(Cycles(1)));
}

void
Scratchpad::enqueueStallRespToSp(PacketPtr pkt_p) {
  m_prefetch_resp_queue.push(pkt_p);
  
  // schedule for next cycle if not already scheduled
  if (!m_process_resp_event.scheduled())
    schedule(m_process_resp_event, clockEdge(Cycles(1)));
}

/*void
Scratchpad::arbitrate() {
}*/

// Handles response from LLC or remote load
// NOTE memory loads through the spad go directly to the CPU and not to the scratchpad
// you need to add an explicit write to the spad afterwards in order to get from CPU to spad

// This just places a packet into another queue to be processed later
void
Scratchpad::wakeup()
{
  
  // handle this cycle, TODO but need to say next cycle so don't double schedule...
  //if (!m_process_resp_event.scheduled())
  //  schedule(m_process_resp_event, clockEdge(Cycles(0)));
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

      // Pop the message from mem_resp_buffer
      m_mem_resp_buffer_p->dequeue(clockEdge());
      
      enqueueRubyRespToSp(pkt_p, Packet::RespPktType::Remote_Resp);
      
    } else if (llc_msg_p && ((llc_msg_p->m_Type == LLCResponseType_REVDATA) || 
        (llc_msg_p->m_Type == LLCResponseType_REDATA))) {
        // data from a vector request on this scratchpads behalf
        // mimic as a remote store from the master core
        
        //DPRINTF(Mesh, "ruby pkt %s\n", *llc_msg_p);
        
        // need to 'fake' a Packet from the LLC response and then go
        // through the regular remote store channels
        std::shared_ptr<Request> req =
                std::make_shared<Request>(llc_msg_p->m_LineAddress,    // vaddr
                                          sizeof(uint32_t),    // size
                                          0, 0);
        
        // req->epoch = llc_msg_p->m_Epoch;
        
        PacketPtr pkt_p = Packet::createWrite(req);
        
        // we really only sent a word of data, so extract that
        int blkIdx = llc_msg_p->m_BlkIdx;
        uint8_t *buff = new uint8_t[sizeof(uint32_t)];
        const uint8_t *data = llc_msg_p->m_DataBlk.getData(sizeof(uint32_t) * blkIdx, sizeof(uint32_t));
        memcpy(buff, data, sizeof(uint32_t));
        pkt_p->dataDynamic(buff);
        
        DPRINTF(Mesh, "Recv remote store %#x from cache epoch %d data %d %d %d %d\n", 
          llc_msg_p->m_LineAddress, llc_msg_p->m_Epoch, data[3], data[2], data[1], data[0]);
        
        assert(getScratchpadIdFromAddr(pkt_p->getAddr()) == m_version);
      
        if (llc_msg_p->m_Type == LLCResponseType_REDATA)
          enqueueRubyRespToSp(pkt_p, Packet::RespPktType::Prefetch_Self_Resp);
        else
          enqueueRubyRespToSp(pkt_p, Packet::RespPktType::Prefetch_Patron_Resp);
      
        // delete the pending packet // why would there be a pending pkt for this?
        // m_pending_pkt_map.erase(llc_msg_p->m_SeqNum);
        m_mem_resp_buffer_p->dequeue(clockEdge());

    } /*else if (m_pending_pkt_map.count(llc_msg_p->m_SeqNum) == 0 && 
              llc_msg_p->m_Type == LLCResponseType_ACK) {
      // we just need to send back to the core that we received an ack
      // make a fake resp packet to send back
      std::shared_ptr<Request> req =
                std::make_shared<Request>(llc_msg_p->m_LineAddress,    // vaddr
                                          sizeof(uint32_t),    // size
                                          0, 0);
        
      PacketPtr pending_mem_pkt_p = Packet::createWrite(req);
      pending_mem_pkt_p->pushSenderState(new MemUnit::SenderState(nullptr));
      pending_mem_pkt_p->makeResponse();

      // Pop the message from mem_resp_buffer
      m_mem_resp_buffer_p->dequeue(clockEdge());
      
      enqueueRubyRespToSp(pending_mem_pkt_p, Packet::RespPktType::LLC_Data_Resp);

    }*/ else {
      // sanity check: make sure this is the response we're waiting for
      assert(m_pending_pkt_map.count(llc_msg_p->m_SeqNum) == 1);
      Packet* pending_mem_pkt_p = m_pending_pkt_map[llc_msg_p->m_SeqNum];

      assert(pending_mem_pkt_p &&
             llc_msg_p->m_LineAddress ==
                            makeLineAddress(pending_mem_pkt_p->getAddr()));

      if (llc_msg_p->m_Type == LLCResponseType_DATA) {
        // copy data from DataBlock to pending_mem_pkt_p
        // just pulls out a single word from the block and discards the rest?
        int offset = pending_mem_pkt_p->getAddr() - llc_msg_p->m_LineAddress;
        int len = pending_mem_pkt_p->getSize();
        const uint8_t* data_p = (llc_msg_p->m_DataBlk).getData(offset, len);
        //DPRINTF(Mesh, "%d %d %#x %#x\n", offset, len, pending_mem_pkt_p->getAddr(), llc_msg_p->m_LineAddress);
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

      // remove pending_mem_pkt_p from the pending pkt map
      m_pending_pkt_map.erase(llc_msg_p->m_SeqNum);

      // Pop the message from mem_resp_buffer
      m_mem_resp_buffer_p->dequeue(clockEdge());
      
      enqueueRubyRespToSp(pending_mem_pkt_p, Packet::RespPktType::LLC_Data_Resp);
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
    
    // If this is a speculative load and the data isn't present, then
    // allow the packets equal to ld queue size be buffered here
    if (pkt_p->getSpecSpad() && !isWordRdy(pkt_p->getAddr())) {
      //m_packet_buffer.push_back(pkt_p);
      //assert(m_packet_buffer.size() <= m_spec_buf_size);
      // just say not rdy actually
      m_not_rdy_stalls++;
      DPRINTF(Mesh, "not rdy for packet to addr %#x\n", pkt_p->getAddr());
      return false;
    }
    
    // TODO stats might be incorrect with these stalls
    // should move into accessDataArray rather than keep out here
    
    // This is a local access
    DPRINTF(Scratchpad, "Doing a local access for pkt %s\n", pkt_p->print());
    // if (m_cpu_p->getEarlyVector()->getConfigured()) 
    //   DPRINTF(Mesh, "Doing a local access for pkt %s coreepoch %d prefetchEpoch %d cnt%d\n", 
    //     pkt_p->print(), getCoreEpoch(), m_cur_prefetch_region, m_region_cntr);
    
    // record local access here
    if (pkt_p->isRead()) m_local_loads++;
    else if (pkt_p->isWrite()) m_local_stores++;
    
    accessDataArray(pkt_p);
    
    /*if (pkt_p->getSpecSpad()) {
      assert(pkt_p->isRead());
      uint8_t tmp_buf[pkt_p->getSize()];
      uint32_t *temp;
      temp = (uint32_t*)&(tmp_buf[0]);
      pkt_p->writeData(tmp_buf);
      DPRINTF(Mesh, "sending pkt back for %#x -- %d \n", pkt_p->getAddr(), *temp);
    }*/
    
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
    // this packet can be modified to not access global memory in case of slave
    // core but rather just update info in the spad
    // TODO currently checking normal pkt map, should we make our own so that can be
    // unlimited pending? doesn't seem like the spad has to track anything for a pending load?
    bool isPrefetch = pkt_p->isSpadPrefetch();
    bool noLLCAck = isPrefetch;
    bool pendingPktSpad = (m_pending_pkt_map.size() < m_max_num_pending_pkts) || noLLCAck;
    if (isPrefetch && pendingPktSpad) {
      // setWordNotRdy(pkt_p->getPrefetchAddr());
      
      // DPRINTF(Mesh, "reset word %#x\n", pkt_p->getPrefetchAddr());
      
      // deliver a response packet to the core that this was completed
      // but need to copy it because will be delete there
      PacketPtr resp_pkt_p = new Packet(pkt_p, true, false);
      resp_pkt_p->makeResponse();
      m_cpu_resp_pkts.push_back(resp_pkt_p);
      if (!m_cpu_resp_event.scheduled())
        schedule(m_cpu_resp_event, clockEdge(Cycles(1)));
    }
    // // immedietly send an ACK back for a write if no syncronization is needed
    // // TODO mark writes as need sync or no need sync
    // else if (pkt_p->getStoreAckFree() && pkt_p->isWrite()) {
    //   noLLCAck |= (pkt_p->getStoreAckFree() && pkt_p->isWrite());
    //   PacketPtr resp_pkt_p = new Packet(pkt_p, true, false);
    //   resp_pkt_p->makeResponse();
    //   m_cpu_resp_pkts.push_back(resp_pkt_p);
    //   if (!m_cpu_resp_event.scheduled())
    //     schedule(m_cpu_resp_event, clockEdge(Cycles(1)));
    // }

    // we aren't going to track any information about the store beyond did we recv
    // an ACK or not
    // noLLCAck |= pkt_p->isWrite();
    
    // This packet will be delivered to LLC
    if (m_pending_pkt_map.size() == m_max_num_pending_pkts && !noLLCAck) {
      DPRINTF(Scratchpad, "Blocking. Pending pkt buffer is full\n");
      if (m_cpu_p->getEarlyVector()->getConfigured()) DPRINTF(Mesh, "Blocking. Pending pkt buffer is full\n");
      m_exceed_stream_width++;
      return false;
    } else {
      dst_port = { MachineType_L2Cache, getL2BankFromAddr(pkt_p->getAddr()) };

      // make and queue an LLCRequest message
      std::shared_ptr<LLCRequestMsg> msg_p
                                = std::make_shared<LLCRequestMsg>(clockEdge());
      msg_p->m_LineAddress = makeLineAddress(pkt_p->getAddr()); // TODO don't really need this?
      msg_p->m_Requestor = m_machineID;
      msg_p->m_MessageSize = MessageSizeType_Request_Control;
      (msg_p->m_Destination).add(dst_port);
      msg_p->m_SeqNum = m_cur_seq_num;
      // access length in x,y -- prob always linearized
      msg_p->m_XDim = pkt_p->getXDim();
      msg_p->m_YDim = pkt_p->getYDim();
      // msg_p->m_FromDA = pkt_p->getFromDecoupledAccess();
      // msg_p->m_VecOffset = pkt_p->getVecOffset();
      // can't just use line address when doing vec load, need to know start and offsets from it
      msg_p->m_WordAddress = pkt_p->getAddr();
      // for prefetches instead of sending data blk we send an address
      // pre-interpret it here, but not actually an additional field
      msg_p->m_PrefetchAddress = pkt_p->getPrefetchAddr();
      msg_p->m_CoreOffset = pkt_p->getCoreOffset();
      // send local epoch so mem can sync
      // msg_p->m_Epoch = pkt_p->getEpoch();
      // whether a store requires an ack
      // msg_p->m_AckFree = pkt_p->getStoreAckFree();

      // fake this to another scratchpad if decoupled access prefetch
      // m_machineID.num is just the flat scratchpad idx (0-numCores)
      // you also need to change PrefetchAddr to appropriate spad location of that core
      if (isPrefetch) {
        int padOriginIdx = pkt_p->getXOrigin() + pkt_p->getYOrigin() * m_grid_dim_x;
        msg_p->m_Requestor.num = padOriginIdx;
        // add coreOffset << 12 to get the right spad address
        int coreOffset = padOriginIdx - m_machineID.num;
        msg_p->m_PrefetchAddress += (coreOffset << 12);
        // DPRINTF(Mesh, "send prelw from spad %d to origin %d offset %d\n", m_machineID.num, padOriginIdx, coreOffset << 12);
      }

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
      } else if (pkt_p->isSpadPrefetch()) {
        msg_p->m_Type = LLCRequestType_SPLOAD;
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

      // set the pending packet only if requires an ack (prefetch does not)
      if (!noLLCAck) {
        m_pending_pkt_map[m_cur_seq_num] = pkt_p;
        m_cur_seq_num++;
      }

      if (msg_p->m_SeqNum == 143 && m_cpu_p->cpuId() == 13) {
        DPRINTF(Mesh, "Sent pkt %s to LLC seq_num %d\n",
          pkt_p->print(), m_cur_seq_num - 1);
        assert(m_pending_pkt_map.count(143) == 1);
      }
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
    // record remote access here
    if (pkt_p->isRead()) m_remote_loads++;
    else if (pkt_p->isWrite()) m_remote_stores++;
    
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
  // write data into this pointer in spad data array
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
  assert(m_cpu_resp_pkts.front() != nullptr);

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

int
Scratchpad::getCoreEpoch() {
  // int coreEpoch = m_cpu_p->getRevecEpoch();
  int coreEpoch = m_cpu_p->getMemEpoch();
  return coreEpoch;
}

int
Scratchpad::getNumRegions() {
  return m_cpu_p->getSpadNumRegions();
}

int
Scratchpad::getRegionElements() {
  return m_cpu_p->getSpadRegionSize();
}

int
Scratchpad::getDesiredRegion(Addr addr) {
  // based on region settings, can figure out which region
  // this addr belongs to
  int padIdx = getLocalAddr(addr) / sizeof(uint32_t);
  // need to consider spad offset to where the prefetch region starts
  // NOTE currently assumed to be directly after metadata bits
  int prefetchSectionIdx = padIdx - SPM_DATA_WORD_OFFSET;
  int region = prefetchSectionIdx / getRegionElements();
  DPRINTF(Mesh, "addr %#x padIdx %d region %d\n", addr, padIdx, region);
  assert(region < getNumRegions());
  return region;
}

bool
Scratchpad::controlDiverged() {
  Vector *vec = m_cpu_p->getEarlyVector();
  return vec && vec->isCurDiverged();
}

bool
Scratchpad::memoryDiverged(Addr addr) {
  // if ahead of current local epoch or the word ready flag has not been
  // reset yet, then memory can't be accepted
  // return (isPrefetchAhead(pktEpoch) || isWordRdy(addr));
  // bool ahead = isPrefetchAhead(pktEpoch);

  // // you also need to prevent current region from moving into the coreepoch region
  // // that would cause deadlock!
  // // int coreEpochMod = getCoreEpoch() % getNumRegions();
  // int coreEpochMod = getCoreEpoch();
  // int nextPrefectchRegion = (m_cur_prefetch_region + 1) % getNumRegions();
  // bool wouldOverlap = (nextPrefectchRegion == coreEpochMod) && (m_region_cntr + 1 == getRegionElements());
  // return ahead || wouldOverlap;

  // TODO actually don't need to send pktEpoch.
  // can figure out which epoch it should be in just based on the address
  return isPrefetchAhead(addr);
}

bool
Scratchpad::isPrefetchAhead(Addr addr) {
  int pktEpochMod = getDesiredRegion(addr);
  int coreEpochMod = getCoreEpoch(); // TODO can we just mod everything to keep numbers cycling rather than go on forever?
  // bool overlap = (pktEpoch - coreEpoch >= getNumRegions());
  // NOTE In the cirular epoch scheme there is no way to know whether you have overlapped b/c mod removes info
  // HOWEVER We prevent overlap from ever happening by preventing the prefetch region from moving into the region currently
  // being accessed by the core.

  // packet is ahead of the prefetch region, so can't process yet
  bool aheadCntr = (pktEpochMod != m_cur_prefetch_region);

  // packet would bring prefetch region to overlap with core access region and would cause undetectable overwrite
  // don't allow to be processed yet
  int nextPrefectchRegion = (m_cur_prefetch_region + 1) % getNumRegions();
  bool wouldOverlap = (nextPrefectchRegion == coreEpochMod) && (m_region_cntr + 1 == getRegionElements());

  DPRINTF(Mesh, "wouldOverlap %d ahead %d pktEpoch %d coreEpoch %d prefetchRegion %d region cntr %d\n", 
    wouldOverlap, aheadCntr, pktEpochMod, coreEpochMod, m_cur_prefetch_region, m_region_cntr);
  return wouldOverlap || aheadCntr;
}

bool
Scratchpad::isWordRdy(Addr addr) {
  // return m_fresh_array[getLocalAddr(addr) / sizeof(uint32_t)] != 0;

  // prefetch region has to be ahead of core epoch to be valid region
  // TODO prefetch region can't go into the current epoch region for this to work
  // i.e. prefetch all 8 regions then move prefetch region into coreepoch, even if don't
  // overfetch will still prevent this condition
  // int epochModRegion = getCoreEpoch() % getNumRegions();
  int epochModRegion = getCoreEpoch();
  bool ret = (epochModRegion != m_cur_prefetch_region);
  return ret;


}

void
Scratchpad::setWordRdy(Addr addr) {
  // bool memDiv = false;
  // int &tag = m_fresh_array[getLocalAddr(addr) / sizeof(uint32_t)];

  // // if was already ready then something wrong
  // if (tag == 1) {
  //   memDiv = true;
  // }
  // assert(!memDiv);

  // tag = 1;

  // increment the counter for number of expected loads
  m_region_cntr++;

  // if reaches number of expected then reset and move to next region 
  if (m_region_cntr == getRegionElements()) {
    resetRdyArray();
  }

  // DPRINTF(Mesh, "increment region wiht addr %#x cnt now %d\n", addr, m_region_cntr);
}

void
Scratchpad::setWordNotRdy(Addr addr) {
  // // spad loads set this as not ready
  // int &tag = m_fresh_array[getLocalAddr(addr) / sizeof(uint32_t)];
  // tag = 0;
}

void
Scratchpad::resetRdyArray() {
  // // just reset for the current region
  // // TODO potentially can get away with only marking region as being ready?
  // // or having the first spad entry at the beginning of each region mark whether ready or not
  // int regionIdx = (getCoreEpoch() - 1) % getNumRegions(); // epoch will have update so use the last one
  // int startOffset = SPM_DATA_WORD_OFFSET; // 4 * 32bits 
  // for (int i = regionIdx * getRegionElements() + startOffset; i < (regionIdx + 1) * getRegionElements() + startOffset; i++) {
  //   m_fresh_array[i] = 0;
  // }

  // we can now prefetch in the next region
  m_cur_prefetch_region = (m_cur_prefetch_region + 1) % getNumRegions();

  // start counting for that next region
  m_region_cntr = 0;
}

void
Scratchpad::regStats()
{
  AbstractController::regStats();
  
  m_local_loads
        .name(name() + ".local_loads")
        .desc("Number of loads completed by the local core")
        ;
        
  m_local_stores
        .name(name() + ".local_stores")
        .desc("Number of stores completed by the local core")
        ;
        
  m_remote_loads
        .name(name() + ".remote_loads")
        .desc("Number of loads completed by a remote core")
        ;
        
  m_remote_stores
        .name(name() + ".remote_stores")
        .desc("Number of stores completed by a remote core")
        ;
        
  m_local_accesses
        .name(name() + ".local_accesses")
        .desc("Number of local accesses completed")
        ;
        
  m_remote_accesses
        .name(name() + ".remote_accesses")
        .desc("Number of remote accesses completed")
        ;
        
  m_total_accesses
        .name(name() + ".total_accesses")
        .desc("Number of accesses completed")
        ;

  m_max_queue_size
        .name(name() + ".max_queue_size")
        .desc("The larget amount of pending entries in this queue")
        ;

  m_not_rdy_stalls
        .name(name() + ".lwspec_not_rdy")
        .desc("lwspec can't proceed due to rdy bit not set")
        ;

  m_exceed_stream_width
        .name(name() + ".exceed_stream_width")
        .desc("spad can't process request because no buffer space")
        ;

  m_local_accesses = m_local_loads + m_local_stores;
  m_remote_accesses = m_remote_loads + m_remote_stores;
  m_total_accesses = m_local_accesses + m_remote_accesses;
  
}

Scratchpad*
ScratchpadParams::create()
{
  return new Scratchpad(this);
}
