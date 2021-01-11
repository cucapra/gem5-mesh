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
#include "arch/registers.hh"
#include "cpu/io/cpu.hh"

#include "debug/Frame.hh"
#include "debug/LoadTrack.hh"
#include "debug/RubyNetwork.hh"

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
      // m_pending_base_addr_req(MachineID(), nullptr),
      // m_pending_go_flag_req(MachineID(), nullptr),
      // m_pending_done_flag_req(MachineID(), nullptr),
      // m_base_addr_p((uint64_t* const) (m_data_array + SPM_BASE_ADDR_OFFSET)),
      // m_go_flag_p((uint32_t* const)(m_data_array + SPM_GO_FLAG_OFFSET)),
      // m_done_flag_p((uint32_t* const)(m_data_array + SPM_DONE_FLAG_OFFSET)),
      m_num_l2s(p->num_l2s),
      m_grid_dim_x(p->grid_dim_x),
      m_grid_dim_y(p->grid_dim_y),
      m_cpu_p(p->cpu),
      m_max_pending_sp_prefetches(p->prefetchBufSize),
      m_num_frame_cntrs(p->numFrameCntrs),
      m_process_resp_event([this]{ processRespToSpad(); }, "Process a resp to spad", false),
      m_proc_ruby_last(false),
      m_net_word_width(p->netWidth)
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
  // for (int i = 0; i < NUM_REGION_CNTRS; i++) {
  //   m_region_cntrs.push_back(0);
  // }
  // m_cur_prefetch_region = 0;
  resetAllRegionCntrs();
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
        
        // just immedieatly fail now
        DPRINTF(Frame, "[[WARNING]] must diverge now\n");
        assert(false);

        // place packet into buffer to use later
        // assure that this is a very small buffer otherwise actually diverge
        // in wakeup check this buffer too to see if rdy and can place into queue (or maybe somehwere else
        //m_sp_prefetch_buffer.push_back(pkt_p);
        enqueueStallRespToSp(pkt_p);
        

        DPRINTF(Frame, "[[WARNING]] potential diverge, now %d pending epoch %d core epoch %d addr %#x spadEntry %d\n", 
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
          DPRINTF(Frame, "[[WARNING]] must diverge now\n");
          assert(false);
          // clear the pending buffer and inform cpu of divergence, to get squash
          //m_sp_prefetch_buffer.clear();
          
          // TODO inform CPU of divergence
          // should be extremely rare
        }
      }
      else {
        uint8_t *allData = pkt_p->getPtr<uint8_t>();

        // support wide responses
        for (int i = 0; i < pkt_p->getRespCnt(); i++) {
          // profile, TODO should classify different than just remote load/store?
          if (pkt_p->isRead())
            m_remote_loads++;
          else if (pkt_p->isWrite())
            m_remote_stores++;

          PacketPtr tpkt_p = new Packet(pkt_p, true, false);
          tpkt_p->dataStatic(&allData[i * sizeof(uint32_t)]);
          Addr effAddr = pkt_p->getAddr() + i * sizeof(uint32_t);
          tpkt_p->setAddr(effAddr);

          // access data array
          accessDataArray(tpkt_p);

          DPRINTF(Frame, "store spec prefetch %#x | %llu\n", tpkt_p->getAddr(), tpkt_p->getSize());

          delete tpkt_p;
          setWordRdy(effAddr);
        }

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

// helper to figure out where the response word is (and maybe the address)
const uint8_t*
Scratchpad::decodeRespWord(PacketPtr pending_pkt_p, const LLCResponseMsg* llc_msg_p) {
  int len = pending_pkt_p->getWordSize();
  int offset = llc_msg_p->m_BlkIdx;
  
  if (llc_msg_p->m_Type == LLCResponseType_REVDATA ||
        llc_msg_p->m_Type == LLCResponseType_REDATA) {
    offset *= len; // blk idx is / sizeof(int) here
    len *= pending_pkt_p->req->respCnt; // if doing wide io may have sent more
  }
  else if (!pending_pkt_p->isVector()) {
    assert(llc_msg_p->m_LineAddress ==
                          makeLineAddress(pending_pkt_p->getAddr()));
  }

  const uint8_t* data_p = (llc_msg_p->m_DataBlk).getData(offset, len);
  return data_p;
}

Addr
Scratchpad::decodeRespAddr(PacketPtr pending_pkt_p, const LLCResponseMsg* llc_msg_p) {
  if (llc_msg_p->m_Type == LLCResponseType_REVDATA ||
        llc_msg_p->m_Type == LLCResponseType_REDATA) {
    return llc_msg_p->m_LineAddress; // word address is encoded in line address for these
  }// above clause if problematic
  else {
    // cant encode word address in line address otherwise theres an error
    if (!pending_pkt_p->isVector()) {
      return pending_pkt_p->getAddr();
    }
    else {
      DPRINTF(RiscvVector, "resp addr %#x\n", llc_msg_p->m_LineAddress + (Addr)llc_msg_p->m_BlkIdx);
      return llc_msg_p->m_LineAddress + (Addr)llc_msg_p->m_BlkIdx;
    }
  }
}


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

      if (pkt_p->isRead())
        DPRINTF(LoadTrack, "Handling remote SP load locally %#x\n", pkt_p->getAddr());

      if (m_cpu_p->getEarlyVector()->getConfigured()) DPRINTF(Scratchpad, "Recv remote resp pkt %#x\n", pkt_p->getAddr());

      DPRINTF(RubyNetwork, "spad pull msg %p @addr %#x\n", mem_msg_p, pkt_p->getAddr());

      // Pop the message from mem_resp_buffer
      m_mem_resp_buffer_p->dequeue(clockEdge());
      
      enqueueRubyRespToSp(pkt_p, Packet::RespPktType::Remote_Resp);
      
    } else if (llc_msg_p && ((llc_msg_p->m_Type == LLCResponseType_REVDATA))) {
        // data from a vector request on this scratchpads behalf
        // mimic as a remote store from the master core
        
        //DPRINTF(Mesh, "ruby pkt %s\n", *llc_msg_p);
        
        // need to 'fake' a Packet from the LLC response and then go
        // through the regular remote store channels
        std::shared_ptr<Request> req =
                std::make_shared<Request>(llc_msg_p->m_LineAddress,    // vaddr
                                          sizeof(uint32_t),    // size
                                          0, 0);
        
        req->respCnt = llc_msg_p->m_Epoch;
        size_t buf_size = sizeof(uint32_t) * req->respCnt;
        
        PacketPtr pkt_p = Packet::createWrite(req);

        DPRINTF(RubyNetwork, "spad pull msg %p @addr %#x\n", llc_msg_p, pkt_p->getAddr());
        
        // we really only sent a word of data, so extract that
        const uint8_t *data = decodeRespWord(pkt_p, llc_msg_p);
        uint8_t *buff = new uint8_t[buf_size];
        memcpy(buff, data, buf_size);
        pkt_p->dataDynamic(buff);
        
        DPRINTF(Frame, "Recv remote store %#x from cache epoch %d data %d %d %d %d\n", 
          llc_msg_p->m_LineAddress, llc_msg_p->m_Epoch, data[3], data[2], data[1], data[0]);
        
        assert(getScratchpadIdFromAddr(pkt_p->getAddr()) == m_version);
      
        enqueueRubyRespToSp(pkt_p, Packet::RespPktType::Prefetch_Patron_Resp);
      
        // delete the pending packet // why would there be a pending pkt for this?
        // m_pending_pkt_map.erase(llc_msg_p->m_SeqNum);
        m_mem_resp_buffer_p->dequeue(clockEdge());

    } else if (m_pending_pkt_map.count(llc_msg_p->m_SeqNum) == 0 && 
              llc_msg_p->m_Type == LLCResponseType_ACK) {
      DPRINTF(Scratchpad, "process store noack, ack\n");
      // we just need to send back to the core that we received an ack
      // make a fake resp packet to send back
      std::shared_ptr<Request> req =
                std::make_shared<Request>(llc_msg_p->m_LineAddress,    // vaddr
                                          sizeof(uint32_t),    // size
                                          0, 0);
      req->isStoreNoAck = true;
      PacketPtr pending_mem_pkt_p = Packet::createWrite(req);
      pending_mem_pkt_p->pushSenderState(new MemUnit::SenderState(nullptr));
      pending_mem_pkt_p->makeResponse();

      // Pop the message from mem_resp_buffer
      m_mem_resp_buffer_p->dequeue(clockEdge());
      
      enqueueRubyRespToSp(pending_mem_pkt_p, Packet::RespPktType::LLC_Data_Resp);

    } else {

      // sanity check: make sure this is the response we're waiting for
      assert(m_pending_pkt_map.count(llc_msg_p->m_SeqNum) == 1);

      Packet* pending_mem_pkt_p = m_pending_pkt_map[llc_msg_p->m_SeqNum]->getPacket();

      DPRINTF(RubyNetwork, "spad pull msg %p @addr %#x\n", llc_msg_p, pending_mem_pkt_p->getAddr());

      // assert(pending_mem_pkt_p &&
      //        llc_msg_p->m_LineAddress ==
      //                       makeLineAddress(pending_mem_pkt_p->getAddr()));
      assert(pending_mem_pkt_p);
      if (pending_mem_pkt_p->getRespCnt() == 1) {
        assert(llc_msg_p->m_LineAddress ==
                      makeLineAddress(pending_mem_pkt_p->getAddr()));
      }

      m_pending_pkt_map[llc_msg_p->m_SeqNum]->recvMemResp();

      if (llc_msg_p->m_Type == LLCResponseType_DATA || 
            llc_msg_p->m_Type == LLCResponseType_REDATA) {
        // copy data from DataBlock to pending_mem_pkt_p
        // just pulls out a single word from the block and discards the rest?
        //DPRINTF(Mesh, "%d %d %#x %#x\n", offset, len, pending_mem_pkt_p->getAddr(), llc_msg_p->m_LineAddress);
        const uint8_t* data_p = decodeRespWord(pending_mem_pkt_p, llc_msg_p);
        Addr wordAddr = decodeRespAddr(pending_mem_pkt_p, llc_msg_p);
        // pending_mem_pkt_p->setData(data_p);
        if (pending_mem_pkt_p->isVector()) {
          DPRINTF(RiscvVector, "load resp %#x(%#x), offset %d\n", 
            wordAddr, llc_msg_p->m_LineAddress, llc_msg_p->m_BlkIdx);
        }
        m_pending_pkt_map[llc_msg_p->m_SeqNum]->setData(
          wordAddr, data_p, pending_mem_pkt_p->getWordSize());


      } else if (llc_msg_p->m_Type == LLCResponseType_ACK) {
        if (pending_mem_pkt_p->isLLSC()) {
          assert(pending_mem_pkt_p->req);
          pending_mem_pkt_p->req->
                            setExtraData((uint64_t) llc_msg_p->m_SC_Success);
        }
      } else {
        panic("Received wrong LLCResponseType");
      }

      // remove pending_mem_pkt_p from the pending pkt map
      bool allRecv = m_pending_pkt_map[llc_msg_p->m_SeqNum]->allRespRecv();
      if (allRecv) {
        // turn around the request packet
        pending_mem_pkt_p->makeResponse();

        DPRINTF(Scratchpad, "Handling mem resp pkt %s from LLC seq_num %d\n",
                            pending_mem_pkt_p->print(), llc_msg_p->m_SeqNum);

        m_pending_pkt_map.erase(llc_msg_p->m_SeqNum);
      }

      // Pop the message from mem_resp_buffer
      m_mem_resp_buffer_p->dequeue(clockEdge());
      
      if (allRecv)
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

std::shared_ptr<LLCRequestMsg>
Scratchpad::createLLCReqPacket(Packet* pkt_p, Addr addr, int respCnt) {
  bool isPrefetch = pkt_p->isSpadPrefetch();
  bool noAckStore = pkt_p->isStoreNoAck();
  bool noLLCAck = isPrefetch || noAckStore;

  MachineID dst_port = { MachineType_L2Cache, getL2BankFromAddr(addr) };

  // make and queue an LLCRequest message
  std::shared_ptr<LLCRequestMsg> msg_p
                            = std::make_shared<LLCRequestMsg>(clockEdge());
  msg_p->m_LineAddress = makeLineAddress(addr); // TODO don't really need this?
  msg_p->m_Requestor = m_machineID;
  msg_p->m_MessageSize = MessageSizeType_Request_Control;
  (msg_p->m_Destination).add(dst_port);
  
  if (noLLCAck)
    msg_p->m_SeqNum = -1;
  else
    msg_p->m_SeqNum = m_cur_seq_num;

  // access length in x,y -- prob always linearized
  msg_p->m_XDim = pkt_p->getXDim();
  msg_p->m_YDim = pkt_p->getYDim();
  // msg_p->m_FromDA = pkt_p->getFromDecoupledAccess();
  // msg_p->m_VecOffset = pkt_p->getVecOffset();
  // can't just use line address when doing vec load, need to know start and offsets from it
  msg_p->m_WordAddress = addr;
  // for prefetches instead of sending data blk we send an address
  // pre-interpret it here, but not actually an additional field
  msg_p->m_PrefetchAddress = pkt_p->getPrefetchAddr();
  msg_p->m_CoreOffset = pkt_p->getCoreOffset();
  msg_p->m_RespCnt = respCnt;
  msg_p->m_PrefetchConfig = pkt_p->getPrefetchConfig();
  // msg_p->m_InstSeqNum = pkt_p->getCoreOffset(); // temp for debug
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

    int offset = addr - makeLineAddress(addr);
    int len = pkt_p->getSize();
    (msg_p->m_writeMask).setMask(offset, len);
    (msg_p->m_writeMask).addAtomicOp(offset, pkt_p->getAtomicOp());
  } else if (pkt_p->isLLSC()) {   // LL/SC ops
    if (pkt_p->isRead()) {
      msg_p->m_Type = LLCRequestType_LL;
    } else if (pkt_p->isWrite()) {
      msg_p->m_Type = LLCRequestType_SC;

      int offset = addr - makeLineAddress(addr);
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
    msg_p->m_RespCnt = 1; // vector stores are processed one at a time
    int offset = addr - makeLineAddress(addr);
    int len = pkt_p->getWordSize();
    int regOffset = pkt_p->getWordOffset(addr);
    if (pkt_p->isVector()) DPRINTF(RiscvVector, "writing offset %d to addr %#x %d\n", regOffset, addr, offset);
    (msg_p->m_DataBlk).setData(&pkt_p->getConstPtr<uint8_t>()[regOffset], offset, len);
    (msg_p->m_writeMask).setMask(offset, len);
  }

  return msg_p;
}

typedef struct WordAndCnt_t {
  Addr baseAddr;
  int respCnt;

  WordAndCnt_t(Addr baseAddr, int respCnt) {
    this->baseAddr = baseAddr;
    this->respCnt =  respCnt;
  }
} WordAndCnt_t;

// determine number of packets and word addresses that we need to send
std::vector<WordAndCnt_t> getNeededReqs(Packet* pkt_p) {
  std::vector<WordAndCnt_t> reqs;

  if (!pkt_p->isVector()) 
    reqs.push_back(WordAndCnt_t(pkt_p->getAddr(), pkt_p->getRespCnt()));
  // try to merge loads into single vector if possible
  else {
    auto vecAddrs = pkt_p->getVecAddrs();
    // writes must be split always
    if (pkt_p->isWrite()) {
      for (auto& a : vecAddrs) {
        reqs.push_back(WordAndCnt_t(a, 1));
      }
    }
    // loads can be merged under to conditions
    // 1) contigous
    // 2) within same cache line
    else {
      // check if words are contigious
      // TODO check cache line breaks
      int numContigReqs = 1;
      Addr baseAddr = vecAddrs[0];
      for (int i = numContigReqs; i < vecAddrs.size(); i++) {
        // break up if not true
        Addr prevAddr = vecAddrs[i - 1];
        Addr nextAddr = vecAddrs[i];
        if ((nextAddr != (prevAddr + pkt_p->getWordSize())) ||
            (makeLineAddress(nextAddr) != makeLineAddress(prevAddr))) {
          // commit the prev request
          DPRINTF(RiscvVector, "create req %#x size %d\n", baseAddr, numContigReqs);
          reqs.push_back(WordAndCnt_t(baseAddr, numContigReqs));

          // restart the count
          baseAddr = nextAddr;
          numContigReqs = 0;
        }

        numContigReqs++;
      }

      // log last req
      DPRINTF(RiscvVector, "create req %#x size %d\n", baseAddr, numContigReqs);
      reqs.push_back(WordAndCnt_t(baseAddr, numContigReqs));
    }
  }


  return reqs;
}

bool
Scratchpad::handleCpuReq(Packet* pkt_p)
{
  assert(pkt_p->isRequest());

  if (pkt_p->isRead())
    DPRINTF(LoadTrack, "Load request received in local SP %#x\n", pkt_p->getAddr());

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
    // if (pkt_p->getSpecSpad() && !isWordRdy(pkt_p->getAddr())) {
    if (m_cpu_p->getEarlyVector()->isSlave()){
      if (isRegionAccess(pkt_p) && !isWordRdy(pkt_p->getAddr())){
        //m_packet_buffer.push_back(pkt_p);
        //assert(m_packet_buffer.size() <= m_spec_buf_size);
        // just say not rdy actually
        m_not_rdy_stalls++;
        DPRINTF(Frame, "not rdy for packet to addr %#x\n", pkt_p->getAddr());
        return false;
      }
    }
    
    // TODO stats might be incorrect with these stalls
    // should move into accessDataArray rather than keep out here
    
    // This is a local access
    DPRINTF(Scratchpad, "Doing a local access for pkt %s\n", pkt_p->print());
    // if (m_cpu_p->getEarlyVector()->getConfigured()) 
    //   DPRINTF(Mesh, "Doing a local access for pkt %s coreepoch %d prefetchEpoch %d cnt%d\n", 
    //     pkt_p->print(), getCoreEpoch(), m_cur_prefetch_region, m_region_cntr);

    if (isRegionAccess(pkt_p) && pkt_p->isRead()) m_local_loads_region++;
    if (isRegionAccess(pkt_p) && pkt_p->isWrite()) m_local_stores_region++;
    // record local access here
    if (pkt_p->isRead())
    {
      m_local_loads++;
      DPRINTF(LoadTrack, "Processing local load with addr %#x\n", pkt_p->getAddr());
    }
    else if (pkt_p->isWrite()) m_local_stores++;
    
    // NOTE vector accesses to scratchpad happen in one cycle (i.e., modeling wide io between cpu and spad. prob fine b/c local)
    accessDataArray(pkt_p);

    // if (isRegionAccess(pkt_p) && pkt_p->isRead()){
    //   uint32_t *local_data_p = (uint32_t*)(m_data_array + getLocalAddr(pkt_p->getAddr()));
    //   float data = *(float*)local_data_p;
    //   DPRINTF(Mesh, "reading packet in Scratchpad addr %#x -- %f \n", pkt_p->getAddr(), data);
    // }

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
  // MachineID dst_port;

  if (dst_sp_id == m_num_scratchpads) {
    // this packet can be modified to not access global memory in case of slave
    // core but rather just update info in the spad
    // TODO currently checking normal pkt map, should we make our own so that can be
    // unlimited pending? doesn't seem like the spad has to track anything for a pending load?
    bool isPrefetch = pkt_p->isSpadPrefetch();
    bool noAckStore = pkt_p->isStoreNoAck();
    bool noLLCAck = isPrefetch || noAckStore;
    // bool pendingPktSpad = (m_pending_pkt_map.size() < m_max_num_pending_pkts) || noLLCAck;
    if (isPrefetch /*&& pendingPktSpad*/) {
      // setWordNotRdy(pkt_p->getPrefetchAddr());
      
      // DPRINTF(Mesh, "reset word %#x\n", pkt_p->getPrefetchAddr());
      
      // deliver a response packet to the core that this was completed
      // but need to copy it because will be delete there
      PacketPtr resp_pkt_p = pkt_p; //new Packet(pkt_p, true, false);
      resp_pkt_p->makeResponse();
      m_cpu_resp_pkts.push_back(resp_pkt_p);
      if (!m_cpu_resp_event.scheduled())
        schedule(m_cpu_resp_event, clockEdge(Cycles(1)));


      // log that we sent
      m_cpu_p->getSystemPtr()->initPrefetch(pkt_p->getPrefetchAddr(),
        pkt_p->getXOrigin(), pkt_p->getYOrigin(),
        pkt_p->getXDim(), pkt_p->getYDim(), m_grid_dim_x,
        pkt_p->getCoreOffset(), pkt_p->getRespCnt(), (bool)pkt_p->getPrefetchConfig());

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
      if (m_cpu_p->getEarlyVector()->getConfigured()) DPRINTF(Scratchpad, "Blocking. Pending pkt buffer is full\n");
      m_exceed_stream_width++;
      return false;
    } else {

      // TODO kind of cheat if vector store and enqueu multiple requests to go off
      // assume that network will serialize
      auto reqFrags = getNeededReqs(pkt_p);
      for (int i = 0; i < reqFrags.size(); i++) {
        Addr wordAddr = reqFrags[i].baseAddr;
        Addr respCnt = reqFrags[i].respCnt;
        // make and queue an LLCRequest message
        std::shared_ptr<LLCRequestMsg> msg_p = createLLCReqPacket(pkt_p, wordAddr, respCnt);

        DPRINTF(RubyNetwork, "spad push msg %p @addr %#x %#x\n", msg_p.get(), pkt_p->getAddr(), wordAddr);

        m_mem_req_buffer_p->enqueue(msg_p,
                                    clockEdge(),
                                    cyclesToTicks(Cycles(1/* + i*/)));
      }

      // std::shared_ptr<LLCRequestMsg> msg_p = createLLCReqPacket(pkt_p);
      // DPRINTF(RubyNetwork, "spad push msg %p @addr %#x\n", msg_p.get(), pkt_p->getAddr());
      // m_mem_req_buffer_p->enqueue(msg_p,
      //                             clockEdge(),
      //                             cyclesToTicks(Cycles(1)));

      // set the pending packet only if requires an ack (prefetch does not)
      if (!noLLCAck) {
        m_pending_pkt_map[m_cur_seq_num] = std::make_shared<pkt_map_entry_t>(pkt_p);
        m_cur_seq_num++;
      }

      // if (msg_p->m_SeqNum == 143 && m_cpu_p->cpuId() == 13) {
      //   DPRINTF(Mesh, "Sent pkt %s to LLC seq_num %d\n",
      //     pkt_p->print(), m_cur_seq_num - 1);
      //   assert(m_pending_pkt_map.count(143) == 1);
      // }
      DPRINTF(Scratchpad, "Sent pkt %s to LLC seq_num %d\n",
                        pkt_p->print(), m_cur_seq_num - 1);

      if (noAckStore) {
        delete pkt_p->popSenderState();
        delete pkt_p;
      }
    }
  } else if (dst_sp_id != m_version) {
    // This packet will be delivered to a remote scratchpad
    MachineID dst_port = { MachineType_Scratchpad, dst_sp_id };

    DPRINTF(Scratchpad, "Sending pkt %s to %s\n", pkt_p->print(), dst_port);
    if (pkt_p->isRead())
      DPRINTF(LoadTrack, "Sending Load request to remote SP %#x\n", pkt_p->getAddr());
    if (m_cpu_p->getEarlyVector()->getConfigured()) DPRINTF(Scratchpad, "Sending remote req pkt %#x to %s\n", pkt_p->getAddr(), dst_port);

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
  if (pkt_p->isRead())
    DPRINTF(LoadTrack, "Remote load request received %#x\n", pkt_p->getAddr());
  // Check if we have enough slot in m_remote_resp_buffer to queue a new
  // response message
  if (!m_remote_resp_buffer_p->areNSlotsAvailable(1, clockEdge())) {
    return false;
  }

  MachineID src_port = m_machineID;
  MachineID dst_port = remote_sender;

  if (m_cpu_p->getEarlyVector()->getConfigured()) DPRINTF(Scratchpad, "Recv remote req pkt %#x\n", pkt_p->getAddr());

  bool respond_sender = true;

  /**
   * From a remote CPU/Xcel, access to data
   */

    // TODO i think should guarentee data will be there??
    if (isRegionAccess(pkt_p) && !isWordRdyForRemote(pkt_p->getAddr()))
    //TODO: Will fail if remote access to past region and prefetching going on, but then again accessing into past region is faulty
    {
      DPRINTF(Frame, "remote region access into a region not ready (not cool bro) %s\n", pkt_p->print());
      // return false;
      assert(false);
    }

      // record remote access here
      if (pkt_p->isRead()) m_remote_loads++;
      else if (pkt_p->isWrite()) m_remote_stores++;
      
      // access data array
      accessDataArray(pkt_p);
    // }
  // }



  // Make and queue the message
  if (respond_sender) {
    std::shared_ptr<MemMessage> msg_p =
        std::make_shared<MemMessage>(clockEdge(), src_port, dst_port, pkt_p);

    DPRINTF(Scratchpad, "Sending pkt %s to %s\n", pkt_p->print(), dst_port);

    if (pkt_p->isRead())
      DPRINTF(LoadTrack, "Sending load request from remote SP %#x\n", pkt_p->getAddr());

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
  if (m_cpu_resp_pkts.front()->isRead())
    DPRINTF(LoadTrack, "Sending Load request to CPU %#x\n", m_cpu_resp_pkts.front()->getAddr());

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
  // int coreEpoch = m_cpu_p->getMemEpoch();
  // return coreEpoch;
  return m_cur_consumer_region;
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
  // DPRINTF(Frame, "addr %#x padIdx %d region %d\n", addr, padIdx, region);
  assert(region < getNumRegions());
  return region;
}

bool
Scratchpad::isRegionAccess(Packet* pkt_p) {
  // if (!pkt_p->isRead()) return false;
  int padIdx = getLocalAddr(pkt_p->getAddr()) / sizeof(uint32_t);

  //DPRINTF(Mesh, "pad ID %d\n", padIdx);
  // need to consider spad offset to where the prefetch region starts
  // NOTE currently assumed to be directly after metadata bits
  int prefetchSectionIdx = padIdx - SPM_DATA_WORD_OFFSET;

  int regionEnd = getRegionElements()*getNumRegions();
  bool ret = (prefetchSectionIdx>=0) && (prefetchSectionIdx<regionEnd);
  return ret;
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

  // packet is ahead of any prefetch region, so can't process yet
  bool aheadCntr = true;
  for (int i = 0; i < m_num_frame_cntrs; i++) {
    if (pktEpochMod == getCurPrefetchRegion(i)) {
      aheadCntr = false;
    }
  }

  // packet would bring prefetch region to overlap with core access region and would cause undetectable overwrite
  // don't allow to be processed yet
  // // int nextPrefectchRegion = (m_cur_prefetch_region + 1) % getNumRegions();
  // // TODO issue if don't consume the whole mem token frame
  // bool wouldOverlap = m_cpu_p->getMemTokens() + getRegionElements() >= getAllRegionSize();

  // TODO i dont think this does anything anymore
  // might need to use max(m_num_frame_cntrs, and num_regions) here
  bool wouldOverlap = (getCurPrefetchRegion(m_num_frame_cntrs) == getCurConsumerRegion(0));

  DPRINTF(Frame, "wouldOverlap %d ahead %d pktEpoch %d consumerRegion %d prefetchRegion %d region cntr %d\n", 
    wouldOverlap, aheadCntr, pktEpochMod, m_cur_consumer_region, m_cur_prefetch_region, m_region_cntrs[0]);
  return wouldOverlap || aheadCntr;


  // Addr thisWord = (getLocalAddr(addr) / sizeof(uint32_t)) - SPM_DATA_WORD_OFFSET;
  // Addr nextWord = (getLastWordRecv() + 1) % getAllRegionSize();
  // DPRINTF(Mesh, "check prefetch addr %lx local %d cur local %d\n", addr, thisWord, getLastWordRecv());
  // return (nextWord != thisWord && m_cpu_p->getMemTokens() < getAllRegionSize());


  // int pktEpochMod = getDesiredRegion(addr);
  // int coreEpochMod = getCoreEpoch(); // TODO can we just mod everything to keep numbers cycling rather than go on forever?
  // // bool overlap = (pktEpoch - coreEpoch >= getNumRegions());
  // // NOTE In the cirular epoch scheme there is no way to know whether you have overlapped b/c mod removes info
  // // HOWEVER We prevent overlap from ever happening by preventing the prefetch region from moving into the region currently
  // // being accessed by the core.

  // // packet is ahead of the prefetch region, so can't process yet
  // bool aheadCntr = (pktEpochMod != m_cur_prefetch_region);

  // // packet would bring prefetch region to overlap with core access region and would cause undetectable overwrite
  // // don't allow to be processed yet
  // int nextPrefectchRegion = (m_cur_prefetch_region + 1) % getNumRegions();
  // bool wouldOverlap = (nextPrefectchRegion == coreEpochMod) && (m_region_cntr + 1 == getRegionElements());

  // DPRINTF(Mesh, "wouldOverlap %d ahead %d pktEpoch %d coreEpoch %d prefetchRegion %d region cntr %d\n", 
  //   wouldOverlap, aheadCntr, pktEpochMod, coreEpochMod, m_cur_prefetch_region, m_region_cntr);
  // return wouldOverlap || aheadCntr;
}
bool Scratchpad::isWordRdyForRemote(Addr addr)
{

  //the region where the packet is targeted to
  int pktEpochMod = getDesiredRegion(addr);

  // if the region being accessed remotely is not being written into by vprefetch the word is ready
  bool ret = (pktEpochMod != m_cur_prefetch_region);
  return ret;
}

// TODO deprecated b/c no lwspec
bool
Scratchpad::isWordRdy(Addr addr) {
  // return m_fresh_array[getLocalAddr(addr) / sizeof(uint32_t)] != 0;

  // prefetch region has to be ahead of core epoch to be valid region
  // TODO prefetch region can't go into the current epoch region for this to work
  // i.e. prefetch all 8 regions then move prefetch region into coreepoch, even if don't
  // overfetch will still prevent this condition
  // int epochModRegion = getCoreEpoch();
  // bool ret = (epochModRegion != m_cur_prefetch_region);
  // return ret;

  // just make sure have enough tokens
  // TODO should check in mem unit
  // DPRINTF(Mesh, "check if enough tokens to read cur %d need %d\n", m_cpu_p->getMemTokens(), getRegionElements());
  // return (m_cpu_p->getMemTokens() >= getRegionElements());

  // ????
  return (getDesiredRegion(addr) == m_cur_consumer_region);
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

  m_cpu_p->getSystemPtr()->cmplPrefetch(addr);


  // increment the counter for number of expected loads
  // need to find the right regon cntr
  // m_region_cntr++;
  incRegionCntr(addr);

  // if reaches number of expected then reset and move to next region 
  // publish group of tokens to be accessed/
  // NOTE need a while loop here b/c possible the next region is also full and needs to be swapped
  // while (m_region_cntrs[0] == getRegionElements()) {
  //   resetRdyArray();
  // }

  // m_last_word_recv = (getLocalAddr(addr) / sizeof(uint32_t)) - SPM_DATA_WORD_OFFSET;
  // m_cpu_p->produceMemTokens(1);
  DPRINTF(Frame, "recv addr %lx first region cntr %d prefetch region %d\n", addr, m_region_cntrs[0], m_cur_prefetch_region);

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
  m_cur_prefetch_region = getCurPrefetchRegion(1); //(m_cur_prefetch_region + 1) % getNumRegions();
  DPRINTF(Frame, "goto next prefetch region %d\n", m_cur_prefetch_region);

  // start counting for that next region
  // do swap chain
  for (int i = 0; i < m_num_frame_cntrs - 1; i++) {
    m_region_cntrs[i] = m_region_cntrs[i + 1];
  }
  m_region_cntrs[m_num_frame_cntrs - 1] = 0;

  // m_cpu_p->produceMemTokens(getRegionElements());
}

bool
Scratchpad::isNextConsumerFrameRdy() {
  // DPRINTF(Frame, "check frames %d ?= %d\n", m_cur_consumer_region, m_cur_prefetch_region);
  return (m_cur_consumer_region == m_cur_prefetch_region) && (m_region_cntrs[0] == getRegionElements());
}

void
Scratchpad::incConsumerFrame() {
  resetRdyArray();
  m_cur_consumer_region = getCurConsumerRegion(1);
  DPRINTF(Frame, "inc consumer frame %d\n", m_cur_consumer_region);
}

void
Scratchpad::setupConfig(int csrId, RegVal csrVal) {
  // // finalize stats when devec, TODO doesn't work for multiple kernels
  // if (csrId == RiscvISA::MISCREG_FETCH) {
  //   if (csrVal == 0) {
  //     for (int i = 0; i < m_num_frame_cntrs; i++) {
  //       m_occupancy_offset[i] = m_occupancy_offset[i].value() / num_occupancy_samples / getRegionElements();
  //     }
  //   }
  //   else {
  //     num_occupancy_samples = 0;
  //   }
  // }


  if (csrId == RiscvISA::MISCREG_PREFETCH) {
    DPRINTF(Frame, "prefetch reg config %#x # %d cnt %d\n", csrVal, getNumRegions(), getRegionElements());
    resetAllRegionCntrs();
  }

    // can also probably store region size and number here rather than going to cpu


  // finalize stats when devec
  if (csrId == RiscvISA::MISCREG_FETCH) {
    if (csrVal == 0) {

      for (int i = 0; i < m_num_frame_cntrs; i++) {
        for (int j = 0; j < m_occupancies.size(); j++) {
          m_occupancy_offset[i] += m_occupancies[j].frac_usage(i);
        }
      }

      int totSamples = 0;
      for (int j = 0; j < m_occupancies.size(); j++) {
        totSamples += m_occupancies[j].num_samples;
      }
      if (totSamples > 0) {
        for (int i = 0; i < m_num_frame_cntrs; i++) {
          m_occupancy_offset[i] = m_occupancy_offset[i].value() / totSamples;
        }
      }
    }
    else {
      // num_occupancy_samples = 0;
      m_occupancies.emplace_back(m_num_frame_cntrs, getRegionElements());
    }
  }


}

void
Scratchpad::resetAllRegionCntrs() {
  // DPRINTF(Mesh, "reset frame cntrs\n");
  m_region_cntrs.clear();
  for (int i = 0; i < m_num_frame_cntrs; i++) {
    m_region_cntrs.push_back(0);
  }
  m_cur_prefetch_region = 0;
  m_cur_consumer_region = 0;
}

void
Scratchpad::incRegionCntr(Addr addr) {
  // figure out which region this belongs to
  int region = getDesiredRegion(addr);
  for (int i = 0; i < m_num_frame_cntrs; i++) {
    if (region == getCurPrefetchRegion(i)) {
      m_region_cntrs[i]++;
      DPRINTF(Frame, "addr %#x region %d inc region cntr %d, count now %d\n",
        addr, region, i, m_region_cntrs[i]);
      return;
    }
  }
  assert(false);
}

int
Scratchpad::getCurPrefetchRegion(int offset) {
  return (m_cur_prefetch_region + offset) % getNumRegions();
}

int
Scratchpad::getCurConsumerRegion(int offset) {
  return (m_cur_consumer_region + offset) % getNumRegions();
}

int
Scratchpad::getAllRegionSize() {
  return getNumRegions() * getRegionElements();
}

int
Scratchpad::getNumClosedFrames() {
  int diff = m_cur_prefetch_region - m_cur_consumer_region;
  int closedFrames;
  if (diff >= 0) {
    closedFrames = diff;
  }
  else {
    closedFrames = getNumRegions() - (-1 * diff);
  }
  return closedFrames;
}

void
Scratchpad::profileFrameCntrs() {
  int oIdx = m_occupancies.size() - 1;
  for (int i = 1; i < m_num_frame_cntrs; i++) {
    // count from cpu frame, if region counter comes later then count as full
    int getClosedFrames = getNumClosedFrames();
    if (i < getClosedFrames) {
      // m_occupancy_offset[i] += getRegionElements();
      m_occupancies[oIdx].frameSums[i] += getRegionElements();
    }
    else {
      // m_occupancy_offset[i] += m_region_cntrs[i - getClosedFrames];
      m_occupancies[oIdx].frameSums[i] += m_region_cntrs[i - getClosedFrames];
    }
  }
  // num_occupancy_samples++;
  m_occupancies[oIdx].num_samples++;
}

void
Scratchpad::regStats()
{
  AbstractController::regStats();
  
  m_local_loads
        .name(name() + ".local_loads")
        .desc("Number of loads completed by the local core")
        ;
  m_local_loads_region
      .name(name() + ".local_loads_region")
      .desc("Number of loads completed by the local core in the region");

  m_local_stores
      .name(name() + ".local_stores")
        .desc("Number of stores completed by the local core")
        ;
  m_local_stores_region
      .name(name() + ".local_stores_region")
      .desc("Number of stores completed by the local core in the region");

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

  m_occupancy_offset
    .init(m_num_frame_cntrs)
    .name(name() + ".occupancy")
    .desc("average occupancy of counters on remem relative to consumer frame. cur frame(0) invalid")
    // .flags(Stats::total | Stats::pdf | Stats::dist)
    ;
  
}

Scratchpad*
ScratchpadParams::create()
{
  return new Scratchpad(this);
}
