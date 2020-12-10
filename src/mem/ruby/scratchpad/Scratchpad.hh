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

#ifndef __MEM_RUBY_SCRATCHPAD_HH__
#define __MEM_RUBY_SCRATCHPAD_HH__

#include <deque>
#include <unordered_map>
#include <queue>

#include "mem/packet.hh"
#include "mem/port.hh"
#include "mem/ruby/slicc_interface/AbstractController.hh"
#include "mem/ruby/system/RubySystem.hh"
#include "params/Scratchpad.hh"

#include "debug/RiscvVector.hh"

class IOCPU;
class LLCRequestMsg;
class LLCResponseMsg;

//-----------------------------------------------------------------------------
// CpuPort
//-----------------------------------------------------------------------------
// Slave port connected to CPU to handle requests from/responses to CPU

class CpuPort : public SlavePort
{
  public:
    CpuPort(Scratchpad* _scratchpad_p, const std::string& _name);
    ~CpuPort() = default;

    /**
     * Mark that CPU will need to retry sending the last request
     */
    void setRetry() { m_need_retry = true; }

    /**
     * Clear retry flag
     */
    void clearRetry() { m_need_retry = false; }

    /**
     * Check if retry is needed
     */
    bool needRetry() const { return m_need_retry; }

    /**
     * Receive timing request from CPU
     */
    bool recvTimingReq(Packet* pkt) override;

    /**
     * Receive functional request from CPU
     */
    void recvFunctional(Packet* pkt) override;

    /**
     * Receive atomic request from CPU
     */
    Tick recvAtomic(Packet *pkt) override
    { panic("recvAtomic Not implemented\n"); }

    void recvRespRetry() override
    { panic("recvRespRetry Not implemented!\n"); }

    AddrRangeList getAddrRanges() const override
    { panic("getAddrRanges Not implemented!\n"); }

    Scratchpad *getAttachedSpad() const {
      return m_scratchpad_p;
    }

  private:
    Scratchpad* m_scratchpad_p;
    bool m_need_retry;
};

// We reserve the following fields for control flags. Since this is scratchpad,
// software should be fully aware of those flags and their locations in each
// scratchpad
//
// +-----------------------+-----------------------+-------------------------+
// | SPMBaseAddr (64 bits) | xcelgo flag (32 bits) | xceldone flag (32 bits) |
// +-----------------------+-----------------------+-------------------------+

// #define SPM_BASE_ADDR_OFFSET  0
// #define SPM_GO_FLAG_OFFSET    (SPM_BASE_ADDR_OFFSET + sizeof(uint64_t))
// #define SPM_DONE_FLAG_OFFSET  (SPM_GO_FLAG_OFFSET   + sizeof(uint32_t))
// #define SPM_ARGS_OFFSET       (SPM_DONE_FLAG_OFFSET + sizeof(uint32_t))
#define SPM_DATA_WORD_OFFSET  0

class Scratchpad : public AbstractController
{
  public:
    typedef ScratchpadParams Params;
    Scratchpad(const Params* p);
    ~Scratchpad();

    /**
     * Return slave port
     */
    //BaseSlavePort& getSlavePort(const std::string& if_name,
    //                            PortID idx = InvalidPortID); //override;
    Port &getPort(const std::string &if_name,
                  PortID idx=InvalidPortID) override;


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
     * handle CPU request
     */
    bool handleCpuReq(Packet* pkt_p);

    /**
     * handle remote request
     */
    bool handleRemoteReq(Packet* pkt_p, MachineID remote_sender);

    /**
     * handle functional request
     */
    void handleFunctionalCpuReq(Packet* pkt_p);

    /**
     * return number of scratchpad controllers
     */
    static int getNumControllers() { return m_num_scratchpads; }

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
    { warn("Scratchpad does not support functionalRead\n"); return false; }

    int functionalWriteBuffers(PacketPtr& pkt) override;

    int functionalWrite(const Addr &addr, PacketPtr pkt) override
    { warn("Scratchpad does not support functionalWrite\n"); return false; }

    void collateStats() override
    { warn("Scratchpad does not support collateStats()\n"); }

    // inform of a csr write and update if correct one
    void setupConfig(int csrId, RegVal csrVal);

    // log the profiling about frame cntrs on current cycle
    void profileFrameCntrs();

    // increment the consumer frame
    void incConsumerFrame();

    // check if the next consumer frame is ready
    bool isNextConsumerFrameRdy();

  private:
    /**
     * Return NodeID of scratchpad owning the given address
     */
    NodeID getScratchpadIdFromAddr(Addr addr) const;

    /**
     * Return local address from the given address
     */
    Addr getLocalAddr(Addr addr) const;

    bool isLocalAddr(Addr addr) const
    { return getScratchpadIdFromAddr(addr) < m_num_scratchpads; }

    /**
     * Access data array
     */
    void accessDataArray(Packet* pkt_p);

    /**
     * Send pending responses to CPU
     */
    void sendCPUResponse();

    /**
     * Get L2 bank ID from address
     */
    NodeID getL2BankFromAddr(Addr addr) const;
    
    /**
     * Mem divergence. Check CPU registers
     */
   int getCoreEpoch();
   int getNumRegions();
   int getRegionElements();
   //void updateEpoch(int epoch);
   bool controlDiverged();
   bool memoryDiverged(Addr addr);
   bool isPrefetchAhead(Addr addr);
   //bool cpuIsLate(int pktEpoch);
   //bool cpuIsEarly(int pktEpoch);
   //bool cpuIsSynced(int pktEpoch);
   //void updateMasterEpoch(const LLCResponseMsg *llc_msg_p);

   //edit: Neil for remote access LW to region
   bool isWordRdyForRemote(Addr addr);

   /**
     * For bitarray accessign to make sure load not too early to prefetch
     */ 
    bool isWordRdy(Addr addr);
    void setWordRdy(Addr addr);
    void setWordNotRdy(Addr addr);

    /**
     * Get expected region/epoch based on pkt address
     */ 
    int getDesiredRegion(Addr addr);

    //Edit: Neil to check if load is within the region for prefetch
    bool isRegionAccess(Packet* pkt_p);
    
    /**
     * Logic for handling any resp packet to spad
     * Have a seperate method from wakeup() because we want to handle 
     * packets not from ruby
     */ 
    void processRespToSpad();
    
    void enqueueRubyRespToSp(PacketPtr pkt_p, Packet::RespPktType type);
    void enqueueStallRespToSp(PacketPtr pkt_p);

    // get the total size all regions take up in the scratchpad
    int getAllRegionSize();

    // reset current region counter
    void resetRdyArray();
    // increment region counter, if multiple use address to determine which one
    void incRegionCntr(Addr addr);
    // get the current region (offset = 0) or a future region (offset > 1)
    int getCurPrefetchRegion(int offset);
    int getCurConsumerRegion(int offset);

    // reset the counters. meant to happen when set prefetch mask 
    void resetAllRegionCntrs();

    int getNumClosedFrames();

    std::shared_ptr<LLCRequestMsg> createLLCReqPacket(Packet* pkt_p, Addr wordAddr, int respCnt);

    const uint8_t* decodeRespWord(PacketPtr pending_pkt_p, const LLCResponseMsg* llc_msg_p);
    Addr decodeRespAddr(PacketPtr pending_pkt_p, const LLCResponseMsg* llc_msg_p);

  private:
    /**
     * Pointer to Ruby system
     */
    RubySystem* m_ruby_system_p;

    /**
     * This scratchpad's size
     */
    const size_t m_size;

    /**
     * Base address of the SPM
     */
    const Addr m_base_spm_addr;

    /**
     * CPU port
     */
    CpuPort* m_cpu_port_p;

    /**
     * Ruby network buffers
     */
    MessageBuffer* m_mem_req_buffer_p;     // Mem req from this scratchpad
    MessageBuffer* m_mem_resp_buffer_p;    // Mem resp to this scratchpad
    MessageBuffer* m_remote_req_buffer_p;  // Remote req from other scratchpads
    MessageBuffer* m_remote_resp_buffer_p; // Remote resp to other scratchpads

    /**
     * Number of scratchpads
     */
    static int m_num_scratchpads;

    /**
     * Internal data array
     */
    uint8_t* m_data_array;

    /**
     * List of pending CPU response packets
     */
    std::deque<Packet*> m_cpu_resp_pkts;

    /**
     * Event used to respond CPU
     */
    EventFunctionWrapper m_cpu_resp_event;

    /**
     * List of all pending memory packets
     */
    class pkt_map_entry_t {
      private:
      Packet* pkt;
      int recvResps;

      public:
      pkt_map_entry_t(Packet* pkt) {
        this->pkt = pkt;
        recvResps = 0;
      }

      void recvMemResp() {
        recvResps++;
      }

      bool allRespRecv() {
        return recvResps == pkt->getRespCnt();
      }

      Packet* getPacket() {
        return pkt;
      }

      // set word of data packet to be return as a whole
      void setData(Addr addr, const uint8_t *incData, int wordSize) {
        if (!pkt->isVector()) { // for scalar loads just write data (also the addr is the line addr is his case)
          pkt->setData(incData);
          return;
        }

        // int word = (int)(addr - pkt->getAddr());
        int word = pkt->getWordOffset(addr);

        auto data = pkt->getPtr<uint8_t>();
        std::memcpy(&data[word], incData, wordSize);
        // for (int i = 0; i < wordSize; i++) {
          // data[word * wordSize + i] = incData[i];
        // }
      }

      // ~pkt_map_entry_t() {
      // }
    };
    std::unordered_map<uint64_t, std::shared_ptr<pkt_map_entry_t>> m_pending_pkt_map;
    uint64_t m_cur_seq_num;
    const uint64_t m_max_num_pending_pkts;

    /**
     * Queue of pending control requests
     */
    // typedef std::pair<MachineID, Packet*> CtrlReq;
    // CtrlReq m_pending_base_addr_req;
    // CtrlReq m_pending_go_flag_req;
    // CtrlReq m_pending_done_flag_req;

    /**
     * Pointers to control fields
     */
    // uint64_t* const m_base_addr_p;
    // uint32_t* const m_go_flag_p;
    // uint32_t* const m_done_flag_p;

    // Number of L2 banks
    const int m_num_l2s;

    // The grid dimensions of the mesh
    const int m_grid_dim_x;
    const int m_grid_dim_y;
    
    /**
     * Keep a pointer to the local CPU to allow reading of some of its 
     * register. This is valid according to the DJ
     */ 
    IOCPU *m_cpu_p;
    
    /**
     * Store the current epoch of the proccessor, given by pkt
     */ 
    //int m_proc_epoch;
    
    /**
     * The number of outstanding sp.loads allowed
     */ 
    const int m_max_pending_sp_prefetches;

    /**
     * The number of frame cntrs we can have
     */ 
    const int m_num_frame_cntrs;
    
    /**
     * Bit array for each word tracking whether a prefetch has arrived
     * Reset on every trace prefetch, and set when recv the prefetch from master
     */ 
    // std::vector<int> m_fresh_array;
    
    /**
     * Event to process a mem resp packet
     */
    EventFunctionWrapper m_process_resp_event;
    
    /**
     * For arbitration keep track of last process resp
     */ 
    bool m_proc_ruby_last;
    
    /**
     * Keep track of the last cycle we processed a packet
     * Because wakeup does the event in the same cycle (b/c already delayed)
     * Possible that one already fired from the stored buffer
     */ 
    
    /**
     * Allow a small number of sp.load packets to be buffered
     * We should only get these if in trace mode. If exceed size, drop all
     * and diverge
     */ 
    std::queue<PacketPtr> m_prefetch_resp_queue;
    
    /**
     * Incoming queue from ruby
     */
    std::queue<PacketPtr> m_ruby_resp_queue;
    
    /**
     * Counter to keep track of how many pkts have arrived for a region
     * Should only be a 10bit counter and adder setup
     */ 
    std::vector<int> m_region_cntrs;

    /**
     * Keep track of which region we are currently prefetching into
     * i.e. which region the counter is associated with (<10 bits)
     */ 
    int m_cur_prefetch_region;

    /**
     * Keep track of which region we are currently reading from the cpu
     */ 
    int m_cur_consumer_region;

    
    /**
     * Stats to keep track of for the scratchpad
     */
   Stats::Scalar m_local_loads;
   Stats::Scalar m_local_loads_region;
   Stats::Scalar m_local_stores;
   Stats::Scalar m_local_stores_region;
   Stats::Scalar m_remote_loads;
   Stats::Scalar m_remote_stores;

   Stats::Formula m_local_accesses;
   Stats::Formula m_remote_accesses;
   Stats::Formula m_total_accesses;

    Stats::Scalar m_max_queue_size;
    Stats::Scalar m_not_rdy_stalls;

    Stats::Scalar m_exceed_stream_width;

    // record occupancy of counters relative to current one
    // Stats::Vector2d m_occupancy_offset;

    Stats::Vector m_occupancy_offset;
    // int num_occupancy_samples;
    typedef struct occupancy_info_t {
      int num_samples;
      std::vector<int> frameSums;
      int regionElements;

      occupancy_info_t(int numFrames, int regElements) {
        num_samples = 0;
        for (int i = 0; i < numFrames; i++) frameSums.push_back(0);
        regionElements = regElements;
      }

      float frac_usage(int i) {
        if (regionElements == 0) return 0.0f;
        return (float)frameSums[i] / (float)regionElements;
      }
    } occupany_info_t;
    std::vector<occupancy_info_t> m_occupancies;
};

#endif // MEM_RUBY_SCRATCHPAD_HH
