// modded from learning_gem5/part2/simple_cache.cc

#include "custom/harness.hh"

#include "debug/Harness.hh"
#include "sim/system.hh"

Harness::Harness(HarnessParams *params) :
    ClockedObject(params),
    blocked(false), originalPacket(nullptr), waitingPortId(-1)
{
    // Since the CPU side ports are a vector of ports, create an instance of
    // the CPUSidePort for each connection. This member of params is
    // automatically created depending on the name of the vector port and
    // holds the number of connections to this port name
    for (int i = 0; i < params->port_from_cpu_connection_count; ++i) {
        slavePorts.emplace_back(name() + csprintf(".from_cpu[%d]", i), i, this);
    }
    
    for (int i = 0; i < params->port_to_cpu_connection_count; ++i) {
        masterPorts.emplace_back(name() + csprintf(".to_cpu[%d]", i), i, this);
    }
}

Port &
Harness::getPort(const std::string &if_name, PortID idx)
{
    // This is the name from the Python SimObject declaration in Harness.py
    if (if_name == "to_cpu" && idx < masterPorts.size()) {
        return masterPorts[idx];
    } else if (if_name == "from_cpu" && idx < slavePorts.size()) {
        // We should have already created all of the ports in the constructor
        return slavePorts[idx];
    } else {
        // pass it along to our super class
        return ClockedObject::getPort(if_name, idx);
    }
}

/*----------------------------------------------------------------------
 * From CPU port defs (slave)
 *--------------------------------------------------------------------*/

void
Harness::CPUSideSlavePort::sendPacket(PacketPtr pkt)
{
    /*// Note: This flow control is very simple since the cache is blocking.

    panic_if(blockedPacket != nullptr, "Should never try to send if blocked!");

    // If we can't send the packet across the port, store it for later.
    DPRINTF(SimpleCache, "Sending %s to CPU\n", pkt->print());
    if (!sendTimingResp(pkt)) {
        DPRINTF(SimpleCache, "failed!\n");
        blockedPacket = pkt;
    }*/
}

AddrRangeList
Harness::CPUSideSlavePort::getAddrRanges() const
{
  // TODO
  return std::list<AddrRange>();
    //return owner->getAddrRanges();
}

void
Harness::CPUSideSlavePort::trySendRetry()
{
    if (needRetry && blockedPacket == nullptr) {
        // Only send a retry if the port is now completely free
        needRetry = false;
        DPRINTF(Harness, "Sending retry req.\n");
        sendRetryReq();
    }
}

void
Harness::CPUSideSlavePort::recvFunctional(PacketPtr pkt)
{
    // Just forward to the cache.
    return owner->handleFunctional(pkt);
}

bool
Harness::CPUSideSlavePort::recvTimingReq(PacketPtr pkt)
{
    DPRINTF(Harness, "Got request %s\n", pkt->print());

    // Just forward to the cache.
    if (!owner->handleRequest(pkt, id)) {
        DPRINTF(Harness, "Request failed\n");
        // stalling
        needRetry = true;
        return false;
    } else {
        DPRINTF(Harness, "Request succeeded\n");
        return true;
    }
}

void
Harness::CPUSideSlavePort::recvRespRetry()
{
    // We should have a blocked packet if this function is called.
    assert(blockedPacket != nullptr);

    // Grab the blocked packet.
    PacketPtr pkt = blockedPacket;
    blockedPacket = nullptr;

    DPRINTF(Harness, "Retrying response pkt %s\n", pkt->print());
    // Try to resend it. It's possible that it fails again.
    sendPacket(pkt);

    // We may now be able to accept new packets
    trySendRetry();
}

/*----------------------------------------------------------------------
 * To CPU port defs (master)
 *--------------------------------------------------------------------*/ 

void
Harness::CPUSideMasterPort::sendPacket(PacketPtr pkt)
{
    // Note: This flow control is very simple since the cache is blocking.

    panic_if(blockedPacket != nullptr, "Should never try to send if blocked!");

    // If we can't send the packet across the port, store it for later.
    if (!sendTimingReq(pkt)) {
        blockedPacket = pkt;
    }
}

bool
Harness::CPUSideMasterPort::recvTimingResp(PacketPtr pkt)
{
    // Just forward to the cache.
    return owner->handleResponse(pkt);
}

void
Harness::CPUSideMasterPort::recvReqRetry()
{
  /*
    // We should have a blocked packet if this function is called.
    assert(blockedPacket != nullptr);

    // Grab the blocked packet.
    PacketPtr pkt = blockedPacket;
    blockedPacket = nullptr;

    // Try to resend it. It's possible that it fails again.
    sendPacket(pkt);
    */
}

void
Harness::CPUSideMasterPort::recvRangeChange()
{
    //owner->sendRangeChange();
}

bool
Harness::handleRequest(PacketPtr pkt, int port_id)
{
    if (blocked) {
        // There is currently an outstanding request so we can't respond. Stall
        return false;
    }

    DPRINTF(Harness, "Got request for addr %#x\n", pkt->getAddr());

/*
    // This cache is now blocked waiting for the response to this packet.
    blocked = true;

    // Store the port for when we get the response
    assert(waitingPortId == -1);
    waitingPortId = port_id;

    
    // Schedule an event after cache access latency to actually access
    schedule(new EventFunctionWrapper([this, pkt]{ accessTiming(pkt); },
                                      name() + ".accessEvent", true),
             clockEdge(latency));
*/
    return true;
}

bool
Harness::handleResponse(PacketPtr pkt)
{
  /*
    assert(blocked);
    DPRINTF(SimpleCache, "Got response for addr %#x\n", pkt->getAddr());

    // For now assume that inserts are off of the critical path and don't count
    // for any added latency.
    insert(pkt);

    missLatency.sample(curTick() - missTime);

    // If we had to upgrade the request packet to a full cache line, now we
    // can use that packet to construct the response.
    if (originalPacket != nullptr) {
        DPRINTF(SimpleCache, "Copying data from new packet to old\n");
        // We had to upgrade a previous packet. We can functionally deal with
        // the cache access now. It better be a hit.
        bool hit M5_VAR_USED = accessFunctional(originalPacket);
        panic_if(!hit, "Should always hit after inserting");
        originalPacket->makeResponse();
        delete pkt; // We may need to delay this, I'm not sure.
        pkt = originalPacket;
        originalPacket = nullptr;
    } // else, pkt contains the data it needs
*/
    sendResponse(pkt);

    return true;
}

void
Harness::sendResponse(PacketPtr pkt)
{
  /*
    assert(blocked);
    DPRINTF(SimpleCache, "Sending resp for addr %#x\n", pkt->getAddr());

    int port = waitingPortId;

    // The packet is now done. We're about to put it in the port, no need for
    // this object to continue to stall.
    // We need to free the resource before sending the packet in case the CPU
    // tries to send another request immediately (e.g., in the same callchain).
    blocked = false;
    waitingPortId = -1;

    // Simply forward to the memory port
    cpuPorts[port].sendPacket(pkt);

    // For each of the cpu ports, if it needs to send a retry, it should do it
    // now since this memory object may be unblocked now.
    for (auto& port : cpuPorts) {
        port.trySendRetry();
    }
    */
}

void
Harness::handleFunctional(PacketPtr pkt)
{
  /*
    if (accessFunctional(pkt)) {
        pkt->makeResponse();
    } else {
        memPort.sendFunctional(pkt);
    }
    */
}

void
Harness::accessTiming(PacketPtr pkt)
{
  /*
    bool hit = accessFunctional(pkt);

    DPRINTF(SimpleCache, "%s for packet: %s\n", hit ? "Hit" : "Miss",
            pkt->print());

    if (hit) {
        // Respond to the CPU side
        hits++; // update stats
        DDUMP(SimpleCache, pkt->getConstPtr<uint8_t>(), pkt->getSize());
        pkt->makeResponse();
        sendResponse(pkt);
    } else {
        misses++; // update stats
        missTime = curTick();
        // Forward to the memory side.
        // We can't directly forward the packet unless it is exactly the size
        // of the cache line, and aligned. Check for that here.
        Addr addr = pkt->getAddr();
        Addr block_addr = pkt->getBlockAddr(blockSize);
        unsigned size = pkt->getSize();
        if (addr == block_addr && size == blockSize) {
            // Aligned and block size. We can just forward.
            DPRINTF(SimpleCache, "forwarding packet\n");
            memPort.sendPacket(pkt);
        } else {
            DPRINTF(SimpleCache, "Upgrading packet to block size\n");
            panic_if(addr - block_addr + size > blockSize,
                     "Cannot handle accesses that span multiple cache lines");
            // Unaligned access to one cache block
            assert(pkt->needsResponse());
            MemCmd cmd;
            if (pkt->isWrite() || pkt->isRead()) {
                // Read the data from memory to write into the block.
                // We'll write the data in the cache (i.e., a writeback cache)
                cmd = MemCmd::ReadReq;
            } else {
                panic("Unknown packet type in upgrade size");
            }

            // Create a new packet that is blockSize
            PacketPtr new_pkt = new Packet(pkt->req, cmd, blockSize);
            new_pkt->allocate();

            // Should now be block aligned
            assert(new_pkt->getAddr() == new_pkt->getBlockAddr(blockSize));

            // Save the old packet
            originalPacket = pkt;

            DPRINTF(SimpleCache, "forwarding packet\n");
            memPort.sendPacket(new_pkt);
        }
    }
    */
}

bool
Harness::accessFunctional(PacketPtr pkt)
{
  /*
    Addr block_addr = pkt->getBlockAddr(blockSize);
    auto it = cacheStore.find(block_addr);
    if (it != cacheStore.end()) {
        if (pkt->isWrite()) {
            // Write the data into the block in the cache
            pkt->writeDataToBlock(it->second, blockSize);
        } else if (pkt->isRead()) {
            // Read the data out of the cache block into the packet
            pkt->setDataFromBlock(it->second, blockSize);
        } else {
            panic("Unknown packet type!");
        }
        return true;
    }
    return false;
    */
    return true;
}

/*
AddrRangeList
Harness::getAddrRanges() const
{
    DPRINTF(SimpleCache, "Sending new ranges\n");
    // Just use the same ranges as whatever is on the memory side.
    return memPort.getAddrRanges();
}

void
SimpleCache::sendRangeChange() const
{
    for (auto& port : cpuPorts) {
        port.sendRangeChange();
    }
}

void
SimpleCache::regStats()
{
    // If you don't do this you get errors about uninitialized stats.
    MemObject::regStats();

    hits.name(name() + ".hits")
        .desc("Number of hits")
        ;

    misses.name(name() + ".misses")
        .desc("Number of misses")
        ;

    missLatency.name(name() + ".missLatency")
        .desc("Ticks for misses to the cache")
        .init(16) // number of buckets
        ;

    hitRatio.name(name() + ".hitRatio")
        .desc("The ratio of hits to the total accesses to the cache")
        ;

    hitRatio = hits / (hits + misses);

}
*/

Harness*
HarnessParams::create()
{
    return new Harness(this);
}
