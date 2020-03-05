/*
 * Copyright (c) 2004-2005 The Regents of The University of Michigan
 * Copyright (c) 2016 The University of Virginia
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met: redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer;
 * redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution;
 * neither the name of the copyright holders nor the names of its
 * contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * Authors: Gabe Black
 *          Ali Saidi
 *          Korey Sewell
 *          Alec Roelke
 */
#include "arch/riscv/process.hh"

#include <algorithm>
#include <cstddef>
#include <iostream>
#include <iterator>
#include <map>
#include <string>
#include <vector>

#include "arch/riscv/isa.hh"
#include "arch/riscv/isa_traits.hh"
#include "arch/riscv/registers.hh"
#include "base/loader/elf_object.hh"
#include "base/loader/object_file.hh"
#include "base/logging.hh"
#include "base/random.hh"
#include "cpu/thread_context.hh"
#include "debug/Stack.hh"
#include "mem/page_table.hh"
#include "params/Process.hh"
#include "sim/aux_vector.hh"
#include "sim/process.hh"
#include "sim/process_impl.hh"
#include "sim/syscall_return.hh"
#include "sim/system.hh"

using namespace std;
using namespace RiscvISA;

RiscvProcess::RiscvProcess(ProcessParams *params, ObjectFile *objFile) :
        Process(params,
                new EmulationPageTable(params->name, params->pid, PageBytes),
                objFile)
{
    fatal_if(params->useArchPT, "Arch page tables not implemented.");
}

RiscvProcess64::RiscvProcess64(ProcessParams *params, ObjectFile *objFile) :
        RiscvProcess(params, objFile)
{
    // find the start of stack, if using scratchpads then put at the end of local scratchpad
    // otherwise throw some place in memory
    useSpad = ((objFile->spmBase() != 0) && (objFile->spmSize() != 0));

    Addr stack_base;
    if (useSpad) {
        Addr sp_base_paddr = system->memSize();
        Addr sp_base_vaddr = objFile->spmBase();
        Addr sp_size = objFile->spmSize(); // NOTE THIS IS ALL THE SPADS
        // let system know about spad settings from objfile
        system->setSpadSettings(sp_base_paddr, sp_base_paddr + sp_size);
        pTable->map(sp_base_vaddr, sp_base_paddr, sp_size,
                    EmulationPageTable::MappingFlags(0));
        // TODO only works if every core in the system has a spad
        // really need to find number of spads, not number of cpus
        Addr single_sp_size = sp_size / system->getNumSpads();
        Addr sp_offset = single_sp_size * spadIdx;
        stack_base = sp_base_vaddr + single_sp_size + sp_offset; // goes down so should start at the top
    }
    else {
        stack_base = 0x7FFFFFFFFFFFFFFFL;
    }

    const Addr max_stack_size = 8 * 1024 * 1024;
    // NOTE nothing actually uses this
    const Addr next_thread_stack_base = stack_base - max_stack_size;
    const Addr brk_point = roundUp(objFile->bssBase() + objFile->bssSize(),
            PageBytes);
    const Addr mmap_end = 0x4000000000000000L;
    memState = make_shared<MemState>(brk_point, stack_base, max_stack_size,
            next_thread_stack_base, mmap_end);

    Addr stack_phys = 0;
    pTable->translate(stack_base - sizeof(uint32_t), stack_phys);
    printf("new process with virt stack ptr (first word) %#lx physical stack ptr %#lx\n", 
        stack_base, stack_phys);
}

RiscvProcess32::RiscvProcess32(ProcessParams *params, ObjectFile *objFile) :
        RiscvProcess(params, objFile)
{
    const Addr stack_base = 0x7FFFFFFF;
    const Addr max_stack_size = 8 * 1024 * 1024;
    const Addr next_thread_stack_base = stack_base - max_stack_size;
    const Addr brk_point = roundUp(objFile->bssBase() + objFile->bssSize(),
            PageBytes);
    const Addr mmap_end = 0x40000000L;
    memState = make_shared<MemState>(brk_point, stack_base, max_stack_size,
                                     next_thread_stack_base, mmap_end);
}

void
RiscvProcess64::initState()
{

    Process::initState();

    argsInit<uint64_t>(PageBytes);
    for (ContextID ctx: contextIds)
        system->getThreadContext(ctx)->setMiscRegNoEffect(MISCREG_PRV, PRV_U);
        
    // // add support for spad in elf?
    // if (objFile->spmBase() != 0 && objFile->spmSize() != 0) {
    //     Addr sp_base_paddr = system->memSize();
    //     Addr sp_base_vaddr = objFile->spmBase();
    //     Addr sp_size = objFile->spmSize();
    //     pTable->map(sp_base_vaddr, sp_base_paddr, sp_size,
    //                 EmulationPageTable::MappingFlags(0));


    //     // warn("spad init vaddr %#lx paddr %#lx size %#x\n", sp_base_vaddr, sp_base_paddr, sp_size);
    //     // // when setting to stack get issue
    //     // // panic: panic condition !clobber occurred: EmulationPageTable::allocate: addr 0x1000f000 already mapped
    //     // pTable->map(sp_base_vaddr, sp_base_paddr, sp_size,
    //     //             EmulationPageTable::MappingFlags::Clobber);
    // }
    printf("finish stack min %#lx\n", memState->getStackMin());
}

void
RiscvProcess32::initState()
{
    Process::initState();

    argsInit<uint32_t>(PageBytes);
    for (ContextID ctx: contextIds) {
        system->getThreadContext(ctx)->setMiscRegNoEffect(MISCREG_PRV, PRV_U);
        PCState pc = system->getThreadContext(ctx)->pcState();
        pc.rv32(true);
        system->getThreadContext(ctx)->pcState(pc);
    }
}

template<class IntType> void
RiscvProcess::argsInit(int pageSize)
{
    const int RandomBytes = 16;
    const int addrSize = sizeof(IntType);

    updateBias();
    objFile->loadSections(initVirtMem);
    ElfObject* elfObject = dynamic_cast<ElfObject*>(objFile);
    memState->setStackMin(memState->getStackBase());

    // Determine stack size and populate auxv
    Addr stack_top = memState->getStackMin();
    stack_top -= RandomBytes;
    for (const string& arg: argv)
        stack_top -= arg.size() + 1;
    for (const string& env: envp)
        stack_top -= env.size() + 1;
    stack_top &= -addrSize;

    vector<AuxVector<IntType>> auxv;
    if (elfObject != nullptr) {
        auxv.emplace_back(M5_AT_ENTRY, objFile->entryPoint());
        auxv.emplace_back(M5_AT_PHNUM, elfObject->programHeaderCount());
        auxv.emplace_back(M5_AT_PHENT, elfObject->programHeaderSize());
        auxv.emplace_back(M5_AT_PHDR, elfObject->programHeaderTable());
        auxv.emplace_back(M5_AT_PAGESZ, PageBytes);
        auxv.emplace_back(M5_AT_SECURE, 0);
        auxv.emplace_back(M5_AT_RANDOM, stack_top);
        auxv.emplace_back(M5_AT_NULL, 0);
    }
    stack_top -= (1 + argv.size()) * addrSize +
                   (1 + envp.size()) * addrSize +
                   addrSize + 2 * sizeof(IntType) * auxv.size();
    stack_top &= -2*addrSize;
    memState->setStackSize(memState->getStackBase() - stack_top);

    // don't reallocate scratchpad memory b/c already allocated technically
    if (!useSpad) {
        allocateMem(roundDown(stack_top, pageSize),
                roundUp(memState->getStackSize(), pageSize));
    }

    // Copy random bytes (for AT_RANDOM) to stack
    memState->setStackMin(memState->getStackMin() - RandomBytes);
    uint8_t at_random[RandomBytes];
    generate(begin(at_random), end(at_random),
             [&]{ return random_mt.random(0, 0xFF); });
    initVirtMem.writeBlob(memState->getStackMin(), at_random, RandomBytes);

    // Copy argv to stack
    vector<Addr> argPointers;
    for (const string& arg: argv) {
        memState->setStackMin(memState->getStackMin() - (arg.size() + 1));
        initVirtMem.writeString(memState->getStackMin(), arg.c_str());
        argPointers.push_back(memState->getStackMin());
        if (DTRACE(Stack)) {
            string wrote;
            initVirtMem.readString(wrote, argPointers.back());
            DPRINTFN("Wrote arg \"%s\" to address %p\n",
                    wrote, (void*)memState->getStackMin());
        }
    }
    argPointers.push_back(0);

    // Copy envp to stack
    vector<Addr> envPointers;
    for (const string& env: envp) {
        memState->setStackMin(memState->getStackMin() - (env.size() + 1));
        initVirtMem.writeString(memState->getStackMin(), env.c_str());
        envPointers.push_back(memState->getStackMin());
        DPRINTF(Stack, "Wrote env \"%s\" to address %p\n",
                env, (void*)memState->getStackMin());
    }
    envPointers.push_back(0);

    // Align stack
    memState->setStackMin(memState->getStackMin() & -addrSize);

    // Calculate bottom of stack
    memState->setStackMin(memState->getStackMin() -
            ((1 + argv.size()) * addrSize +
             (1 + envp.size()) * addrSize +
             addrSize + 2 * sizeof(IntType) * auxv.size()));
    memState->setStackMin(memState->getStackMin() & (-2 * addrSize));
    Addr sp = memState->getStackMin();
    const auto pushOntoStack =
        [this, &sp](IntType data) {
            initVirtMem.write(sp, data, GuestByteOrder);
            sp += sizeof(data);
        };

    // Push argc and argv pointers onto stack
    IntType argc = argv.size();
    DPRINTF(Stack, "Wrote argc %d to address %#x\n", argc, sp);
    pushOntoStack(argc);

    for (const Addr& argPointer: argPointers) {
        DPRINTF(Stack, "Wrote argv pointer %#x to address %#x\n",
                argPointer, sp);
        pushOntoStack(argPointer);
    }

    // Push env pointers onto stack
    for (const Addr& envPointer: envPointers) {
        DPRINTF(Stack, "Wrote envp pointer %#x to address %#x\n",
                envPointer, sp);
        pushOntoStack(envPointer);
    }

    // Push aux vector onto stack
    std::map<IntType, string> aux_keys = {
        {M5_AT_ENTRY, "M5_AT_ENTRY"},
        {M5_AT_PHNUM, "M5_AT_PHNUM"},
        {M5_AT_PHENT, "M5_AT_PHENT"},
        {M5_AT_PHDR, "M5_AT_PHDR"},
        {M5_AT_PAGESZ, "M5_AT_PAGESZ"},
        {M5_AT_SECURE, "M5_AT_SECURE"},
        {M5_AT_RANDOM, "M5_AT_RANDOM"},
        {M5_AT_NULL, "M5_AT_NULL"}
    };
    for (const auto &aux: auxv) {
        DPRINTF(Stack, "Wrote aux key %s to address %#x\n",
                aux_keys[aux.type], sp);
        pushOntoStack(aux.type);
        DPRINTF(Stack, "Wrote aux value %x to address %#x\n", aux.val, sp);
        pushOntoStack(aux.val);
    }

    ThreadContext *tc = system->getThreadContext(contextIds[0]);
    tc->setIntReg(StackPointerReg, memState->getStackMin());
    tc->pcState(getStartPC());

    memState->setStackMin(roundDown(memState->getStackMin(), pageSize));
}

RegVal
RiscvProcess::getSyscallArg(ThreadContext *tc, int &i)
{
    // If a larger index is requested than there are syscall argument
    // registers, return 0
    RegVal retval = 0;
    if (i < SyscallArgumentRegs.size())
        retval = tc->readIntReg(SyscallArgumentRegs[i]);
    i++;
    return retval;
}

void
RiscvProcess::setSyscallArg(ThreadContext *tc, int i, RegVal val)
{
    tc->setIntReg(SyscallArgumentRegs[i], val);
}

void
RiscvProcess::setSyscallReturn(ThreadContext *tc, SyscallReturn sysret)
{
    if (sysret.successful()) {
        // no error
        tc->setIntReg(SyscallPseudoReturnReg, sysret.returnValue());
    } else {
        // got an error, return details
        tc->setIntReg(SyscallPseudoReturnReg, sysret.encodedValue());
    }
}
