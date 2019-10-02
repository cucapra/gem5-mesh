# BRG Cycle-Level In-Order CPU Model (BRG IO CPU)

Author: Tuan Ta

Last Update: 2019/09/15

## What is BRG IO CPU?

- A _realistic_ cycle-level in-order CPU model supporting in-order issue,
  out-of-order execution and in-order commit of instructions. This is modeled
  after some current in-order CPUs like ARM Little Cores and Ariane CPU.

- The model is heavily based on existing O3 CPU model in gem5. It faithfully
  obeys execute-in-execute modeling approach to make the model highly timing
  accurate. Functional and timing behaviors are co-simulated at the cycle
  granularity.

- The model supports only syscall emulation mode in gem5. Full-system support
  is future work.

- The model has been tested with RISC-V ISA only. Although no ISA-specific
  information is baked into the model, the model just has not been tested with
  other ISAs.

## Why BRG IO CPU?

- _Highly timing accurate_: Compared to MinorCPU which is another in-order CPU
  model in gem5, this model is _much much more_ timing accurate.

- _Highly modular and extensible_: New back-end execution units like RoCC units
  can be easily plugged into the model. Credit-based control flow scheme
  between major stages also make it easy to add or remove stages to/from the
  pipeline.

## Main pipeline stages

- *Fetch*
  - PC-generation
  - Access i-cache and fetch an entire cache line into its internal L0 buffer
  - Branch prediction
  - Generate dynamic instruction objects that go through the pipeline
- *Decode*
  - Detect mispredicted branch instructions (for unconditional branches) and generate squash signals
- *Rename*
  - Rename source and destination registers
  - Maintain free list of registers
- *Issue-Execute-Writeback*
  - This is a combination of three stages: Issue, Execute, and Writeback
  - _Issue_
    - Strictly issue instructions in their program order
    - Check RAW dependencies. Stall the pipeline if dependencies are not resolved yet.
    - Check for available resources like ROB and execution units
  - _Execute_
    - This is actually a pool of multiple execution units that share the same interface with Issue and Writeback stage
    - Each execution unit works independently and in parallel
    - Execution units can have different latencies and can be either pipelined or unpipelined
    - Consists of a memory unit that is able to handle multiple load, store and atomic instructions
  - _Writeback_
    - Access and update physical register file
    - Mark registers ready as instructions write back
    - This is the only place where value bypassing happens
    - Multiple ready-to-writeback instructions from multiple execution units arbitrate to update the register file
    - Detect mispredicted branch instructions (for conditional branches) and generate squash signals
- *Commit*
  - Commit instructions in program order
  - Handle fault (e.g., page fault, mispredictions, exceptions), system calls and interrupts

## Important implementation choices

### Credit-based flow control between stages

Instruction flows and back pressure between stages are handled by a
credit-based flow control scheme. This makes the model much more intuitive and
hence less error-prone than the existing O3 model. A sender stage maintains the
number of available credits and stops sending further instructions if it runs
out of credits.  A receiver stage acks how many credits it gives back to the
sender every cycle. This enables IO CPU to tick forward (e.g., Fetch ticks
before Decode).

### Execution flow of each stage

TODO: sequence of steps: check squash, check stall, check signal, ...

### Execution context and dynamic instruction class

TODO

### Thread context and thread state

TODO How a thread context is activated, suspended and halted?

### IO CPU and syscall emulation mode

A syscall is executed with a fault in IEW. It is then passed to commit stage.
When it's time to process the syscall in Commit (i.e., when the syscall becomes
the ROB's head instruction), the commit stage inspects that the syscall has a
fault and initiates a squash signal. In the next cycle, the pipeline processes
the squash. One cycle after the squash is completed, the syscall is
_functionally_ handled (i.e., in syscall-emulation mode) and finally committed.

### Single-cycle squash

A stage may initiate a squash signal to other stages. It takes one cycle for a
squash signal to arrive at a target stage after the signal is generated.

After receiving a squash signal, a stage takes one cycle to complete squashing
instructions in the stage. No other instruction can be processed during
squashing cycles.

Single-cycle squash is reasonable for IO CPU in my opinion. We don't have as
many in-flight instructions at a time as O3 CPUs.

This assumption affects how the pipeline deals with emulated syscall, so if
there is any need to change this assumption in the future, one needs to
carefully examine how the pipeline will behave after the change.

## Current status

- [x] Passed single-thread assembly tests
- [x] Test with multiple cores and multiple threads per core
- [ ] Support draining pipeline and switching CPUs (in progress)
- [ ] Adding statistics (e.g., CPI stack previously implemented by Cheng)

## Future features

- [ ] Cache-line-misaligned instruction and data fetches. This requires to split a request into multiple memory packets.
- [ ] Skip cycles when the entire pipeline waits for external events like memory response packets. This will probably improves the simulation performance
- [ ] Store buffer used to commit store instructions early without waiting for memory to respond. Probably not super critical for an in-order CPU like this, but should be fairly easy to implement in the model
