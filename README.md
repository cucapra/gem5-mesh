Trillium
========

[![build status](https://github.com/cucapra/gem5-mesh/workflows/trillium/badge.svg)](https://github.com/cucapra/gem5-mesh/actions)

This is a fork of the [gem5][] simulator repository with a bunch of other stuff for the Trillium architecture.

There are two ways to set up the infrastructure: an automated way inside a [Docker][] container, or a manual way on your host machine.

The following instructions use `top/` as an alias for the top level of this repository.

### Environment Setup with Docker:

1. Install [Docker][].
2. `cd top/docker && docker build -t gem5-mesh .`
3. `./enter_docker.sh` (from inside `top/docker`)

### Environment Setup, Natively (Docker-Free):

If you don't want to use Docker, you can install things manually. You'll need to install some packages and fetch some repositories.

1. Install `build-essential gcc g++ m4 scons zlib1g zlib1g-dev libprotobuf-dev python-dev python autoconf automake autotools-dev curl python3 libmpc-dev libmpfr-dev libgmp-dev gawk bison flex texinfo gperf libtool patchutils bc libexpat-dev python-pip python3-pip`
2. `pip3 install regex colorlog`
3. `pip install numpy matplotlib scipy`
4. `git clone -b rvv-intrinsic https://github.com/riscv/riscv-gnu-toolchain`
5. `cd riscv-gnu-toolchain && ./configure --prefix=<local_install_path> --with-arch=rv64gv_zfh`
6. `make -j16 linux`
7. `export RV_CC10=<local_install_path>/bin/riscv64-unknown-linux-gnu-gcc` or edit path directly in `top/programs-spad/common/common.mk`

### Building:

Regardless of the route you choose, the first thing you'll need to do is build the simulator:

1. `cd top/`
2. `scons -j16 ./build/RVSP/gem5.opt`

### Running Quickstart:

The benchmarks used in our system's evaluation are in the `top/programs-spad` directory. In each subdirectory there, you can type `make run` to execute an individual benchmark.

To collect the data for an entire batch of experiments, there is a collection of scripts available in `top/scripts-phil/eval`. We provide a top-level script to automate simulation, data collection, and plotting:

1. `cd top/scripts-phil/eval`
2. `python artifact.py --experiment=[small,medium,large]`

The `artifact.py` script invokes these other scripts:

1. `run_sim.py`: runs an experiment
2. `extract_stats.py`: extracts gem5 simulation data
3. `organizer.py`: plots graph of data

Each experiment size produces a part or all of the key data presented in the original paper. The time will vary depending on the number of cores available to parallelize the simulations. Each simulation takes 2-30 hours depending on the benchmark and configuration.

These are the available experiment sizes:

1. `small`: 10 simulations (5 benchmarks, 2 configs) -- recommended on 4-core systems.
2. `medium`: 30 simulations (15 benchmarks, 2 configs) -- recommended on 16-core systems.
3. `large`: 65 simulations (15 benchmarks, 4-5 configs) -- recommended on 32-core or more systems.

Plots will be generated in the same directory (`top/scripts-phil/eval`):

1. `artifact_speedup.png`: compares the execution time between configs.
2. `artifact_icache.png`: compares the icache accesses between configs.
3. `artifact_energy.png`: compares the energy consumption between configs.

### Experimenting:

Aside from this recommended batch of experiments, you can also design and run your own custom experiments. Use the `run_sim.py` script and supply it with a different JSON file describing the data you want to collect:

    python run_sim.py --sim-list=./experiments/test.json --results=./example

The `./experiments/full.json` file gives an example of every experiment that can be run.

Then, you can extract and plot the data from these experiments using:

    python extract_stats.py --cpu-sims=./example

If you just wish to plot previously extracted data (a pickle file is produced from the previous step), use:

    python organizer.py

### Benchmarks:

The benchmark suite for this evaluation is [Polybench/GPU][]. The code in this repository consists of ports from the original C source code to use our architecture.

### Key Source:

Some important source code directories to check out include:

- `top/src/custom/` contains source for simulating vector groups
- `top/src/mem/ruby/scratchpad/Scratchpad.cc` contains source for "frames," which are the architecture's mechanism for decoupled access/execute (DAE) within vector groups

### Compiler Pass:

Code for the architecture relies on a custom compiler pass found in `top/trillium`, which post-processes the assembly produced by GCC to produce runnable code.

[gem5]: https://gem5.googlesource.com
[PolyBench/GPU]: https://web.cse.ohio-state.edu/~pouchet.2/software/polybench/GPU/index.html
[docker]: https://www.docker.com
