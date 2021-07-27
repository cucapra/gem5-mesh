Trillium
========

[![build status](https://github.com/cucapra/gem5-mesh/workflows/trillium/badge.svg)](https://github.com/cucapra/gem5-mesh/actions)

This is a fork of the [gem5][] simulator repository with a bunch of other stuff for the Trillium architecture.

Environment Setup (docker):

1. cd ./docker && docker build -t gem5-mesh .
2. ./enter_docker.sh

Environment Setup (native):

In case you don't want to use docker. The following packages and repos are required.

1. build-essential gcc g++ m4 scons zlib1g zlib1g-dev libprotobuf-dev python-dev python autoconf automake autotools-dev curl python3 libmpc-dev libmpfr-dev libgmp-dev gawk bison flex texinfo gperf libtool patchutils bc libexpat-dev
2. git clone -b rvv-intrinsic https://github.com/riscv/riscv-gnu-toolchain
3. cd riscv-gnu-toolchain && ./configure --prefix=path --with-arch=rv64gv_zfh
4. make -j16 linux
5. setenv RV_CC10=path

Building:

1. cd path/to/git/top
2. scons -j16 ./build/RVSP/gem5.opt

Running:

Benchmarks can be run individually by navigating to directory and doing 'make run'.

Scripts are provided to run experiments (in ./scripts-phil/eval)
1. python artifact.py --experiment=[small,medium,large] does simulation and plotting.

It invokes the following scripts
1. run_sim.py : runs an experiment
2. extract_stats.py : extracts gem5 simulation data
3. organizer.py : plots graph of data

Experimenting:

Custom experiments can be run by giving run_sim.py a different json file.
python run_sim.py --sim-list=./experiments/test.json --results=./example

./experiments/full.json gives an example of every experiment that can be run.

Key source files:

Important source directories:
./src/custom - contains source for vector groups

Custom compiler pass:

A custom compiler pass found in ./trillium post processes assembly to produce runnable code for our architecture.


[gem5]: https://gem5.googlesource.com
