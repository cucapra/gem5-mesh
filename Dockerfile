FROM ubuntu:18.04

#
### Get need packages

# make non interactive so packages dont ask for config info
ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update

# gem5 stuff
RUN apt-get install -y \
    build-essential \
    gcc \
    g++ \
    git \
    m4 \
    scons \
    zlib1g \
    zlib1g-dev \
    libprotobuf-dev \
    python-dev \
    python

# riscv stuff
RUN apt-get install -y \
	autoconf automake autotools-dev curl python3 libmpc-dev libmpfr-dev libgmp-dev gawk build-essential bison flex texinfo gperf libtool patchutils bc zlib1g-dev libexpat-dev

#
### build riscv compiler

WORKDIR /
RUN git clone -b rvv-intrinsic https://github.com/riscv/riscv-gnu-toolchain
WORKDIR riscv-gnu-toolchain
RUN ./configure --prefix=/riscv-rv64gv --with-arch=rv64gv_zfh
RUN make -j16 linux

# set environment variable RV_CC10
ENV RV_CC10 /riscv-rv64gv/bin/riscv64-unknown-linux-gnu-gcc

# install python libaries
RUN apt-get install -y python-pip python3-pip
RUN pip3 install regex colorlog
RUN pip install numpy matplotlib scipy

# copy simulator source to workspace
RUN mkdir /workspace
COPY . /workspace/gem5-mesh
WORKDIR /workspace/gem5-mesh

# build simulator
RUN scons -j16 ./build/RVSP/gem5.opt
