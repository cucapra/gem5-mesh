# no spad (should not use this branch)
scons -j16 build/RISCV/gem5.opt

# spad
scons -j16 build/RVSP/gem5.opt

# for 16 core (+1 host core) sim with scratchpad
./build/RVSP/gem5.opt -d results/sddmm-cpu \
  configs/phil/brg_hammerblade.py \
  --cmd=programs-phil/spad/sddmm-cpu/sddmm_cpu \
  --network=garnet2.0 \
  --num-cpus=17

# no spad
./build/RISCV/gem5.opt -d results/test_ configs/phil/mesh.py \
  --cmd=programs-phil/gemm/basic_riscv --cpu-type=MinorCPU \
  --num-cpus=5 --caches --l2cache --l1d_size=2kB --l1i_size=2kB \
  --num-l2caches=1 --l2_size=8kB


./build/RISCV/gem5.opt -d results/test configs/phil/mesh.py \
  --cmd=programs-phil/gemm-systolic/gemm_systolic --cpu-type=TimingSimpleCPU --num-cpus=10 \
  --caches --l2cache --l1d_size=2kB --l1i_size=2kB --num-l2caches=1 --l2_size=8kB

./build/RISCV/gem5.opt --debug-flags=Mesh -d results/test configs/phil/mesh.py \
  --cmd=programs-phil/test-asm/test_asm_riscv --cpu-type=TimingSimpleCPU --num-cpus=5 \
  --caches --l2cache --l1d_size=2kB --l1i_size=2kB --num-l2caches=1 --l2_size=8kB

./build/RISCV/gem5.opt -d results/test configs/phil/se.py \
  --cmd=programs-phil/gemm/basic_riscv --cpu-type=MinorCPU \
  --num-cpus=4 --caches --l2cache --l1d_size=2kB --l1i_size=2kB \
  --num-l2caches=1 --l2_size=8kB

./build/RISCV/gem5.opt --debug-flags=Harness -d results/test \
  configs/phil/mesh.py --cmd=programs-phil/hello/hello_riscv \
  --cpu-type=TimingSimpleCPU --num-cpus=4 --caches --l2cache \
  --l1d_size=2kB --l1i_size=2kB --num-l2caches=1 --l2_size=8kB


# http://gem5.org/Garnet_Synthetic_Traffic
scons -j16 build/NULL/gem5.opt PROTOCOL=Garnet_standalone

./build/NULL/gem5.opt configs/example/garnet_synth_traffic.py \
  --num-cpus=16 --num-dirs=16 --network=garnet2.0 --topology=Mesh_XY \
  --mesh-rows=4  --sim-cycles=1000 --synthetic=uniform_random --injectionrate=0.01



