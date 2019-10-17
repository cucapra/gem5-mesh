# build, you need newer gcc (due to cpu/io) 
scl enable devtoolset-7 bash 
scons -j16 build/RVSP/gem5.opt

# for 4 core sim with scratchpad (host core is one of the cores)
./build/RVSP/gem5.opt -d results/vvadd \
  configs/phil/brg_hammerblade.py \
  --cmd=programs-phil/spad/vvadd/vvadd \
  --num-cpus=4
  
# if want to use arguments in program
./build/RVSP/gem5.opt -d results/vvadd \
  configs/phil/brg_hammerblade.py \
  --cmd=programs-phil/spad/vvadd/vvadd --options="4" \
  --num-cpus=4

# to run with modular vector stage in pipelines
./build/RVSP/gem5.opt -d results/vvadd \
  configs/phil/brg_hammerblade.py \
  --cmd=programs-phil/spad/vvadd-big/vvadd \
  --num-cpus=4 --vector
