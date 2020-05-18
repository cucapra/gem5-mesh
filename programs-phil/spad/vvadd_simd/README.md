# VVADD SIMD

Compiler hack for `vvadd_simd`. Aim is to write the scalar and vector code separately with different register mappings and compiler optimizations since they are meant to run on separate cores. We then compile them into separate assembly files which need to be combined/glued together in a smart way. As with 19th century industrialization, it can be hand-tuned but eventually will be replaced by an automatic process for better scaling (yes, compiler).

```
make scalar
make vector
```
This is followed by generating a `vvadd_combined.s` file. Then do the following:
```
make pass
```
`make pass` runs a python pass `vissue-asm.py` which although is capable of a ton of things, is used to add terminator instructions at the end of vector blocks. The python pass has a clause which checks for `*_combined.s` files, so make sure to run on files with that specific name appended at the end. This spits out `vvadd_final.s`. 
```
make combine
```
`make combine` compiles `vvadd_final.s` to generate an object file.
```
make 
make run
```


 