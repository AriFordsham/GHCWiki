## NDP benchmarks

### Sparse matrix vector multiplication


This benchmark is explained it much detail in [ Data Parallel Haskell: a status report](http://www.cse.unsw.edu.au/~chak/papers/CLPKM07.html).  Runtimes comparing to sequential C code on Intel Xeon (x86) and Sun SunFire9600 (Sparc) are in [time-colour.png](/trac/ghc/attachment/wiki/DataParallel/Benchmarks/time-colour.png)[](/trac/ghc/raw-attachment/wiki/DataParallel/Benchmarks/time-colour.png).  The parallel Haskell code is more efficient from 2 PEs for the SunFire and from 4 PEs for the Xeon processors.  We blame the low sequential performance for the Xeon on the lack of effort that has been put into generating good straight-line code (both in the NCG and when compiling via C), this includes inadequate register allocation and lack of low-level optimisations.


The speedup for the Xeon box and the SunFire are in [speedup-colour.png](/trac/ghc/attachment/wiki/DataParallel/Benchmarks/speedup-colour.png)[](/trac/ghc/raw-attachment/wiki/DataParallel/Benchmarks/speedup-colour.png) and the speedup for our 8x dualcore Opteron NUMA box is in [serenity-all-speedup-colour.png](/trac/ghc/attachment/wiki/DataParallel/Benchmarks/serenity-all-speedup-colour.png)[](/trac/ghc/raw-attachment/wiki/DataParallel/Benchmarks/serenity-all-speedup-colour.png).  The speedup of on the NUMA machine is limited by the memory bandwidth for smvm.  When we only use one core per CPU, the benchmark scales much better.  Moreover, the memory traffic/compute ratio is slightly more favourable when processing arrays of `Float`s than when processing arrays of `Double`s.
