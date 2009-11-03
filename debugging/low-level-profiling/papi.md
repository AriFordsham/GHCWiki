# Using PAPI

[ PAPI](http://icl.cs.utk.edu/papi/) is a library providing largely CPU-independent support for performance-counter measurements.  We have used it in the past in GHC for doing low-level performance measurements; for example when developing [Pointer Tagging](commentary/rts/haskell-execution/pointer-tagging) we used PAPI to measure the number of branch prediction misses.


Nowadays it might be better to use the Performance Events? infrastructure in Linux 2.6.31 and later.


For some notes on installing PAPI on Linux, see [Debugging/LowLevelProfiling/PAPI/Installing](debugging/low-level-profiling/papi/installing).
