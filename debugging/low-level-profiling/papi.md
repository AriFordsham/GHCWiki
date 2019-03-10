# Using PAPI

[ PAPI](http://icl.cs.utk.edu/papi/) is a library providing largely CPU-independent support for performance-counter measurements.  We have used it in the past in GHC for doing low-level performance measurements; for example when developing [Pointer Tagging](commentary/rts/haskell-execution/pointer-tagging) we used PAPI to measure the number of branch prediction misses.


Nowadays it might be better to use the Performance Events? infrastructure in Linux 2.6.31 and later.


For some notes on installing PAPI on Linux, see [Debugging/LowLevelProfiling/PAPI/Installing](debugging/low-level-profiling/papi/installing).

# Measuring program performance using CPU events


The GHC runtime has been extended to support the use of the [ PAPI](http://icl.cs.utk.edu/papi/) library to count occurrences of CPU events such as cache misses and branch mispredictions. The PAPI extension separates the events occurring in the garbage collector and mutator code for more accurate pinpointing of performance problems.


This page describes how to compile the RTS with PAPI enabled and explains the RTS options for CPU event selection. This page also contains patches to collect CPU event information in nofib runs and to allow their comparison using nofib-analyse. This is especially useful to measure the effects of optimisations accross a whole range of programs systematically.

# Status of the implementation


GHC with PAPI support should compile on any platform where PAPI is installed. It should also be possible to monitor the cache miss events of a ghc compiled program.


At present, the monitoring of branch mispredictions and stalled cycles is AMD Opteron specific. In the case of branch mispredictions, the portable PAPI API only monitors conditional jumps. We would like to monitor all jumps, especially indirect jumps, that is why we used a native AMD PAPI counter. For strange reasons, the PAPI conditional jump counter maps to the native counter we are using, but we cannot rely on this behaviour on other platforms, so we use the native counter anyway.

# Compiling and running programs with PAPI


First of all, make sure that you have installed the [ PAPI library](http://icl.cs.utk.edu/papi/).


Follow the instructions in [Building/Hacking](building/hacking) and add the following line to `build.mk` before compiling the RTS:

```wiki
GhcRtsWithPapi = YES
```


You may need to add the PAPI library directory to your LD_LIBRARY_PATH. For example in bash:

```wiki
$ export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/path/to/papi/lib
```


Now, to monitor and report level 1 cache misses, invoke a program compiled by ghc as follows:

```wiki
./program +RTS -sstderr -a1 -RTS
```


The help screen provides options to monitor more events:

```wiki
./program +RTS -h -RTS
```

# Using PAPI with the nofib benchmarking suite


In order to use the nofib suite with PAPI, you have to use apply the three patches at the bottom of this page.

1. The first patch adds a PAPI flag to the perl testing script.
1. The second patch adds a make argument to the nofib suite to enable the collection of PAPI number.
1. The third patch makes nofib-analyse able to process the output produced in the second patch. The standard nofib-analyse won't cut it.


These patches are not submitted to the HEAD (yet?) because they are not mature, but they are useful. Probably the (only?) patch that needs more work is the third one.


To collect statistics just run make inside nofib as usual, as an example let's collect statistics together with cache misses: `make papi=1`.

# Resources

- [ http://icl.cs.utk.edu/papi/](http://icl.cs.utk.edu/papi/) PAPI home page.
- [ http://developer.amd.com/article_print.jsp?id=90](http://developer.amd.com/article_print.jsp?id=90) An article introducing the business of using CPU counters for performance measurement.
- [ http://developer.amd.com/articles.jsp?id=2&num=1](http://developer.amd.com/articles.jsp?id=2&num=1) An article introducing AMD's code analyst. It even has pipeline simulation, though I haven't tried it out yet.
- [ http://www.cs.mu.oz.au/\~njn/pubs/cache-large-lazy2002.ps.gz](http://www.cs.mu.oz.au/~njn/pubs/cache-large-lazy2002.ps.gz) The Cache Behaviour of Large Lazy Functional Programs on Stock Hardware.
