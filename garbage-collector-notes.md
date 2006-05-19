# The GHC Garbage Collector Notes


These are my notes of the Glasgow Haskell Compiler’s Garbage Calloector made over my period of internship at Microsoft Research in Summer 2006. These notes are in process of constantly being updated as I study the system further. The objective of my work at MSRC is to implement a parallel GC for Haskell – one that will allow multiple threads to simultaneously garbage collect.  

## Capabilities


Lets dive right into the working of things. GHC has an abstraction called capabilities. A capability is a 1 or more OS threads. They may or maynot have processor affinities on multiprocessor machines. Each capability can run multiple Haskell threads. These Haskell threads are what are known as interpreter threads or green threads or user threads in the terminology of other systems. The OS is not aware of their presence and the switching of context between these threads is controlled purely by the Haskell runtime system. The runtime system, abbreviated as RTS is something we will keep referring to again and again. 

## Runtime System


The RTS is located in the folder “rts” of the ghc tree. It consists mostly of C code that is compiled into the resulting Haskell executable so that the required runtime services for the executable are packaged in. This is unlike .Net, JVM, Chez Scheme and other systems where the binaries rely heavily on the runtime support provided by their host VMs and thus the binaries cannot be independently deployed onto machines that that don’t have whole or part of the host VM services. Haskell executables are designed to be standalone executable requiring only standard OS services and do not usually require language support binaries. 


The tradeoff is in the fact that every Haskell binary has the RTS compiled into it, making Haskell binaries rather large. The RTS consists of facilities like the support of user threads (or Haskell threads), garbage collection etc. We are interested in focusing on Garbage Collection. However before we get into the GC, let us look at how that is connected to the rest of the system.

## [CapabilitiesAndScheduling](capabilities-and-scheduling)

## Stepping into the GC


The part that we are interested in is the Garbage Collector. The main entry point into the GC is the GarbageCollect() function  defined in GC.c.


The existing GC in GHC is a single threaded one. When the RTS detects memory pressure the GC stops all the Haskell threads and then one thread that does the garbage collection and then resumes all the other suspended threads. On a multiprocessor machine such a design is obviously a bottle neck and it is desirable to garbage collect using multiple parallel threads. 

## [GcDataStructures](gc-data-structures)

## Allocation

## Scavenging

## Copy

# [MotivationForParallelization](motivation-for-parallelization)

# [ProblemsCompilingGhc](problems-compiling-ghc)

---


Roshan James (rpjames \[at\] cs \[dot\] indiana \[dot\] edu)

---