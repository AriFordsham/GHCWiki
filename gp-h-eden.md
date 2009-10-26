# Glasgow Parallel Haskell (GpH) and Eden documentation


This page will describe the GpH and Eden parallel variants of GHC.  This is work in progress.


St Andrews will host the [HackPar](hack-par) parallel hackathon in December to progress this...

---


What follows is an entry page to describe technical documentation of the Eden/GpH implementation in parallel systems with distributed memory.


The different aspects are covered by separate pages reachable from here.

## Modifications to implement Eden (and parallel RTS instances in general)

- [Compiler Ways](gp-h-eden/compiler-ways) for parallelism
- [Startup and Shutdown](gp-h-eden/start-stop) of the parallel system
- [Packing and Unpacking Heap Structures](gp-h-eden/packing)
- [Primitive Operations for Coordination Control](gp-h-eden/primitives)
- [Placeholders, Garbage Collection, other Aspects](gp-h-eden/placeholders-and-gc)


Future work

- event logging for parallel Haskell
- GUM variant of packing code, using global addressing
