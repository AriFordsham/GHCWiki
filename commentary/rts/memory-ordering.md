# Memory ordering

This page describes the memory ordering design in GHC.

Please update this page freely.


## Background

There are various architectures about memory ordering (see [here](https://en.wikipedia.org/wiki/Memory_ordering)).
GHC needs to correspond to each sort.

  * Strong memory ordering (TSO):
    * X86, SPARC in TSO mode

  * Weak memory ordering:
    * ARM, PowerPC

On weakly ordering machines, store-store and load-load instructions may be also reorderd.
So we need appropriate memory barriers.


## Heap memory barriers

Here is the design note about heap memory barriers in GHC:

  * `Note [Heap memory barriers]` in [includes/stg/SMP.h](https://gitlab.haskell.org/ghc/ghc/blob/master/includes/stg/SMP.h) 

Related marge request and mails are here:

  * https://gitlab.haskell.org/ghc/ghc/merge_requests/1128 (originaly https://gitlab.haskell.org/ghc/ghc/merge_requests/734)
  * [Cmm Memory Model (Understanding #15449)](https://mail.haskell.org/pipermail/ghc-devs/2018-November/016581.html) at ghc-devs ML.


## Related documents and articles

Here are some useful resources:

- [The JSR-133 Cookbook for Compiler Writers](http://g.oswego.edu/dl/jmm/cookbook.html)
- [LINUX KERNEL MEMORY BARRIERS](https://github.com/torvalds/linux/blob/master/Documentation/memory-barriers.txt)
- [Memory ordering](https://en.wikipedia.org/wiki/Memory_ordering)
- [LLVM Language Reference Manual, ‘cmpxchg’ Instruction](https://llvm.org/docs/LangRef.html#cmpxchg-instruction)
