# Lightweight Concurrency in GHC


This page documents the effort to move GHC's concurrency support from its current location in the C part of the runtime system (RTS) to Haskell. This works builds on Peng Li's earlier work ([ http://community.haskell.org/\~simonmar/papers/conc-substrate.pdf](http://community.haskell.org/~simonmar/papers/conc-substrate.pdf)). This page contains information about the design, implementation, problems and potential solutions for building user-level concurrency primitives in GHC. Currently, the focus is on implementing non-deterministic parallelism in GHC ([Control.Concurrent](http://www.haskell.org/ghc/docs/latest/html/libraries/base/Control-Concurrent.html)) in Haskell instead of C.

## Introduction


GHC has a rich support for concurrency (forkIO, MVars, STM, Asynchronous exceptions, bound threads, safe FFI, transparent scaling on multicores, etc.) and a fast and robust runtime system. However, the concurrency support is implemented in C and baked into the RTS. The concurrency primitives non-trivially interact among each other, and along with the lightweight thread scheduler, through a cascade of locks and condition variables. Often times, the invariants on which RTS fields can be accessed when are expressed as comments, and enforced through assertions (See [here](/trac/ghc/browser/rts/Task.h?rev=085c7fe5d4ea6e7b59f944d46ecfeba3755a315b#L37)[](/trac/ghc/export/085c7fe5d4ea6e7b59f944d46ecfeba3755a315b/ghc/rts/Task.h#L37) for one such fascinating example). This policy of enforcing through assertions keeps the overheads low, but makes the task of modifying and extending the runtime cumbersome.


But, why would we be interested in modifying GHC's concurrency environment? There are several good reasons to believe that a particular concurrent programming model, or a scheduling policy would not suit every application. With the emergence of many-core processors, we see NUMA effects becoming more prominent, and applications might benefit from NUMA aware scheduling and load balancing policies. Moreover, an application might have a better knowledge of the scheduling requirements -- a thread involved in user-interaction is expected to be given more priority over threads performing background processing. We might want to experiment with various work-stealing or work-sharing policies. More ambitiously, we might choose to build X10 style async-finish or Cilk style spawn-sync task parallel abstractions. Ideally, we would like allow the programmer to write an application that can  seamlessly combine all of these different programming abstractions, with pluggable scheduling and load balancing policies.


While we want to provide flexibility to the Haskell programmer, this should not come at a cost of added complexity and decreased performance. This idea reflects in the synchronization abstractions exposed to the programmer ([PTM](lightweight-concurrency#)), and our decision to keep certain pieces of the concurrency puzzle in the RTS ([Safe FFI](lightweight-concurrency#),[Blackholes](lightweight-concurrency#)). The figure below captures the key design principles of the proposed system.

[](/trac/ghc/attachment/wiki/LightweightConcurrency/GHC_LWC_Key.jpg)

## Related Work

- [Concurrent Programming in GHC](lightweight-concurrency#)
- [ Lightweight Concurrent Primitives for GHC](http://community.haskell.org/~simonmar/papers/conc-substrate.pdf)
- [ Tackling the awkward squad](http://research.microsoft.com/en-us/um/people/simonpj/papers/marktoberdorf/)
- [ Runtime Support for Multicore Haskell](http://community.haskell.org/~simonmar/papers/multicore-ghc.pdf)