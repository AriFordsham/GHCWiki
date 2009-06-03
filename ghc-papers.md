## Papers related to GHC


Here we've collected together lots of documentation and papers that
generally describe stuff we've implemented in GHC.


All docs are gzipped A4 Postscript unless otherwise specified.

- [The Concurrent Haskell Foreign Function Interface](http://www.haskell.org/ghc/docs/papers/threads.ps.gz).  This document is a draft Haskell addendum that describes GHC's approach to concurrency and its interaction with the FFI and OS threads.

- [An External Representation for the GHC Core Language](http://www.haskell.org/ghc/docs/papers/core.ps.gz).  This document describes the syntax of GHC Core (.hcr) files, which can be used to feed GHC intermediate code to other (non-GHC) back-end processors.

- [Haskell Execution Platform](http://www.haskell.org/ghc/docs/papers/hep.ps.gz) design document.  This document describes the design for an integrated compiler/interpreter API for executing Haskell programs, which forms the heart of the combined Hugs/GHC system.

- [Run-time System document](http://www.haskell.org/ghc/docs/papers/run-time-system.ps.gz).  This document (is supposed to) describe the new run-time system in GHC 4.xx.  Be warned that the implementation and this document are evolving in parallel, so they might not be quite in sync.

- [ The Spineless Tagless G-Machine](http://research.microsoft.com/en-us/um/people/simonpj/papers/spineless-tagless-gmachine.ps.gz#26pub=34).  This paper describes the execution model used by GHC.  It is most relevant to GHC up to version 3.xx---in version 4 we made some changes which are described in the [RTS document](http://www.haskell.org/ghc/docs/papers/run-time-system.ps.gz).

- The UsageSP analysis (ghc/compiler/usageSP/) is described in Keith Wansbrough and Simon Peyton Jones, [ Once Upon a Polymorphic Type](http://www.cl.cam.ac.uk/users/kw217/research/phd/usptr-10pt.ps.gz), Technical Report TR-1998-19, Department of Computing Science, University of Glasgow, 1998.  Conference version [ Once Upon a Polymorphic Type](http://www.cl.cam.ac.uk/users/kw217/research/phd/popl99-usage.ps.gz), in The Twenty-sixth ACM SIGPLAN-SIGACT Symposium on Principles of Programming Languages, January 20-22, 1999, San Antonio, Texas.

-  GHC's typechecker implementation is described in the paper [ Type classes in Haskell](http://research.microsoft.com/~simonpj/Papers/classhask.ps.gz), CV Hall, K Hammond, SL Peyton Jones, and PL Wadler, European Symposium On Programming, LNCS 788, Springer Verlag, pp.  241-256, April 1994.

- [The New GHC/Hugs Runtime System](http://www.haskell.org/ghc/docs/papers/new-rts.ps.gz)---a summary of the [RTS document](http://www.haskell.org/ghc/docs/papers/run-time-system.ps.gz).

- [ Asynchronous Exceptions in Haskell](http://www.haskell.org/~simonmar/papers/async.ps.gz)

- [ Lightweight Extensible Records for Haskell](http://research.microsoft.com/~simonpj/Papers/records.htm)

- [ Derivable Type Classes](http://research.microsoft.com/~simonpj/Papers/derive.htm)

- [ Pattern Guards and Transformational Patterns](http://research.microsoft.com/~simonpj/Papers/pat.htm)

- [ Secrets of the GHC inliner](http://www.research.microsoft.com/~simonpj/Papers/inlining/index.htm)

- [ Non-stop Haskell](http://research.microsoft.com/~simonpj/Papers/inc-gc.htm) the workings of an incremental garbage collector (not included in GHC, so far).

- [Concurrent Haskell](http://www.haskell.org/ghc/docs/papers/concurrent-haskell.ps.gz)

- [ Imprecise Exceptions, Co-Inductively](http://research.microsoft.com/~simonpj/Papers/imprecise-exn-sem.htm)

- [A semantics for imprecise exceptions](http://www.haskell.org/ghc/docs/papers/except_ps.gz)

- [Imperative Functional Programming](http://www.haskell.org/ghc/docs/papers/imperative.ps.gz)

- [Lazy Functional State Threads](http://www.haskell.org/ghc/docs/papers/lazy-functional-state-threads.ps.gz)

- [Unboxed Values as First-Class Citizens](http://www.haskell.org/ghc/docs/papers/unboxed-values.ps.gz)

- [Time and Space Profiling for non-strict, higher-order functional programs](http://www.haskell.org/ghc/docs/papers/profiling.ps.gz)

- [The Glasgow Haskell Compiler - a Technical Overview](http://www.haskell.org/ghc/docs/papers/grasp-jfit.ps.gz) - from 1992, not entirely relevant any more.

- [Adding an Optimisation Pass to the Glasgow Haskell Compiler](http://www.haskell.org/ghc/docs/papers/extendGHC.ps.gz) (Olaf Chitil).  Somewhat out of date.
