## Papers related to GHC


Here we've collected together lots of documentation and papers that
generally describe stuff we've implemented in GHC. 


Many of these papers are old.  For more up-to-date ones look on:

- [ Simon PJ's home page](http://research.microsoft.com/~simonpj)
- [ Simon PJ's papers page](http://research.microsoft.com/~simonpj/papers)
- [ Simon Marlow's home page](http://www.haskell.org/~simonmar/)


All docs are gzipped A4 Postscript unless otherwise specified.

- [An External Representation for the GHC Core Language](http://www.haskell.org/ghc/docs/latest/html/ext-core/core.pdf).  This document describes the syntax of GHC Core (.hcr) files, which can be used to feed GHC intermediate code to other (non-GHC) back-end processors.

- [Haskell Execution Platform](http://www.haskell.org/ghc/docs/papers/hep.ps.gz) design document.  This document describes the design for an integrated compiler/interpreter API for executing Haskell programs, which forms the heart of the combined Hugs/GHC system.

## Language design

- \[1999\][A semantics for imprecise exceptions](http://www.haskell.org/ghc/docs/papers/except_ps.gz) describes how exceptions work in GHC.  A follow-up paper is [ Imprecise Exceptions, Co-Inductively](http://research.microsoft.com/~simonpj/Papers/imprecise-exn-sem.htm)

- \[1999\][ Lightweight Extensible Records for Haskell](http://research.microsoft.com/~simonpj/Papers/records.htm).  This design is not implemented.

- [\[2000\]](/trac/ghc/changeset/2000/ghc)[ Derivable Type Classes](http://research.microsoft.com/~simonpj/Papers/derive.htm).  Fully implemented in GHC.

- [\[2000\]](/trac/ghc/changeset/2000/ghc)[ Pattern Guards and Transformational Patterns](http://research.microsoft.com/~simonpj/Papers/pat.htm).  This isn't implemented, but a related idea, called [view patterns](view-patterns) is.

- [\[1991\]](/trac/ghc/changeset/1991/ghc)[Unboxed Values as First-Class Citizens](http://www.haskell.org/ghc/docs/papers/unboxed-values.ps.gz).   Fully implemented.

## Types and typechecking

- \[1994\] GHC's typechecker implementation is described in the paper [ Type classes in Haskell](http://research.microsoft.com/~simonpj/Papers/classhask.ps.gz), CV Hall, K Hammond, SL Peyton Jones, and PL Wadler, European Symposium On Programming, LNCS 788, Springer Verlag, pp.  241-256, April 1994.

- [\[1998\]](/trac/ghc/changeset/1998/ghc) The UsageSP analysis is described in Keith Wansbrough and Simon Peyton Jones, [ Once Upon a Polymorphic Type](http://www.cl.cam.ac.uk/users/kw217/research/phd/usptr-10pt.ps.gz), Technical Report TR-1998-19, Department of Computing Science, University of Glasgow, 1998.  Conference version [ Once Upon a Polymorphic Type](http://www.cl.cam.ac.uk/users/kw217/research/phd/popl99-usage.ps.gz), in The Twenty-sixth ACM SIGPLAN-SIGACT Symposium on Principles of Programming Languages, January 20-22, 1999, San Antonio, Texas.  This work isn't in GHC.

## The innards of the complier

- [ Implementing lazy functional languages on stock hardware: the Spineless Tagless G-Machine](http://research.microsoft.com/en-us/um/people/simonpj/papers/spineless-tagless-gmachine.ps.gz#26pub=34).  This paper describes the execution model used by GHC.  It is most relevant to GHC up to version 3.xx---in version 4 we made some changes which are described in the [RTS document](http://www.haskell.org/ghc/docs/papers/run-time-system.ps.gz).

- [\[2002\]](/trac/ghc/changeset/2002/ghc)[ Secrets of the GHC inliner](http://www.research.microsoft.com/~simonpj/Papers/inlining/index.htm) is still highly relevant; it is still a more-or-less accurate description of GHC's "simplifier".

- \[1993\][The Glasgow Haskell Compiler - a Technical Overview](http://www.haskell.org/ghc/docs/papers/grasp-jfit.ps.gz) - from 1992, not entirely relevant any more.

- [Adding an Optimisation Pass to the Glasgow Haskell Compiler](http://www.haskell.org/ghc/docs/papers/extendGHC.ps.gz) (Olaf Chitil).  Somewhat out of date.

## Runtime system

- [Run-time System document](http://www.haskell.org/ghc/docs/papers/run-time-system.ps.gz).  This document (is supposed to) describe the new run-time system in GHC 4.xx.  Be warned that the implementation and this document are evolving in parallel, so they might not be quite in sync.

- [\[2000\]](/trac/ghc/changeset/2000/ghc)[ Non-stop Haskell](http://research.microsoft.com/~simonpj/Papers/inc-gc.htm) the workings of an incremental garbage collecton.  Not included in GHC, so far.

- \[1999\][The New GHC/Hugs Runtime System](http://www.haskell.org/ghc/docs/papers/new-rts.ps.gz)---a summary of the [RTS document](http://www.haskell.org/ghc/docs/papers/run-time-system.ps.gz).

## The awkward squad: I/O, concurrency, exceptions, and foreign functions

- [The Concurrent Haskell Foreign Function Interface](http://www.haskell.org/ghc/docs/papers/threads.ps.gz).  This document is a draft Haskell addendum that describes GHC's approach to concurrency and its interaction with the FFI and OS threads.

- [\[2001\]](/trac/ghc/changeset/2001/ghc)[ Asynchronous Exceptions in Haskell](http://www.haskell.org/~simonmar/papers/async.ps.gz).  The ideas here are fully implemented in GHC.  

- [\[1996\]](/trac/ghc/changeset/1996/ghc)[Concurrent Haskell](http://www.haskell.org/ghc/docs/papers/concurrent-haskell.ps.gz). The original paper about Concurrent Haskell, fully implemented.

- \[1993\][Imperative Functional Programming](http://www.haskell.org/ghc/docs/papers/imperative.ps.gz). The original introduction of monads for I/O and state in Haskell, this paper won a 10-year impact award for POPL. A follow-up paper is [Lazy Functional State Threads](http://www.haskell.org/ghc/docs/papers/lazy-functional-state-threads.ps.gz)

## Profiling

- [Time and Space Profiling for non-strict, higher-order functional programs](http://www.haskell.org/ghc/docs/papers/profiling.ps.gz).  Fully implemented.
