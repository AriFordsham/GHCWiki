# The GHC Commentary


This tree of wiki pages is a "commentary" on the GHC source code.  It contains all the explanatory material that doesn't belong in comments in the source code itself, because the material is wide-ranging, usually covers multiple source files, and is more architectural in nature.  The commentary can also be considered a design document for GHC.


Please feel free to add material to this commentary: don't worry too much about accuracy, in due course someone will edit your contribution.  Try to link to source files as much as possible by using this macro: `[[GhcFile(compiler/Makefile)]]` (the usual Trac `source:` macro doesn't work here because the GHC darcs repository isn't integrated into this Trac).  Also try to add appropriate links to other parts of the commentary.

## Contents

- [Source Tree Roadmap](commentary/source-tree)
- [The compilation pipeline](commentary/pipeline)

- [The Compiler](commentary/compiler)

- [The Runtime System](commentary/rts)

- Cross-cutting concerns: topics which span both the compiler and the runtime system

  - [Profiling](commentary/profiling)
  - [Primitive Operations (PrimOps)](commentary/prim-ops)

- [The User Manual](commentary/user-manual) (formatting guidelines etc)

- [The Evil Mangler](commentary/evil-mangler)

## Contributed Documentation


Please feel free to add new pages here.  In due course information will migrate from here to the main commentary above.

- The Compiler

  - [TypeFunctions](type-functions): Notes concerning the implementation of type functions and associated types, which was merged in the HEAD during ICFP'06.
  - [IntermediateTypes](intermediate-types): Notes about the type system of GHC's new intermediate language (in the HEAD since ICFP'06)
  - [DataParallel](data-parallel): Notes about the implementation of Data Parallel Haskell
  - [RewriteRules](rewrite-rules): Notes about the implementation of RULEs in GHC
  - [BackEndNotes](back-end-notes): Some ideas and notes about the back end.
  - [Cmm: Implementing Exception Handling](commentary/cmm-exceptions): Implementing exception handling for primitive operations in Cmm
  - [GhciDebugger](ghci-debugger): Some notes about the implementation of the GHCi debugger. Probably uninteresting unless you want to work on the debugger.

    - [AddingNewPrimitiveOperations](adding-new-primitive-operations): How to add new primitive operations to GHC Haskell.
  - [Replacing GMP](replacing-gmp-notes): Notes from an effort to replace GMP with another Bignum library.
  - [ExternalCore](external-core): Describes the process of bringing External Core up to speed. Once finished, this will simply describe what External Core is, and how it works. 
  - [HaddockComments](haddock-comments): Some notes about how the Haddock comment support is implemented.
  - [ExplicitCallStack](explicit-call-stack): Notes about maintaining an explicit call stack, to support error attribution and profiling.

- The Runtime System

  - [GarbageCollectorNotes](garbage-collector-notes) Notes about GHC's existing single threaded garbage collector and development of a parallel GC.
  - [SemiTagging](semi-tagging): Describes how the semi-tagging optimisation will be implemented.

- Getting to grips with the code base

  - BeginnersNotes: Some notes about getting started hacking GHC and the structure of the compiler (especially types and typecheck)
  - [DebuggingGhcCrashes](debugging-ghc-crashes): how to use gdb to debug a crash in GHC-compiled code.

- [Hackathon presentations](about-videos) (video)

## Old but useful


Finally, here are some generally-useful, but now somewhat-out-of-date resources:

- [ The old GHC Commentary](http://www.cse.unsw.edu.au/~chak/haskell/ghc/comm/): Information on the internals of GHC, in various states of up-to-dateness.  We are keen to move this stuff out of its current location and onto this Wiki.  If anyone is willing to help do that, even for just a part in which you are interested, we would be delighted.
- [GhcPapers](ghc-papers): papers and pointers to other documents that relate to the inner workings of GHC.


Convert type diagram to SVG? Upload graphviz source?
