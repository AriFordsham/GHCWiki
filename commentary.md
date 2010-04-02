# The GHC Commentary


This tree of wiki pages is a "commentary" on the GHC source code.  It contains all the explanatory material that doesn't belong in comments in the source code itself, because the material is wide-ranging, usually covers multiple source files, and is more architectural in nature.  The commentary can also be considered a design document for GHC.


For the dedicated, there are [videos of Simon and Simon giving an overview of GHC](about-videos), at the 2006 [GHC Hackathon](hackathon).


Please feel free to add material to this commentary: don't worry too much about accuracy, in due course someone will edit your contribution.  Try to link to source files as much as possible by using this macro: `[[GhcFile(compiler/Makefile)]]` (the usual Trac `source:` macro doesn't work here because the GHC darcs repository isn't integrated into this Trac).  Also try to add appropriate links to other parts of the commentary.

## Contents

- [Source Tree Roadmap](commentary/source-tree)
- [Coding style guidelines](working-conventions)
- [The compilation pipeline](commentary/pipeline)

- [The Compiler](commentary/compiler)

- [The Runtime System](commentary/rts)
  In addition to the usual runtime support contains information on:

  - [The Haskell Execution Model](commentary/rts/haskell-execution)
  - [The memory layout of heap and stack objects](commentary/rts/storage)

- Cross-cutting concerns: topics which span both the compiler and the runtime system

  - [Profiling](commentary/profiling)
  - [Primitive Operations (PrimOps)](commentary/prim-ops); see also [Wired-in and known-key things](commentary/compiler/wired-in)
  - [Support for shared libraries](shared-libraries)
  - [The Package System](commentary/packages)

- [The Libraries](commentary/libraries)

- [The User Manual](commentary/user-manual) (formatting guidelines etc)

- [The Evil Mangler](commentary/evil-mangler)

- Working on GHC

  - [DebuggingGhcCrashes](debugging-ghc-crashes): how to use gdb to debug a crash in GHC-compiled code.

## Contributed Documentation


Please edit and improve the Commentary above, or the Building Guide.  


However sometimes you may want to add new material that doesn't quite fit into the existing structure.  Or perhaps your pages are about work-in-progress, status reports and suchlike, which don't belong in the Commentary per se.  Regardless, you can add your new material here.

- The Compiler

  - Notes about Template Haskell?
  - [Notes about the new code generator](commentary/compiler/new-code-gen)
  - [Type families/type functions](type-functions): Notes concerning the implementation of type families, associated types, and equality constraints as well as the extension of the type checker with a contraint solver for equality constraints.
  - [IntermediateTypes](intermediate-types): Notes about the type system of GHC's new intermediate language (in the HEAD since ICFP'06)
  - [DataParallel](data-parallel): Notes about the implementation of Data Parallel Haskell
  - [RewriteRules](rewrite-rules): Notes about the implementation of RULEs in GHC
  - [BackEndNotes](back-end-notes): Some ideas and notes about the back end.
  - [LLVM Back-end](commentary/compiler/backends/llvm): Some notes about the new LLVM back-end for GHC
  - [Cmm: Implementing Exception Handling](commentary/cmm-exceptions): Implementing exception handling for primitive operations in Cmm
  - [Cmm: Using Kinds to implement calling conventions](commentary/cmm-kinds): Pointers to explanations of what a `CmmKind` is and why you might find one useful.
  - [GhciDebugger](ghci-debugger): Some notes about the implementation of the GHCi debugger. Probably uninteresting unless you want to work on the debugger.
  - [NewGhciDebugger](new-ghci-debugger): The new GHCi debugger.
  - [AddingNewPrimitiveOperations](adding-new-primitive-operations): How to add new primitive operations to GHC Haskell.
  - [Replacing GMP](replacing-gmp-notes): Notes from an effort to replace GMP with another Bignum library.
  - [ExternalCore](external-core): Describes the process of bringing External Core up to speed. Once finished, this will simply describe what External Core is, and how it works. 
  - [HaddockComments](haddock-comments): Some notes about how the Haddock comment support is implemented.
  - [ExplicitCallStack](explicit-call-stack): Notes about maintaining an explicit call stack, to support error attribution and profiling.
  - [SQL-Like Comprehensions](sql-like-comprehensions): Notes on the ongoing implementation of SPJs "Comprehensive Comprehensions".

- The Runtime System

  - [GarbageCollectorNotes](garbage-collector-notes) Notes about GHC's existing single threaded garbage collector and development of a parallel GC.
  - [GMPMemoryManagement](gmp-memory-management) Describes how the garbage collector cooperates with GMP for Integer.
  - [SemiTagging](semi-tagging): Describes how the semi-tagging optimisation will be implemented.
  - [PAPI](papi): Measurement of program performance usign CPU events (cache misses, branch mispredictions).

- Other Cross-cutting concerns: topics which span both the compiler and the runtime system

  - How the [Haskell Program Coverage](commentary/hpc) option works
  - [Building/RunningNoFib](building/running-no-fib): The nofib benchmark suite

- Getting to grips with the code base

  - BeginnersNotes: Some notes about getting started hacking GHC and the structure of the compiler (especially types and typecheck)

- [Hackathon presentations](about-videos) (video)

## Old but useful


Finally, here are some generally-useful, but now somewhat-out-of-date resources:

- [ The old GHC Commentary](http://darcs.haskell.org/ghc/docs/comm/): Information on the internals of GHC, in various states of up-to-dateness.  We are keen to move this stuff out of its current location and onto this Wiki.  If anyone is willing to help do that, even for just a part in which you are interested, we would be delighted.
- [GhcPapers](ghc-papers): papers and pointers to other documents that relate to the inner workings of GHC.


Convert type diagram to SVG? Upload graphviz source?
