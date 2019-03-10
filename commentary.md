# The GHC Commentary


This tree of wiki pages is a "commentary" on the GHC source code.  It contains all the explanatory material that doesn't belong in comments in the source code itself, because the material is wide-ranging, usually covers multiple source files, and is more architectural in nature.  The commentary can also be considered a design document for GHC.


For the dedicated, there are [videos of Simon and Simon giving an overview of GHC](about-videos), at the 2006 [GHC Hackathon](hackathon).

## Editing the Commentary


Please feel free to add material to the rest of the wiki: don't worry too much about accuracy (in due course someone will edit your contribution). When unsure though please indicate this and its best to ask on the GHC mailing list so you can correct the commentary. Please give some thought to where in the commentary your contribution belongs. GHC has an older commentary (non wiki based) that read like a single coherent narrative, made sure to define terms before using them, and introduced concepts in the order which made them easiest to understand.  Please do try to preserve those properties in this wiki commentary. If you're unsure or in a hurry, consider creating a wiki page outside the commentary and linking to it from the commentary (or the "contributed documentation" section below).


Try to link to source files as much as possible by using this macro: `[[GhcFile(compiler/Makefile)]]`. Also try to add appropriate links to other parts of the commentary.

## Contents

- [Getting Started](commentary/getting-started)

  - [Source Tree Roadmap](commentary/source-tree)
  - [Module Structure](commentary/module-structure)
  - [Coding Style](commentary/coding-style)
  - [Abbreviations in GHC](commentary/abbreviations)
  - [Platforms and their Naming Convention](commentary/platform-naming)

- [The Compiler](commentary/compiler)

- [The Libraries on which GHC depends](commentary/libraries)

  - [The Integer libraries (\`integer-gmp\` and \`integer-simple\`)](commentary/libraries/integer)

- [The Runtime System (RTS)](commentary/rts)

  - [RTS Coding Conventions](commentary/rts/conventions)
  - [The Haskell Execution Model](commentary/rts/haskell-execution)
  - [The memory layout of heap and stack objects](commentary/rts/storage)

- Cross-cutting concerns: topics which span both the compiler and the runtime system

  - [Profiling](commentary/profiling)
  - [Wired-in and known-key things](commentary/compiler/wired-in)
  - [Primitive Operations (PrimOps)](commentary/prim-ops)
  - [The Package System](commentary/packages)

- [The User Manual](commentary/user-manual) (formatting guidelines etc)

## Contributed Documentation


The above commentary covers the source code of GHC. For material that doesn't concern this topic (such as proposals, work-in-progress and status reports) or that don't fit into the existing structure, you will find them below. Feel free to add new material here but please categorise it correctly.

- General Notes on the GHC compiler

  - Edward Yang's blog post about [ the entire complilation pipeline for \`factorial\`](http://blog.ezyang.com/2011/04/tracing-the-compilation-of-hello-factorial/)
  - [New Prim Ops](adding-new-primitive-operations): How to add new primitive operations to GHC Haskell.
  - [Replacing GMP](replacing-gmp-notes): Notes from an effort to replace GMP with another Bignum library.
  - [External Core](external-core): Describes the process of bringing External Core up to speed. Once finished, this will simply describe what External Core is, and how it works. 
  - [ The Scrap your boilerplate homepage](http://sourceforge.net/apps/mediawiki/developers/index.php?title=ScrapYourBoilerplate).
  - [Optimisation Ordering](commentary/compiler/opt-ordering) Describe the ordering and interaction of optimisation passes (Old).

- Notes on implemented GHC features:

  - [Kind polymorphism and data type promotion](ghc-kinds)
  - [A kind for class constraints. Implemented as ConstraintKinds](kind-fact)
  - [LLVM back end](commentary/compiler/backends/llvm)
  - [Support for generic programming](commentary/compiler/generic-deriving)
  - Notes about Template Haskell?
  - [Rewrite Rules](rewrite-rules): Notes about the implementation of RULEs in GHC
  - [Monad Comprehensions](monad-comprehensions): Translation rules and some implementation details 
  - [Haddock](haddock-comments): Some notes about how the Haddock comment support is implemented.  
  - [Intermediate Types](intermediate-types): Notes about the type system of GHC's new intermediate language (in the HEAD since ICFP'06)  
  - [Type families/type functions](type-functions): Notes concerning the implementation of type families, associated types, and equality constraints as well as the extension of the type checker with a contraint solver for equality constraints.
  - [Magic to do with \`seq\` and friends](commentary/compiler/seq-magic)
  - [Compiler plug-ins](new-plugins)
  - [memcpy/memmove/memset optimizations](memcpy-optimizations)
  - [Backend Ideas](back-end-notes): Some ideas and notes about the back end.
  - [Notes about the new code generator](commentary/compiler/new-code-gen)
  - [A record of improvements made to the performance of the Hoopl library for dataflow optimisation](commentary/compiler/hoopl-performance)
  - [DPH](data-parallel): Notes about the implementation of Data Parallel Haskell
  - [Safe Haskell](safe-haskell): The design of the GHC Safe Haskell extension
  - [SQL-Like Comprehensions](sql-like-comprehensions): Notes on SPJs "Comprehensive Comprehensions" (TransformComprehensions)
  - [Deferring compilation type errors to runtime (\`-fwarn-type-errors\`)](defer-errors-to-runtime)
  - [Demand analyser](commentary/compiler/demand) Notes on the meanings, worker-wrapper splitting of demand signatures and relevant components of the compiler
  - [The solve for type-level naturals](commentary/compiler/type-nat-solver)

- Notes on proposed or in progress (but out of tree) GHC compiler features:

  - [Allowing multiple instances of the same package to be installed](commentary/g-so-c-multiple-instances), each instance having different dependencies
  - [Contracts in Haskell](commentary/contracts)
  - [Agda-style holes in terms](holes) which supports writing partial programs.
  - [Records](records)
  - [New coercion axioms design](new-axioms)
  - [ Cloud Haskell](http://haskell.org/haskellwiki/GHC/CouldAndHPCHaskell)
  - [A modular package language for Haskell](package-language) Scott Kilpatrick and Derek Dreyer are designing a new “package language” for Haskell in the style of the ML module system.
  - [Pattern synonyms](pattern-synonyms)
  - [Type level naturals](type-nats)
  - [Polymorphic Dynamic](polymorphic-dynamic): Notes on adding ad-hoc polymorphic dynamic types
  - [Proposal to allow classes to give default implementations for their superclasses](default-superclass-instances)
  - [Cmm: Implementing Exception Handling](commentary/cmm-exceptions): Implementing exception handling for primitive operations in Cmm
  - [Cmm: Using Kinds to implement calling conventions](commentary/cmm-kinds): Pointers to explanations of what a `CmmKind` is and why you might find one useful.
  - [SIMD](simd): Notes on adding SIMD instructions and primOps to GHC
  - [Explicit Call Stack](explicit-call-stack): Notes about maintaining an explicit call stack, to support error attribution and profiling.
  - [Objective-C FFI](objective-c): Haskell FFI support for Objective-C
  - [C Blocks](block-objects): Haskell FFI support for block objects (closures) in C
  - [Syntax for explicit type application and scoped type variables](explicit-type-application)
  - [Syntax for defining kinds that do not arise from datatype promotion](ghc-kinds/kinds-without-data)
  - [Lambda-Case](lambdas-vs-pattern-matching): Syntax for full (with branching) pattern matching on arguments of lambda abstractions

- GHCi Debuger

  - [GHCi Debugger](ghci-debugger): Some notes about the implementation of the GHCi debugger. Probably uninteresting unless you want to work on the debugger.
  - [New GHCi Debugger](new-ghci-debugger): The new GHCi debugger.

- The Runtime System

  - [A new lightweight concurrency substrate for GHC](lightweight-concurrency)
  - [Garbage Collector](garbage-collector-notes): Notes about GHC's existing single threaded garbage collector and development of a parallel GC.
  - [GMP Memory Managment](gmp-memory-management): Describes how the garbage collector cooperates with GMP for Integer.
  - [SemiTagging](semi-tagging): Describes how the semi-tagging optimisation will be implemented.
  - [PAPI](papi): Measurement of program performance using CPU events (cache misses, branch mis-predictions).

- Cross-cutting concerns: topics which span both the compiler and the runtime system

  - [Haskell Program Coverage](commentary/hpc): How HPC works

- [Parallel Haskell Variants](gp-h-eden): All aspects of the GpH and Eden parallel variants of GHC.

## Old Documentation


Here are some useful, but somewhat-out-of-date resources:

- [ The old GHC Commentary](http://darcs.haskell.org/ghc/docs/comm/): Information on the internals of GHC, in various states of up-to-dateness.  We are keen to move this stuff out of its current location and onto this Wiki.  If anyone is willing to help do that, even for just a part in which you are interested, we would be delighted.  There is a [page](commentary/migrating-old-commentary) which tracks the progress of migrating information from the old commentary to this wiki.
- [GHC Papers](ghc-papers): Papers and pointers to other documents that relate to the inner workings of GHC.
