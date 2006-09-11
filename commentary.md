# The GHC Commentary


This tree of wiki pages is a "commentary" on the GHC source code.  It contains all the explanatory material that doesn't belong in comments in the source code itself, because the material is wide-ranging, usually covers multiple source files, and is more architectural in nature.  The commentary can also be considered a design document for GHC.


Please feel free to add maaterial to this commentary: don't worry too much about accuracy, in due course someone will edit your contribution.  Try to link to source files as much as possible by using this macro: `[[GhcFile(compiler/Makefile)]]` (the usual Trac `source:` macro doesn't work here because the GHC darcs repository isn't integrated into this Trac).  Also try to add appropriate links to other parts of the commentary.

## Contents

- [The overall organisation of GHC](commentary/organisation)
- [Source Tree Roadmap](commentary/source-tree)
- [The compilation pipeline](commentary/pipeline)

- [The Compiler](commentary/compiler)

  - [Organisation](commentary/compiler): high-level structure
  - [Compiler Module Dependencies](module-dependencies)
  - [Compiling one module: HscMain](commentary/compiler/hsc-main)
  - Key data types:

    - Source-language syntax: [HsSyn](commentary/compiler/hs-syn-type)
    - Names: [RdrName and OccName](commentary/compiler/rdr-name-type), [Name](commentary/compiler/name-type)
    - [Entities](commentary/compiler/entity-types): variables, type constructors, data constructors, and classes.
    - Types: [Type and Kind](commentary/compiler/type-type), [equality types and coercions](commentary/compiler/fc).
    - [The core language](commentary/compiler/core-syn-type).
    - Cmm.
  - Passes:

    - [Renamer](commentary/compiler/renamer)
    - Typechecker
    - Desugarer
    - Core-\>core
  - SPJ ModIface, ModDetails, ModGuts
  - SPJ Core-\>CorePrep-\>Stg-\>Cmm
  - [Primitive Operations (PrimOps)](commentary/prim-ops)
  - [The GHC API](commentary/compiler/api)
  - SM HscMain upwards: ModSummary, Finder, upsweep, downsweep,
  - [Coding guidelines](commentary/coding-style)

- [The Runtime System](commentary/rts)

  - [RTS Configurations](commentary/rts/config)
  - [The Word](commentary/rts/word)
  - [What the hell is a {{{.cmm}}} file?](commentary/rts/cmm)
  - [Layout of heap objects](commentary/rts/heap-objects)
  - [Layout of the stack](commentary/rts/stack)
  - [Haskell Execution](commentary/rts/haskell-execution)
  - [The Scheduler](commentary/rts/scheduler)
  - [The Storage Manager](commentary/rts/storage)
  - [So how does {{{foreign import "wrapper"}}} work?](commentary/rts/ffi)
  - [GHCi support: the byte-code interpreter and dynamic linker](commentary/rts/interpreter)
  - [Asynchronous exceptions](commentary/rts/async-exceptions)
  - [Software Transactional Memory (STM)](commentary/rts/stm)
  - [Garbage Collecting CAFs](commentary/rts/ca-fs)
  - [Weak Pointers and Finalizers](commentary/rts/weak)
  - [Coding conventions in the RTS](commentary/rts/conventions)

## Contributed Documentation


Please feel free to add new pages here.  In due course information will migrate from here to the main commentary above.

- The Compiler

  - BeginnersNotes: Some notes about getting started hacking GHC and the structure of the compiler (especially types and typecheck)
  - [TypeFunctions](type-functions): Notes concerning the implementation of type functions and associated types in the [ FC branch](http://darcs.haskell.org/ghc-fc2/) of GHC.
  - [IntermediateTypes](intermediate-types): Notes about the type system of GHC's new intermediate language, in the [ FC branch](http://darcs.haskell.org/ghc-fc2/)
  - [RewriteRules](rewrite-rules): Notes about the implementation of RULEs in GHC
  - [BackEndNotes](back-end-notes): Some ideas and notes about the back end.
  - [GhciDebugger](ghci-debugger): Some notes about the implementation of the GHCi debugger. Probably uninteresting unless you want to work on the debugger.
  - [DebuggingGhcCrashes](debugging-ghc-crashes): how to use gdb to debug a crash in GHC-compiled code.
  - [AddingNewPrimitiveOperations](adding-new-primitive-operations): How to add new primitive operations to GHC Haskell.
  - [Replacing GMP](replacing-gmp-notes): Notes from an effort to replace GMP with another Bignum library.
- The Runtime System

  - [GarbageCollectorNotes](garbage-collector-notes) Notes about GHC's existing single threaded garbage collector and development of a parallel GC.
