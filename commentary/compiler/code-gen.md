# GHC Commentary: The Code Generator

[compiler/codeGen](/trac/ghc/browser/ghc/compiler/codeGen)

## Storage manager representations


The code generator needs to know the layout of heap objects, because it generates code that accesses and constructs those heap objects.  The runtime also needs to know about the layout of heap objects, because it contains the garbage collector.  How can we share the definition of storage layout such that the code generator and the runtime both have access to it, and so that we don't have to keep two independent definitions in sync?


Currently we solve the problem this way:

- C types representing heap objects are defined in the C header files, see for example [includes/Closures.h](/trac/ghc/browser/ghc/includes/Closures.h).

- A C program, [includes/mkDerivedConstants.c](/trac/ghc/browser/ghc/includes/mkDerivedConstants.c),  `#includes` the runtime headers.
  This program is built and run when you type `make` or `make boot` in `includes/`.  It is
  run twice: once to generate `includes\DerivedConstants.h`, and again to generate 
  `includes/GHCConstants.h`.

- The file `DerivedConstants.h` contains lots of `#defines` like this:

  ```wiki
  #define OFFSET_StgTSO_why_blocked 18
  ```

  which says that the offset to the why_blocked field of an `StgTSO` is 18 bytes.  This file
  is `#included` into [includes/Cmm.h](/trac/ghc/browser/ghc/includes/Cmm.h), so these offests are available to the
  [hand-written .cmm files](commentary/rts/cmm).

- The file `GHCConstants.h` contains similar definitions:

  ```wiki
  oFFSET_StgTSO_why_blocked = 18::Int
  ```

  This time the definitions are in Haskell syntax, and this file is `#included` directly into
  [compiler/main/Constants.lhs](/trac/ghc/browser/ghc/compiler/main/Constants.lhs).  This is the way that these offsets are made
  available to GHC's code generator.
