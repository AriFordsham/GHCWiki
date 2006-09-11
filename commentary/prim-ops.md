# Primitive Operations (PrimOps)


A PrimOp is a function that cannot be implemented in Haskell, and are provided natively by GHC.  For example, adding two `Int#` values is provided as the PrimOp `+#`, and allocating a new mutable array is the PrimOp `newArray#`.

[PrimOps](commentary/prim-ops) are made available to Haskell code through the virtual module `GHC.Prim`.  This module has no implementation, and its interface never resides on disk: if `GHC.Prim` is imported, we use a built-in `ModIface` value - see `ghcPrimIface` in [compiler/iface/LoadIface.lhs](/trac/ghc/browser/ghc/compiler/iface/LoadIface.lhs).

## The primops.txt.pp file


The file [compiler/prelude/primops.txt.pp](/trac/ghc/browser/ghc/compiler/prelude/primops.txt.pp) includes all the information the compiler needs to know about a PrimOp, bar its actual implementation.  For each PrimOp, `primops.txt.pp` lists:

- Its name, as it appears in Haskell code (eg. int2Integer\#)
- Its type
- The name of its constructor in GHC's `PrimOp` data type.
- Various properties, such as whether the operation is commutable, or has side effects.


For example, here's the integer multiplication PrimOp:

```wiki
primop   IntegerMulOp   "timesInteger#" GenPrimOp   
   Int# -> ByteArr# -> Int# -> ByteArr# -> (# Int#, ByteArr# #)
   with commutable = True
        out_of_line = True
```


The `primops.txt.pp` file is processed first by CPP, and then by the `genprimopcode` program (see [utils/genprimopcode](/trac/ghc/browser/ghc/utils/genprimopcode)).  `genprimopcode` generates the following bits from `primops.txt.pp`:

- Various files that are `#include`d into [compiler/prelude/PrimOp.lhs](/trac/ghc/browser/ghc/compiler/prelude/PrimOp.lhs),
  containing declarations of data types and functions describing the PrimOps.  See
  [compiler/Makefile](/trac/ghc/browser/ghc/compiler/Makefile).

- `libraries/base/GHC/PrimopWrappers.hs`, a file that contains (curried) wrapper
  functions for each of the PrimOps, so that they are accessible from byte-code, and
  so that the [byte-code interpreter](commentary/rts/interpreter) doesn't need to implement any PrimOps at all: it
  just invokes the compiled ones from `GHC.PrimopWrappers`.

- `libraries/base/GHC/Prim.hs`, a source file containing dummy declarations for
  all the PrimOps, solely so that Haddock can include documentation for `GHC.Prim`
  in its documentation for the `base` package.  The file `GHC/Prim.hs` is never
  actually compiled, only processed by Haddock.

## Implementation of PrimOps


PrimOps are divided into two categories for the purposes of implementation: inline and out-of-line.

### Inline PrimOps

### Out-of-line PrimOps

## Adding a new PrimOp