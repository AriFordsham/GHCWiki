
\[ Up: [Commentary](commentary) \]

# Primitive Operations ([PrimOps](commentary/prim-ops))


A PrimOp is a function that cannot be implemented in Haskell, and are provided natively by GHC.  For example, adding two `Int#` values is provided as the PrimOp `+#`, and allocating a new mutable array is the PrimOp `newArray#`.

[PrimOps](commentary/prim-ops) are made available to Haskell code through the virtual module `GHC.Prim`.  This module has no implementation, and its interface never resides on disk: if `GHC.Prim` is imported, we use a built-in `ModIface` value - see `ghcPrimIface` in [compiler/iface/LoadIface.lhs](/trac/ghc/browser/ghc/compiler/iface/LoadIface.lhs).

## Implementation of [PrimOps](commentary/prim-ops): MachOps

## The primops.txt.pp file

## Adding a new PrimOp