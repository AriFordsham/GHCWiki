# The new Generic Deriving mechanism (ongoing work)


GHC includes a new (in 2010) mechanism to let you write generic functions.  It is described in [ A generic deriving mechanism for Haskell](http://www.dreixel.net/research/pdf/gdmh_nocolor.pdf), by Magalhães, Dijkstra, Jeuring and Löh.  This page sketches the specifics of the implementation; we assume you have read the paper.


This mechanism replaces the [previous generic classes implementation](http://www.haskell.org/ghc/docs/6.12.2/html/users_guide/generic-classes.html).

## Main components

- `TcDeriv.tcDeriving` generates an `InstInfo` for each data type that fulfills the `isRep0` predicate. This `InstInfo` is the `Representable0` instance for that type, allowing it to be handled generically (by kind-`*` generic functions).

- The representation types and core functionality of the library live on `GHC.Generics` (on the `ghc-prim` package).

- Many names have been added as known in `prelude/PrelNames`

- Most of the code generation is handled by `types/Generics`

## To do

- Generate meta-information empty datatypes and instances (`Datatype`, `Constructor`, and `Selector` instances)

- Generate `Representable1` instances

- Generic instances

  - Add `deriving` as a keyword. This replaces the `DERIVABLE` pragma from the UHC implementation, and is attached to a default method on a class declaration.
  - Change the `Class` definition to allow for generic defaults (in addition to standard defaults).
  - Generate default instances for representable types which derive generic classes.

## Problems/questions

- When representations are generated for more than one datatype, assembler errors appear: `symbol `ghczmprim_GHCziGenerics_Representable0_closure' is already defined`

- Currently, in `TcDeriv.genGenericRepBind` we generate instances using `mkLocalInstance`. Is this right, or should we use `mkImportedInstance` instead?
