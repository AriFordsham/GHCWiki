# The new Generic Deriving mechanism (ongoing work)


GHC includes a new (in 2010) mechanism to let you write generic functions.  It is described in [ A generic deriving mechanism for Haskell](http://www.dreixel.net/research/pdf/gdmh_nocolor.pdf), by Magalhães, Dijkstra, Jeuring and Löh.  This page sketches the specifics of the implementation; we assume you have read the paper. The [ HaskellWiki page](http://www.haskell.org/haskellwiki/Generics) gives a more general overview.


This mechanism replaces the [previous generic classes implementation](http://www.haskell.org/ghc/docs/6.12.2/html/users_guide/generic-classes.html). The code is in the `ghc-generics` branch of the [ ghc](https://github.com/ghc/ghc/commits/ghc-generics), [ base](https://github.com/ghc/packages-base/commits/ghc-generics), [ ghc-prim](https://github.com/ghc/packages-ghc-prim/commits/ghc-generics), [ haddock2](https://github.com/ghc/haddock2/commits/ghc-generics), and [ testsuite](https://github.com/ghc/testsuite/commits/ghc-generics) repos.

## Main components

- `TcDeriv.tcDeriving` now allows deriving `Generic` instances.

- The representation types and core functionality of the library live on `GHC.Generics` (on the `ghc-prim` package).

- Many names have been added as known in `prelude/PrelNames`

- Most of the code generation is handled by `types/Generics`

## Things that have been removed

- All of the [generic classes stuff](http://www.haskell.org/ghc/docs/6.12.2/html/users_guide/generic-classes.html). In particular, the following have been removed:

  - `hasGenerics` field from `TyCon`;
  - `HsNumTy` constructor from `HsType`;
  - `TypePat` constructor from `Pat`.

- The `-XGenerics` flag is now deprecated.

## What already works

- `Generic` instances can be derived when `-XDeriveGeneric` is enabled.

- The `default` keyword can used for generic default method signatures when `-XDefaultSignatures` is enabled.

- Generic defaults are properly instantiated when giving an instance without defining the generic default method.

- Base types like `[]`, `Maybe`, tuples, come with Generic instances.

## To do

- Generate `Generic1` instances

- Print also the `Rep` type instance when -ddump-deriving is on

- Register the `DeriveGeneric` and `DefaultSignatures` extensions with Cabal.

- Do we want `Show`, etc. instances for types like `U1`, `:+:`, ...?

## Testing

- Tests are available under the `generics` directory of the testsuite.
