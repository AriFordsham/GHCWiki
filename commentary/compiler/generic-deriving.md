# Support for generic programming


GHC includes a new (in 2010) mechanism to let you write generic functions.  It is described in paper [ A generic deriving mechanism for Haskell](http://www.dreixel.net/research/pdf/gdmh_nocolor.pdf). This page sketches the specifics of the implementation; we assume you have read the paper. The [ HaskellWiki page](http://www.haskell.org/haskellwiki/Generics) gives a more general overview.


This mechanism replaces the [previous generic classes implementation](http://www.haskell.org/ghc/docs/6.12.2/html/users_guide/generic-classes.html). What we describe until the "Kind polymorphic overhaul" section is implemented and released in GHC 7.0.1.

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

## Testing

- Tests are available under the `generics` directory of the testsuite.

# Kind polymorphic overhaul


The current implementation supports defining both functions over types of kind `*` (such as `show`) and functions over types of kind `* -> *` (such as `fmap`). Although care has been taken to reduce code duplication, we still need two generic classes, one for each kind (`Generic` and `Generic1`).


With the new `-XPolyKinds` functionality, we can make the support for generic programming better typed and more general. The basic idea is to define the universe codes (`M1`, `:+:`, etc.) as constructors of a datatype. Promotion then lifts these constructors to types, which we can use as before, only that now we have them all classified under a new kind:

```wiki
data Universe x = U
                | K x
                | P Nat
                | Universe x :+: Universe x
                | Universe x :*: Universe x
                | M MetaData (Universe x)

data MetaData = C | D | S
```