# GHC Kind level


This page gives the theory, implementation overview and details about GHC's kind level.  This work is related to [ Conor's SHE system](http://personal.cis.strath.ac.uk/~conor/pub/she/) and will be related to Iavor's work on [TypeNats](type-nats) to deal with primitive types (promoted `Int` and `Char`).

## Theory


We use the mechanism of promotion to lift a data type to the kind level.  This gives access at the type level to the data constructors, and at the kind level to the type constructor.  All data types cannot be promoted.  For examples GADTs or data types with higher-order kinds.  We add kind polymorphism to allow promotion of polymorphic data constructors (like `Nil` or `Cons`).


More details can be found in [ this theory pdf](http://gallium.inria.fr/~jcretin/ghc/theory.pdf).

## Examples


Examples of reimplementation of existing Haskell librairies can be found in [ this examples pdf](http://gallium.inria.fr/~jcretin/ghc/examples.pdf).

## Implementation


The GHC branch is called `ghc-kinds`.  There is also a Haddock branch with the same name.


The implementation will follow these steps (in bold is the first phase (parser, renamer, type checker, ...) that does not work):

1. **\[Type checker\]** Promotion of Haskell98 data types of kind star: `*`.
1. Kind polymorphism in Core.
1. Promotion of Haskell98 data types of first order kind: `* -> .. * -> *`.
1. Kind polymorphic data types, type families, and type classes.
1. Singleton types.
1. Built-in types.


Promotion-related changelog:

- Change the kind representation in `HsSyn` from `Kind` to `LHsKind name` adding some `PostTcKind` when necessary.

  - Rename `rnHsType` into `rnHsTyKi` and parametrize with a boolean to know if we are renaming a type or a kind.
- Allow promoted data and type constructors:

  - Extend the parser to allow ticked names like `'Zero` or `'Nat.Succ` as atoms in types.
  - Extend the parser to allow *optionally* ticked names like `Nat` or `'Bool` as atoms in kinds.
  - Extend `HsType name` with `HsPromotedConTy name` to represent ticked names.
  - Extend the renamer to handle implicit promotion.
  - Extend `TyCon` with `PromotedDataTyCon` to represent promoted data constructors.
- Rename `KindVar` which is used during type checking into `MetaKindVar`, since we will add kind variables later.


Not promotion-related changelog:

- Use `HsDocContext` instead of `SDoc` to track renaming context.
