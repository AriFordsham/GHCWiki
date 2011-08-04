# GHC Kind level


This page gives the theory, implementation overview and details about GHC's kind level.  This work is related to [ Conor's SHE system](http://personal.cis.strath.ac.uk/~conor/pub/she/) and will be related to Iavor's work on [TypeNats](type-nats) to deal with primitive types (promoted `Int` and `Char`).

## Theory


We use the mechanism of promotion to lift a data type to the kind level.  This gives access at the type level to the data constructors, and at the kind level to the type constructor.  All data types cannot be promoted.  For examples GADTs or data types with higher-order kinds.  We add kind polymorphism to allow promotion of polymorphic data constructors (like `Nil` or `Cons`).


More details can be found in [ this theory pdf](http://gallium.inria.fr/~jcretin/ghc/theory.pdf).

## Examples


Examples of reimplementation of existing Haskell librairies can be found in [ this examples pdf](http://gallium.inria.fr/~jcretin/ghc/examples.pdf).

## Implementation


The branch is called `ghc-kinds`.  Its current state is:

<table><tr><th></th>
<th> ADT promotion </th>
<th> Primitives </th>
<th> Kind polymorphism 
</th></tr>
<tr><th> Parser      </th>
<th>      Yes      </th>
<th>    Yes     </th>
<th>       Yes         
</th></tr>
<tr><th> Renamer     </th>
<th>  In progress  </th>
<th>    Yes     </th>
<th></th></tr>
<tr><th> Typechecker </th>
<th></th>
<th></th>
<th></th></tr></table>


Promotion-related changelog:

- Change the kind representation in `HsSyn` from `Kind` to `LHsKind name` adding some `PostTcKind` when necessary.

  - Rename `rnHsType` into `rnHsTyKi` and parametrize with a boolean to know if we are renaming a type or a kind.
- Allow promoted data and type constructors:

  - Extend the parser to allow ticked names like `'Zero` or `'Nat.Succ` as atoms in types.
  - Extend the parser to allow *optionally* ticked names like `Nat` or `'Bool` as atoms in kinds.
  - Extend `HsType name` with `HsPromotedConTy name` to represent ticked names.
  - Extend `TyCon` with `PromotedDataTyCon` to represent promoted data constructors.
- Rename `KindVar` which is used during type checking into `MetaKindVar`, since we will add kind variables later.


Not promotion-related changelog:

- Use `HsDocContext` instead of `SDoc` to track renaming context.


The stage1 compiler does not work, since there is some `undefined`s in the typechecker.  So you won't be able to build a stage2 or even run validate.  This is the first priority.
