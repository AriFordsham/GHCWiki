## Associating pattern synonyms with types


This section is based upon [\#10653](https://gitlab.haskell.org//ghc/ghc/issues/10653) and D1258.


Pattern synonyms allow for constructors to be defined, exported and imported separately from the types which they build.
However, it is sometimes convenient to associate synonyms with types so that we can more closely model ordinary data constructors.


If we want to refactor to change the internal representation of this maybe-like type to use Maybe.

```wiki
-- Main.hs
module Main where

import Internal ( A(..))

-- Internal.hs
module Internal where

data A = MkA Int | NoA
```


If we modify `Internal.hs` as follows

```wiki
{-# LANGUAGE PatternSynonyms #-}
module Internal where

newtype A = NewA (Just Int)

pattern MkA n = A (Just n)

pattern NoA = A Nothing
```


Then local definitions to `Internal` which used `A` would work as before but modules importing `Internal` and `A`
will no longer work as importing `A(..)` will import the type `A` and the constructor `NewA`. We can explicitly import the
new patterns but the usage of pattern synonyms should be transparent to the end user. What's needed is to be able to
associate the new synonyms with a type such that client code is oblivious to this implementation.

### Proposal


Richard proposes that synonyms are associated at the export of a datatype. Our running example would then look as follows:

```wiki
{-# LANGUAGE PatternSynonyms #-}
module Internal(A(MkA, NoA)) where

newtype A = NewA (Just Int)

pattern MkA n = A (Just n)

pattern NoA = A Nothing
```

### Specification


This proposal only changes module imports and exports. 

#### Definition


We say that "a pattern synonym `P` is associated with a type `T` relative to module `M`" if and only if "`M` exports `T` whilst associating `P`". 

#### Exports


For any modules `M``N`, we say that "`M` exports `T` whilst associating `P`" just when

- The export has the form `T(c1, ..., cn, P)` where c1 to cn (n \>= 0) are a mixture of other field names, constructors, pattern synonyms and the special token `..`. The special token `..`, which indicates either 

  1. all constructors and field names from `T`'s declaration, if `T` is declared in this module; or 
  1. all symbols imported with `T`, which might perhaps include patterns associated with `T` in some other module.


In case (2), `..` might in fact be a union of sets if `T` is imported from multiple modules with different sets of associated definitions.

#### Imports


For any modules `M``N`, if we import `N` from `M`,

- The abbreviated form `T(..)` brings into scope all the constructors, methods or field names exported by `N` as well any patterns associated with `T` relative to `N`. 
- The explicit form `T(c1,...,cn)` can name any constructors, methods or field names exported by `N` as well as any patterns associated with `T` relative to `N`. 

#### Typing


WIP


If we associate a pattern synonym `P` with a type `T` then we consider two separate cases depending on the type of `P`. 

- If `P :: T t1 t2 .. tn` then `P` is an instance of \`T. We completely ignore constraints in this case.
- If `P :: f t1 t2 .. tn`, in other words, if `P` is polymorphic in `f`, then `f` must be brought into scope by a constraint. In this case we check that `T u1 ... un` is a subtype of `ReqTheta => f t1 t2 ... tn`. We must involve `ReqTheta` in the case that it contains equality constraints and/or type families. In the case that ReqTheta contains a class constraint, we require that the correct instance for `T` is in scope.
- If `P :: F v1 .. vm` where `F` is a type family then unless `F v1 ... vm ~ T t1 t2 ... tn` we do not allow `P` to be associated with `T`.


A few examples are included for clarification in the final section.

#### Clarification

- Hence, all synonyms must be initially explicitly associated but a module which imports an associated synonym is oblivious to whether they import a synonym or a constructor.

- According to this proposal, only pattern synonyms may be associated with a datatype. But it would be trivial to expand this proposal to allow arbitrary associations. 

#### Examples

```
moduleN(T(..,P))wheredataT=MkTIntpatternP=MkT5-- M.hsmoduleMwhereimportN(T(..))
```

`P` is associated with `T` relative to `N`. M imports `T`, `MkT` and `P`.

```
moduleN(T(..))wheredataT=MkTIntpatternP=MkT5-- M.hsmoduleMwhereimportN(T(..))
```

`P` is unassociated. `M` imports `T` and `MkT`. 

```
moduleN(T(P))wheredataT=MkTIntpatternP=MkT5-- M.hsmoduleMwhereimportN(T(..))
```

`P` is associated with `T` relative to `N`. M imports `T`, and `P`.

```
moduleN(T(P))wheredataT=MkTIntpatternP=MkT5-- M.hsmoduleM(T(..))whereimportN(T(..))-- O.hsmoduleOwhereimportM(T(..))
```

`P` is associated with `T` relative to `N`.


As `M` imports `N` and imports `T`, `P` is associated with `T` relative to `M`. Thus `M` exports `T` and `P`.


Therefore when `O` imports `T(..)` from `M`, it imports `T` and `P`. 

```
moduleN(T(..))wheredataT=MkTInt-- M.hsmoduleM(T(P))whereimportN(T(..))patternP=MkT5-- O.hsmoduleOwhereimportM(T(..))
```


This example highlights being able to freely reassociate synonyms. 

`M` imports `T` and `MkT` from `N` but then as `M` associates `P` with `T`, when `O` imports `M`, `T` and `P` are brought into scope. 

#### Typing Examples

```
{-# LANGUAGE PatternSynonyms #-}moduleFoo(A(P))wheredataA=ApatternP::ApatternP=A
```


Pattern `P` has type `A` therefore we allow this export.

```
{-# LANGUAGE PatternSynonyms #-}moduleFoo(A(P))wheredataA=AdataB=BpatternP::BpatternP=B
```


Pattern `P` has type `B` therefore we do not allow this export.

```
{-# LANGUAGE PatternSynonyms #-}moduleFoo(A(P))wheredataA a =ApatternP::AIntpatternP=A
```


Pattern `P` has type `A Int` which is an instance of `A a` therefore we allow
this export

##### Constraints

```
{-# LANGUAGE PatternSynonyms #-}moduleFoo(A(P))wheredataA=ApatternP::()=>(A~ f)=> f
patternP=A
```


Pattern `P` has type `f`, when solving the required constraints we find that
`A ~ f` so `P :: A` and hence we allow the export.

```
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}moduleFoo(Identity(P))wheredataIdentity a =Identity a

instanceCIdentitywhere
  build a =Identity a
  destruct (Identity a)= a

classC f where
  build :: a -> f a
  destruct :: f a -> a

patternP::()=>C f => a -> f a
patternP x <-(destruct -> x)whereP x = build x
```


In this example, `P` is once again polymorphic in the constructor `f`. It might
seem that we should only allow `P` when there is an instance for `C Identity`
in scope. However, we completely ignore class constraints as a user may
provide an orphan instance whichs allows the pattern to be used. However,
it is more conservative and perhaps less surprising to require that the correct
instance is in scope.


It may seen like we should not allow any synonym which is polymorphic in the
constructor to be associated with a type but this is too strict. The previous
example showed a synonym which was polymorphic in `f` but `f` has arity 0. In
general, we see that equality constraints mean that we have to do at least some
constraint solving even if it is a very strange way to define a pattern synonym.


For example, we certainly do not want to allow the following association.

```
{-# LANGUAGE PatternSynonyms #-}moduleFoo(A(P))wheredataA=AdataB=BpatternP::()=>(B~ f)=> f
patternP=B
```


Things get even more hairy when we remember that classes can have equality constraints.
Consider the quite weird example.

```
{-# LANGUAGE PatternSynonyms #-}{-# LANGUAGE MultiParamTypeClasses #-}{-# LANGUAGE GADTs #-}{-# LANGUAGE ViewPatterns #-}moduleFoo(A(P))whereclass(f ~A)=>C f a where
  build :: a -> f a
  destruct :: f a -> a

dataA a =A a

instanceCAIntwhere
  build n =A n
  destruct (A n)= n


patternP::()=>C f a => a -> f a
patternP x <-(destruct -> x)whereP x = build x
```


We should only allow `P` to be associated with `A` due to the superclass constraint
`f ~ A`.

##### Type Families =


The final example is when an equality constraint involves a type family.

```
{-# LANGUAGE PatternSynonyms #-}moduleFoo(A(P))wheredataA=AtypefamilyF a

patternP::FBoolpatternP=A
```


There are no instances for `F` but a client module which imports `A` and `P`
could provide such an instance which then may or may not type check.


I think that such patterns should not be allowed to be associated.

### Unnatural Association


There is some discussion about what should happen with synonyms which target types defined in the prelude. 
It is very uncommon to explicitly import datatypes defined in the prelude, thus this kind of association is very rare in practice
but should be allowed.

### Module Chasing


Simon is a bit worried that once we allow this association then there is no limit to the number of constructors which
can be associated with a type. With normal datatypes, `T(..)` means some subset of the constructors defined where `T` is
defined. With this proposal `T(..)` has no maximal meaning, I don't see any problem with this behaviour as the meaning can still
be determined by the renamer.

### The Privilege Objection


Simon also wonders why it is possible to privilege pattern synonyms in this way and not normal functions for example.
I don't think this is particularly puzzling as at their most general pattern synonyms allow for the definition of unassociated 
data constructors. Being able to later associate them with types only brings their behaviour closer to ordinary data constructors.

### Polymorphic Synonyms


The following is a valid pattern synonym declaration which doesn't have a definite constructor.

```wiki
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
module Foo where

class C f where
  build :: a -> f a
  destruct :: f a -> a

pattern P :: () => C f => a -> f a
pattern P x <- (destruct -> x)
  where
    P x = build x
```


I propose that we allow such synonyms to be associated with a type `T` as long as it typechecks. I don't expect this to be much used in practice. 

### Associatation at definition


Simon proposed that synonyms are associated at the definition of a datatype. Our running example would look as follows:

```wiki
{-# LANGUAGE PatternSynonyms #-}
module Internal(A(MkA, NoA)) where

newtype A = NewA (Just Int)
    with (MkA, NoA)

pattern MkA n = A (Just n)

pattern NoA = A Nothing
```


The proposal refers to Richard's suggestion rather than Simon's refinement for the following reasons.


Consider two packages `old-rep` and `new-rep` which have  different representations of the same structure. 
The library author wants to smooth the transition for his users by providing a compatibility package `compat-rep`
so that code using the old representation in `old-rep` can work seamlessly with `new-rep`. 


The problem is to define `compat-rep` such that by changing the dependencies of our package, our code to continues to work
but without depending on `old-rep`. More generally, an author may want to write a `*-compat` package for two packages which they do
not control. Having to define these synonyms at the definition site is too restrictive for this case .
