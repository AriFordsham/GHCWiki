# Type Indexed Type Representations Proposal

## Overview


This page describes a new API for type indexed type representations, basic `Dynamic` functionality, static pointers and distributed closures.  It is a specific realisation of the ideas in [DistributedHaskell](distributed-haskell).


We consider 4 APIs 

- `Data.Typeable`: type-indexed type representation.  This replaces the existing `Data.Typeable`, and the kind of `TypeRep` changes. 

- `Data.Dynamic`: dynamically-typed values; replaces the existing `Data.Dynamic`.  The API is almost unchanged.

- `Data.StaticPtr`

- and `DistributedClosure`, built up in that order.


We consider two varients, one for ghc as of 2015-07-10 with kind-homogenous equalities `a:~:b` only, and one for kind-hetrogenous type equalities `a:~~:b`.


The TCB consists of (in the homogenous case), the implementation of `data TypeRepT`, `class TypeableT` and its implementations, `eqRR` and `eqRRHom` (comparison of `TypeRepT`s), `getR1` and `getR2` (decomposing `TypeRepT`s); and the RTS support for building the static pointer table.
As is currently done for `Typeable`, the instances for `TypeableT` should be "magically" provided by GHC.


In the kind-hetrogenous case, `getR1` and `getR2` come out of the TCB.

## Transition Plan


We propose to

- *Outright replace* the existing `Typeable` class with the new one; ditto the `TypeRep` type.

- GHC magic for the `Typeable` class will apply to the new class.

- To ease the transition we will provide both

  - The old API via a (deprecated) new module `Data.Typeable710`.
  - The old API via the exiting module `Data.Typeable` but with new names for (deprecated) old types and functions.  


This seems better than inventing new names for everything (e.g. class `TypeableT`, data type `TypeRepT`.  Not many packages use `TypeRep` explicitly, we want to encourage those that do to switch over.


Dynamic should be a fairly seamless changeover, since `Dynamic` is abstract currently (although we cannot provide a `dynTypeRep :: Dynamic -> TypeRepT ?` function - this does not seem oft-used & can be avoided by pattern matching as our `DynamicT` is not abstract, or by providing a `Typeable` based on top of `TypeableT`).


Note that the static pointer support requires a static pointer table in a different form to what GHC already supports, and an extension to the static keyword.

## Questions

- Now is the easiest time to rename things - do you have suggestions for better naming schemes?
- How many `getR1`, `getR2` etc should we provide?
- Do we want explicit names for some type representations?
  Perhaps `typeRepTBool` etc., and just for Prelude defined types.
  (It is nice to avoid writing `typeRepT :: TypeRepT Bool`)
- Do we want to provide `dynApp` which calls `error` instead of returning `Nothing`?
  It seems to be much less used than `dynApply`, and personally I dislike that style.
- The static "polymorphism" support is a bit kludgy - any comments on this would be most helpful!
- Any comments would be gladly recieved!

---

## Data.Typeable

### Homogenous Case


Naming scheme: put an `R` suffix on variants that take an explicit `TypeRepT` parameter, no suffix for `TypeableT` constraint versions.

```
dataTypeRepT(a :: k)-- abstractappR::TypeRepT(a :: k -> k')->TypeRepT(b :: k)->TypeRepT(a b)classTypeableT(a :: k)where
  typeRepT ::TypeRepT a

-- GHC has magic built-in support for Typeable instances-- but the effect is similar to declarations like these:instance(TypeableT c,TypeableT a)=>TypeableT(c a)instanceTypeableTBoolinstanceTypeableT(->)withTypeRepT::TypeRepT a ->(Typeable a => b)-> b
-- c.f. Trac #2439eqRR::TypeRepT(a :: k1)->TypeRepT(b :: k2)->BooleqRRHom::TypeRepT(a :: k)->TypeRepT(b :: k)->Maybe(a :~: b)dataGetAppT(a :: k)whereGA::TypeRepT(a :: k1 -> k2)->TypeRepT(b :: k1)->GetAppT(a b)getAppR::TypeRepT(a :: k)->Maybe(GetAppT a)dataG1 c a whereG1::TypeRepT(a :: k)->G1(c :: k -> k')(c a)getR1::TypeRepT(ct :: k1 -> k)->TypeRepT(c'at :: k)->Maybe(G1 ct c'at)-- Implementation uses an unsafeCoercedataG2 c a whereG2::TypeRepT(a :: k1)->TypeRepT(b :: k2)->G2(c :: k1 -> k2 -> k3)(c a b)getR2::TypeRepT(c :: k2 -> k1 -> k)->TypeRepT(a :: k)->Maybe(G2 c a)-- Implementation uses an unsafeCoerce-- rest are for conveniencetypeOf::TypeableT a =>(a ::*)->TypeRepT a
getFnR::TypeRepT(a ::*)->Maybe(G2(->) a)castR::TypeRepT(a ::*)->TypeRepT(b ::*)-> a ->Maybe b
cast::(TypeableT(a ::*),TypeableT(b ::*))=> a ->Maybe b
gcastR::TypeRepT(a :: k)->TypeRepT(b :: k)-> c a ->Maybe(c b)gcast::(TypeableT(a :: k),TypeableT(b :: k))=> c a ->Maybe(c b)gcastR1::TypeRepT(t :: k1 -> k2)->TypeRepT(t' :: k1 -> k2)-> c (t a)->Maybe(c (t' a))gcast1::(TypeableT(t :: k1 -> k2),TypeableT(t' :: k1 -> k2))=> c (t a)->Maybe(c (t' a))gcastR2::TypeRepT(t :: k1 -> k2 -> k3)->TypeRepT(t' :: k1 -> k2 -> k3)-> c (t a b)->Maybe(c (t' a b))gcast2::(TypeableT(t :: k1 -> k2 -> k3),TypeableT(t' :: k1 -> k2 -> k3))=> c (t a b)->Maybe(c (t' a b))
```


Note that the type `(:~:)` comes from `Data.Type.Equality`.
Note also `eqRR` is not hugely useful as (if it returns True) we know that types and kinds are the same, but GHC doesn't, so unsafeCoerce is often needed.

### Hetrogenous Case


In this case, where we have a kind-hetrogenous `:~~:`, life becomes simpler: we now never need unsafeCoerce in `getT1` and the like, so we can now just export `getAppT` and leave the rest to the users.


The changes are that `eqRR` now return `a:~~:b` rather than `Bool`, and are more useful (don't force us to use `unsafeCoerce`); `getT1` and `getT2` don't need `unsafeCoerce`, and we can generalise `getFnT` to be poly-kinded.
We obviously may want to provide (and deprecate) `getT1`, `eqRRHom` etc. for compatibility, and maybe provide `getFnT` for convenience, if it turns out to be used a lot etc.


(I am not yet sure whether it would be useful to keep the homogenous equality functions around --- potentially enforcing kind homogeneity could be useful)

---

## Data.Dynamic


API follows current API, except missing `dynTypeRep`, as detailed above.
Provide varients of functions that take explicit `TypeRepT` arguments.

```
dataDynamicwhereDynamic::TypeRepT a -> a ->DynamictoDynR::TypeRepT a -> a ->DynamictoDyn::TypeableT a => a ->DynamicfromDynamicR::TypeRepT a ->Dynamic->Maybe a
fromDynamic::TypeableT a =>Dynamic->Maybe a
fromDynR::TypeRepT a ->Dynamic-> a -> a
fromDyn::TypeableT a =>Dynamic-> a -> a
dynApp::Dynamic->Dynamic->Dynamic-- may call errordynApply::Dynamic->Dynamic->MaybeDynamic-- This is a mid-point between fully dynamic & fully static-- We statically know some "Shape" information, but not all info about type-- e.g., could know is a list, but not know type of elements.dataSDynamic s whereSDynamic::TypeRepT a -> s a ->SDynamic s

toSDynR::TypeRepT a -> s a ->SDynamic s
toSDyn::TypeableT a => s a ->SDynamic s
fromSDynamicR::TypeRepT a ->SDynamic s ->Maybe(s a)fromSDynamic::TypeableT a =>SDynamic s ->Maybe(s a)fromSDynR::TypeRepT a ->SDynamic s -> s a -> s a
fromSDyn::TypeableT a =>SDynamic s -> s a -> s a
```

## Data.StaticPtr

TODO api, talk about poly

## Control.DistributedClosure

TODO