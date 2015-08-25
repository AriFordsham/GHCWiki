# Type Indexed Type Representations Proposal

## Overview


This page describes a new API for type indexed type representations, basic `Dynamic` functionality, static pointers and distributed closures.  It is a specific realisation of the ideas in [DistributedHaskell](distributed-haskell).


Just as in [DistributedHaskell](distributed-haskell), we provide 4 APIs 

- `Data.Typeable`: type-indexed type representation.  This replaces the existing `Data.Typeable`, and the kind of `TypeRep` changes. 

- `Data.Dynamic`: dynamically-typed values; replaces the existing `Data.Dynamic`.  The API is almost unchanged.

- `Data.StaticPtr`: static pointers. The idea is that this will ultimately by the foundation for the Cloud Haskell `xxx` package.

- `DistributedClosure`: serialisable closures.  The idea is that this will ultimately by the foundation for the Cloud Haskell `distributed-closure` package.


Each is described in more detail below.

## Transition Plan


We propose to

- *Outright replace* the existing `Typeable` class with the new one; ditto the `TypeRep` type. This seems better than inventing new names for everything (e.g. class `TypeableT`, data type `TypeRepT`.  Not many packages use `TypeRep` explicitly, we want to encourage those that do to switch over.

- GHC magic for the `Typeable` class will apply to the new class.

- To ease the transition we will provide both

  - The old API via a (deprecated) new module `Data.Typeable710`.
  - The old API via the exiting module `Data.Typeable` but with new names for (deprecated) old types and functions.  

## Questions

- Now is the easiest time to rename things - do you have suggestions for better naming schemes?
- Any comments would be gladly recieved!

---

## Data.Typeable


For `Data.Typeable` we ultimately need Richard Eisenberg's kind
equalities.  But until GHC gets kind equalities we offer variant ("homogeneous case") 
that doens't need them,
but has an extra `unsafeCoerce` or two.


offer two variants, one for ghc as of 2015-07-10 with kind-homogenous equalities `a:~:b` only, and one for kind-hetrogenous type equalities `a:~~:b`.

### Without kind equalities

```
dataTypeRep(a :: k)-- abstractappR::TypeRep(a :: k -> k')->TypeRep(b :: k)->TypeRep(a b)classTypeable(a :: k)where
  typeRepT ::TypeRep a

-- GHC has magic built-in support for Typeable instances-- but the effect is similar to declarations like these:instance(Typeable c,Typeable a)=>Typeable(c a)instanceTypeableBoolinstanceTypeable(->)withTypeRep::TypeRep a ->(Typeable a => b)-> b
-- c.f. Trac #2439eqRR::TypeRep(a :: k1)->TypeRep(b :: k2)->BooleqRRHom::TypeRep(a :: k)->TypeRep(b :: k)->Maybe(a :~: b)dataGetAppT(a :: k)whereGA::TypeRep(a :: k1 -> k2)->TypeRep(b :: k1)->GetAppT(a b)getAppR::TypeRep(a :: k)->Maybe(GetAppT a)dataG1 c a whereG1::TypeRep(a :: k)->G1(c :: k -> k')(c a)getR1::TypeRep(ct :: k1 -> k)->TypeRep(c'at :: k)->Maybe(G1 ct c'at)-- Implementation uses an unsafeCoercedataG2 c a whereG2::TypeRep(a :: k1)->TypeRep(b :: k2)->G2(c :: k1 -> k2 -> k3)(c a b)getR2::TypeRep(c :: k2 -> k1 -> k)->TypeRep(a :: k)->Maybe(G2 c a)-- Implementation uses an unsafeCoerce-- rest are for conveniencetypeOf::Typeable a =>(a ::*)->TypeRep a
getFnR::TypeRep(a ::*)->Maybe(G2(->) a)castR::TypeRep(a ::*)->TypeRep(b ::*)-> a ->Maybe b
cast::(Typeable(a ::*),Typeable(b ::*))=> a ->Maybe b
gcastR::TypeRep(a :: k)->TypeRep(b :: k)-> c a ->Maybe(c b)gcast::(Typeable(a :: k),Typeable(b :: k))=> c a ->Maybe(c b)gcastR1::TypeRep(t :: k1 -> k2)->TypeRep(t' :: k1 -> k2)-> c (t a)->Maybe(c (t' a))gcast1::(Typeable(t :: k1 -> k2),Typeable(t' :: k1 -> k2))=> c (t a)->Maybe(c (t' a))gcastR2::TypeRep(t :: k1 -> k2 -> k3)->TypeRep(t' :: k1 -> k2 -> k3)-> c (t a b)->Maybe(c (t' a b))gcast2::(Typeable(t :: k1 -> k2 -> k3),Typeable(t' :: k1 -> k2 -> k3))=> c (t a b)->Maybe(c (t' a b))
```


Notes:

- Many of these functions come in two variants: one which takes an explicit `TypeRep` argument, and one that take an implicit `TypeRep` argument via a `Typeable a` constraint.  We use a consistend naming scheme: put an `R` suffix on variants that take an explicit `TypeRep` parameter, no suffix for `Typeable` constraint versions.

- Note that the type `(:~:)` comes from `Data.Type.Equality`.

- Note also `eqRR` is not hugely useful as (if it returns True) we know that types and kinds are the same, but GHC doesn't, so `unsafeCoerce` is often needed.

### Key differences from GHC 7.10

- The key difference is that `TypeRep` becomes type-indexed, so that if `x :: TypeRep [Int]` then `x` is a runtime-inspectable type representation for `[Int]`.   It is poly-kinded, of course, so that `TypeRep Maybe` is fine.  

- In class `Typeable`, the `typeRep` method therefore no longer needs a proxy arguemnt.  Indeed the class dictionary precisely is a single type representation.

### With kind equalities


Once we have kind equalities, we have a kind-hetrogenous `:~~:`.  Now we do not `unsafeCoerce` in `getT1` and the like, so we can now just export `getAppT` and leave the rest to the users.


The changes are that `eqRR` now return `a:~~:b` rather than `Bool`, and are more useful (don't force us to use `unsafeCoerce`); `getT1` and `getT2` don't need `unsafeCoerce`, and we can generalise `getFnT` to be poly-kinded.
We obviously may want to provide (and deprecate) `getT1`, `eqRRHom` etc. for compatibility, and maybe provide `getFnT` for convenience, if it turns out to be used a lot etc.


(I am not yet sure whether it would be useful to keep the homogenous equality functions around --- potentially enforcing kind homogeneity could be useful)

### Trusted code base


The TCB consists of (in the homogenous case), the implementation of `data TypeRep`, `class Typeable` and its implementations, `eqRR` and `eqRRHom` (comparison of `TypeRep`s), `getR1` and `getR2` (decomposing `TypeRep`s); and the RTS support for building the static pointer table.
As is currently done for `Typeable`, the instances for `Typeable` should be "magically" provided by GHC.


In the kind-hetrogenous case, `getR1` and `getR2` come out of the TCB.

### Questions

- How many `getR1`, `getR2` etc should we provide?
- Do we want explicit names for some type representations?
  Perhaps `typeRepTBool` etc., and just for Prelude defined types.
  (It is nice to avoid writing `typeRepT :: TypeRep Bool`)

---

## Data.Dynamic


Dynamic should be a fairly seamless changeover, since `Dynamic` is abstract currently (although we cannot provide a `dynTypeRep :: Dynamic -> TypeRep ?` function - this does not seem oft-used & can be avoided by pattern matching as our `DynamicT` is not abstract, or by providing a `Typeable` based on top of `Typeable`).


API follows current API, except missing `dynTypeRep`, as detailed above.
Provide varients of functions that take explicit `TypeRep` arguments.

```
dataDynamicwhereDynamic::TypeRep a -> a ->DynamictoDynR::TypeRep a -> a ->DynamictoDyn::Typeable a => a ->DynamicfromDynamicR::TypeRep a ->Dynamic->Maybe a
fromDynamic::Typeable a =>Dynamic->Maybe a

fromDynR::TypeRep a ->Dynamic-> a -> a
fromDyn::Typeable a =>Dynamic-> a -> a

dynApp::Dynamic->Dynamic->Dynamic-- Existing function; calls error on failuredynApply::Dynamic->Dynamic->MaybeDynamicdataSDynamic s whereSDynamic::TypeRep a -> s a ->SDynamic s

toSDynR::TypeRep a -> s a ->SDynamic s
toSDyn::Typeable a => s a ->SDynamic s
fromSDynamicR::TypeRep a ->SDynamic s ->Maybe(s a)fromSDynamic::Typeable a =>SDynamic s ->Maybe(s a)fromSDynR::TypeRep a ->SDynamic s -> s a -> s a
fromSDyn::Typeable a =>SDynamic s -> s a -> s a
```


Notes

- These is no trusted code here; i.e. no `unsafeCoreces` in the implementation.

- `Dynamic` is *not* abstract so that you can pattern match on it. If it was abstract we'd need to add

  ```wiki
  unpackDynamic :: Dynamic -> (forall a. TypeRep a -> a -> r) -> r
  ```

- `SDynamic` a mid-point between fully dynamic & fully static types.  We statically know some "Shape" information, but not all info about type.  e.g., `SDynamic Maye` contains a value that is definitely a `Maybe ty` for some type `ty`, but the type `ty` can vary between values of type `SDynamic Maybe`.

>
> One use-case is in the implementation of `StaticPtr`.

### Questions

- Do we want to provide `dynApp` which calls `error` instead of returning `Nothing`?
  It seems to be much less used than `dynApply`, and personally I dislike that style.

---

## Data.StaticPtr


Note that the static pointer support requires a static pointer table in a different form to what GHC already supports, and an extension to the static keyword.

TODO api, talk about poly

### Questions

- The static "polymorphism" support is a bit kludgy - any comments on this would be most helpful!

---

## Control.DistributedClosure

TODO