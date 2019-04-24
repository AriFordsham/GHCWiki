# Top level kind signatures – Tricky examples

Top-level kind signatures (TLKSs) are introduced in [GHC Proposal #36](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0036-kind-signatures.rst) as a replacement for CUSKs. However, there are some design questions not covered by the proposal which are discussed here.

See also [GhcKinds/KindInference](ghc-kinds/kind-inference/) for a broader context.

## Local kind signatures

A data declaration can include local kind signatures, which must be unified with the top-level kind signature:

```
type G :: k_top -> Type -> ksig_top
data G (a :: k_local) b :: ksig_local where
```

We must unify `k_top ~ k_local` and `ksig_top ~ ksig_local`.

### Q1: Do we want to support local kind signatures?

Vlad: they are unlikely to be useful in the presence of a TLKS, and the implementation is much easier without supporting them.

Simon/Richard: we want them for consistency with terms. Consider:

```
f :: a -> [a]
f (x::b) = [x] :: [b]
```

Vlad: OK, but the `:: [b]` in this example isn't quite the same thing as `ksig_local`, we should rather be talking about accepting https://github.com/ghc-proposals/ghc-proposals/pull/185 and making a comparison to this:

```
f :: a -> [a]
f (x::b) :: [b] = [x]
```

This implies that comments under that proposal apply to kind signatures as well.

### Q2: Where do we put the coercions that arise from unification?

The unification of `k_top` and `k_local` (via e.g. `unifyKind`) will result in a `Coercion`. What happens to this coercion next, do we discard it, or do we save it in the AST and make some use of it later?

Here is an example:

```hs
type family F a where
  F Bool = Type
type T :: F Bool -> F Bool
data T a = MkT a
```

`T` really comes into scope with the kind `F Bool -> F Bool`. The LHS of the `data` declaration is effectively redundant with a TLKS. RAE proposes we check it for consistency (and bring vars into scope, etc.), but we don't use it in desugaring the declaration. On the other hand, the RHS is relevant. Here, I would expect `MkT :: forall (a :: F Bool). (a |> axF) -> T a`, where `axF` is a coercion derived by unification. We might have to look at the different declaration forms independently and figure out the right answer for each one. (But before doing this, RAE requests further input from others.)

SPJ: yes, we should allow these coercions wherever possible.  I agree with the elaboration of `MkT`.   But there may be a few places where it is really troublesome -- your `k_local` and `ksig_local` example may be one -- because there is no natural place to put the coercion.  In those cases, we should tread carefully; either make it illegal (as you suggest) or require the coercsions to be the identity (which is a bit more friendly).

## Type variables without binders

```
type T :: Type -> Type
data T = MkT Int
```

### Q1: Do we want to accept `T`?

SPJ: no.  Manifestly `T :: Type`, no? How could we even consider accepting it?

Vlad: yes. With GADTs, we can already say this:

```
data T1 :: Type -> Type where
  MkT1 :: Int -> T1 a

data T2 (a :: Type) :: Type where
  MkT2 :: Int -> T2 a
```

Note that `T1` and `T2` have a different amount of binders, but their kinds are the same.

RAE: I object strenuously. Yes, `T1` and `T2` have different numbers of binders, but **their kinds are the same** (emphasis mine). In the `T` example, the kinds are **not** the same. Allowing this strikes me as tantamount to allowing

```hs
f :: Int -> Bool
f = True
```

where we simply assume an ignored argument of type `Int` to `f`. End RAE.

We can rewrite these examples with TLKSs to make this clear:

```
type T1 :: Type -> Type
data T1 where
  MkT1 :: Int -> T1 a

type T2 :: Type -> Type
data T2 a where
  MkT2 :: Int -> T2 a
```
SPJ:  That's quite different to the `data T = MkT Int` decl above (in H98 style).  But even in GADT syntax, I'm not sure we want to accept a data type declaration that (on the face of it) appears to have kind `T1 :: Type`. The kind signature might be far away.  And, absent a TLKS if we say
```
data T1 where
  ...
```
then, regardless of the `...` we'll infer a kind for `T1` ending in `Type`.  Let's be consistent with that.

RAE: Admittedly, with GADT syntax, where the variable names in the head have a very limited scope, this makes a touch more sense. But I still don't like it. End RAE.

Note that `T1` and `T2` already differ only in the amount of binders. Of course, there is no reason to limit this to GADT syntax:

```
type T1 :: Type -> Type
data T1 = MkT1 Int

type T2 :: Type -> Type
data T2 a = MkT2 Int
```

RAE: Do you intend to have `T1` and `T2` mean the same thing? End RAE.

### Q2: Do we ascribe any semantics to the amount of binders?

In other words, is there any difference between `T1` and `T2` at use sites? There appears to be no need to distinguish between them for now.

## Dependent local signatures

```
type Q :: forall k -> k -> Type
data Q j (a :: j)
```

This example demonstrates that we must kind-check type variables in the declaration header left-to-right, extending the environment appropriately (like we do in `bindExplicitTKBndrsX`).

It would be a shame to duplicate this logic, so Simon suggests to reuse this part of `kcLHsQTyVars_NonCusk`:

```
(scoped_kvs, (tc_tvs, res_kind))
           <- bindImplicitTKBndrs_Q_Tv kv_ns            $
              bindExplicitTKBndrs_Q_Tv ctxt_kind hs_tvs $
              thing_inside
```

This results in a list of `TyVarTv` given by `scoped_kvs ++ tc_tvs`, which we could immediately zip and unify with `splitPiTys` of the TLKS.

However, this plan fails to account for dependent higher rank kinds. Richard gives an example:

```
type W :: forall (a :: forall k. k -> Type) -> a Int -> a Maybe -> Type
data W x (y :: x Int) (z :: x Maybe)
```

`x` must have a polykind from the start because we instantiate `k ~ Type` in `x Int` and `k ~ (Type -> Type)` in `x Maybe`.

## Scoped kind variables

```
type T :: forall k. k-> Type
data T a = MkT (Proxy (a :: k))
```

Question: Do we allow the use of `k` in `a :: k` here, even though it is not mentioned in the declaration header?

Vlad and RAE: yes, if `-XScopedTypeVariables` are enabled then the TLKS brings `k` into scope. (Despite the proposal saying otherwise, it needs to be amended).

Even trickier, does `-XScopedTypeVariables` bring into scope type variables that aren't in TLKSs? The following is possible at the term level, for instance:

```hs
-- f :: [a -> Either a ()]
f = [Left @a :: forall a. a -> Either a ()]
```

Does this suggest that this should work?

```hs
-- type F :: [a -> Either a ()]
type F = '[Left @a :: forall a. Either a ()]
```

Vlad and RAE: yes.

## Matchability and arity inference

#16571 arose from this example:

```
type D :: forall j. j -> Type
type D = Ap TypeRep
```

### Q1: Do we allow `D`?

Currently, allow `D2` but disallow `D3`:

```
type D2 = (Ap TypeRep :: j -> Type)
type D3 = (Ap TypeRep :: forall j. j -> Type)
```

Reason for allowing `D2`: we can generalize by putting `j` on the LHS of `D2`:

```
type D2 @j = (Ap TypeRep :: j -> Type)
```

This means `D2` gets arity=1.

Reason for disallowing `D3`: we cannot generalize to `/\j. Ap (TypeRep @j)`. If we could, `D3` would have arity=0.

Arity determines matchability. Assuming `forall j :. blah` is syntax for a matchable forall, we are actually looking for this:

```
type D2 :: forall j :. j -> Type
type D2 = Ap TypeRep
```

Vlad: as a stopgap solution, we could indicate the arity explicitly by `@`-binders:

```
type D2 :: forall j. j -> Type
type D2 @j = Ap TypeRep
```

This assigns arity=1 to `D2` because of the `@j` binder on the LHS, which might as well be `@_` as it is unused on the RHS.


### Q2: When do we want to generalize over implicit variables?

Consider this type family:

```
type family F :: k where
  F = Int
  F = Maybe
```

At first glance it seems we have duplicate clauses, but in reality we generalize over `k` and the definition is equivalent to:

```
type family F @k :: k where
  F @Type = Int
  F @(Type -> Type) = Maybe
```

This behavior may lead to unintuitive behavior at use sites:

1. Getting stuck instead of producing a kind error
2. Being unusable at higher rank kinds

#### Getting stuck instead of producing a kind error

The first type of unintuitive behavior can be demonstrated with `F2`:

```
type family F2 a :: k where
  F2 Int = Maybe
  F2 Bool = Either

f :: F2 Int -> F2 Int
```

Naively, one would expect `F2 Int` to reduce to `Maybe`, and `f :: Maybe -> Maybe` to result in a kind error. Instead `F2 Int` gets stuck. This happens because we generalize over `k`:

```
type family F2 @k a :: k where
  F2 @(Type -> Type) Int = Maybe
  F2 @(Type -> Type -> Type) Bool = Either

f :: F2 @Type Int -> F2 @Type Int
```

Sure enough, `F2 @Type Int` has no defining clause.

On one hand, these examples suggest we shouldn't add implicit variables on the LHS. On the other hand, the following example looks reasonable because `k` is mentioned syntactically on the LHS:

```
type family F3 (a :: k) where
  F3 Maybe = Int
  F3 Either = Bool
```

Here we generalize over `k` and it does not lead to any unintuitive results:

```
type family F3 @k (a :: k) where
  F3 @(Type -> Type) Maybe = Int
  F3 @(Type -> Type -> Type) Either = Bool
```

However, a syntactic check might not be sufficient because of non-injective type families:

```
type family F4 (a :: G k) :: k where
```

In `F4`, `k` is mentioned syntactically on the LHS, but it is an argument to a non-injective type family `G`, so this definition is likely to exhibit unintuitive behavior at use sites if we generalize over `k`.

#### Being unusable at higher rank kinds

The second type of unintuitive behavior is inability to use type families and type synonyms in higher rank positions.

Let us define a class with a higher rank parameter and a few synonyms for `Proxy`:

```
class HR (x :: forall k. k -> Type) where ...

type ProxySyn1 (a :: k) = Proxy (a :: k)
type ProxySyn2 = (Proxy :: forall j. j -> Type)
type ProxySyn3 = Proxy
```

* `HR Proxy` is well-kinded. 
* `HR ProxySyn1` is ill-kinded because type synonyms may not be used unsaturated.
* `HR ProxySyn2` is well-kinded (arity=0)
* `HR ProxySyn3` is ill-kinded. Why?

The reason `HR ProxySyn3` fails is that by default we instantiate and generalize over implicit type variables:

```
type ProxySyn3 @k = Proxy @k
```

This means `ProxySyn3` gets arity=1 and `HR ProxySyn3` is an unsaturated use of `ProxySyn3`.

RAE: This is a mess! (But it's great having all the examples in one place.) I am at a loss as to how to clean this up at the moment.