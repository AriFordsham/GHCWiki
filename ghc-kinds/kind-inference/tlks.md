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

## Type variables without binders

```
type T :: Type -> Type
data T = MkT Int
```

### Q1: Do we want to accept `T`?

Vlad: yes. With GADTs, we can already say this:

```
data T1 :: Type -> Type where
  MkT1 :: Int -> T1 a

data T2 (a :: Type) :: Type where
  MkT2 :: Int -> T2 a
```

Note that `T1` and `T2` have a different amount of binders, but their kinds are the same.

We can rewrite these examples with TLKSs to make this clear:

```
type T1 :: Type -> Type
data T1 where
  MkT1 :: Int -> T1 a

type T2 :: Type -> Type
data T2 a where
  MkT2 :: Int -> T2 a
```

Note that `T1` and `T2` already differ only in the amount of binders. Of course, there is no reason to limit this to GADT syntax:

```
type T1 :: Type -> Type
data T1 = MkT1 Int

type T2 :: Type -> Type
data T2 a = MkT2 Int
```

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

`x` must be generalized right away because we instantiate `k ~ Type` in `x Int` and `k ~ (Type -> Type)` in `x Maybe`.

## Scoped kind variables

```
type T :: forall k. k-> Type
data T a = MkT (Proxy (a :: k))
```

Question: Do we allow the use of `k` in `a :: k` here, even though it is not mentioned in the declaration header?

Vlad: yes, if `-XScopedTypeVariables` are enabled then the TLKS brings `k` into scope. (Despite the proposal saying otherwise, it needs to be amended).

## Matchability and arity inference

#16571 arose from this example:

```
type D :: forall j. j -> Type
type D = Ap TypeRep
```

Do we allow `D`? Currently, allow `D2` but disallow `D3`:

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