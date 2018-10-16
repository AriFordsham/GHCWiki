# Kind inference examples


This page is intended to collect examples of tricky cases for kind inference. Any proposed algorithm should be applied to each of these cases to see how it would behave.


More discussion is at [GhcKinds/KindInference](ghc-kinds/kind-inference).

## Associated types

```
classC1(a :: k)wheretypeF a
```


Question: What should the visibilities on `F` be?


Ryan and Richard think `F :: forall k. k -> Type`. That is, `k` is Specified, because we can always order implicit kind variables using the same ordering that appears in the class header (after kind inference).

```
classC2(a :: k)(b :: k2)wheretypeT a
```


Proposed: `T :: forall k. k -> Type`, with no mention of `b` or `k2`.

```
classC3(a :: k)(b :: k2) hwere
  typeT(z :: k3) a
```


Proposed: `T :: forall k k3. k3 -> k -> Type`. This puts `k`*before*`k3`, because class variables come before other ones (unless the user explicitly puts them later, as has been done with `a`). This rule always works because class variables cannot depend on local ones.

```
classC4 a (b :: a)wheretypeT b a
```


This must be rejected, as `b` depends on `a`.

```
classC5(a :: k)wheretypeT(a :: k2)
```


Reject: `k` and `k2` are distinct skolems.

```
classC6 a (b :: a)(c ::Proxy b)wheretypeT(x ::Proxy'(a, c))
```


Proposed: `T :: forall (a :: Type) (b :: a) (c :: Proxy b). Proxy '(a, c) -> Type`. Note that `b` is included here as a Specified variable. It could also be an Inferred, if we prefer.

```
classC7 a (b :: a)(c ::Proxy b)wheretypeT a c
```


Proposed: `T :: forall (a :: Type) -> forall (b :: a). Proxy b -> Type`. We've inserted `b` between `a` and `c`, but `b` is Specified, not Required. Other possibilities: make `b` Inferred, or reject altogether.

## Datatypes, dependency, and polymorphic recursion


Assume

```
dataProx k (a :: k)
```

```
dataProx2 k a =MkP2(Prox k a)
```


Question: Do we allow `k` to be dependently quantified, even if this is not lexically apparent from the declaration? This is rejected today.

```
dataS2 k (a :: k) b =MkS(S2 k b a)
```


Proposed: `S2 :: forall k -> k -> k -> Type`. Note that `a` and `b` are inferred to have the same kind, as that avoid polymorphic recursion.

```
dataS3 k (a :: k) b =MkS(S3Type b a)
```


Proposed: reject as polymorphically recursive. Yet the idea in [GhcKinds/KindInference\#Simonssuggestion](ghc-kinds/kind-inference#simon's-suggestion) accepts this.

```
dataQ2 k a whereMkQ2::Prox k a ->Q2 k a

dataQ3 k a whereMkQ3::Q3 k a ->Prox k a ->Q3 k a

dataQ4 k a whereMkQ4::Q4BoolFalse->Prox k a ->Q4 k a

dataQ5 k a whereMkQ5::Q5BoolFalse->Q5Nat3->Prox k a ->Q5 k a
```


Agda accepts all of the above. It puts us to shame!

```
dataProxy2 a whereMk1::Proxy2(a :: k)Mk2::Proxy2(b :: j)
```


This should really be accepted. But it's challenging to arrange this, because `a`, `k`, `b`, and `j` all scope locally within their constructors. How can the kind of `Proxy2` unify with any of them?

```
dataT a whereMk:: forall k1 k2 (a :: k1)(b :: k2).T b ->T a
```


This is polymorphically recursive. Yet hGhcKinds/KindInference\#SimonsProposedSolution accepts it. (That's what's implemented in GHC 8.6.) Richard thinks we should reject.

```
dataT2 a whereMk:: forall (a :: k).T2Maybe->T2 a
```


This one is rejected, as it should be. So we don't accept *all* polymorphic recursion (how could we?). But we don't have a good specification for what we do accept and what we don't.

```
dataT3 a b whereMk::T3 b a ->T3 a b
```


This should be accepted with `T3 :: forall k. k -> k -> Type`; it's not polymorphically recursive. Yet, it would seem any specification which accepted `T` would also give `T3` the polymorphically recursive kind `forall k1 k2. k1 -> k2 -> Type`.

## Dependency ordering


What if we do something simple? Like just use lexical ordering.

```
dataProxy(a :: k)
```


Then this example fails, with `k` after `a`.


Refinement: consider the RHS of `::` before the LHS.


Then this one fails:

```
dataT4 a (b :: k)(x ::SameKind a b)
```


The `k` would end up between the `a` and the `b`, even though `a` depends on `k`.


Also, consider

```
dataT5 a (c ::Proxy b)(d ::Proxy a)(x ::SameKind b d)
```


Here, `b` needs to be between `a` and `x` somewhere. But where? Currently (GHC 8.6), this is rejected because implicitly declared variables come before explicit ones.

## And more


There are many more examples in the testsuite, of course. In particular, see the T14066 tests and the BadTelescope tests.
