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
