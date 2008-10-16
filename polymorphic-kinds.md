# Polymorphic Kinds for Haskell


Currently thinking about adding **Polymorphic Kinds** to GHC...
(Currently very WIPish)

## Example: At the term level

```
f:: forall_kind k . forall (m :: k ->*)(a :: k). m a ->Intf_=2dataT m =MkT(m Int)foo= f (Just2)-- m = Maybe, a = Intbar= f (MkT(Just2))-- m = T    , a = Maybe
```

## Example: Typeable\[123..\]


With polymorphic kinds, it should be possible to remove the need for
`Typeable[1,2,3,..]` classes from SYB for kinds that arn't `*`.


Because we need a way of talking about the types (and hence their kinds) in the
type classes' functions, we will need a proxy data type:

```
dataProxy:: forall k . k ->*forall_kind k .classTypeable(t :: k)where
  typeOf ::Proxy t ->TypeRepinstanceTypeableBoolwhere
  typeOf _= mkTyCon "Prelude.Bool"[]instanceTypeableMaybewhere
  typeOf _= mkTyConApp (mkTyCon "Prelude.Mabe")[]instanceTypeableEitherwhere
  typeOf _=...instance(Typeable(t1 ::(*->*),Typeable(t2 ::*)))=>Typeable(t1 t2)where
  typeOf _=(typeOf (undefined :: t1))`mkAppTy`(typeOf (undefined :: t2))
```

## Functions Quantifying over Kinds


For function signatures, we need a way of quantifying over kinds.  Options:

### Option 1: Add forall_kind (or equivt.) notation


As in the example above, functions would need a new quantifier to explicitly
mark when a new kind is being quantified over.  


If it were to take the form  `forall_kind vars .` then it shouldn't interact
with existing forms.

### Option 2: Use forall and infer kind variables from usage

```
f:: forall k (m :: k ->*)(a :: k). m a ->Intf_=2
```


In the above example, it is clear\* that k must quantify over kinds as it appears
in the kind signatures.


\*=well...

** Pros: **

- No new syntax

** Cons: **

- More complicated for both users and implementaion logic to work out what's going on

### Option 3: Completely implicit quantified kind variables

```
f:: forall (m :: k ->*)(a :: k). m a ->Int
```

**Pros:**

- In line with haskell type variables being implicitly quantified

** Cons: **

- This makes it hard to add additional constraints to the k in future (sort annotations, kind classes?)

- A typo with a rank-n kind could be very confusing, e.g.

```
  f :: forall (m :: k ->(forall k . k' ->*))
```

## Type Classes

```
classBar(a :: k ->*)where-- standaloneclassBar(a :: k ->*)=>Baz(a ::*-> k)where-- superclass, explicit name (shared)classBar a =>Baz(a ::*-> k)where-- superclass, implicit or new name?instanceBang(a :: k ->*)=>Bar(a :: k ->*)-- instance implication, explic
```

```
class forall k .Blah(a :: k ->*)where
```

```
class forall k .Baz(a :: k ->*)=>Bar(a ::*-> k)where
```

## Syntax of Kinds

```wiki
kind ::=  * | # | ? | (kind) | kind -> kind | 
          ty :=: ty | forall var . kind | var
```

## Syntax of Types


Type syntax need to be extended with a new binder TODO

## Type Classes

TODO

## To classify


Other 'issues' (probably non-issues).  Kinds in rank-n types?
foobar :: forall k1 (b :: k1) (c :: k1 -\> \*) . (forall k2 (e :: k2 -\> \*) (f :: k) . e f -\> Int ) -\> b c -\> Int


Impredicativity

```
dataProxy:: forall k . k ->*foo:: forall (m :: forall k . k ->*). m Int-> m Maybe->Int-- This is okbar= foo ProxyProxy
```

```
dataFoo:: forall k . k ->*foo:: forall (m ::(forall k . k ->*)->*). m Proxy->Int-- This line is ok-- has a higher-- ranked kind, but-- that's not an-- issue as we-- have to be-- explicitbar= foo Foo-- This is impredicative (and rejected) as it requires instantiating -- Foo's k to (forall k .  k -> *)
```