# Polymorphic Kinds for Haskell


Currently thinking about adding **Polymorphic Kinds** to GHC...
(Currently very WIP)

## Example: At the term level

```
f:: forall_kind (k ::**). forall (m :: k ->*)(a :: k). m a ->Intf_=2dataT m =MkT(m Int)foo= f (Just2)-- m = Maybe, a = Intbar= f (MkT(Just2))-- m = T    , a = Maybe
```

## Example: Typeable\[123..\]


With polymorphic kinds, it should be possible to remove the need for
`Typeable[1,2,3,..]` classes from SYB for kinds that arn't `*`.


Because we need a way of talking about the types (and hence their kinds) in the
type classes' functions, we will need a proxy data type:

```
dataProxy:: forall k . k ->*classTypeable(k ::**)(t :: k)where
  typeOf ::Proxy t ->TypeRep-- Typeable :: forall (k :: **). k -> Class-- f :: forall a. a -> Int-- forall a. f (x::a) = 3???-- At term level we scope type variables from a separate signature-- For class decls it's unclear. One possiblity: implicitly bring k into scope, and-- infer its sort. -- When we call f, we write (f 3) not (f Int 3)-- Similarly we want to write (Typeable Int) not (Typable * Int)instanceTypeableBoolwhere
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


Declaration options:

### Option 1: Implicit kind variables

```
classBar(a :: k ->*)where-- standaloneclassBar(a :: k ->*)=>Baz(a ::*-> k)where-- superclass, explicit name (shared)classBar a =>Baz(a ::*-> k)where-- superclass, new k?instanceBang(a :: k ->*)=>Bar(a :: k ->*)-- instance implication, explicinstanceBang a =>Bar(a :: k ->*)-- instance implication, explicclassFoo(a :: k ->*)(b :: k ->*)where-- MPTC name shared
```

### Option 2: Explicit kind variables


Here reusing `forall` is probably safe, although possibly inconsistent if we go for `forall_kind` (or other) as a term level quantifier...

```
class forall k .Blah(a :: k ->*)where-- standaloneclass forall k .Baz(a :: k ->*)=>Bar(a ::*-> k)where-- superclass, shared kclass forall k .Baz a =>Bar(a ::*-> k)where-- superclass, new kinstance forall k .Bang(a :: k ->*)=>Bar(a :: k ->*)-- instance implication, shared kinstance forall k .Bang a =>Bar(a :: k ->*)-- instance implication, new kclass forall k .Foo(a :: k ->*)(b :: k ->*)where-- MPTC name shared
```


Again, this doesn't seem to add much now, however if we ever want to constrain the kind variables to particular sorts in the future, we will probably need binders for them.

### Option 3: Explicit kind signatures for type classes


Can't do this in general, as you need to name the variables that index the type class for use in member functions.  Although for type classes with no member functions this may be a viable option.  The question is what is the result of a type class?

```wiki
class Blah :: forall k . (k -> *) -> ??? where
```

## Type functions


Follow as per type classes

## Syntax of Kinds

```wiki
kind ::=  * | # | ? | (kind) | kind -> kind | 
          ty :=: ty | forall var . kind | var
```

## Syntax of Types


Type syntax needs to be extended with a new binder.

## Implementation route


Places that would be hit TODO:

- Parser

  - New Language Flag {-\# [PolymorphicKinds](polymorphic-kinds) \#-} ?
  - Syntax of kinds
  - Possibly syntax of function types

- Type checker

- Core / Core Lint

- Module interfaces

  - To expose kind-quantified variables (does this drop out of any other change)

- Test cases

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