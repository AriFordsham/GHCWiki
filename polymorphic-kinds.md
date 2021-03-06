# Polymorphic Kinds for Haskell


Currently thinking about adding **Polymorphic Kinds** to GHC ... this document is a WIP


This is strongly motivated by wanting to add a new [KindSystem](kind-system), but does not require it.  
See [KindSystem](kind-system) for a discussion of the syntax of sorts (which classify kinds).


## Example: At the term level


```
f :: forall_kind (k :: **). forall (m :: k -> *) (a :: k) . m a -> Int
f _ = 2

data T m = MkT (m Int)

foo = f (Just 2)        -- m = Maybe, a = Int
bar = f (MkT (Just 2))  -- m = T    , a = Maybe
```

## Example: Typeable\[123..\]


With polymorphic kinds, it should be possible to remove the need for
`Typeable[1,2,3,..]` classes from SYB for kinds that arn't `*`.


Because we need a way of talking about the types (and hence their kinds) in the
type classes' functions, we will need a proxy data type:


```
data Proxy :: forall k . k -> *

class Typeable (k :: **) (t :: k) where
  typeOf :: Proxy t -> TypeRep

-- Typeable :: forall (k :: **). k -> Class

-- f :: forall a. a -> Int
-- forall a. f (x::a) = 3???
-- At term level we scope type variables from a separate signature
-- For class decls it's unclear. One possiblity: implicitly bring k into scope, and
-- infer its sort. 

-- When we call f, we write (f 3) not (f Int 3)
-- Similarly we want to write (Typeable Int) not (Typable * Int)

instance Typeable Bool where
  typeOf _ = mkTyCon "Prelude.Bool" []

instance Typeable Maybe where
  typeOf _ = mkTyConApp (mkTyCon "Prelude.Mabe") []

instance Typeable Either where
  typeOf _ = ...

instance (Typeable (t1 :: (* -> *), 
          Typeable (t2 :: *))) => Typeable (t1 t2) where
  typeOf _ = (typeOf (undefined :: Proxy t1)) `mkAppTy` (typeOf (undefined :: Proxy t2))
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
f :: forall k (m :: k -> *) (a :: k) . m a -> Int
f _ = 2
```


In the above example, it is clear\* that k must quantify over kinds as it appears
in the kind signatures.


\*=well...

** Pros: **

- No new syntax

** Cons: **

- More complicated for both users and implementaion logic to work out what's going on

### Option 3: Completely implicit quantified kind variables



In any type signature, find all the free kind variables; bring them into scope; kind-check the type signature; fix the sorts of the kind variables. Entirely local to an explicit, user-written type signature.


```
f :: forall (m :: k -> *) (a :: k) . m a -> Int
```


**Pros:**


- In line with haskell type variables being implicitly quantified


** Cons: **


- This makes it hard to add additional constraints to the k in future (sort annotations, kind classes?)


Just occasionally, a Haskell program *needs* an explicit kind signature, because the defaulting mechanism makes the wrong choice:

```wiki
data T m = MkT    -- m defaults to (m::*)
```


Suppose you really wanted

```wiki
data T (m::*->*) = MkT
```


Question: could the same thing happen at the next level up, so that we want explicit sort signatures?  And if so, does that mean we need explicit binding sites for kind variables? 

## Type Classes



Declaration options:


### Option 1: Implicit kind variables


```
class Bar (a :: k -> *) where                      -- standalone

class Bar (a :: k -> *) => Baz (a :: * -> k) where -- superclass, explicit name (shared)
class Bar a => Baz (a :: * -> k) where             -- superclass, new k?

instance Bang (a :: k -> *) => Bar (a :: k -> *)   -- instance implication, explic
instance Bang a => Bar (a :: k -> *)   -- instance implication, explic

class Foo (a :: k -> *) (b :: k -> *) where        -- MPTC name shared
```

### Option 2: Explicit kind variables



Here reusing `forall` is probably safe, although possibly inconsistent if we go for `forall_kind` (or other) as a term level quantifier...


```
class forall k . Blah (a :: k -> *) where                     -- standalone
class forall k . Baz (a :: k -> *) => Bar (a :: * -> k) where -- superclass, shared k
class forall k . Baz a => Bar (a :: * -> k) where             -- superclass, new k

instance forall k . Bang (a :: k -> *) => Bar (a :: k -> *)   -- instance implication, shared k
instance forall k . Bang a => Bar (a :: k -> *)               -- instance implication, new k

class forall k . Foo (a :: k -> *) (b :: k -> *) where        -- MPTC name shared
```


Again, this doesn't seem to add much now, however if we ever want to constrain the kind variables to particular sorts in the future, we will probably need binders for them.

### Option 3: Explicit kind signatures for type classes


Can't do this in general, as you need to name the variables that index the type class for use in member functions.  Although for type classes with no member functions this may be a viable option.  The question is what is the result of a type class?

```wiki
class Blah :: forall k . (k -> *) -> CLASS where
```