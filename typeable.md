# Safer and more expressive type representations


This page summarises a proposed re-design (again!) of the `Typeable` class, to better support static values.  It should be read in conjunction with the [root page for distributed Haskell](distributed-haskell), which was a major driving force for the design described here.


The names of functions and type constructors is totally up for grabs.

## Goal


Consider `Dynamic`:

```wiki
data Dynamic where
  Dyn :: TypeRep -> a -> Dynamic
```


We'd like to write `dynApply`:

```wiki
dynApply :: Dynamic -> Dynamic -> Maybe Dynamic
```


which succeeds only when the application is type correct. But how?  Let's try

```wiki
dynApply (Dyn tf f) (Dyn tx x) = ???
```


We need some way to decompose `tf` (the type representation for `f`), ask if it is an arrow type, and if so extract the type representation for the argument and result type.  Then we need to compare the argument type with `xt`'s representation. If so, we are good to go. 


But at the moment we don't have any *type-safe* way to decompose `TypeRep`.  Indeed `dynApply` is defined like this right now:

```wiki
dynApply (Dyn tf f) (Dyn tx x)
  = case funResultTy tf tx of
       Just tr -> Just (Dyn tr (unsafeCoerce f x))
```


where `funResultTy :: TypeRep -> TypeRep -> Maybe TypeRep` does the decomposition of `tf`, checking that it is of form `tx -> tr`. 


The `unsafeCoerce` makes `dynApply` part of the Trusted Code Base (TCB).  That might not be so bad, but it is a poster-child for a raft of other similar functions (e.g. in Cloud Haskell). 


So **our main goal is to be able to write `dynApply` in a type-safe way** so that it is not part of the TCB.

---

## Step 1: Type-indexed type representations


The first obvious thing is that we must make a connection between the type-rep arg of `Dyn` and the value itself. Something like this:

```wiki
data Dynamic where
  Dyn :: TTypeRep a -> a -> Dynamic
```


Here we are using a *type-indexed* type representation, `TTypeRep`.  Now the connection betweeen the two is visible to the type system.


We can re-define the `Typeable` class and `TypeRep` thus:

```wiki
class Typeable a where
  tTypeRep :: TTypeRep a     -- No need for a proxy argument!

data TypeRep where
  TypeRep :: TTypeRep a -> TypeRep

typeRep :: Typeable a => proxy a -> TypeRep
-- The current typeRep function
typeRep (p :: proxy a) = TypeRep (tTypeRep :: TTypeRep a)
```


It is helpful to have both `Typeable a` (the class) and `TTypeRep a` (the value).

- It is sometimes convenient to pass a `TTypeRep` around implicitly (via a `Typeable a =>` constraint).  
- But in tricky cases, it is also often much clearer (and less laden with proxy arguments) to pass it around as an ordinary, named value.  


We can get from `Typeable a` to `TTypeRep` by using the class method `tTypeRep`.  But what about the other way round? 
We need to add the following primitive: 

```wiki
withTypeable :: TTypeRep a -> (Typeable a => b) -> b
```


(This seems both simpler and more useful than making the Typeable class recursive through TypeRep data declaration.)


We can also compare two `TTypeReps` to give a statically-usable proof of equality:

```wiki
eqTT :: TTypeRep a -> TTypeRep b -> Maybe (a :~: b)
eqT  :: (Typeable a, Typeable b) => Maybe (a :~: b)

data a :~: b where  -- Defined in Data.Typeable.Equality
   Refl :: a :~: a
```


(`eqT` and `:~:` exist already as part of the exports of `Data.Typeable`.) 

---

## Step 2: Decomposing functions


If we want to decompose `TTypable`, at least in the function arrow case, we need a function like this:

```wiki
decomposeFun :: TTypeRep fun
             -> r
             -> (forall arg res. (fun ~ (arg->res)) 
                              => TypeRep arg -> TTypeRep res -> r)
             -> r
-- (decomposeFun tf def k) sees if 'tf' is a function type
-- If so, it applies 'k' to the argument and result type
-- representations; if not, it returns 'def'
```


This function is part of `Typeable`, and replaces `funResultTy`.


Now we can write `dynApply`, in a completely type-safe way, outside the TCB:

```wiki
dynApply :: Dynamic -> Dynamic -> Maybe Dynamic
dynApply (Dyn tf f) (Dyn tx x)
  = decomposeFun tf Nothing $ \ ta tr ->
    case eqTT ta tx of
      Nothing   -> Nothing
      Just Refl -> Dyn tr (f x)
```

**Pattern synonyms**.  An alternative, rather nicer interface for `decomoposeFun` would use a [pattern synonym](pattern-synonyms) instead of continuation-passing style.  Here is the signature for the pattern synonym:

```wiki
pattern type TRFun :: TTypeRep arg 
                   -> TTypeRep res 
                   -> TypeRep (TTypeRep (arg->res))
```


which looks (by design) very like the signature for a GADT data constructor.  Now we can use `TRFun` in a pattern, thus:

```wiki
dynApply :: Dynamic -> Dynamic -> Maybe Dynamic
dynApply (Dyn (TRFun ta tr) f) (Dyn tx x)
  = case eqTT ta tx of
      Nothing   -> Nothing
      Just Refl -> Dyn tr (f x)
dynApply _ _ = Nothing
```


Is that not beautiful?  The second equation for `dynApply` is needed in case the `TRFun` pattern does not match.

---

## Step 3: Decomposing arbitrary types


It is all very well being able to decompose functions, but what about decomposing other types, like `Maybe Int`?  


To do this it is natural to regard types as built from type constructors and binary application, like this:

```wiki
data TTypeRep (a :: k) :: * where
    TRApp :: TTypeRep a -> TTypeRep b -> TTypeRep (a b)
    TRCon :: TTyCon a -> TTypeRep a

-- TTyCon must be polykinded, so that we can have
--    (TTyCon Maybe), (TTyCon Either), (TTyCon Int) etc
-- TTyCon :: forall k. k -> *
```


(We could, and ultimately should, use pattern synonyms again, but it's more concrete to use a GADT for now.  Perhaps surprisingly, it is actually fine to expose `TTypeRep` concretely, with its constructors; we can't construct ill-formed `TTypeRep`s.)


While this GADT is expressible in GHC now (note the existential kind in TRApp), **it is not very useful without kind equalities**.   (GHC does not currently support kind equalities, but Richard Eisenberg is working that very question.)  Why?  Here is the type of `TRApp` in its full glory, with normally-invisible kind args in angle brackets:

```wiki
TRApp :: forall k1 k2. forall (a :: k1 -> k2) (b :: k1).
         TTypeRep <k1->k2> a -> TTypeRep <k2> b -> TTypeRep <k2> (a b)
```


Or, to be really explicit about the existentials:

```wiki
TRApp :: forall k2 (c:k2).                      -- Universal
         forall k1 (a :: k1 -> k2) (b :: k1).   -- Existential
         (c ~ a b)
      => TTypeRep <k1->k2> a 
      -> TTypeRep <k2>     b 
      -> TTypeRep <k2>     c
```


Now suppose we want to implement `decomposeFun`.  We should be able to do this outside the TCB, i.e. without `unsafeCoerce`:

```wiki
arrowCon :: TTyCon (->)   -- The type rep for (->)

decomposeFun :: TTypeRep fun
             -> r
             -> (forall arg res. (fun ~ (arg->res)) 
                              => TypeRep arg -> TTypeRep res -> r)
             -> r
decomposeFun tr def kont
  = case tr of
      TRApp (TRApp (TRCon c) r1) r2 -> case eqTyCon arrowCon c of 
          Just HRefl -> kont r1 r2
          Nothing    -> def
      _ -> default 
```


But look at the arguments of `eqTyCon`:

```wiki
   arrowCon :: TTyCon <*->*->*>   (->)
   c        :: TTyCon <k1->k2->*> tc
```


where `k1` and `k2` are existential kinds bound by the two nested `TRApp` constructors, and `tc` the existential bound by the inner `TRApp`.  But `kont` is expecting `arg` and `res` to have kind `*`!  So we need proofs that `k1 ~ *` and `k2 ~ *`.


The real work is done by `eqTyCon`:

```wiki
eqTyCon :: forall (k1 k2 :: *). 
           forall (a :: k1) (b :: k2). 
           TTyCon <k1> a -> TTyCon <k2> b -> Maybe (a :~~: b)
```


where `:~~:` is a *kind-heterogeneous* version of `:~:`:

```wiki
data (a::k1) :~~: (b::k2) where
  HRefl :: forall (a::k).  a :~~: a
```


Or, to write the type of `HRefl` with its constraints explicit:

```wiki
HRefl :: forall k1 k2. forall (a::k1) (b::k2).
         (k1 ~ k2, a ~ b) 
      => a :~~: b
```


That is, `HRefl` encapsulates a proof of kind equality as well as one of type equality.  


So Step 3 (allowing `TypeRep` to be fully decomposed in a type safe way) absolutely requires kind equalities.

---

## Other points

### Fast comparison of `TypeRep`


The current implementation of `TypeRep` allows constant-type comparison based on fingerprints.  To support this in the new scheme we would want to add a fingerprint to every `TypeRep` node. But we would not want clients to see those fingerprints, lest they forge them.


Conclusion: make `TypeRep` abstract.  But then how can we pattern-match on it?  Pattern synonyms seem to be exactly what we need.

### Trusted computing base


With all of this, what is left in the TCB?  The `TyCon` type is a new abstract type, and the comparison (based on fingerprints) must be in the TCB.

```wiki
TTyCon a  --- abstract type
eqTyCon :: forall k1 k2. forall (a :: k1) (b :: k2). 
           TTyCon a -> TTyCon b -> Maybe (a :~~: b)
```

`withTypeable` could be written in core (and perhaps could be generalized to other constraints) but not in source Haskell:

```wiki
withTypeable :: TypeRep a -> (Typeable a => b) -> b
```

### What about structure information?


Related but different issue: [ https://ghc.haskell.org/trac/ghc/ticket/7897](https://ghc.haskell.org/trac/ghc/ticket/7897)

---

## Step by step


We can do steps 1 and 2 without kind equalities, although the 
implementation of `decomposeFun` will use `unsafeCoerce` and will be part of the TCB.

## Proposed API

```wiki
data TTypeRep (a :: k)        -- abstract
data TTyCon (a :: k)          -- abstract

-- Costructors
trCon :: forall k (a :: k). TTyCon a -> TTypeRep a  -- in TCB
trApp :: forall k k1 (a :: k1 -> k) (b :: k1). 
         TTypeRep a -> TTypeRep b -> TTypeRep (a b)  -- in TCB

-- Destructors
pattern type TRCon :: forall k (a :: k). TyCon a -> TTypeRep a
pattern type TRApp :: forall k. exists k1 (a :: k1 -> k) (b :: k1). 
                      TTypeRep a -> TTypeRep b -> TTypeRep (a b)
pattern type TRFun :: TTypeRep arg 
                   -> TTypeRep res 
                   -> TypeRep (TTypeRep (arg->res))


-- For now, homogeneous equalities
eqTyCon :: forall k (a :: k) (b :: k). 
           TTyCon a -> TTyCon b -> Maybe (a :~: b)
eqTT    :: forall k (a :: k) (b :: k). 
           TTypeRep a ->T TypeRep b -> Maybe (a :~: b)
```


When we get kind equalities, we can generalise `eqTyCon` and `eqTT`:

```wiki
data (a :: k1) :~~: (b :: k2) where
   HRefl :: a :~~: a

eqTyCon :: forall k1 k2 (a :: k1) (b :: k2). 
           TTyCon a -> TTyCon b -> Maybe (a :~~: b)
eqT :: forall k1 k2 (a :: k1) (b :: k2). 
       TTypeRep a -> TTypeRep b -> Maybe (a :~~: b)
```

**SLPJ**: Do we need both `:~:` and `:~~:`?


Some of this design is motivated by the desire to allow flexibility in the implementation to allow for fingerprinting for fast equality comparisons. Naturally, the fingerprints have to be in the TCB. If it weren't for them, though, the `TypeRep` type could be exported concretely.


Could we simplify this a bit by removing `TyCon`?
