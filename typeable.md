## How can we have safer and more expressive type representations?


Our goal is some polykinded type 

```wiki
TypeRep a  
```


so that casting is safe

```wiki
gcast :: forall k. forall (a :: k). forall (b :: k). TypeRep a -> TypeRep b -> Maybe (a :~: b)
```


and that is also safely decomposable, i.e. we need some analogue of 

```wiki
splitTyConApp :: TypeRep -> (TyCon, [TypeRep])
```


for `TypeRep a` so that we can figure out whether a given type is an arrow type, product type, etc.  


This capability is necessary to implement `dynApply` for Typeable based dynamic typing.

```wiki
data Dynamic = forall a. Typeable a => Dyn a

dynApply :: Dynamic -> Dynamic -> Dynamic
```

### Kind equalities are sufficient


Consider the following GADT

```wiki
{-# LANGUAGE PolyKinds, DataKinds, EmptyDataDecls, GADTs  #-}
data TypeRep (a :: k) :: * where
    TRApp :: TypeRep a -> TypeRep b -> TypeRep (a b)
    TRCon :: TyCon a -> TypeRep a
```


While this GADT is expressible in GHC now (note the existential kind in TRApp), it is not very useful without kind equalities. 
Pattern matching TypeRep would produce a TyCon with unknown kind but any cast function we could write, i.e. 

```wiki
eqTyCon :: forall (a b :: k). TyCon a -> TyCon b -> Maybe (a :~: b)
```


must require the two arguments to have the same kind. 


If we have kind equalities available, we could produce evidence that the kinds are equal  too. We need a kind-indexed GADT 
to package that evidence up in a heterogenous equality type.

```wiki
data JMeq (a :: k1) (b :: k2) where
    JMRefl :: JMeq a a
```


With this GADT, we can generalize the type of eqTyCon

```wiki
eqTyCon :: forall (k1 k2 :: *). forall (a :: k1). forall (b :: k2). TyCon a -> TyCon b -> Maybe (JMeq a b)
```


For example, imagine if we had this representation available

```wiki
arrowCon :: TyCon (->)
```


and wanted to implement the following function:

```wiki
ifArrow :: forall (a d :: *).  TypeRep a -> (forall b c . TypeRep b -> TypeRep c -> d) -> d -> d
```


This implementation could be:

```wiki
ifArrow tr f default = case tr of 
   TRApp (TRApp c r1) r2 -> case eqTyCon c arrowCon of 
       Just JMRefl -> f r1 r2
       Nothing -> default
   _ -> default 
```


Note that with the first version of `eqTyCon` this code would not type check.


This interface of `ifArrow` is just what we need for `dynApply`.

```wiki
dynApply :: Dynamic -> Dynamic -> Dynamic
dynApply (Dyn a) (Dyn (b :: b)) = 
  ifArrow a (\ dom rng -> gcast dom (typeRep :: TypeRep b) of 
        Just Refl -> Dyn (a b)
        Nothing  -> default)  default where default =  Dyn (error "can't apply")
```


Alternatively, we could give a different interface for decomposing arrow types with the addition of a new data structure.

```wiki
data ArrowTy where 
    Arrow :: forall (a b :: *). TypeRep a -> TypeRep b -> ArrowTy

ifArrow :: TypeRep a -> Maybe ArrowTy

```

### What can we do without kind equalities ?

- Make `ifArrow` and other destructors primitive

### What about the Typeable class?


This class should be a wrapper for the runtime type representation.

```wiki
class Typeable a where
    typeRep :: TypeRep a
```


We'll need to add the following primitive to make sure that we always 
have access to the Typeable class, even if we produce the TypeRep by pattern matching.  

```wiki
withTypeable :: TypeRep a -> (Typeable a => b) -> b
```


(This seems both simpler and more useful than making the Typeable class recursive through TypeRep data declaration.)

### Trusted computing base


The `TyCon` type is a new abstract type, and the comparison (based on fingerprints) must be in the TCB.

```wiki
TyCon a  --- abstract type
eqTyCon :: forall (k1 k2 :: *). forall (a :: k1). forall (b :: k2). TyCon a -> TyCon b -> Maybe (JMeq a b)
```


This could be written in core (and perhaps could be generalized to other constraints) but not in source Haskell:

```wiki
withTypeable :: TypeRep a -> (Typeable a => b) -> b
```

### What about structure information?


Related but different issue: [ https://ghc.haskell.org/trac/ghc/ticket/7897](https://ghc.haskell.org/trac/ghc/ticket/7897)

## Proposed API

```wiki
data TypeRep (a :: k)        -- abstract
data TyCon (a :: k)          -- abstract

pattern TyCon :: forall k (a :: k). TyCon a -> TypeRep a
pattern TyApp :: forall k. exists k1 (a :: k1 -> k) (b :: k1). TypeRep a -> TypeRep b -> TypeRep (a b)

tyCon :: forall k (a :: k). TyCon a -> TypeRep a   -- in TCB
apply :: forall k k1 (a :: k1 -> k) (b :: k1). TypeRep a -> TypeRep b -> TypeRep (a b)  -- in TCB

-- data (a :: k1) :~~: (b :: k2) where
--   HRefl :: a :~~: a

-- eqTyCon :: forall k1 k2 (a :: k1) (b :: k2). TyCon a -> TyCon b -> Maybe (a :~~: b)
-- eqT :: forall k1 k2 (a :: k1) (b :: k2). TypeRep a -> TypeRep b -> Maybe (a :~~: b)

eqTyCon :: forall k (a :: k) (b :: k). TyCon a -> TyCon b -> Maybe (a :~: b)
eqT :: forall k (a :: k) (b :: k). TypeRep a -> TypeRep b -> Maybe (a :~: b)

ifArrow :: forall (a :: *) (d :: *). TypeRep a -> (forall (b :: *) (c :: *). TypeRep b -> TypeRep c -> (a :~: (b -> c)) -> d) -> d
```


The commented out bit in the middle is what would be used if we had kind equalities. Sadly, we don't yet.


Some of this design is motivated by the desire to allow flexibility in the implementation to allow for fingerprinting for fast equality comparisons. Naturally, the fingerprints have to be in the TCB. If it weren't for them, though, the `TypeRep` type could be exported concretely.


Could we simplify this a bit by removing `TyCon`?
