# Support for generic programming


GHC includes a new (in 2010) mechanism to let you write generic functions.  It is described in paper [ A generic deriving mechanism for Haskell](http://www.dreixel.net/research/pdf/gdmh_nocolor.pdf). This page sketches the specifics of the implementation; we assume you have read the paper. The [ HaskellWiki page](http://www.haskell.org/haskellwiki/Generics) gives a more general overview.


This mechanism replaces the [previous generic classes implementation](http://www.haskell.org/ghc/docs/6.12.2/html/users_guide/generic-classes.html). What we describe until the "Kind polymorphic overhaul" section is implemented and released in GHC 7.0.1.

## Main components

- `TcDeriv.tcDeriving` now allows deriving `Generic` instances.

- The representation types and core functionality of the library live on `GHC.Generics` (on the `ghc-prim` package).

- Many names have been added as known in `prelude/PrelNames`

- Most of the code generation is handled by `types/Generics`

## Things that have been removed

- All of the [generic classes stuff](http://www.haskell.org/ghc/docs/6.12.2/html/users_guide/generic-classes.html). In particular, the following have been removed:

  - `hasGenerics` field from `TyCon`;
  - `HsNumTy` constructor from `HsType`;
  - `TypePat` constructor from `Pat`.

- The `-XGenerics` flag is now deprecated.

## What already works

- `Generic` instances can be derived when `-XDeriveGeneric` is enabled.

- The `default` keyword can used for generic default method signatures when `-XDefaultSignatures` is enabled.

- Generic defaults are properly instantiated when giving an instance without defining the generic default method.

- Base types like `[]`, `Maybe`, tuples, come with Generic instances.

## To be done

- Derive `Generic1` instances

## Testing

- Tests are available under the `generics` directory of the testsuite.

# Kind polymorphic overhaul


With the new `-XPolyKinds` functionality we can make the support for generic programming better typed. The basic idea is to define the universe codes (`M1`, `:+:`, etc.) as constructors of a datatype. Promotion then lifts these constructors to types, which we can use as before, only that now we have them all classified under a new kind. The overhaul of the main module is explained below; for easier comparison with the current approach, names are kept the same whenever possible.

## Generic representation universe

`m` is the only real parameter here. `f` and `x` are there because we
can't write kinds directly, since `Universe` is also a datatype (even if
we're only interested in its promoted version). So we pass `f` and `x`
only to set them to `* -> *` and `*`, respectively, in `Interprt`.
`m` is different: it stands for the kind of metadata representation types,
and we really want to be polymorphic over that, since each user datatype
will introduce a new metadata kind.

```wiki
data Universe f x m = 
  -- Void (used for datatypes without constructors)
    VV
    
  -- Unit
  | UU
  
  -- The parameter
  | PAR
  
  -- Recursion into a type of kind * -> *
  | REC f
  
  -- Constants (either other parameters or recursion into types of kind *)
  | KK Constant x
  
  -- Metadata
  | MM MetaData m (Universe f x m)
  
  -- Sum, product, composition
  | Universe f x m :++: Universe f x m
  | Universe f x m :**: Universe f x m
  | f :..: Universe f x m
  -- Note that we always compose a concrete type on the left (like []) with
  -- a generic representation on the right

infixr 5 :++:
infixr 6 :**:
infixr 6 :*:
infixr 7 :..:

-- Some shortcuts
data MetaData = CC | DD | SS
data Constant = PP | RR

data ConstantV (c :: Constant) where
  P :: ConstantV PP
  R :: ConstantV RR
  
data MetaDataV (m :: MetaData) where
  C :: MetaDataV CC
  D :: MetaDataV DD
  S :: MetaDataV SS
```

## Universe interpretation


As promised, we set `f` to `* -> *` and `x` to `*`.
Unfortunately we don't have [explicit kind variable annotations](ghc-kinds#explicit-kind-variables)
yet, so we cannot leave `m` polymorphic! So this code doesn't compile:

```wiki
data Interprt :: Universe (* -> *) * m -> * -> * where

  -- No interpretation for VV, as it shouldn't map to any value
  
  -- Unit
  U1     :: Interprt UU p
  
  -- The parameter
  Par1   :: p -> Interprt PAR p
  
  -- Recursion into a type of kind * -> *
  Rec1   :: r p -> Interprt (REC r) p
  
  -- Constants
  K1     :: x -> Interprt (KK c x) p
  -- Constants shortcuts
  Par0   :: x -> Interprt (KK PP x) p
  Rec0   :: x -> Interprt (KK RR x) p
  
  -- Metadata
  M1     :: Interprt x p -> Interprt (MM m c x) p
  -- Metadata shortcuts
  D1     :: Interprt x p -> Interprt (MM DD c x) p
  C1     :: Interprt x p -> Interprt (MM CC c x) p
  S1     :: Interprt x p -> Interprt (MM SS c x) p
  
  -- Sum, product, and composition
  L1     :: Interprt a r -> Interprt (a :++: b) r
  R1     :: Interprt b r -> Interprt (a :++: b) r
  (:*:)  :: Interprt a r -> Interprt b r -> Interprt (a :**: b) r
  Comp1  :: f (Interprt g r) -> Interprt (f :..: g) r
```

### Names


As an aside, note that we have to come up with names like `UU` and `KK` for the `Universe`
even though we really just wanted to use `U1` and `K1`, like before. Then we would have
a type and a constructor with the same name, but that's ok. However, `Universe` defines
both a type (with constructors) and a kind (with types). So if we were to use `U1` in the
`Universe` constructors, then we could no longer use that name in the `Interprt`
constructors. It's a bit annoying, because we are never really interested in the type
`Universe` and its constructors: we're only interested in its promoted variant.
This is a slight annoyance of automatic promotion: when you define a "singleton type"
(like our GADT `Interprt` for `Universe`) you cannot reuse the constructor names.

## Metadata representation

```wiki
data Proxy d = Proxy -- kind polymorphic

-- Meta data classes
class Datatype d where -- kind polymorphic
  -- The name of the datatype, fully qualified
  datatypeName :: Proxy d -> String
```


There's more of these, but they don't add any new concerns.

## Conversion between user datatypes and generic representation


We now get a more precise kind for `Rep`:

```wiki
-- Representable types of kind *
class Generic a where
  type Rep a :: Universe (* -> *) * m
  from :: a -> Interprt (Rep a) x
  to   :: Interprt (Rep a) x -> a
  
-- Representable types of kind * -> *
class Generic1 (f :: * -> *) where
  type Rep1 f :: Universe (* -> *) * m
  from1  :: f a -> Interprt (Rep1 f) a
  to1    :: Interprt (Rep1 f) a -> f a
```

## Example generic function: `fmap` (kind `* -> *`)


User-visible class, exported:

```wiki
class Functor (f :: * -> *) where
  fmap :: (a -> b) -> f a -> f b
  default fmap :: (Generic1 f, GFunctor (Rep1 f)) => (a -> b) -> f a -> f b
  fmap f = to1 . gfmap f . from1  
```


Defined by the generic programmer, not exported:

```wiki
class GFunctor (f :: Universe (* -> *) * m) where
  gfmap :: (a -> b) -> Interprt f a -> Interprt f b
  
instance GFunctor UU where
  gfmap _ U1 = U1
  
instance GFunctor PAR where
  gfmap f (Par1 a) = Par1 (f a)

instance GFunctor (KK i c) where
  gfmap _ (K1 a) = K1 a

instance (Functor f) => GFunctor (REC f) where
  gfmap f (Rec1 a) = Rec1 (fmap f a)

instance (GFunctor f) => GFunctor (MM m c f) where
  gfmap f (M1 a) = M1 (gfmap f a)

instance (GFunctor f, GFunctor g) => GFunctor (f :++: g) where
  gfmap f (L1 a) = L1 (gfmap f a)
  gfmap f (R1 a) = R1 (gfmap f a)

instance (GFunctor f, GFunctor g) => GFunctor (f :**: g) where
  gfmap f (a :*: b) = gfmap f a :*: gfmap f b

instance (Functor f, GFunctor g) => GFunctor (f :..: g) where
  gfmap f (Comp1 x) = Comp1 (fmap (gfmap f) x)
```


Note that previously `Functor` and `GFunctor` had exactly the same types.
Now we can make clear what the difference between them is.
  

## Example generic function: `show` (kind `*`, uses metadata)


User-visible class, exported:

```wiki
class Show (a :: *) where
  show :: a -> String
  default show :: (Generic a, GShow (Rep a)) => a -> String
  show = gshow . from
```


  
Defined by the generic programmer, not exported:

```wiki
class GShow (f :: Universe (* -> *) * m) where
  gshow :: Interprt f x -> String
  
instance GShow UU where
  gshow U1 = ""
  
instance (P.Show c) => GShow (KK i c) where
  gshow (K1 a) = P.show a
  
instance (Datatype c, GShow f) => GShow (MM DD c f) where
  gshow (M1 x) = datatypeName (Proxy :: Proxy c) ++ " " ++ gshow x
```


The other cases do not add any further complexity.
  
  

## Example datatype encoding: lists (derived by the compiler)

```wiki
instance Generic [a] where
  type Rep [a] = MM DD DList 
                   (MM CC DList_Nil UU :++: 
                    MM CC DList_Cons (KK PP a :**: KK RR [a]))

  from [] = D1 (L1 (C1 U1))
  from (h:t) = D1 (R1 (C1 (Par0 h :*: Rec0 t)))
  to (D1 (L1 (C1 U1))) = []
  to (D1 (R1 (C1 (Par0 h :*: Rec0 t)))) = h:t
  
-- Metadata
data List_Meta = DList | DList_Nil | DList_Cons
```


Note that we use only one datatype; more correct would be to use 3, one for
`DList`, another for the constructors, and yet another for the selectors
(or maybe even n datatypes for the selectors, one for each constructor?)
But we don't do that because `Universe` is polymorphic only over `m`, so
a single metadata representation type. If we want a more fine-grained
distinction then we would need more parameters in `Universe`, and also to
split the `MM` case.

```wiki
instance Datatype DList where datatypeName _ = "[]"
```

### Digression


Even better would be to index the metadata representation types over
the type they refer to. Something like:

```wiki
  data family MetaTypes a -- kind polymorphic
  data instance MetaTypes [] = DList | DList_Nil | DList_Cons
```


But now we are basically asking for promotion of data families, since we want
to use promoted `DList`. Also, the case for `MM` in `Universe` would then
be something like:

```wiki
  | MM MetaData (MetaTypes m) (Universe f x m)
```


But I'm not entirely sure about this.
