# Naming issues in type-level programming modules


This page summarizes current type-level programming constructs in modules that ship with GHC. At the bottom is a list of open questions about good names for these operations.

## Module `Data.Type.Equality`

```wiki
-- Reified equality
data a :~: b where
  Refl :: a :~: a

sym :: (a :~: b) -> (b :~: a)
trans :: (a :~: b) -> (b :~: c) -> (a :~: c)
castWith :: (a :~: b) -> a -> b
liftEq :: (a :~: b) -> (f a :~: f b)
liftEq2 :: (a :~: a') -> (b :~: b') -> (f a b :~: f a' b')
liftEq3 :: (a :~: a') -> (b :~: b') -> (c :~: c') -> (f a b c :~: f a' b' c')
liftEq4 :: (a :~: a') -> (b :~: b') -> (c :~: c') -> (d :~: d')
        -> (f a b c d :~: f a' b' c' d')
lower :: (f a :~: f b) -> a :~: b

class EqualityT f where
  equalsT :: f a -> f b -> Maybe (a :~: b)

type family a == b where
  a == a = True
  a == b = False
```

## Module `Data.Type.Coercion`

```wiki
-- Reified representational equality
data Coercion a b where
  Coercion :: Coercible a b => Coercion a b

coerceWith :: Coercion a b -> a -> b
sym :: forall a b. Coercion a b -> Coercion b a
trans :: Coercion a b -> Coercion b c -> Coercion a c

class CoercionT f where
  coercionT :: f a -> f b -> Maybe (Coercion a b)
```

## Module `Data.Proxy`

```wiki
data Proxy t = Proxy
data KProxy (t :: *) = KProxy

asProxyTypeOf :: a -> Proxy a -> a
asProxyTypeOf = const
```

## Module `Data.Typeable`

```wiki
cast :: forall a b. (Typeable a, Typeable b) => a -> Maybe b
eqT :: forall a b. (Typeable a, Typeable b) => Maybe (a :~: b)
gcast :: forall a b c. (Typeable a, Typeable b) => c a -> Maybe (c b)
gcast1 :: forall c t t' a. (Typeable t, Typeable t')
       => c (t a) -> Maybe (c (t' a)) 
gcast2 :: forall c t t' a b. (Typeable t, Typeable t')
       => c (t a b) -> Maybe (c (t' a b)) 
```

## Module `GHC.TypeLits`

```wiki
data Nat
data Symbol

class KnownNat (n :: Nat) where
  natSing :: SNat n

class KnownSymbol (n :: Symbol) where
  symbolSing :: SSymbol n

natVal :: forall n proxy. KnownNat n => proxy n -> Integer
symbolVal :: forall n proxy. KnownSymbol n => proxy n -> String

data SomeNat    = forall n. KnownNat n    => SomeNat    (Proxy n)
data SomeSymbol = forall n. KnownSymbol n => SomeSymbol (Proxy n)

someNatVal :: Integer -> Maybe SomeNat  -- partial because of negative Ints
someSymbolVal :: String -> SomeSymbol

infix  4 <=?, <=
infixl 6 +, -
infixl 7 *
infixr 8 ^

type x <= y = (x <=? y) ~ True    -- Note: in Constraint
type family (m :: Nat) <=? (n :: Nat) :: Bool
type family (m :: Nat) + (n :: Nat) :: Nat
type family (m :: Nat) * (n :: Nat) :: Nat
type family (m :: Nat) ^ (n :: Nat) :: Nat
type family (m :: Nat) - (n :: Nat) :: Nat
```

## Module `GHC.Prim`

```wiki
class Coercible a b where
  coerce :: a -> b
```

# Issues


These are in no particular order, but they are numbered for easy reference.

1. `EqualityT` and `CoercionT` sound like monad transformers. I (Richard) have actually been confused by this at one point.

1. Should `KProxy`'s data constructor be named `KProxy`? Reusing the name here requires users to use a `'` every time they want to use the promoted data constructor `KProxy`.

1. There are up to three different ways type-level predicates can be defined: as Constraints, as GADTs wrapping constraints, or as type families returning `Bool`s. Is there a common naming convention among these? Right now, we have `(~)` (constraint) vs `(:~:)` (GADT); `(<=?)` (Boolean-valued) vs. `(<=)` (constraint); and `Coercible` (constraint) vs. `Coercion` (GADT).

1. `eqT` (from `Data.Typeable`) and `equalsT` (from `Data.Type.Equality`) have similar names, achieve similar functions, but are subtly different. This may or may not be confusing.

1. I (Richard) want to add the following function to `Data.Type.Equality`:

  ```wiki
  gcastWith :: (a :~: b) -> ((a ~ b) => r) -> r
  gcastWith Refl x = x
  ```

>
>
> I've tested this function in a real setting, and it (that is, type inference for it) works great.
>
>

