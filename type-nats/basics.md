
There is a new kind, `Nat`.  It is completely separate from GHC's hierarchy of sub-kinds, so `Nat` is only a sub-kind of itself.


The inhabitants of `Nat` are an infinite family of (empty) types, corresponding to the natural numbers:

```wiki
0, 1, 2, ... :: Nat
```


These types are linked to the value world by a small library with the following API:

```wiki
module GHC.TypeNats where
```

## Singleton Types

```wiki
data Nat n

class TypeNat n where
  nat :: Nat n

natToInteger :: Nat n -> Integer
```

## Type-Level Operations

```wiki
type family m ^ n :: Nat
type family m * n :: Nat
type family m + n :: Nat
class m <= n
```

## Natural Numbers

```wiki
data Natural = forall n . Natural !(Nat n)

data NaturalInteger
  = Negative Natural
  | NonNegative Natural

toNaturalInteger :: Integer -> NaturalInteger

subNatural :: Natural -> Natural -> NaturalInteger
```