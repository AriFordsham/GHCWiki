
There is a new kind, `Nat`.  It is completely separate from GHC's hierarchy of sub-kinds, so `Nat` is only a sub-kind of itself.


The inhabitants of `Nat` are an infinite family of (empty) types, corresponding to the natural numbers:

```wiki
0, 1, 2, ... :: Nat
```


These types are linked to the value world by a small library with the following API:

```wiki
module GHC.TypeNats where

data Nat (n :: Nat)                                     -- Abstract "singleton" types.

natToInteger :: Nat n -> Integer                        -- Convert a singleton to an integer.
integerToNat :: Integer -> (forall n. Nat n -> a) -> a  -- Convert an integer into a singleton.

class TypeNat n where
  nat :: Nat n                                          -- A value in a "singleton" type.

 instance TypeNat 0
 instance TypeNat 1
 instance TypeNat 2
 ...

-- property:  natToInteger (nat :: Nat n) == n
```