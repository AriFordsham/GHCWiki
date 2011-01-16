## Natural Numbers

```wiki
data Natural = forall n . Natural !(Nat n)

data NaturalInteger
  = Negative Natural
  | NonNegative Natural

toNaturalInteger :: Integer -> NaturalInteger

subNatural :: Natural -> Natural -> NaturalInteger
```