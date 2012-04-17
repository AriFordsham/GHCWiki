
Consider a type for arrays with a statically known size.

```wiki
newtype ArrPtr (n :: Nat) a = SA (Ptr a)
```

```wiki
data ArrayS :: * -> * where
  ArrS :: Sing n -> ArrPtr n a -> ArrayS a
```

```wiki
data ArrayD :: * -> * where
  ArrD :: SingI n => ArrPtr n a -> ArrayD a
```