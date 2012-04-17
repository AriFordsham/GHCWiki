
Consider a type for arrays with a statically known size.

```wiki
newtype StaticArray (n :: Nat) a = SA (Ptr a)
```

```wiki
data ArrayS :: * -> * where
  ArrS :: Sing n -> StaticArray n a -> ArrayS a
```

```wiki
data ArrayD :: * -> * where
  ArrD :: SingI n => StaticArray n a -> ArrayD a
```