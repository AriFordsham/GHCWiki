## Type-Level Naturals


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


We relate type-level natural numbers to run-time values via a family of singleton types:

```wiki
data Nat (n :: Nat)

nat          :: NatI n => Nat n
natToInteger :: Nat n -> Integer
```


The only "interesting" value of type *Nat n* is the number *n*.  Technically, there is also an undefined element.
The value of a singleton type may be named using *nat*, which is a bit like a "smart" constructor for *Nat n*.
Note that because *nat* is polymorphic, we may have to use a type signature to specify which singleton we mean.  For example:

```wiki
> natToInteger (nat :: Nat 3)
3
```


One may think of the smart constructor *nat* as being a method of a special built-in class, *NatI*:

```wiki
class NatI n where
  nat :: Nat n

instance NatI 0 where nat = "singleton 0 value"
instance NatI 1 where nat = "singleton 1 value"
instance NatI 2 where nat = "singleton 2 value"
etc.
```


The name *NatI* is a mnemonic for the different uses of the class:

- It is the *introduction* construct for 'Nat' values,
- It is an *implicit* parameter of kind 'Nat' (this is discussed in more detail bellow)

## Examples


Here is how we can use the basic primitives to define a `Show` instance for singleton types:

```wiki
instance Show (Nat n) where
  showsPrec p n = showsPrec p (natToInteger n)
```


A more interesting example is to define a function which maps integers into singleton types:

```wiki
integerToMaybeNat :: NatI n => Integer -> Maybe (Nat n)
integerToMaybeNat x = check nat
  where check y = if x == natToInteger y then Just y else Nothing
```


The implementation of `integerToMaybeNat` is a little subtle: by using
the helper function `check`, we ensure that the two occurrences of
`nat` (aka `y`) both have the same type, namely `Nat n`.  There are other
ways to achieve the same, for example, by using scoped type variables,
and providing explicit type signatures.


Now, we can use `integerToNat` to provide a `Read` instance for singleton types:

```wiki
instance NatI n => Read (Nat n) where
  readsPrec p x       = do (x,xs) <- readsPrec p x
                           case integerToMaybeNat x of
                             Just n  -> [(n,xs)]
                             Nothing -> []
```

## Implicit vs. Explicit Parameters


There are two different styles of writing functions which need the integer corresponding to a type level natural.


One approach is to use an explicit parameter of type `Nat n`.  For example:

```wiki
memset :: Storable a => ArrPtr n a -> a -> Nat n -> IO ()
memset = ...
```


This style is, basically, a more typed version of what is found in many standard C libraries.
Callers of this function have to pass the size of the array explicitly, and the type system checks that the size matches that of the array.
When defining `memset` we can just use `natToInteger` on the `Nat n` parameter to get the actual value of the array size.


Another approach is to let the system infer the parameter by using the class `TypeNat`.  For example:

```wiki
memsetAuto :: (Storable a, TypeNat n) => ArrPtr n a -> a -> IO ()
```


In this style, the caller of the function does not need to provide the type of the array explicitly.
Instead, it is computed automatically from the type of the array.
When defining `memsetAuto` we can use `nat`, the method of `TypeNat`, to get access to the value corresponding to the type level natural.


When using the implicit style, it is important that the type of `nat` is specified precisely.  Failing to do so typically results in ambiguity errors
(i.e., GHC does not know which integer it should use).  Another common mistake is to forget that 'nat' is a polymorphic value and so every time it is used it may refer to a different value.


An easy way to avoid such problems is to implement the implicit style functions in terms of the explicit ones.  For example, we can implement `memsetAuto` like this:

```wiki
memsetAuto arr val = memset arr val nat
```

## Type-Level Operations

```wiki
type family m ^ n :: Nat
type family m * n :: Nat
type family m + n :: Nat
class m <= n
```