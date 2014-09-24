# Infer DatatypeContexts


Haskell prime has decided to [ depricate DatatypeContexts](https://ghc.haskell.org/trac/haskell-prime/wiki/NoDatatypeContexts).


Running Example:

```wiki
data (Eq e) => HasEq e = HasEq e
```


In haskell98, 2010 these are not typically usable as you might expect

```wiki
eq :: HasEq e -> HasEq e -> Bool
eq (HasEq e) (HasEq e') = e == e'
```


Would give a missing context error, so contexts always had to be specified.

```wiki
eq :: (Eq e) => HasEq e -> HasEq e -> Bool
eq (HasEq e) (HasEq e') = e == e'
```

## GADT alternative


An alternative in terms of GADTs that works as expected has been suggested.

```wiki
data HasEq e where
    HasEq :: (Eq e) => e -> HasEq e

eq :: HasEq e -> HasEq e -> Bool
eq (HasEq e) (HasEq e') = e == e'
```

### Problems with GADT alternative

- For performance sensitive types this might be problematic since they
  are always stored in a box that could be bottom and that needs to be checked
  at runtime.

- If you have a num container,

  ```wiki
  data IsNum n where
      IsNum :: (Num n) => n -> IsNum n
  ```

  you can not write the type of the following expression without stating the type class

  ```wiki
  IsNum $ fromInteger 1 :: (Num n) => IsNum n
  ```

## Uses