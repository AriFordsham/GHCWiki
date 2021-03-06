
The module `GHC.TypeLits` provides two views on values of type `TNat`,
which make it possible to define inductive functions using `TNat` values.

## Checking for Zero (Unary Strucutre of Nat)


The first view provides the same functionality as the usual
Peano arithmetic definition of the natural numbers.  It
is useful when using `TNat` to count something.

```wiki
isZero :: Sing n -> IsZero n

data IsZero :: Nat -> * where
  IsZero ::              IsZero 0
  IsSucc :: !(Sing n) -> IsZero (n + 1)
```


By using `isZero` we can check if a number is 0 or the successor
of another number.  The interesting aspect of `isZero` is that
the result is typed:  if `isZero x` returns `IsSucc y`,
then the type checker knows that the type of `y` is one smaller
than `x`.

## Checking for Evenness (Binary Structure of Nat)


The other view provides a more "bit-oriented" view of
the natural numbers, by allowing us to check if the least
significant bit of a number is 0 or 1.  It is useful
when we use `TNat` values for splitting things
in half:

```wiki
isEven :: Sing n -> IsEven n

data IsEven a :: Nat -> * where
  IsEvenZero ::                  IsEven 0
  IsEven     :: !(Sing (n+1)) -> IsEven (2 * n + 2)
  IsOdd      :: !(Sing n)     -> IsEven (2 * n + 1)
```