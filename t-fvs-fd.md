# Type Families (TF) vs Functional Dependencies (FD)


I've gathered here some comments from the web and some of my personal
experiences concerening the advantages and disadvantages of TF and FD
over one another.

## Advantages of Functional Dependencies

### Mutual dependencies


TF are one way type functions, but a class can have multiple FD such as in

[ http://okmij.org/ftp/Haskell/PeanoArithm.lhs](http://okmij.org/ftp/Haskell/PeanoArithm.lhs)


The `Sum` class can be used to both add and substract.

```wiki
class Sum a b c | a b -> c, a c -> b, b c -> a
```

### TF are too strict


For example, you cannot recurse with `If`

```wiki
type family If p a b
type instance If HTrue a b = a
type instance If HFalse a b = b

type family Gcd a b
type instance Gcd a b = If (IsZero b) a (Gcd b (Rem a b))
```


The type family has to use a helper TF (or in this case a plain type):

```wiki
type Gcd a b = Gcd_helper (IsZero b) a b
type family Gcd_helper p a b
type instance Gcd_helper HTrue a b = a
type instance Gcd_helper HFalse a b = Gcd b (Rem a b)
```


More complicated type-level programs get very ugly using this style.


The same `If` works in a class instance using constraint kinds.

```wiki
class Gcd a b c | a b -> c
instance (
  IsZero b p,
  If p
    (b ~ c)
    (Rem a b r,
     Gcd b r c))
  => Gcd a b c
```

### Partial application


It is possible to partially apply class predicates using constraint kinds.

```wiki
class F a b | a -> b
instance F Int Char

class FMap (f :: * -> * -> Constraint) a b | f a -> b
instance (f a b) => FMap f (HJust a) (HJust b)

x = undefined :: FMap F (HJust Int) a => a
```


But type families cannot be partially applied (`Type synonym F' should have 1 argument, but has been given none`):

```wiki
type family FMap (f :: * -> *) a
type instance FMap f (HJust a) = HJust (f a)

type family F a
type instance F Int = Char

x = undefined :: FMap F (HJust Int)
```


The standard encoding of the above class from FD to
superclass equalities does not work because type instances
can't have type variables on the RHS which weren't introduced
in the LHS:

```wiki
class (f :<$>: a) ~ b =>  FMap (f :: * -> * -> Constraint) a b
  where type f :<$>: a
instance f a b => FMap f (HJust a) (HJust b)
  where type f :<$>: (HJust a) = HJust b -- error, b not available
```

### No overlapping type instances

#### Identical alternatives

`IsZero` for two's complement integers is longer than it could be.

```wiki
type family IsZero i
type instance IsZero Zeros = True
-- type instance IsZero i  = False
type instance IsZero Ones  = False
type instance IsZero Zero  = False
type instance IsZero One   = False
```

`MonadState` cannot be implemented easily without overlap. It works with FD.

```wiki
class Monad m => MonadState s m | s -> m where
  get :: m s
  put :: s -> m ()

instance (Monad m) => MonadState s (StateT s m) where
  get = StateT $ \s -> return (s, s)

instance (Monad (t m), MonadTrans t, MonadState s m) =>
    MonadState s (t m) where
  get = lift get
  put = lift . put
```


But not with TF.

```wiki
class (Monad m) => MonadState m where
  type MonadStateType m
  get :: m (MonadStateType m)
  put :: (MonadStateType m) -> m ()

instance (Monad m) => MonadState (StateT s m) where
  type MonadStateType (StateT s m) = s
  get = StateT $ \s -> return (s, s)
  put s = StateT $ \_ -> return ((), s)

instance (Monad (t m), MonadTrans t, MonadState m) =>
    MonadState (t m) where
  type MonadStateType (t m) = MonadStateType m
  get = lift get
  put = lift . put
```

#### Nat


The new Nat kind contains an unlimited amount of types. Overlapping
type instances are needed to write total type instances that don't use
the built-in solver.

```wiki
type family F (a :: Nat)
type instance F 0 = ...
type instance F 1 = ...
type instance F 2 = ...
...
-- type instance F n = ...
```

#### `TypeEq`


This does not work (`Conflicting family instance declarations`).

```wiki
type family TypeEq a b
type instance TypeEq a a = HTrue
type instance TypeEq a b = HFalse
```


But this does.

```wiki
class TypeEq a b p | a b -> p
instance TypeEq a a HTrue
instance false ~ HFalse => TypeEq a b false
```

#### See Also

- [ http://okmij.org/ftp/Haskell/typeEQ.html](http://okmij.org/ftp/Haskell/typeEQ.html)
- [NewAxioms](new-axioms)
- [TypeFunctions/TotalFamilies](type-functions/total-families)

### See Also

- Injective type families ([\#6018](https://gitlab.haskell.org//ghc/ghc/issues/6018))
- TF overlap check is limited ([\#4259](https://gitlab.haskell.org//ghc/ghc/issues/4259))

## Advantages of Type Families

### GADTs and existential types


From Manuel M T Chakravarty on Sun Feb 15 22:02:48 EST 2009

[ http://www.haskell.org/pipermail/haskell-cafe/2009-February/055890.html](http://www.haskell.org/pipermail/haskell-cafe/2009-February/055890.html)

```wiki
* GADTs:
   - GADTs and FDs generally can't be mixed (well, you
     can mix them, and when a program compiles, it is
     type correct, but a lot of type correct programs
     will not compile)

   - GADTs and TFs work together just fine

* Existential types:
   - Don't work properly with FDs; here is an example:

       class F a r | a -> r
       instance F Bool Int

       data T a = forall b. F a b => MkT b

       add :: T Bool -> T Bool -> T Bool
       add (MkT x) (MkT y) = MkT (x + y)  -- TYPE ERROR

   - Work fine with TFs; here the same example with TFs

       type family F a
       type instance F Bool = Int

       data T a = MkT (F a)

       add :: T Bool -> T Bool -> T Bool
       add (MkT x) (MkT y) = MkT (x + y)

     (Well, strictly speaking, we don't even need an
     existential here, but more complicated examples are fine,
     too.)
```

### Type-level programming with TF is generally nicer

- Type-level programming with classes looks like an ugly Prolog,
  while similar code using TF looks more functional and has no free
  variables.
- The class instance being defined is unhelpfully placed after the
  constraints.

```wiki
class Minus (a :: k) (b :: k) (dif :: k) | a b -> dif
instance (
  Negate b nb,
  (a + nb) dif)
  => Minus a b dif
```

```wiki
type family (a :: k) `Minus` (b :: k) :: k
type instance a `Minus` b = a + Negate b
```

### FD are not real FD, they only help pick instances

#### The result of a class constraint is not obvious.

`C a b` could have the FD `a -> b`, `b -> a`, neither, both or even someting like
`-> a b`. Looking at the `:info` doesn't always help to figure out the
resulting FD of a class. The kind of a class does not include the
FD. There is also no FD inference.

```wiki
>>> class C a b | a -> b
>>> class C a b => C' a b
>>> :i C'
class C a b => C' a b
>>> :k C
C :: * -> * -> Constraint
```


With constraint kinds, a constraint can be an open type family. There is no
way at all of telling what its FD are.

#### `UndecidableInstances` allows instances that violate the FD ([\#1241](https://gitlab.haskell.org//ghc/ghc/issues/1241), [\#2247](https://gitlab.haskell.org//ghc/ghc/issues/2247))

```wiki
class F a b | a -> b where f :: (a,b)
instance F Int b
```


Allows F Int Bool and F Int Char.

#### FD don't provide evidence ([\#4894](https://gitlab.haskell.org//ghc/ghc/issues/4894))

```wiki
class F a b | a -> b

f :: (F a b, F a c) => a -> b -> c
f _ = id
```


Gives the error

```wiki
Could not deduce (b ~ c)
```

#### See also

- FD superclass variables need to be in scope ([\#714](https://gitlab.haskell.org//ghc/ghc/issues/714), [\#3490](https://gitlab.haskell.org//ghc/ghc/issues/3490))

## Converting from FD to TF


The standard encoding of FD using TF is:


With FD

```wiki
class C a b | a -> b
```


With TF

```wiki
class (F a ~ b) => C a b where
  type F a
```


Examples:

- [ http://www.haskell.org/pipermail/haskell-cafe/2012-June/101576.html](http://www.haskell.org/pipermail/haskell-cafe/2012-June/101576.html)
- [\#1614](https://gitlab.haskell.org//ghc/ghc/issues/1614)
- [\#816](https://gitlab.haskell.org//ghc/ghc/issues/816)

## See Also

- [ ReadingList\#TypeEqualities](ReadingList#TypeEqualities)
- [ https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/jfp06.pdf](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/jfp06.pdf)
- [TypeFunctions](type-functions)
- [TypeFunctionsStatus](type-functions-status)
- [TypeFunctions/IntegratedSolver](type-functions/integrated-solver)
- [ http://www.haskell.org/haskellwiki/GHC/Indexed_types](http://www.haskell.org/haskellwiki/GHC/Indexed_types)
- [ http://blog.omega-prime.co.uk/?p=127](http://blog.omega-prime.co.uk/?p=127)
- [ http://www.haskell.org/pipermail/haskell-cafe/2010-July/080043.html](http://www.haskell.org/pipermail/haskell-cafe/2010-July/080043.html)
- [ http://www.haskell.org/pipermail/haskell-prime/2011-June/003423.html](http://www.haskell.org/pipermail/haskell-prime/2011-June/003423.html)
- [ http://code.atnnn.com/projects/type-prelude/wiki](http://code.atnnn.com/projects/type-prelude/wiki)
- [ http://www.haskell.org/haskellwiki/Functional_dependencies_vs._type_families](http://www.haskell.org/haskellwiki/Functional_dependencies_vs._type_families)