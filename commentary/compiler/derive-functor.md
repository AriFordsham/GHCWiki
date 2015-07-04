# Support for deriving `Functor`, `Foldable`, and `Traversable` instances


GHC 6.12.1 introduces an extension to the `deriving` mechanism allowing for automatic derivation of `Functor`, `Foldable`, and `Traversable` instances using the `DeriveFunctor`, `DeriveFoldable`, and `DeriveTraversable` extensions, respectively. Twan van Laarhoven [ first proposed this feature](https://mail.haskell.org/pipermail/haskell-prime/2007-March/002137.html) in 2007, and [ opened a related GHC Trac ticket](https://ghc.haskell.org/trac/ghc/ticket/2953) in 2009.

## Example

```
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}dataExample a =Ex a Char(Example a)(ExampleChar)deriving(Functor,Foldable,Traversable)
```


The derived code would look something like this:

```
instanceFunctorExamplewhere
    fmap f (Ex a1 a2 a3 a4)=Ex(f a1) a2 (fmap f a3) a4

instanceFoldableExamplewhere
    foldr f z (Ex a1 a2 a3 a4)= f a1 (foldr f z a3)
    foldMap f (Ex a1 a2 a3 a4)= mappend (f a1)(mappend mempty (mappend (foldMap f a3) mempty))instanceTraversableExamplewhere
    traverse f (Ex a1 a2 a3 a4)=Ex<$>(f a)<*> pure a2 <*> traverse f a3 <*> pure a4
```

## Algorithm description

`DeriveFunctor`, `DeriveFoldable`, and `DeriveTraversable` all operate using the same underlying mechanism. GHC inspects the arguments of each constructor and derives some operation to perform on each argument, which depends of the type of the argument itself. In a `Functor` instance, for example `fmap` would be applied to occurrences of the last type parameter, but `id` would be applied to other type parameters. Typically, there are five cases to consider. (Suppose we have a data type `data A a = ...`.)

1. Terms whose type does not mention `a`
1. Terms whose type mentions `a`
1. Occurrences of `a`
1. Tuple values
1. Function values


After this is done, the new terms are combined in some way. For instance, `Functor` instances combine terms in a derived `fmap` definition by applying the appropriate constructor to all terms, whereas in `Foldable` instances, a derived `foldMap` definition would `mappend` the terms together.

### `DeriveFunctor`


A comment in [ TcGenDeriv.hs](http://git.haskell.org/ghc.git/blob/9f968e97a0de9c2509da00f6337b612dd72a0389:/compiler/typecheck/TcGenDeriv.hs#l1476) lays out the basic structure of `DeriveFunctor`, which derives an implementation for `fmap`.

```wiki
For the data type:

  data T a = T1 Int a | T2 (T a)

We generate the instance:

  instance Functor T where
      fmap f (T1 b1 a) = T1 b1 (f a)
      fmap f (T2 ta)   = T2 (fmap f ta)

Notice that we don't simply apply 'fmap' to the constructor arguments.
Rather
  - Do nothing to an argument whose type doesn't mention 'a'
  - Apply 'f' to an argument of type 'a'
  - Apply 'fmap f' to other arguments
That's why we have to recurse deeply into the constructor argument types,
rather than just one level, as we typically do.

What about types with more than one type parameter?  In general, we only
derive Functor for the last position:

  data S a b = S1 [b] | S2 (a, T a b)
  instance Functor (S a) where
    fmap f (S1 bs)    = S1 (fmap f bs)
    fmap f (S2 (p,q)) = S2 (a, fmap f q)

However, we have special cases for
         - tuples
         - functions

More formally, we write the derivation of fmap code over type variable
'a for type 'b as ($fmap 'a 'b).  In this general notation the derived
instance for T is:

  instance Functor T where
      fmap f (T1 x1 x2) = T1 ($(fmap 'a 'b1) x1) ($(fmap 'a 'a) x2)
      fmap f (T2 x1)    = T2 ($(fmap 'a '(T a)) x1)

  $(fmap 'a 'b)          =  \x -> x     -- when b does not contain a
  $(fmap 'a 'a)          =  f
  $(fmap 'a '(b1,b2))    =  \x -> case x of (x1,x2) -> ($(fmap 'a 'b1) x1, $(fmap 'a 'b2) x2)
  $(fmap 'a '(T b1 b2))  =  fmap $(fmap 'a 'b2)   -- when a only occurs in the last parameter, b2
  $(fmap 'a '(b -> c))   =  \x b -> $(fmap 'a' 'c) (x ($(cofmap 'a 'b) b))

For functions, the type parameter 'a can occur in a contravariant position,
which means we need to derive a function like:

  cofmap :: (a -> b) -> (f b -> f a)

This is pretty much the same as $fmap, only without the $(cofmap 'a 'a) case:

  $(cofmap 'a 'b)          =  \x -> x     -- when b does not contain a
  $(cofmap 'a 'a)          =  error "type variable in contravariant position"
  $(cofmap 'a '(b1,b2))    =  \x -> case x of (x1,x2) -> ($(cofmap 'a 'b1) x1, $(cofmap 'a 'b2) x2)
  $(cofmap 'a '[b])        =  map $(cofmap 'a 'b)
  $(cofmap 'a '(T b1 b2))  =  fmap $(cofmap 'a 'b2)   -- when a only occurs in the last parameter, b2
  $(cofmap 'a '(b -> c))   =  \x b -> $(cofmap 'a' 'c) (x ($(fmap 'a 'c) b))
```

`DeriveFunctor` is special in that it can recurse into function types, whereas `DeriveFoldable` and `DeriveTraversable` cannot (see the section on covariant and contravariant positions).

### `DeriveFoldable`


Another comment in [ TcGenDeriv.hs](http://git.haskell.org/ghc.git/blob/9f968e97a0de9c2509da00f6337b612dd72a0389:/compiler/typecheck/TcGenDeriv.hs#l1725) reveals the underlying mechanism behind `DeriveFoldable`:

```wiki
Deriving Foldable instances works the same way as Functor instances,
only Foldable instances are not possible for function types at all.
Here the derived instance for the type T above is:

  instance Foldable T where
      foldr f z (T1 x1 x2 x3) = $(foldr 'a 'b1) x1 ( $(foldr 'a 'a) x2 ( $(foldr 'a 'b2) x3 z ) )

The cases are:

  $(foldr 'a 'b)         =  \x z -> z     -- when b does not contain a
  $(foldr 'a 'a)         =  f
  $(foldr 'a '(b1,b2))   =  \x z -> case x of (x1,x2) -> $(foldr 'a 'b1) x1 ( $(foldr 'a 'b2) x2 z )
  $(foldr 'a '(T b1 b2)) =  \x z -> foldr $(foldr 'a 'b2) z x  -- when a only occurs in the last parameter, b2

Note that the arguments to the real foldr function are the wrong way around,
since (f :: a -> b -> b), while (foldr f :: b -> t a -> b).
```


In addition to `foldr`, `DeriveFoldable` also generates a definition for `foldMap` as of GHC 7.8.1 (addressing [ \#7436](https://ghc.haskell.org/trac/ghc/ticket/7436)). The pseudo-definition for `$(foldMap)` would look something like this:

```wiki
  $(foldMap 'a 'b)         = \x -> mempty     -- when b does not contain a
  $(foldMap 'a 'a)         = f
  $(foldMap 'a '(b1,b2))   = \x -> case x of (x1, x2) -> mappend ($(foldMap 'a 'b1) x1) ($(foldMap 'a 'b2) x2)
  $(foldMap 'a '(T b1 b2)) = \x -> foldMap $(foldMap 'a 'b2) x -- when a only occurs in the last parameter, b2
```

### `DeriveTraversable`


From [ TcGenDeriv.hs](http://git.haskell.org/ghc.git/blob/9f968e97a0de9c2509da00f6337b612dd72a0389:/compiler/typecheck/TcGenDeriv.hs#l1800):

```wiki
Again, Traversable is much like Functor and Foldable.

The cases are:

  $(traverse 'a 'b)          =  pure     -- when b does not contain a
  $(traverse 'a 'a)          =  f
  $(traverse 'a '(b1,b2))    =  \x -> case x of (x1,x2) -> (,) <$> $(traverse 'a 'b1) x1 <*> $(traverse 'a 'b2) x2
  $(traverse 'a '(T b1 b2))  =  traverse $(traverse 'a 'b2)  -- when a only occurs in the last parameter, b2

Note that the generated code is not as efficient as it could be. For instance:

  data T a = T Int a  deriving Traversable

gives the function: traverse f (T x y) = T <$> pure x <*> f y
instead of:         traverse f (T x y) = T x <$> f y
```

### Covariant and contravariant positions


One challenge of deriving `Functor` instances for arbitrary data types is handling function types. To illustrate this, note that these all can have derived `Functor` instances:

```
dataCovFun1 a =CovFun1(Int-> a)dataCovFun2 a =CovFun2((a ->Int)-> a)dataCovFun3 a =CovFun3(((Int-> a)->Int)-> a)
```


but none of these can:

```
dataContraFun1 a =ContraFun1(a ->Int)dataContraFun2 a =ContraFun2((Int-> a)->Int)dataContraFun3 a =ContraFun3(((a ->Int)-> a)->Int)
```


In `CovFun1`, `CovFun2`, and `CovFun3`, all occurrences of the type variable `a` are in *covariant* positions (i.e., the `a` values are produced), whereas in `ContraFun1`, `ContraFun2`, and `ContraFun3`, all occurrences of `a` are in *contravariant* positions (i.e., the `a` values are consumed). If we have a function `f :: a -> b`, we can't apply `f` to an `a` value in a contravariant position, which precludes a `Functor` instance.


Most type variables appear in covariant positions. Functions are special in that the lefthand side of a function arrow reverses variance. If a function type `a -> b` appears in a covariant position (e.g., `CovFun1` above), then `a` is in a contravariant position and `b` is in a covariant position. Similarly, if `a -> b` appears in a contravariant position (e.g., `CovFun2` above), then `a` is in a covariant position and `b` is in a contravariant position.


If we annotate covariant positions with `p` (for positive) and contravariant positions with `n` (for negative), then we can examine the above examples with the following pseudo-type signatures:

```wiki
CovFun1/ContraFun1 :: n -> p
CovFun2/ContraFun2 :: (p -> n) -> p
CovFun3/ContraFun3 :: ((n -> p) -> n) -> p
```


Since `ContraFun1`, `ContraFun2`, and `ContraFun3` all use the last type parameter in at least one `n` position, GHC would reject a derived `Functor` instance for each of them.

## Requirements for legal instances


This mechanism cannot derive `Functor`, `Foldable`, or `Traversable` instances for all data types. Currently, GHC checks if a data type meets the following criteria:

1. The data type has at least one type parameter. (For example, `data NoArg = NoArg` cannot have a `Functor` instance.)
1. The data type's last type parameter cannot be used contravariantly. (see the section on covariant and contravariant positions.)
1. The data type's last type parameter cannot be used in the "wrong place" in any constructor's data arguments. For example, in `data Right a = Right [a] (Either Int a)`, the type parameter `a` is only ever used as the last type argument in `[]` and `Either`, so both `[a]` and `Either Int a` values can be `fmap`ped. However, in `data Wrong a = Wrong (Either a a)`, the type variable `a` appears in a position other than the last, so trying to `fmap` an `Either a a` value would not typecheck.

>
> Note that there are two exceptions to this rule: tuple and function types.

1. The data type's last type variable cannot used in a `-XDatatypeContexts` constraint. For example, `data Ord a => O a = O a deriving Functor` would be rejected.
1. The data type's last type variable must be truly universally quantified, i.e., it must not have any class or equality constraints. This means that the following is legal:

```
dataT a b whereT1:: a -> b ->T a b      -- Fine! Vanilla H-98T2:: b -> c ->T a b      -- Fine! Existential c, but we can still map over 'b'T3:: b ->TInt b         -- Fine! Constraint 'a', but 'b' is still polymorphicderivinginstanceFunctor(T a){-
instance Functor (T a) where
    fmap f (T1 a b) = T1 a (f b)
    fmap f (T2 b c) = T2 (f b) c
    fmap f (T3 x)   = T3 (f x)
-}
```

>
> but the following is not legal:

```
dataT a b whereT4::Ord b => b ->T a b  -- No!  'b' is constrainedT5:: b ->T b b           -- No!  'b' is constrainedT6::T a (b,b)-- No!  'b' is constrained
```


For derived `Foldable` and `Traversable` instances, a data type cannot use function types. This restriction does not apply to derived `Functor` instances, however.

### Proposal: relax universality check for `DeriveFoldable`


In the Requirements for legal instances section, restriction 5 applies to `DeriveFunctor`, `DeriveFoldable`, and `DeriveTraversable` alike. However, `Foldable` instances are unique in that they do not produce constraints, but only consume them. Therefore, it is permissible to derive `Foldable` instances for constrained data types (e.g., GADTs), so restriction 5 should be lifted for `DeriveFoldable`.


For example, consider the following GADT:

```
dataT a whereT1::Ord a => a ->T a
```


In the type signatures for `fmap :: Functor t => (a -> b) -> t a -> t b` and `traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)`, the `t` parameter appears both in an argument and the result type, so pattern-matching on a value of `t` must not impose any constraints, as neither `fmap` nor `traverse` would typecheck.

`Foldable`, however, only mentions `t` in argument types:

```
classFoldable t where
    fold ::Monoid m => t m -> m
    foldMap ::Monoid m =>(a -> m)-> t a -> m
    foldr ::(a -> b -> b)-> b -> t a -> b
    foldr' ::(a -> b -> b)-> b -> t a -> b
    foldl ::(b -> a -> b)-> b -> t a -> b
    foldl' ::(b -> a -> b)-> b -> t a -> b
    foldr1 ::(a -> a -> a)-> t a -> a
    foldl1 ::(a -> a -> a)-> t a -> a
    toList :: t a ->[a]
    null :: t a ->Bool
    length :: t a ->Int
    elem ::Eq a => a -> t a ->Bool
    maximum :: forall a.Ord a => t a -> a
    minimum :: forall a.Ord a => t a -> a
    sum ::Num a => t a -> a
    product ::Num a => t a -> a
```


Therefore, a derived `Foldable` instance for `T` would typecheck:

```
instanceFoldableTwhere
    foldr f z (T1 a)= f a z -- foldr :: Ord a => (a -> b -> b) -> b -> T a -> b
    foldMap f (T1 a)= f a   -- foldMap :: (Monoid m, Ord a) => (a -> m) -> T a -> m
```


Deriving `Foldable` instances for GADTs with equality constraints can become murky, however. Consider this GADT:

```
dataE a whereE1::(a ~Int)=> a   ->E a
    E2::Int->EIntE3::(b ~Int)=> b   ->EIntE4::(a ~Int)=>Int->E a
```


All four `E` constructors have the same "shape" in that they all take an argument of type `a` (or `Int`, to which `a` is constrained to be equal). Does that mean all four constructors would have their arguments folded over? While it is possible to derive perfectly valid code which would do so:

```
instanceFoldableEwhere
    foldr f z (E1 e)= f e z
    foldr f z (E2 e)= f e z
    foldr f z (E3 e)= f e z
    foldr f z (E4 e)= f e z

    foldMap f (E1 e)= f e
    foldMap f (E2 e)= f e
    foldMap f (E3 e)= f e
    foldMap f (E4 e)= f e
```


it is much harder to determine which arguments are equivalent to `a`. Also consider this case:

```
dataUnknownConstraints a whereUC::Mystery a =>Int->UnknownConstraints a
```


For all we know, it may be that `a ~ Int => Mystery a`. Does this mean that the `Int` argument in `UC` should be folded over?


To avoid these thorny edge cases, we only consider constructor arguments (1) whose types are *syntactically* equivalent to the last type parameter and (2) in cases when the last type parameter is a *simple* type variable. In the above `E` example, only `E1` fits the bill, so the derived `Foldable` instance should actually be:

```
instanceFoldableEwhere
    foldr f z (E1 e)= f e z
    foldr f z (E2 e)= z
    foldr f z (E3 e)= z
    foldr f z (E4 e)= z

    foldMap f (E1 e)= f e
    foldMap f (E2 e)= mempty
    foldMap f (E3 e)= mempty
    foldMap f (E4 e)= mempty
```


To expound more on the meaning of criterion (2), we want not only to avoid cases like `E2 :: Int -> E Int`, but also something like this:

```
dataHigherKinded f a whereHigherKinded:: f a ->HigherKinded f (f a)
```


In this example, the last type variable is instantiated with `f a`, which contains one type variable `f` applied to another type variable `a`. We would *not* fold over the argument of type `f a` in this case, because the last type variable should be *simple*, i.e., contain only a single variable without any application.


For the original discussion on this proposal, see [ \#10447](https://ghc.haskell.org/trac/ghc/ticket/10447).