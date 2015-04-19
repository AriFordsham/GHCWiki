# Applicative do-notation


This is a proposal to add support to GHC for desugaring do-notation into Applicative expressions where possible.


To jump to the code, see [ https://phabricator.haskell.org/D729](https://phabricator.haskell.org/D729).

## Related proposals

- [ Max's proposal on haskell-cafe](http://www.haskell.org/pipermail/haskell-cafe/2011-September/095153.html)
- [ Control.Applicative.QQ.ADo](http://hackage.haskell.org/package/applicative-quoters-0.1.0.7/docs/Control-Applicative-QQ-ADo.html)

## Motivation

1. Some Monads have the property that Applicative bind is more
  efficient than Monad bind.  Sometimes this is *really
  important*, such as when the Applicative bind is 
  concurrent whereas the Monad bind is sequential (c.f. [ Haxl](https://github.com/facebook/Haxl)).  For
  these monads we would like the do-notation to desugar to
  Applicative bind where possible, to take advantage of the improved
  behaviour but without forcing the user to explicitly choose.

1. Applicative syntax can be a bit obscure and hard to write.
  Do-notation is more natural, so we would like to be able to write
  Applicative composition in do-notation where possible.  For example:

  ```wiki
  (\x y z -> x*y + y*z + z*x) <$> expr1 <*> expr2 <*> expr3
  ```

  vs.

  ```wiki
  do x <- expr1; y <- expr2; z <- expr3; return $ x*y + y*z + z*x
  ```

1. Do-notation can't be desugared into Applicative in general, but a certain
  subset of it can be.  For Applicatives that aren't also Monads, we would still like to
  be able to use the do-notation, albeit with some restrictions,
  and have an Applicative constraint inferred rather than Monad.


Clearly we need Applicative to be a superclass of Monad for this to
work, hence this can only happen after the AMP changes have landed.


Since in general Applicative composition might behave differently from monadic bind, any automatic desugaring to Applicative operations would be an opt-in extension:

```wiki
   {-# LANGUAGE ApplicativeDo #-}
```

## Example 1


We'll cover a few examples first, and then describe the transformation in general.

```wiki
   do
     x <- A
     y <- B  -- B does not refer to x
     return (f x y)
```


desugars to

```wiki
   (\x y -> f x y) <$> A <*> B
```


which simplifies further to 

```wiki
   f <$> A <*> B
```


Since this doesn't refer to any of the `Monad` operations, the typechecker will infer that it only requires `Applicative`. 

## Example 1a


We might not have a `return`, e.g.:

```wiki
   do
     x <- A
     y <- B  -- B does not refer to x
     f x y
```


In which case the result is similar, but we need to add a `join`:

```wiki
   join ((\x y -> f x y) <$> A <*> B)
```


Of course this requires a `Monad`, since `join` is a `Monad` operation.

## Example 2

```wiki
   do
     x <- A
     y <- B x
     z <- C
     return (f x y z)
```


Now we have a dependency: `y` depends on `x`, but there is still an opportunity to use `Applicative` since `z` does not depend on `x` or `y`.  In this case we end up with:

```wiki
  (\(x,y) z -> f x y z) <$> (do x <- A; y <- B x; return (x,y)) <*> C
```


Note that we had to introduce a tuple to return both the values of `x` and `y` from the inner `do` expression


It's important that we keep the original ordering.  For example, we don't want this:

```wiki
  do 
    (x,z) <- (,) <$> A <*> C
    y <- B
    return (f x y z)
```


because now evaluating the expression in a monad that cares about ordering will give a different result.


Another wrong result would be:

```wiki
  do
    x <- A
    (\y z -> f x y z) <$> B <*> C
```


Because this version has less parallelism than the first result, in which `A` and `B` could be performed at the same time as `C`.

## Example 3

```wiki
  do
    x1 <- A
    x2 <- B
    x3 <- C x1
    x4 <- D x2
    return (x1,x2,x3,x4)
```


Here we can do `A` and `B` in parallel, and `C` and `D` in parallel.  We could do it like this:

```wiki
  do
    (x1,x2) <- (,) <$> A <*> B
    (\x3 x4 -> (x1,x2,x3,x4)) <$> C x1 <*> D x2
```


But it is slightly more elegant like this:

```wiki
   join ((\x1 x2 -> (\x3 x4 -> (x1,x2,x3,x4)) <$> C x1 <*> D x2)) <$> A <*> B)
```


because we avoid the intermediate tuple.

## In general


Let's use a more concise syntax to talk about the structure of these expressions.  We'll use ; to mean bind, and \| to mean an `Applicative` composition of two expressions.  So the solution to Example 3 would be written `(A | B) ; (C | D)`, making it clear that we want `A` and `B` in parallel, then a bind, followed by `C` and `D` in parallel.


The general problem can be stated like this:


Given a sequence of statements S1...Sn, find a way to express the list as a nested application of the operators `|` and `;`, such that 

- in every `A | B`, `B` does not depend on anything in `A`, and
- we get the maximum available parallelism.


To define parallelism, we could replace each statement with the value 1, `;` with `+`, and `|` with `max`, and evaluate the expression.  A lower result indicates more parallelism.

## Implementation


The implementation is tricky, because we want to do a transformation that affects type checking (and renaming, because we might be using `RebindableSyntax`), but we still want type errors in terms of the original source code.  Therefore we calculate everything necessary to do the transformation during renaming, but leave enough information behind to reconstruct the original source code for the purposes of error messages.


See comments in [ https://phabricator.haskell.org/D729](https://phabricator.haskell.org/D729) for more details.
