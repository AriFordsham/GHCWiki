# Applicative do-notation


This is a proposal to add support to GHC for desugaring do-notation into Applicative expressions where possible.

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

```wiki
  do
    .. stmts1 ..
    x <- A
    y <- B
    z <- E[y]
    .. stmts2 ..
```


which we desugar to

```wiki
  do
    .. stmts1 ..
    (x,y) <- (,) <$> A <*> B
    z <- E[y]
    .. stmts2 ..
```


this is the best we can do: the rest of the do expression might refer
to x or y.


So in general we want to take the largest consecutive sequence of
statements where none of the rhs's refer to any of the bound
variables, and lift them into an Applicative expression.


A non-binding statement can be considered to be a binding statement
with a wildcard pattern.

```wiki
   do
     x <- A
     y <- B  -- B does not refer to x
     C       -- C does not refer to x or y
     return (f x y)
```


desugars to

```wiki
   do
     (x,y,_) <- (,,) <$> A <*> B <*> C
     return (f x y)
```


or we can be slightly more clever:

```wiki
   do
     (x,y) <- (,) <$> A <*> (B <* C)
     return (f x y)
```


What if there are more than 63(?) statements, and we don't have a
tuple big enough?  We have to desugar to nested tuples in this case.
Not a huge problem, this is exactly what we do for pattern bindings.

### No unique grouping


There isn't a guaranteed unique way of doing the grouping. Eg

```wiki
do { x <- A
   ; y <- B  -- no x
   ; z <- C x }
```


could be grouped with the first two in an applicative, or the second two, but not all three. Which one "wins"?

## Stage 2


This covers a more comprehensive transformation that would also enable
us to drop a Monad constraint to an Applicative constraint in the
typing of do expressions for a certain well-defined subset of the do
syntax.


Back to our first example:

```wiki
   do
     x <- A
     y <- B  -- B does not refer to x
     return (f x y)
```


we can go further in desugaring this:

```wiki
    (\x y -> f x y) <$> A <*> B
```


(obviously the lambda expression can be eta-reduced in this example,
but that's not the case in general).


For this to work we have to recognise "return".  Or perhaps "pure".


There are two advantages here:

- This code could be typed with an Applicative constraint rather than
  Monad.

- It leads to more efficient code when the Monad type is not known,
  because we have eliminated the intermediate pair.


What if the final expression is not a "return"?

```wiki
   do
     x <- A
     y <- B  -- B does not refer to x
     f x y
```


this is

```wiki
   join ((\x y -> f x y) <$> A <*> B)
```


Note: \*not\* an Applicative, because "join" is a Monad operation.
However we have eliminated the pair.


Problems:

- desugaring comes after typechecking, so the type checker would need
  its own version of the desugaring rules to be able to tell when a
  do expression can be fully desugared to Applicative syntax.
