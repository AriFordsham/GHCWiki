# Class Method Deprecations ([\#10071](https://gitlab.haskell.org//ghc/ghc/issues/10071))

## What?


Class method deprecations are a `DEPRECATED`-variant to be attached to method declarations (in `class`-definitions) that triggers warnings when

1. an `instance` overrides the default implementation of that method, and/or when
1. the method is explicitly im/(re)exported via `Class(method)`-syntax.


But not when

1. imported as e.g. `import Mod1 (Class, method)` (which doesn't require `method` to be a method of `Class`), nor when
1. merely referring to `method` in an expression (as that doesn't require `method` to be a method either).


Or put differently, the warning shall forecast the compile errors that would occur if a method `bar` as in the class `C` (see syntax section) was moved out of the class `C` to either a different class `D` or to a top-level binding.

## Syntax


It's already possible to attach (deprecation) warnings to class methods. However, we need a different syntax for this new variant of warnings attached to methods:

```
moduleM1whereclassC a where
  foo :: a

  bar :: a -> a
  bar x = x
 
  -- New class-method deprecation annotation-- NB: the pragma is indented at the class body level!{-# DEPRECATED bar "'bar' will cease to be a method of C, please avoid referring to it as a method of C!" #-}

  doo :: a

-- This is an ordinary (old-style) top-level indented deprecation{-# DEPRECATED foo "'foo' is obsolete and going away soon, please use 'doo' instead" #-}
```

## Examples


For the example of class `C` from the previous section, the following code fragments exemplify the expected warnings

```
importM1x= bar ()-- no warning, because the import doesn't limit `bar` to be a method of `C`
```

```
importM1(C,bar)x= bar ()-- no warning, because the import doesn't limit `bar` to be a method of `C`
```

```
importM1(C(foo,bar))x= bar ()-- triggers warning, because `bar` is imported via `C(foo,bar)` syntax
```

```
importM1(C(..))x= bar ()-- triggers warning, because `bar` is imported via `C(..)` syntax
```

```
importM1(C(..),bar)x= bar ()-- no warning, because the import doesn't limit `bar` to be a method of `C`
```

## Practical Use Case


This would aid long-term transitions like phasing out class-methods, such as e.g. `Monad(return)` in the spirit of [\#4834](https://gitlab.haskell.org//ghc/ghc/issues/4834):


With the AMP, `Monad(return)` being a class method becomes an historic artifact. The ideal long-term situation would rather be to have `return` become a top-level definition (i.e. outside the `Monad`-class), generalised to `Applicative` just aliasing `Applicative(pure)`. Moreover, the [MonadFail](design/monad-fail) proposal would have the `fail` method move to a new `MonadFail` class. I.e.

```
-- Haskell 2010 ReportclassMonad m where(>>=):: m a ->(a -> m b)-> m b
  (>>):: m a -> m b -> m b
  return :: a -> m a
  fail   ::String-> m a
```


is to become

```
-- Hypothetical Haskell 201x ReportclassApplicative m =>Monad m where(>>=):: m a ->(a -> m b)-> m b
  (>>):: m a -> m b -> m b

classMonad m =>MonadFailwhere
  fail   ::String-> m a

-- legacy synonym generalised to Applicativereturn::Applicative f => a -> f a
return= pure
```


The generalised `return` alias may be beneficial for things like `ApplicativeDo` which otherwise would require a `return` to be handled is if it was `pure` in order to weaken the type-constraint in an `do`-expression like e.g.

```
do{ x <- f; return (fst x)}
```


Right now, we can attach a `DEPRECATED`-pragma to the `return`-class-method. That however would trigger warnings on *all* uses of `return`, rather than only when using `return` in a context that requires it to be a class-method (like overriding `return` in instance definitions, or importing/exporting it via the explicit `Monad(return)`-syntax)
