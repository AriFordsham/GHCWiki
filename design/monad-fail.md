# `MonadFail`


This page contains some notes and ideas that predate the official **[MonadFail](design/monad-fail) Proposal (MFP)** which can be found at

[prime:Libraries/Proposals/MonadFail](https://gitlab.haskell.org/haskell/prime/-/wikis/libraries/proposals/monad-fail)

## The Basic Idea



The basic idea is to split the `Monad` class into


```
class Applicative m => Monad m where
  (>>=)  :: m a -> (a -> m b) -> m b

  (>>)   :: m a -> m b -> m b
  m >> k = m >>= \_ -> k 

  return :: a -> m a -- still in Monad for historic reasons
  return = pure

class Monad m => MonadFail m where
  fail :: String -> m a
```


and adapt `do`-desugaring accordingly.
See [https://github.com/quchen/articles/blob/master/monad_fail.md](https://github.com/quchen/articles/blob/master/monad_fail.md) for more details.

### Misc Ideas



To aid transition, we can keep `fail` in `Monad`, and start out with


```
class Monad m => MonadFail m where
  mfail :: String -> m a
```


(or maybe even require `MonadPlus`, as `fail _ = mzero` is a sensible default). This is a comparable situation as  with post-AMP `pure`/`return` , where the now redundant `return` (which is now an alias for `pure`) is to be phased out into an ordinary top-level (non-method) function in the long term.


The `MonadFail(mfail)` desugaring of `do` could then be enabled via a language pragma `{-# LANGUAGE MonadFail -#}`, allowing for have a future `-XHaskell201x` to switch that feature on by default, while retaining `-XHaskell2010` with the current old `Monad(fail)` desugaring semantics.

`Monad(fail)` could default to `MonadFail(mfail)` via `-XDefaultSignatures`

## History



In [Haskell 1.4 (postscript)](http://haskell.org/definition/haskell-report-1.4.ps.gz) `fail` was not part of the `Monad` class. Instead there was a separate `MonadZero` class containing the `zero` operation whose purpose was to handle pattern-failures in `do`-syntax (akin to what `fail` does today). Here are the original Haskell 1.4 class definitions quoted from section "6.2.5 Monadic Classes":


```
class Functor f where
  map :: (a -> b) -> (f a -> f b)

class Monad m where
  (>>=)  :: m a -> (a -> m b) -> m b
  (>>)   :: m a -> m b -> m b
  return :: a -> m a

class (Monad m) => MonadZero m where
  zero :: m a

class (MonadZero m) => MonadPlus m where
  (++) :: m a -> m a -> m a
```


However, when Haskell 98 was drafted [issues with irrefutable patterns](http://marc.info/?l=haskell&m=66622011823641)  lead to `MonadZero` being folded into the `Monad` class (but it doesn't seem to have been an unanimous nor easy decision back then).



The issue is highlighted by deconstructing a monadic action returning a single-constructor value:


```
f :: Monad m => m (a,b) -> m a
f m1 = do { x <- m1; return (fst x) }

g :: ??? m => m (a,b) -> m a
g m1 = do { (a,_) <- m1; return a }

h :: Monad m => m (a,b) -> m a
h m1 = do { ~(a,_) <- m1; return a }
```


Should `???` for `g` be `Monad` or `MonadZero`? The single-constructor pattern match will never fail ("unfailable"), so `zero` is never used. In fact, in Haskell 1.4 `g` would only require `Monad`, but requires the concept of \`"unfailable" pattern matches (in addition to "irrefutable" pattern matches).


## Related Concepts/Proposals


- [MonadPlus reform proposal](https://wiki.haskell.org/MonadPlus_reform_proposal)
- [ApplicativeDo](applicative-do)
