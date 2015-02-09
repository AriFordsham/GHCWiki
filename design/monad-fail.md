# `MonadFail`


At this point this is just a scratch pad for collecting ideas

## History


In [ Haskell 1.4 (postscript)](http://haskell.org/definition/haskell-report-1.4.ps.gz)`fail` was not part of the `Monad` class. Instead there was a separate `MonadZero` class containing the `zero` operation whose purpose was to handle pattern-failures in `do`-syntax (akin to what `fail` does today). Here are the original Haskell 1.4 class definitions quoted from section "6.2.5 Monadic Classes":

```
classFunctor f where
  map ::(a -> b)->(f a -> f b)classMonad m where(>>=):: m a ->(a -> m b)-> m b
  (>>):: m a -> m b -> m b
  return :: a -> m a

class(Monad m)=>MonadZero m where
  zero :: m a

class(MonadZero m)=>MonadPlus m where(++):: m a -> m a -> m a
```