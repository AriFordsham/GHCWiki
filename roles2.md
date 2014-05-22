# Update to Roles


It has become (somewhat) clear that the Roles mechanism as implemented in GHC 7.8 is insufficient. (See examples below.) This page is dedicated to creating a new design for roles that might fix the problems.

## Problem examples


We have three known examples of where current roles are failing us.

### Adding `join` to `Monad`


As part of the [ Applicative-Monad Proposal](http://www.haskell.org/haskellwiki/Functor-Applicative-Monad_Proposal), we wish to add `join` to `Monad`, thus:

```wiki
class Applicative m => Monad m where
  ...
  join :: forall a. m (m a) -> m a
```


This is all well and good, and would work with GeneralizedNewtypeDeriving (GND) most of the time. But, consider this:

```wiki
newtype T m a = T (m a)
  deriving (Functor, Applicative, Monad)
```