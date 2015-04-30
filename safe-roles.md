# Roles, Abstraction & Safety


GHC 7.8 introduced a new mechanism, roles, for implementing `GeneralizedNewtypeDeriving` safely. Roles solves a big issue with GND, type-safety. Previously, GND could be used to generate an `unsafeCoerce` function, which can easily segfault a program.


However, GND had a second issue, it's ability to break module boundaries. How this should be handled with the new roles infrastructure and what the default should be was a major point of discussion before GHC 7.8 and after.


The design chosen settled on enabling easier use of GND over enforcing module boundaries. This document tries to summaries the situation and propose alternatives for future GHC versions.


A major focus is on improving the situation of Roles & GND for Safe Haskell.

## Problem Pre-GHC-7.8


We will ignore the type-safety issues as they have been resolved, instead we'll just look at the module boundary / abstraction issues.


In GHC 7.6 or earlier, assume a library author writes the following `MinList` data type:

```wiki
module MinList (
        MinList,
        newMinList,
        insertMinList,
        printIntMinList
    ) where

data MinList a = MinList a [a] deriving (Show)

newMinList :: Ord a => a -> MinList a
newMinList n = MinList n []

insertMinList :: Ord a => MinList a -> a -> MinList a
insertMinList s@(MinList m xs) n | n > m     = MinList m (n:xs)
                                 | otherwise = s
```


The `MinList` data type has an invariant (that depends on the `Ord` typeclass for the type parameter `a` that `MinList` is instantiated at) that after initialization it doesn't accept any element less than the initial element.


In GHC 7.6 and earlier, we could use GND to violate this invariant:

```wiki
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import MinList

class IntIso t where
    intIso :: c t -> c Int

instance IntIso Int where
    intIso = id

newtype Down a = Down a deriving (Eq, IntIso)

instance Ord a => Ord (Down a) where
    compare (Down a) (Down b) = compare b a

fine :: MinList (Down Int)
fine = foldl (\x y -> insertMinList x $ Down y) (newMinList $ Down 0) [-1,-2,-3,-4,1,2,3,4]

unsafeCast :: MinList (Down Int) -> MinList Int
unsafeCast = intIso

bad :: MinList Int
bad = unsafeCast fine

main = do
    print bad
```


Essentially, through GND we have created the function `unsafeCast :: MinList (Down Int) -> MinList Int`. This is a function we can't write by hand since we don't have access to the `MinList` constructor and so can't "see" into the data type.

## Safe Haskell Pre-GHC-7.8


Due to both the type-safety and abstraction issues, GND was considered unsafe in Safe Haskell.

## Background Reading


Userguide:

- [ https://downloads.haskell.org/\~ghc/latest/docs/html/users_guide/roles.html](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/roles.html)


GHC Wiki:

- [ https://ghc.haskell.org/trac/ghc/wiki/Roles](https://ghc.haskell.org/trac/ghc/wiki/Roles)
- [ https://ghc.haskell.org/trac/ghc/wiki/Roles2](https://ghc.haskell.org/trac/ghc/wiki/Roles2)


Email Threads:

- "Default Roles" -- [ https://mail.haskell.org/pipermail/glasgow-haskell-users/2013-October/024360.html](https://mail.haskell.org/pipermail/glasgow-haskell-users/2013-October/024360.html)
- "Role Signatures in Libraries" -- [ https://mail.haskell.org/pipermail/libraries/2013-November/021707.html](https://mail.haskell.org/pipermail/libraries/2013-November/021707.html)
- "We need to add role annotations for 7.8" -- [ https://mail.haskell.org/pipermail/libraries/2014-March/022321.html](https://mail.haskell.org/pipermail/libraries/2014-March/022321.html)


Tickets:

- "Inferring Safe mode with GeneralizedNewtypeDeriving is wrong" -- [ https://ghc.haskell.org/trac/ghc/ticket/8827](https://ghc.haskell.org/trac/ghc/ticket/8827)
- "GeneralizedNewtypeDeriving is still not Safe" -- [ https://ghc.haskell.org/trac/ghc/ticket/8745](https://ghc.haskell.org/trac/ghc/ticket/8745)
- "Require -XIncoherentInstances to write role annotations on class definitions" -- [ https://ghc.haskell.org/trac/ghc/ticket/8773](https://ghc.haskell.org/trac/ghc/ticket/8773)
- "Incoherent instances without -XIncoherentInstances" -- [ https://ghc.haskell.org/trac/ghc/ticket/8338](https://ghc.haskell.org/trac/ghc/ticket/8338)

## Roles Overview

[Roles Overview](safe-roles/roles-overview)


Consider the code:

```wiki
newtype Set a = MkSet [a]
```


This gives the following instances of `Coercible`:


First, the **unwrapping instances**:

```wiki
instance Coercible [a] b => Coercible (Set a) b
instance Coercible a [b] => Coercible a (Set b)
```


These are only available when the `Set` constructor, `MkSet`, is in scope. This was specifically done to preserve some notion of abstraction.


Secondly, you get the **lifting instances**:

```wiki
instance Coercible a b => Coercible (Set a) (Set b)
```


This instance is produced for both `data` and `newtype` types and is available for both regardless of if or if not the constructors of the type are in scope.


The only way to control the availability of the **lifting instance** is to use a role annotation:

```wiki
type role Set nominal
```

## Roles & Safe Haskell


Roles are an unfortunate mechanism for control right now. Since
representational is the default role for most type constructors, to enforce
invariants on abstract data types, library authors need to set their type
constructors to have nominal roles.


This requires that library authors understand roles to enforce what they expect
to happen according to Haskell2010 semantics. It also prevents them using
`coerce` internally and gaining the optimization, which is insulting as they
can write the code that coerce is semantically equivalent to.


It seems a different approach is needed, of either:


1) Require that all constructors are in scope when calling `coerce`. There is
some precedence for this as 7.10 requires that a newtype's constructor is in
scope to use `coerce`.

**NO**: This was requirement wasn't place of data types since some types (like
`IORef`) don't even have constructors that can be in scope.


2) Allow specifying internal + external role annotations.


3) Change the default to be nominal when all the constructors aren't exported,
and allow weakening of this to referential with role annotations.

## Opinions

[ https://mail.haskell.org/pipermail/glasgow-haskell-users/2013-October/024368.html](https://mail.haskell.org/pipermail/glasgow-haskell-users/2013-October/024368.html)

[ https://mail.haskell.org/pipermail/glasgow-haskell-users/2013-October/024378.html](https://mail.haskell.org/pipermail/glasgow-haskell-users/2013-October/024378.html)


2.) It also indicates that making any typeclass with a representational (/
phantom?) argument shouldn't be possible in valid [SafeHaskell](safe-haskell), as you can
use it to subvert the current restrictions on OverlappingInstances.

- If you could use GND only where the constructors are available, then some valid current use of GND would break, I believe. It would mean that GND would be unable to coerce a (Map String Int) to a (Map String Age), because the constructor of Set is (rightly) not exported. This would have a direct runtime significance for some users -- their code would run slower.

## Pathways


Changing default role to nominal
In GHC 7.8, unannotated datatype parameters default to phantom. This means that most normal parameters are given a representational role. It has been argued that perhaps nominal is a better (safer) default, and that users should specify representational when they want it. The problem with a nominal default is that it breaks all current usages of GND by default. Furthering the problem, when a user is unable to use GND it's the library that has to change, not the user's code.


On Mar 31, 2014, Dominique Devriese writes the following suggestion:


What I was wondering about is if the dilemma could be solved by choosing nominal-by-default in the long term for the role inference (so that library writers cannot accidentally leave abstraction holes open by forgetting to add role annotations) and use them in the long-term-supported SafeNewtypeDeriving extension, but provide a deprecated not-quite-as-safe GND extension for helping out users of libraries that have not yet added role annotations. I would fancy that this not-quite-as-safe GND could use unsafeCoerce wherever the safe one would give an error about annotated roles.
