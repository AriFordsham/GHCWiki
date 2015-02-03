# The GHC 7.10 Prelude


The [ Core Libraries Committee](https://wiki.haskell.org/Core_Libraries_Committee) is responsible for developing the core libraries that ship with GHC.  This is an important but painstaking task, and we owe the CLC a big vote of thanks for taking it on.


For over a year the CLC has been working on integrating the `Foldable` and `Traversable` classes (shipped in `base` in GHC 7.8) into the core libraries, and into the Prelude in particular.  Then we had a failure of communication.  Detailed planning for GHC 7.10 started in the autumn of 2014, and the CLC went ahead with this integration.  *That entails substantial changes to the `Prelude`*; that is, changes to the experience that everyone willy-nilly has when they fire up `ghci`.  These changes were not well signposted, so many people have only recently woken up to them, and some have objected (both in principle and detail).


This is an extremely unfortunate situation.  On the one hand we are at RC2 for GHC 7.10, so library authors have invested effort in updating their libraries to the new Prelude.  On the other, altering the Prelude is in effect altering the language, something we take pretty seriously.  We should have had this debate back in 2014, but here we are, and it is unproductive to argue about whose fault it is.  We all share responsiblity.


We need to decide what to do now.  A small group of us met by Skype and we've decided to do this:

- Push back GHC 7.10's release by at least a month
- Invite input from the Haskell community on which of two approaches to adopt (see below)
- Ask Simon Marlow and Simon Peyton Jones to decide which approach to follow for GHC 7.10.

## The two alternative approaches


We discussed many alternatives, but ended up with two simple ones:

- **Plan Foldable** - generalize many of the functions in Prelude, Data.List and Control.Monad from lists to Foldable/Traversable.  Specifically, 

  - From Prelude: all, and, any, concat, concatMap, elem, foldl, foldl1, foldr, foldr1, length, mapM, mapM_, maximum, minimum, notElem, null, or, product, sequence, sequence_, sum
  - From Control.Monad: foldM, foldM_, forM, forM_, mapM, mapM_, msum, sequence, sequence_
  - From Data.List: all, and, any, concat, concatMap, elem, find, foldl, foldl', foldl1, foldr, foldr1, length, mapAccumL, mapAccumR, maximum, maximumBy, minimum, minimumBy, notElem, null, or, product, sum
    The generalised types are almost entirely backward compatible.  However the `Foldable` class (in particular) has almost certainly not reached its final form, and is likely to evolve further.

- **Plan List** - leave the functions working on lists, as they do in GHC 7.8.


Regardless of which plan is chosen the GHC 7.10 base libraries will

- Incorporate the [ Applicative-Monad proposal](https://wiki.haskell.org/Functor-Applicative-Monad_Proposal)
- Include `Data.Traversable` and `Data.Foldable` as in 7.8, but evolved somewhat. 


More details:

- [ Plan Foldable](https://wiki.haskell.org/Foldable_and_Traversable)
- [ Concerns about Plan Foldable](https://ghc.haskell.org/trac/ghc/wiki/BurningBridgesSlowly)

## The plan for GHC 7.12


This discussion only covers what will ship with GHC 7.10. The expectation is that if we pick Plan List, then work will be put in place to achieve the goals of the Foldable/Traversable proposal in a future release. Alternatively, if we pick Plan Foldable, it is expected that the Foldable class will evolve in future releases and that some methods in Data.List may become specialized to list once again.

## Reasons for Plan Foldable: Generalize in GHC 7.10

- GHC 7.10 RC2 contains an implementation of the Foldable/Traversable generalizations. Authors who have modified their code to work with GHC 7.10 may have to undo some of those changes (but not the Applicative-Monad related changes).
- With Foldable/Traversable there are fewer name clashes in the base library.
- The functions in Prelude become applicable to other data structures, for example Vector and Map.

## Reasons for Plan List: Stay with list in GHC 7.10

- There is not yet a consensus on many of the Plan Foldable changes. Another release cycle would give us time to build such a consensus, possibly leading to alterations in the proposal.
- The Prelude is special, as it provides the default functions everyone gets. After Foldable and Traversable are incorporated into the Prelude, they are harder to change.
- There are concerns about which methods are generalized in Plan Foldable, whether methods in Data.List should be restricted to list even if the Prelude is generalized etc. Another release will give us the time to address some of these issues.
- [Prelude710List](prelude710-list) outlines further concerns and possible approaches to take to achieve the Foldable/Traversable goals in GHC 7.12.

## What next?


We're actively seeking feedback. Once the feedback is collected, Simon Peyton Jones and Simon Marlow will make a final decision.
