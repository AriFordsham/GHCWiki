# The GHC 7.10 Prelude


GHC 7.10 is coming soon, and we need to decide what the Prelude for GHC 7.10 will look like. The question is whether to generalize many of the functions in Prelude, Data.List and Control.Monad from lists to Foldable/Traversable. Regardless of which option is chosen, the GHC 7.10 base libraries will incorporate the [ Applicative-Monad proposal](https://wiki.haskell.org/Functor-Applicative-Monad_Proposal) and the extensions to the Foldable class required to support a future generalization. The Foldable/Traversable proposal then goes on to generalize the following methods:

- From Prelude: all, and, any, concat, concatMap, elem, foldl, foldl1, foldr, foldr1, length, mapM, mapM_, maximum, minimum, notElem, null, or, product, sequence, sequence_, sum
- From Control.Monad: foldM, foldM_, forM, forM_, mapM, mapM_, msum, sequence, sequence_
- From Data.List: all, and, any, concat, concatMap, elem, find, foldl, foldl', foldl1, foldr, foldr1, length, mapAccumL, mapAccumR, maximum, maximumBy, minimum, minimumBy, notElem, null, or, product, sum

## The plan for GHC 7.12


This discussion only covers what will ship with GHC 7.10. The expectation is that if the decision is to stick with list versions, then work will be put in place to achieve the goals of the Foldable/Traversable proposal in a future release. Alternatively, if the Foldable/Traversable versions are used, it is expected that the Foldable class will evolve in future releases and that some methods in Data.List may become specialised to list once again.

## Reasons we should generalise in GHC 7.10

- GHC 7.10 RC2 contains an implementation of the Foldable/Traversable generalisations. Authors who have modified their code to work with GHC 7.10 may have to undo some of those changes (but not the Applicative-Monad related changes).
- With Foldable/Traversable there are fewer name clashes in the base library.
- The functions in Prelude become applicable to other data structures, for example Vector and Map.

## Reasons we should stay with list in GHC 7.10

- There is not yet a consensus on many of the changes. Another release cycle would give us time to build such a consensus, possibly leading to alterations in the proposal.
- The Prelude is special, as it provides the default functions everyone gets. After Foldable and Traversable are incorporated into the Prelude, they are harder to change.
- There are concerns about which methods have been generalised, whether methods in Data.List should be restricted to list even if the Prelude is generalised etc. Another release will give us the time to address some of these issues.
- Further concerns and outlines of possible approaches to take to enable the Foldable/Traversable goals in GHC 7.12 are outlined in [Prelude710List](prelude710-list).

## What next?


We're actively seeking feedback. Once the feedback is collected, Simon Peyton Jones and Simon Marlow will make a final decision.
