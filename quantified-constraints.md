# Quantified constraints


This wiki page summarises the state of play on the idea of allowing quantification in class constraints.  For example

```haskell
data Rose f a = Branch a (f (Rose f a))

instance (Eq a, forall b. (Eq b) => Eq (f b))
       => Eq (Rose f a)
  where ...
```


The new bit is the `forall` in the context of the instance declaration. This is allowed in GHC 8.6 and later using the `QuantifiedConstraints` extension.


Here are some resources

- The [GHC Proposal discussing quantified constraints](https://github.com/ghc-proposals/ghc-proposals/pull/109)

- [Derivable type classes](https://www.microsoft.com/en-us/research/publication/derivable-type-classes), Section 7, where the idea was first proposed (I think).

- #2893, a ticket about the idea

- [Quantified class constraints](http://i.cs.hku.hk/~bruno//papers/hs2017.pdf), a Haskell 2017 paper that works out the idea in some detail, and a [ Reddit thread](https://www.reddit.com/r/haskell/comments/6me3sv/quantified_class_constraints_pdf/) about it.

- [An old haskell.org Wiki page about it](http://haskell.org/haskellwiki/Quantified_contexts)
- [A Libraries thread (Dec 18)](https://mail.haskell.org/pipermail/libraries/2017-December/028377.html).

## Status

See the ~QuantifiedConstraints label for tickets.


