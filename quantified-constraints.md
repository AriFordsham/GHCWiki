# Quantified contexts


This wiki page summarises the state of play on the idea of allowing quantification in class contexts.  For example

```wiki
data Rose f a = Branch a (f (Rose f a))

instance (Eq a, forall b. (Eq b) => Eq (f b))
       => Eq (Rose f a)
  where ...
```


The new bit is the `forall` in the context of the instance declaration.


Here are some resources

- [ Derivable type classes](https://www.microsoft.com/en-us/research/publication/derivable-type-classes), Section 7, where the idea was first proposed (I think).

- [\#2893](https://gitlab.haskell.org//ghc/ghc/issues/2893), a ticket about the idea

- [ Quantified class constraints](http://i.cs.hku.hk/~bruno//papers/hs2017.pdf), a Haskell 2017 paper that works out the idea in some detail.

- [ An old haskell.org Wiki page about it](http://haskell.org/haskellwiki/Quantified_contexts)