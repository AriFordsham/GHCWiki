# Overlapping Type Family Instances with Coincident Right-Hand Sides


In the official release of GHC, it is permitted to write something like this:

```wiki
type instance F Int = Int
type instance F a   = a
```


These instances surely overlap, but in the case when they do, the right-hand sides coincide. This page explores the design space around this feature with an eye toward adding instance groups, as discussed [here](new-axioms).

- In the current (Aug 2012) implementation of family instance groups, no overlap (even coincident) overlap is allowed between a group and another instance. It may be possible to change the implementation to allow such overlap, but it adds another layer of complexity to the GHC code and may not be worth it. Add comments if you have a good use case for why we need coincident overlap involving an instance group.

- The current implementation retains the ability for coincident overlap among non-group instances, for backward compatibility (and because this case is not hard to implement).

- The current implementation considers coincident overlap among equations within an instance group when matching. See below for more info.

- Coincident overlap (both between single instances and within an instance group) requires syntactic equality after expansion of vanilla type synonyms. In particular, type families are not expanded. Chak defends this design decision in the comments of [\#4259](https://gitlab.haskell.org//ghc/ghc/issues/4259); that bug report is a request to lift this restriction. I ([ Richard](http://www.cis.upenn.edu/~eir)) think we should work to lift this restriction, taking Chak's points into consideration. For example, the following is tantalizing:

  ```wiki
  type instance Plus n m where
    Plus Zero     n        = n
    Plus (Succ m) n        = Succ (Plus m n)
    Plus m        Zero     = m
    Plus m        (Succ n) = Succ (Plus m n)
  ```

  Figuring out that the right-hand sides are confluent requires expanding the `Plus` type family one time. What do you think? Would this be useful?

## Coincident overlap within a branched instance


I (Richard) would like to be able to say this:

```wiki
type family And (a :: Bool) (b :: Bool) :: Bool
type instance And a b where
  And False a     = False
  And True  b     = b
  And c     False = False
  And d     True  = d
  And e     e     = e
```


According to the [main page](new-axioms) about branched instances, "We only match a type against an equation in an instance group when no previous equation can unify against the type." Unfortunately, that kills us here. Consider reducing `And f True`. The first three branches clearly don't match. The fourth matches. But, the first two branches unify (i.e., they might apply later, depending on the instantiation for `f`), so we're stuck. Instead, we propose to scan through the branched instances at the declaration, and look to find only problematic orderings. In the declaration above, there are no problematic orderings -- if a previous branch might apply later, we'd get the same result anyway, so we're OK. Then, during matching, we only check problematic previous branches.


Another example is in order:

```wiki
type instance F a where
  F Int  = Int
  F Bool = Char
  F a    = a
```


The only problematic ordering is `F Bool` before `F a`. Why? Let's consider this piece by piece. `F Int` can have no problematic previous branches, because it has no previous branches. A use site that matches `F Bool` will never later match `F Int`, `F Int` is not a problematic previous branch of `F Bool`. `F Int` might later match something that matches `F a`, but the right-hand sides coincide, so `F Int` is not problematic here either. On the other hand, `F Bool` might also later match something that matches `F a` and the right-hand sides *don't* coincide, so `F Bool`*is* problematic. The upshot is that, if `F a` matches a use site, we have to make sure that `F Bool` cannot later apply. Otherwise, we can skip that check.


This is perhaps somewhat subtle, but it seems to be the right way to do it.
