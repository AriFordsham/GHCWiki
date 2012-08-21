# Overlapping Type Family Instances with Coincident Right-Hand Sides


In the official release of GHC, it is permitted to write something like this:

```wiki
type instance F Int = Int
type instance F a   = a
```


These instances surely overlap, but in the case when they do, the right-hand sides coincide. This page explores the design space around this feature with an eye toward adding instance groups, as discussed [here](new-axioms).

- In the current (Aug 2012) implementation of family instance groups, no overlap (even coincident) overlap is allowed between a group and another instance. It may be possible to change the implementation to allow such overlap, but it adds another layer of complexity to the GHC code and may not be worth it. Add comments if you have a good use case for why we need coincident overlap involving an instance group.

- The current implementation retains the ability for coincident overlap among non-group instances, for backward compatibility (and because this case is not hard to implement).

- The current implementation considers coincident overlap among equations within an instance group when matching. See the implementation notes [here](new-axioms) for more info.

- Coincident overlap (both between single instances and within an instance group) requires syntactic equality after expansion of vanilla type synonyms. In particular, type families are not expanded. Chak defends this design decision in the comments of [\#4259](https://gitlab.haskell.org//ghc/ghc/issues/4259); that bug report is a request to lift this restriction. I ([ Richard](http://www.cis.upenn.edu/~eir)) think we should work to lift this restriction, taking Chak's points into consideration. For example, the following is tantalizing:

  ```wiki
  type instance where
    Plus Zero     n        = n
    Plus (Succ m) n        = Succ (Plus m n)
    Plus m        Zero     = m
    Plus m        (Succ n) = Succ (Plus m n)
  ```

  Figuring out that the right-hand sides are confluent requires expanding the `Plus` type family one time. What do you think? Would this be useful?
