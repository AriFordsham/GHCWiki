# Closed Type Families with Coincident Right-Hand Sides


In GHC 7.6, it is permitted to write something like this:

```wiki
type instance F Int = Int
type instance F a   = a
```


These instances surely overlap, but in the case when they do, the right-hand sides coincide. This page explores the design space around this feature with an eye toward adding instance groups, as discussed [here](new-axioms).

## Coincident overlap within a closed type family


This section discusses a part of the "Concrete Proposal" on [this page](new-axioms/nonlinearity) to patch a potential hole in Haskell's type system.


I (Richard) would like to be able to say this:

```wiki
type family And (a :: Bool) (b :: Bool) :: Bool where
  And False a     = False
  And True  b     = b
  And c     False = False
  And d     True  = d
  And e     e     = e
```


According to the [main page](new-axioms) about closed type families, "We only match a type against an equation in a closed type family when no previous equation can unify against the type." Unfortunately, that kills us here. Consider reducing `And f True`. The first three branches clearly don't match. The fourth matches. But, the first two branches unify (i.e., they might apply later, depending on the instantiation for `f`), so we're stuck. Instead, we scan through the equations at the declaration and look to find compatible equations. Two equations are compatible if their right-hand sides match whenever their left-hand sides do. In the declaration above, all equations are compatible -- if a previous equation might apply later, we'd get the same result anyway, so we're OK. Then, during matching, we only check *in*compatible previous equations.


Another example is in order:

```wiki
type family F a where
  F Int  = Int
  F Bool = Char
  F a    = a
```


The only incompatible pair of equations is `F Bool` and `F a`. Why? Let's consider this piece by piece. A use site that matches `F Bool` will never later match `F Int`, so `F Int` is compatible with `F Bool`. `F Int` might later match something that matches `F a`, but the right-hand sides coincide, so these equations are compatible. On the other hand, `F Bool` might also later match something that matches `F a` and the right-hand sides *don't* coincide, so these equations are not compatible. The upshot is that, if `F a` matches a use site, we have to make sure that `F Bool` cannot later apply. Otherwise, we can skip that check.


This is perhaps somewhat subtle, but it seems to be the right way to do it.


The ideas discussed here stem from posts by AntC, Andy Adams-Moran, and Richard on Richard's [blog post](http://typesandkinds.wordpress.com/2013/04/29/coincident-overlap-in-type-families/) on the subject.
