# Nonlinear type family instances considered dangerous


This page discusses problems and solutions that come up when thinking about type family instances with repeated variables on the left-hand side.

## The Problem


Consider the following:

```wiki
type family F a b
type instance F x   x = Int
type instance F [x] x = Bool

type family G
type instance G = [G]
```


Here, `G` is a nullary type family, but its nullariness is just for convenience -- no peculiarity of nullary type families is involved.


These declarations compile just fine in GHC 7.6.3 (with `-XUndecidableInstances`), and on the surface, this seems OK. After all, the two instances of `F` cannot unify. Thus, no usage site of `F` can be ambiguous, right? Wrong. Consider `F G G`. We might simplify this to `Int`, using the first instance, or we might first simplify to `F [G] G` and then to `Bool`. Yuck!


I (Richard/goldfire) have tried to use this inconsistency to cause a seg fault, but after a few hours, I was unable to do so. However, my inability to do so seems more closely related to the fact the type families are strict than anything more fundamental.


It's worth noting that `-XUndecidableInstances` is necessary to exploit this problem. However, we still want `-XUndecidableInstances` programs to be type-safe (as long as GHC terminates).

## General idea for the solution


We need to consider the two instances of `F` to be overlapping and inadmissible. There are a handful of ways to do this, but the best seems to be this: 

- (A) **when performing the overlap check between two instances, check a version of the instances where all variables are distinct**


We call the "version of the instance where all variables are distinct" the "linearized form" of the instance.
Using such a check, the two instances for `F` above indeed conflict, because we would compare `(F a b)` against `(F [c] d)`, where a,b,c,d are the fresh distinct variables.


This can break existing code. But, a medium-intensity search did not find *any* uses of nonlinear (i.e. with a repeated variable) family instances in existing code, so I think we should be OK. However, a change needs to be made to be confident that nonlinear axioms and undecidable instances do not introduce a type-soundness hole into GHC.


(Interestingly, proofs of the soundness of the existing system have been published. For example, see [ here](http://research.microsoft.com/en-us/um/people/simonpj/papers/ext-f/fc-tldi.pdf) and [ here](http://www.cis.upenn.edu/~stevez/papers/WVPJZ11.pdf). These proofs are not  incorrect, but they  don't allow both undecidable instances and nonlinear family instances.)


Conor's alternative general idea:

- (B) **when performing the overlap check, during unification succeed (instead of failing) if the occurs check happens**


Remember "unification succeeds" means "overlap detected", so (B) is a bit more permissive than (A).  For example

```wiki
  type instance Good x   x    = blah
  type instance Good Int Bool = boo
```


These would be considered overlapping by (A), but accepted as non-overlapping (ie unification fails) by (B).  And indeed these two are fine (ie cannot give rise to unsoundness).


Intuition: under (B) the only way that unification fails is because of a definite conflict (eg `Int` \~ `Bool`).  And unification failing is what allows us to accept the two equations as non-overlapping.  So if we accept them, they have a definite conflict.

## Problem: coincident overlap


In the official release of GHC, it is permitted to write something like this (see [manual](http://www.haskell.org/ghc/docs/latest/html/users_guide/type-families.html#type-family-overlap)), and the extensive discussion on [\#4259](https://gitlab.haskell.org//ghc/ghc/issues/4259):

```wiki
type instance F Int = Int
type instance F a   = a
```


These instances surely overlap, but in the case when they do, the right-hand sides coincide. We call this **coincident overlap**.


However, the above proposal of linearizing the LHS before checking overlap makes a nonsense of exploiting coincident overlap, because when we freshen the LHS we no longer bind the variables in the RHS. **So the proposal abandons support for coincident overlap between standalone type instances**.  (It's worth noting, though, that there is already no support for coincident overlap between branched type instances, or between a singleton type instance and a branched one; it is currently only supported between singleton type instances.)


However, we can recover coincident overlap with in a branched instance: see [here](new-axioms/coincident-overlap). 

## Problem: branched instances


But, how does this interact with branched instances (those new instance forms that allow for ordered overlapping)? We still need nonlinear branched instances, as the canonical example of a nonlinear instance is an equality test:

```wiki
type family Equals a b :: Bool

type instance where
  Equals x x = True
  Equals x y = False
```


...


The solution is to declare a *type space* that all branches fit within.


For example: 

```wiki
type family Equals a b :: Bool

type instance Equals x y where
  Equals x x = True
  Equals x y = False
```


The header "`type instance Equals x y`" declares the "type space" of the declaration; the `where` part says how to match calls within that type space (top to bottom).


In this case the type space is the whole space, but that need not be the case:

```wiki
type family F a b :: *

type instance F Int x where
  F Int Bool = Double
  F Int Int  = Char
  F Int a    = Bool

type instance F Bool y = Int
```


Here the first `type instance` covers calls of form `(F Int x)`, while the second covers
calls of form `(F Bool y)`.


All the equations in the `where` part must be part of (i.e. an instance of) the 
type space declared in the header.  For example, this will be disallowed:

```wiki
type instance Bad [x] y where
  Bad [Int] Int = Bool -- OK
  Bad a     b   = ()   -- not OK: outside the type space (Bad [x] y)
```


The declared type space will be checked for overlap with other instances using the same linearization check that unbranched instances use.

## Concrete Proposal

- Among unbranched instances, check linearized forms of the LHS of instances when doing overlap checking. Thus, our two problematic instances of `F`, at the top, will conflict.

- After linearizing a left-hand side, the right-hand side of the instance is ill-defined. Thus, the current coincidence check (see [here](new-axioms/coincident-overlap) for more information) is no longer possible and will be removed. (Don't yell yet. Keep reading.)

- Allow coincident overlap within branched instances. This recovers the lost coincident overlap check on unbranched instances. See [here](new-axioms/coincident-overlap) for more information.

- Optional: Add type space declarations to the `type instance where` syntax, checking to make sure that branches fit within the declared space. [This page](new-axioms/type-spaces) has the details.

- Optional: Add new syntax `type family Foo a b where { Foo ... = ... ; Foo ... = ... } ` to declare a type family with one branched instance covering the entire type space. This would be a *closed* type family.

## Discussion about coincident overlap


This proposal has the net effect of forcing all uses of coincident overlap to appear in one module, instead of spread across modules. That's admittedly not great, but it's possible that no one will be negatively affected. The only alternative we've (Simon, Dimitrios, Richard) thought of is to disallow coincident overlap when the left-hand sides are non-linear, but that seems very ugly and ad-hoc.


To see why disallowing coincident overlap checking for unbranched axioms is vitally necessary, consider this example:

```wiki
type instance F a a Int = a
type instance F c d d   = Int
```


This overlap is most certainly are not OK (think about `F G G G`, where this is the malignant `G` from the beginning of this page), but they do coincide in the region of overlap. Yuck Yuck! So, we give up and just say "no" to coincident overlap in this case.

## Alternative (Non-)Solution


One way we (Simon, Dimitrios, and Richard) considered proceeding was to prohibit nonlinear unbranched instances entirely. Unfortunately, that doesn't work. Consider this:

```wiki
type family H (x :: [k]) :: *
type instance H '[] = Bool
```


Innocent enough, it seems. However, that instance expands to `type instance H k ('[] k) = Bool` internally. And that expansion contains a repeated variable! Yuck. We Thought Hard about this and came up with various proposals to fix it, but we weren't convinced that any of them were any good. So, we concluded to allow nonlinear unbranched instances, but we linearize them when checking overlap. This may surprise some users, but we will put in a helpful error message in this case.
