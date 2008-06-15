# GHC Plugin Phase Control

## The Problem


We need to be able to specify compiler phases to:

- Be able to specify where user phases from plugins should fit
- Control when rules and inlinings will fire


The current solution where we have phases 0, 1 and 2 is too restrictive to let us do this well, hence this page proposes a generalization that should solve our problems.


I believe the proposed system represents a significant improvement in modularity and usability of phase control within GHC.

## Solution

```wiki

module Foo where

{-# PHASE A #-}

{-# RULE "map_map" [A] map f (map g xs) = map (f . g) xs #-}

```


This code has a PHASE declaration which brings a new phase into being. Later rules then use that phase name to control their firing, in contrast to the current system of controlling firing with a limited set of phase numbers.


Phase names have the same name format as data constructors. There is no technical reason for this, as there is never any ambiguity as to whether a name is that of a phase: the choice is purely asthetic and could be changed.


Phase names are exported, so:

```wiki

module Bar where

import Foo

{-# PHASE B < A #-}

{-# RULE "unComp" [B] (f . g) = (\x -> f (g x)) #-}

```


Imports the phase A from Foo and creates its own phase, B, that must occur before A. B is in turn used to control a rule activation.


GHC would gather up the phase ordering constraints from the module it is compiling as well as all imported modules and produce a consistent topological sort. This would be used to order compiler passes and when rules may fire.


What if you want to have an internal phase name that won't clash with other peoples? Then don't export it!

```wiki

module Spqr(..., {-# PHASE C #-}, ...) where

{-# PHASE C < SpecConstr #-}

{-# RULE "silly" [~C] id = (\x -> x) #-}

```


This module explicitly exports its local phase C, which is defined to occur before the [SpecConstr](spec-constr) phase. However the programmer is totally free to remove it from the exports list and hence prevent other modules from referring to it. Likewise, you can selectively import phases:

```wiki

module Baz where

import Spqr({-# PHASE C #-})

{-# PHASE D > C #-}

{-# INLINE [~D] foo #-}

foo = ..

```

## Expressing Dependence


Assuming we just have two levels of ordering we want to express:

- Strict ordering (A MUST appear before/after B)
- Lenient ordering (A SHOULD appear before/after B)


Then a possible syntax is:

```wiki

{-# PHASE A < B, [< C], > D, [> E] #-}

```


To express that A:

- MUST appear before B
- SHOULD appear before C
- MUST appear after D
- SHOULD appear after E


The square brackets are meant to be evocative of optionality in Backus-Naur form, but I'm not yet sure if that is too easily confused with Haskell list syntax.

## Compatability Concerns


There are two principal concerns:

- Code that assumes the current phase control mechanism where we have phases 0, 1 and 2 should still work in this new system
- Compilers that are unable to parse the PHASE pragma should still be able to deal with source code that uses it


To handle these concerns, first we must provide three "wired in" phase names that support the old usage:

```wiki

module Buzz where

{-# PHASE E < 1, > 0 #-}

{-# INLINE [~0] bar #-}
{-# INLINE [E] sqpr #-}

bar = ...
spqr = ...

```


Note that actually the old syntax allowed arbitrary positive integers to be used, not just the set 0-2. However, supporting an infinite set of wired in names is a bit of a headache and I believe that the higher phase numbers were sufficiently rarely used that supporting them is not a major concern. You currently have to supply an additional flag to the compiler (to change the number of simplifier iterations) to even make the higher phases behave differently than phase 2.


The PHASEs 0, 1 and 2 will be implicitly and irrevocably imported into every program GHC compiles. A possible alternate design choice is to have them live in the Prelude, so e.g. you can get rid of them by e.g. explicitly importing the Prelude with an empty import list. This reduces backwards compatability however, and is a little trickier to implement.


Supporting compilers that do not understand the pragma is mostly easy, with the subtelty that we must not require commas between PHASE pragmas that appear in import/export lists. In my opinion we should not even accept such commas on the basis that by doing so would allow users to inadvertently write programs that do not compile on non-GHC and old-GHC compilers.


An example of how it would look is:

```wiki

module Qux({-# PHASE F #-} {-# PHASE G #-} {-# PHASE H #-}) where

import Quux({-# PHASE I #-} {-# PHASE J #-})

... PHASE declarations and uses ...

```


It turns out that we have exactly the required code for this already in the parser to deal with Haddock pragmas.
