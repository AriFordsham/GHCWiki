# GHC Plugin Phase Control

## The Problem


We need to be able to specify compiler phases to:

- Be able to specify where user phases should fit
- Control when rules fire

## Possible Solution

```wiki

module Foo where

{-# PHASE A #-}

{-# RULE "map_map" [A] map f (map g xs) = map (f . g) xs #-}

```


This code has a PHASE declaration which brings a new phase into being. Later rules then use that phase name to control their firing, in contrast to the current system of controlling firing with a limited set of phase numbers.


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


This module explicitly exports its local phase C, which is defined to occur before the [SpecConstr](spec-constr) phase. However the programmer is totally free to remove it from the exports list and hence prevent other modules from referring to it.
