# Why we should split off CPR analysis from the demand analyser


This page collects some illustrative arguments. Here's the summary:


### Pro

- Demand Analysis is a backwards analysis, CPR is a forward analysis. This notion of direction stems from the order in which we analyse the parts of a `case` expression. Forward -> scrutinee first, Backward -> alts first. See [Forward vs. Backward Analysis](#forward-vs-backward-analysis)
- Separation of concerns: Strictness/usage analysis is independent of CPR, while CPR relies on strictness info to be present. Makes you ask at every line of code "Is this relevant to CPR?" + Virgin run. Other examples: IO hack (irrelvant to CPR), annotating lambda and case binders (only relevant to CPR)
- Efficiency: Running CPR as part of demand analysis means one additional virgin run for each top-level binding. Also we run CPR as part of the final demand analysis run, which only important for identifying single-entry thunks.
- Precision: See the forward vs. backward argument; no compromises to precision to satisfy both directions. Also aborting fixed-point iteration due to e.g. usage analysis also means discarding a possibly perfectly valid CPR signature.

### Cons

- Possible code duplication. Counter-argument: Could extract overlapping logic into a "projection-based analysis" skeleton, instantiate with demand/CPR
- CPR and strictness feel like they are dual to another, hence it makes sense to compute them together. Counter-argument: Strictness can be computed independent of CPR, CPR analysis needs a sound approximation of strictness info. Also the whole point about forward vs. backward analysis

## Forward vs. Backward analysis

[Joachim was probably the first to realise this](https://gitlab.haskell.org/ghc/ghc/issues/12364#note_122038), but CPR analysis is a forward analysis. That led to a reeeeeeally complicated, duplicated `Case` case in his take on nested CPR analysis: [ https://phabricator.haskell.org/D4244\#inline-35408](https://phabricator.haskell.org/D4244#inline-35408). For that reason, I feel strongly about splitting off CPR before we try this again.

Note that this isn't an issue for non-nested CPR (at least I couldn't come up with an example). But `Note [CPR in a product case alternative]` seems we already suffer in a similar way. The idea is the following:

```hs
module Foo where

f :: Int -> (Int, Int)
f !n = (n+1, n+2)
{-# NOINLINE f #-}

g :: Int -> Int
g n = case f n of
  (a, b) -> a
{-# NOINLINE g #-}
```

Imagine we'd do nested CPR. The current approach would compute a CPR signature for `f` of `m(tm(t),tm(t))`, stating that it constructs a pair of constructed (and terminating) `Int`s.

So far, so good! Now look at the call site in `g`. The demand analyser handles the `case` backwards, because there might be worthwhile strictness info to be unleashed on the scrutinee. However, that's bad for CPR when the scrutinee is an application, as the example demonstrates: At the time we see `a` in the alt of the case, we don't know that `a` really has the CPR property once we WW'd `f`. Hence, `g` itself doesn't get the CPR property, and after inlining we get:

```
$wf :: Int# -> (# Int#, Int# #)
$wf n# = (# n#+ #1, n# +# 2# #)
{-# NOINLINE $wf #-}

f :: Int -> (Int, Int)
f (Int# n) = case $wf n of
  (# a, b #) -> (Int# a, Int# b)
{-# INLINE f #-}

-- wrapper for g's strict argument omitted
g :: Int -> Int
g (Int# n) = case $wf n of
  (# a, _ #) -> Int# a
{-# NOINLINE g #-}
```

Note how `g` didn't have the CPR property and thus there will be no useful wrapper to split off (modulo strictness). Any call site of `g` matching on it's result has to go through an `Int` instead of a direct `Int#`.

What happens if we analyse the `case` expression in a forward manner instead? We first analyse the scrutinee and unleash the nested CPR signature `m(tm(t),tm(t))` of `f`. This tells us that we really pattern match on a constructed pair of constructed `Int`s. Now in the single case alternative, not only do we know that the case binder has the CPR property, but also that *the pair's components* have it, including `a`. This is enough to see that `g` has the CPR property:

```
$wf :: Int# -> (# Int#, Int# #)
$wf n# = (# n#+ #1, n# +# 2# #)
{-# NOINLINE $wf #-}

f :: Int -> (Int, Int)
f (Int# n) = case $wf n of
  (# a, b #) -> (Int# a, Int# b)
{-# INLINE f #-}

$wg :: Int# -> Int#
$wg n# = case $wf n# of
  (# a, _ #) -> a
{-# NOINLINE $wg #-}

g :: Int -> Int
g (Int# n) = case $wg n of p -> Int# p
{-# INLINE g #-}
```

`g`'s wrapper will now successfully inline at call sites and all is well.

Note that for CPR for sum types to be useful, we need at least nested CPR of depth 2, which has all the same problems ([https://ghc.haskell.org/trac/ghc/ticket/12364\#comment:3](https://ghc.haskell.org/trac/ghc/ticket/12364#comment:3)) wrt. termination and analysis "direction".

The old prototype for nested CPR in [D4244](https://phabricator.haskell.org/D4244) also suffers from the forward vs. backward issue when trying to analyse DataCon applications. Grep for `dmdAnalVarApp ::` to find the definition and notice how its own way of calling `dmdAnalStar` in `anal_con_args`. This will basically perform a backwards pass for demand information on arguments, then go back to the application head to reconstruct the DmdResult from the arguments' DmdResults. Yuck!

## Separation of concerns


Take [this binding](https://github.com/ghc/ghc/blob/a5373c1fe172dee31e07bcb7c7f6caff1035e6ba/compiler/stranal/DmdAnal.hs#L663). Is it related to strictness analysis? Or just important for CPR analysis?


Or the whole [lazy_fv business](https://github.com/ghc/ghc/blob/a5373c1fe172dee31e07bcb7c7f6caff1035e6ba/compiler/stranal/DmdAnal.hs#L1029). Do I have to pay attention/re-read related notes when all I do is hunting down a bug in CPR analysis? Assume this hack is just necessary because we do usage and strictness analysis in the same pass. Does this have side-effects on the precision of CPR analysis? I really can't tell without spending a few hours to fully understand this through reading notes and printf debugging.


Beyond better CPR, does the [weird additional non-virgin run](https://github.com/ghc/ghc/blob/a5373c1fe172dee31e07bcb7c7f6caff1035e6ba/compiler/stranal/DmdAnal.hs#L73) due to `Note [CPR for thunks]` and `Note [Optimistic CPR in the "virgin" ase]` improve any strictness or usage info or could we drop it when CPR is split off? I sure hope so (implying there's no information flowing from CPR -\> Strictness), but we can't know until we try out.


The point I'm trying to make: Any benefits to interleaving the analyses I could think of don't make up for the nasty side-effects of the interleaving. I don't see the principle behind it.


Note that the same argument probably applies to splitting usage and strictness analysis. The latter doesn't need the LetUp case (we even give up precision for that) and it necessitates the strange lazy_fv business ([yuck](https://ghc.haskell.org/trac/ghc/ticket/14816#comment:13)). But we'll leave that for another time.
