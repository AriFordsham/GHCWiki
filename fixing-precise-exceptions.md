See the [root page for precise exceptions](https://gitlab.haskell.org/ghc/ghc/-/wikis/exceptions/precise-exceptions).

# Fixing precise exceptions in strictness analysis

Precise exception semantics are really sensitive to transformations changing evaluation order. The prime example of a transformation that changes evaluation order is the worker/wrapper transformation, which feeds on information from strictness analysis to turn call-by-need into call-by-value.

## Problem

#13380 shows that we can't treat precise exceptions as just any kind of divergence wrt. strictness analysis. It turns out that we were eagerly evaluating a variable (`y`) that we shouldn't actually be strict in as per precise exception semantics. 

Here we describe the measures taken to fix that ticket (along with #148, #1592 and #17676).

## Solution: Make `defaultFvDmd` of `raiseIO#` lazy to preserve precise exceptions, hackily

This was implemented in !2956.

`raiseIO#` used to have a `Divergence` of `botDiv`. This means that it is strict in any free variable (such as `y`) and easily fixed by giving it `topDiv`.

But that leads to a lot of dead code when a `raiseIO#` appliction occurs as a case scrutinee, as the Simplifier fails to eliminate `raiseIO#`'s continuation (e.g. its alts) as it could before. There's a simple solution: Treat `raiseIO#` specially in the simplifier, so that we drop its continuation although it has `topDiv`. That's the hack.

So all that really needs to be done to fix #17676 and #13380 is
1. Change `raiseIO#` to have `topDiv`
2. Give it special treatment in `mkArgInfo`, treating it as if it had `botDiv`.

## Replacing hacks by principled program analyses

This was implemented in !3014.

### Dead code elimination for `raiseIO#` with `isDeadEndDiv`, introducing `ExnOrDiv`

Special casing on `raiseIO#` in the Simplifier is gross, and only needed because it now has `topDiv` for its lazy default free variable demand (`defaultFvDmd`). We can fix that by introducing `ExnOrDiv` to `Divergence`, denoting that evaluation will diverge(, throw an imprecise exception) or throw a precise exception, but surely never converge. The `defaultFvDmd` of `exnDiv` then is as lazy as for `topDiv`. But entering an expression for which we infer such a `Divergence` will never return, thus is a dead end. Thus we rename `isBotDiv` to `isDeadEndDiv`, similarly all functions that use it and can delete the special case for `raiseIO#` in `SimplUtils.mkArgInfo`.

The only analysis that I recognise plays a little fast and loose with `exnDiv` vs. `botDiv` probably is `CoreArity` (which will turn `exnDiv` into `botDiv` in `exprBotStrictness_maybe`), but I guess we'll fix that when it has bitten us.

### Turn the "IO hack" into the proper analysis it should have been, introducing `ConOrDiv`

(In hindsight, this improvement is quite independent of #17676 and #13380, but similarly revolves around precise exceptions)

[The "IO hack"](https://gitlab.haskell.org/ghc/ghc/-/blob/5ac04eed98056e82d9648c39bacd477aac8b49ff/compiler/GHC/Core/Utils.hs#L1036) (which should rather be called `scrutineeMayThrowPreciseException`) is incredibly imprecise (as a program analysis) because it's so syntactic and just assumes that any composite `IO` action throws a precise exception. At the same time, it is unsound: For example, it will only recognise `IO` happening when the `case` matches on `(# State# RealWorld, a #)`, not `State# RealWorld` (an action ultimately calling `writeMutVar#`) or `(# State# RealWorld, Int#, Int#, Int# #)` (an action ending in `threadStatus#`). I imagine that when we have nested CPR, there will be a lot more variants of these unboxed tuples returning a `State# RealWorld` token.

We can easily be more precise by extending the `Divergence` lattice with `ConOrDiv`, signifying that an expression may diverge(, throw an imprecise exception) or converge, but not throw a precise exception. Every converging primop except `raiseIO#` would have this new `conDiv`. Thus we can see whether an expression may throw a precise exception by checking its inferred demand type.

As for soundness: It turns out that such an analysis, while sound, is too imprecise and would give `error` `topDiv`, ironically inferring that it might throw a precise exception (see https://gitlab.haskell.org/ghc/ghc/-/merge_requests/2525#note_260430). Thus, to be useful, we have to make the assumption that only `IO` code can throw a precise exception (so we disregard `unsafePerformIO` and `realWorld#`). Or, more specifically, any code that constructs a `State RealWorld#` token. This analysis is now done by [`forcesRealWorld`](https://gitlab.haskell.org/ghc/ghc/-/blob/28ed3fb4fed153f97237600c2839d76d6de0f701/compiler/stranal/DmdAnal.hs#L345), which is consulted in the new [`mayThrowPreciseException` check](https://gitlab.haskell.org/ghc/ghc/-/blob/28ed3fb4fed153f97237600c2839d76d6de0f701/compiler/stranal/DmdAnal.hs#L334). And whenever we annotate a strictness signature, we [try to clear the exception flag](https://gitlab.haskell.org/ghc/ghc/-/blob/28ed3fb4fed153f97237600c2839d76d6de0f701/compiler/stranal/DmdAnal.hs#L369), so that the precise exception "taint" is contained as much as possible. Why is that? Consider

```hs
let err = error "boom" -- has topDiv
in case writeMutVar# var err of s -> x
```

Is this strict in `x`? I'd Yes, very much! But considering that we fail to prove (for the above reasons) that the `err` can't throw a precise exception, without our measures to limit taint based on type (which clears `err` to `conDiv`), the answer produced by the analysis will be No: Although `writeMutVar#` in itself doesn't throw a precise exception, it *might* evaluate its argument, which has `topDiv`. This taints the whole IO computation and we come out lazy in `x`.