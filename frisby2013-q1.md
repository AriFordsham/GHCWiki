
This pages serves as a public log what I did for my GHC internship from 21 Jan 2013 to 12 April 2013.

## Plan for my internship summary


Compared to [351a8c6bbd53ce07d687b5a96afff77c4c9910cc](/trac/ghc/changeset/351a8c6bbd53ce07d687b5a96afff77c4c9910cc/ghc), we implemented OPTIMIZATIONS with a cumulative effect of EFFECT on the generated code as well as EFFECT on the compiler's code. The hope is for the optimizations to have beneficial non-trivial interactions and to simplify/tidy the GHC code base.

## general core knowledge

- Max's page about code generation: really good.
- document ticky profiling
- Core -\> STG -\> CMM and _what you can learn by looking at each one_

## Late Lambda Float


LLF = Late Lambda Float


As the GHC optimization papers explain, it is an early design decision to \*not\* perform lambda lifting. My initial project was to investigate the effects of aggressively floating lambdas to the top-level at the end of the core2core pipeline.

- The main reason to not perform lambda lifting is that abstracting over free variables loses information and thereby inhibits \*downstream\* optimization.
- Doing it \*late\* (ie just before CorePrep) circumvents this issue.
- The original conjecture was that doing it would save allocation: a dynamically allocated closure becomes a static top-level function.
- Max Bolingbroke did a quick implementation of this idea some years ago (\~mid 2000s), but it seems it was abandoned. I don't know why.

### Method


We decided to implement LLF by re-using most of the [FloatOut](float-out) machinery.

[FloatOut](float-out) is structured in three phases.

1. Annotate all expressions with their free variables.
1. Consume those annotations while annotating each binder with the target "level" (essentially a depth wrt value lambdas) to which we want to float it.
1. Consume those annotations while actually relocating the bindings.


We wholesale re-use the third phase (compiler/simplCore/FloatOut) with no changes, add logic to the middle phase, and enrich the first phase with more analyses.


Most of my changes were

- Adding flags (compiler/main/DynFlags compiler/simplCore/CoreMonad compiler/simplCore/SimplCore)
- Implementing the LLF logic in the first two [FloatOut](float-out) phases (compiler/simplCore/SetLevels)
- Adding LLF to the core2core pipeline (compiler/simplCore/SimplCore)

### Overview


In order to minimize factors, I decided to float only lambdas during LLF. Thus there is no need to perform FloatIn afterwards: all of our floats are to the top-level, so there will be nothing to FloatIn.


We placed LLF as the last pass before CorePrep. After experimentation, we decided to follow it with a simplifier pass.


The basic shape of things:

```wiki
outer = CTX[let f x = RHS[x]
            in BODY[f]]
```


where `outer` is a top-level binding. LLF transforms this to:

```wiki
poly_f FVS x = RHS[x]

outer = CTX[BODY[f FVS]]
```


wbere FVS are the free variables of RHS\[x\]. We'll use `a`, `b`, `c`, ... for particular variables in FVS.


The poly prefix is vestigial: in the past, floated bindings could never cross lambdas, so the abstracted variables were only type variables. Hence the machinery that adds the new parameters was only ever adding type parameters; it was creating polymorphic functions. This scheme was not updated even when the machinery was enriched to also abstract over values.

### Background

- join points
- let-no-escape
- Note \[join point abstraction\]

### Discovered Detriments of LLF


These are the various negative consequences that we discovered on the way. We discuss mitigation below.

- Unapplied occurrences of f in BODY results in the creation of PAPs, which increases allocation. For example: `map f xs` becomes `map (f a b c) xs`. Max had identified this issue earlier.
- Abstracting over a known function might change a fast entry call in RHS to a slow entry call. For example, if CTX binds `a` to a lambda, that information is lost in the right-hand side of poly_f. This can increase runtime.
- Replacing a floated binder's occurrence (ie `f` becomes `f a b c`) can add free variables to a thunk's closure, which increases allocation.
- TODO putStr (eg sphere)

#### Mitigating PAP Creation

TODO

#### Preserving Fast Entries

TODO

#### Mitigating Thunk Growth

TODO

- easier: if f occurs inside of a thunk in BODY, then limit its free variables.
- harder: approximate the maximum number of free variables that floating f would add to a thunk in BODY, and limit that.

### Discovered Benefits of LLF


We didn't see as much decrease in allocation as we would have liked.

- nice demonstration of the basic effect in puzzle
- [\#7663](https://gitlab.haskell.org//ghc/ghc/issues/7663) - simulates having the inliner discount for free variables like it discounts for parameters
- Floating functions to the top-level creates more opportunities for the simplifier.

```wiki
CTX[case a of [p1 -> let f x = ... in case a of ...]]
```

> >
> > The let prevents the cases from being merged. Since LLF is so aggressive, it floats f when it otherwise wouldn't be.

### Miscellaneous Findings

#### Thunk Join Points


We discovered that the worker-wrapper was removing the void argument from join points (eg knights and mandel2). This ultimately resulted in LLF \*increasing\* allocation. A thunk was let-no-escape before LLF but not after, since it occurred free in the right-hand side of a floated binding and hence now occurred (escapingly) as an argument.


SPJ was expecting no such non-lambda join points to exist. We identified where it was happening (WwLib.mkWorkerArgs) and switched it off.

TODO the result?
