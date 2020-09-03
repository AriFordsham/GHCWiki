# Agenda

## State hack/Eta expansion

- #18231: eta expansion. Mysteries remain.
  - In particular, we wondered whether (or when, rather) `etaExpand` has to expose lambdas manifestly. Makes a difference for PAPs (special case: trivial exprs?)
  - We investigated call sites of `etaExpand` and concluded that the only call site that really needs lambdas manifestly is CoreToStg.Prep
  - On inlining PAPs: Makes sense operationally (so do it before STG), but keeping PAPs makes bindings much more likely to inline
  - (Apparently, CoreToStg.Prep has its own eta expander)
  - SPJ: "in mkLam I think it'd be fine not to eta-expand a trivial exprssion" (despite Note [Eta expanding lambdas])
- #18154: CPR for data con apps.  More to do here.
  - SPJ: Suggestion: don't produce CPR info for NOINLINE zero-arity things, OR for zero-arity expandable things
- #18202, #18238: state hack
  - We don't care to preserve one-shot-ness in the compiler. But it's also only use site info, so that should be fine
  - e.g. `exprEtaExpandArity` only returns `Arity`, not `ArityType`, and so on
  - Also eta reduction (for e.g. trivial expressions) loses one-shot-ness
  - Idea: Preserve one-shot-ness in the pipeline (for eta contraction in particular!), only do full eta contraction in CorePrep
  - Thought: one-shot is like call-by-name, cf. "Kinds are Calling conventions", then `multiShot (a ~> b) -> (a -> b)` and `oneShot` other way around
  - We floated around the idea of having an explicit `MultiShot` annotation for lambdas, but that isn't effective
    - Think about `multiShot expensive ==> let f = expensive in \x{ms}. f x`. What if `expensive = \y{os}. e`? Then we will inline into `e`! Bad
  - Fundamental: `let f y{ms} = e in \x{os}. f x`
    - eta reduce, then inline ==> `\y{ms}.e`
    - inline, then beta reduce ==> `\x{os}.e[x/y]`
    - In some situations, we want one, in some we want the other!
    - Idea: No eta reduction whenever there's os or ms, only if there is no annotation

## PmCheck

- #18565, !3937: Guard tree variants following syntactic structure
  - No type-unsafe panics, so an improvement, I think
- #18341, !3633: Strict fields are unlifted
  - Performance troubles with lazy constructor pattern guards in T12227, so
    kept what we had. Now it's even decreasing there
- #14422, #18277, !3959: Disattach COMPLETE pragmas from TyCons
  - This allows "polymorphic" use, for example `pattern P :: C f => f a`, `{-#
    COMPLETE P #-}`, like what we had wanted for the now extinct `LL` pattern
    of TTG
  - Nice -20% metric decrease in #18478!
- #18645, #17836, !3971: Incremental `tcCheckSatisfiability` API
  - TODO;
- #17340, !2938: Detecting redundant bangs: A new extension for LYGs! Inspires need for unlifted types (#18249)
- #18249: Support for unlifted types in PmCheck
  - Solution: Add PmCtNotBot at *binding sites*
  - We lack a way to identify them reliably, because we didn't need to
  - It's the same mechanism we use for strict fields: Add an unliftedness constraint when brought into scope (Gamma)
  - So we have to add constraints when
    1. we start the pattern match checker and we initialise Deltas, for the match variables
       (Basically any free variable, really! But I don't think we care about these until they are matched upon.)
    2. We `checkGrdTree` a `PmLet` guard for the bound thing
    3. We add a `PmCon` guard for the field bindings
  - The latter case overlaps with what we do for strict fields in the oracle
    (`mkOneConFull` etc.). Now we should also do the same for unlifted fields.
    It feels wrong to duplicate the logic between the checker invokation, `checkGrdTree` and the oracle.
    Although there is nothing more elementary

## Misc

- #17900, !3013: primop effects
- #17881, #17896: eta reduction (based on usage Demand)

- #18174, !1866: Nested CPR. See below

# Nested CPR

Main ticket: #18174, MR !1866.

- #18154: CPR of DataCon wrappers (see above)
- `divergeCpr` is strange, but probably correct  
  - We never really use `botCpr` (no well-typed expr will ever have this as a
    denotation!), thus we don't export it.
- CPR transformers based on strictness signatures. This is the logical
  extension of `Note [CPR for binders that will be unboxed]`. Specifically
  > It's sound (doesn't change strictness) to give it the CPR property
  > because by the time 'x' is returned (case A above), it'll have been
  > evaluated (by the wrapper of 'h' in the example).


# Pattern-match checking

## Patches

Those with an MR actually have code.

- Clean up `provideEvidence`, define `ensureInhabited delta = null <$> provideEvidence 1 delta`
  - !1975 featured a rewrite, which makes for better warning messages
  - But `provideEvidence` currently assumes that every COMPLETE set is inhabited, and thus implicitly assumes that `ensureInhabited` is true for that data type. So we can't actually just re-define it in terms of the other just yet.
  - `provideEvidence` currently picks the smallest residual COMPLETE set for reports. But it doesn't consider type information! So it may indeed happen that we pick a residual COMPLETE set that looks smaller (say, size 2) and is still inhabited in favor of one that disregarding type info looks bigger (size 3) but actually only 1 is inhabited. For the same reason, we can't use `provideEvidence` as a replacement for `ensureInhabited`.
  - It *is* possible to make `provideEvidence` behave appropriately to replace `ensureInhabited`, but it's not very efficient. Also `ensureInhabited` is entirely orthogonal to what `provideEvidence` does. Think of recursive data types, for example: `provideEvidence` doesn't attempt to recurse *at all*. It just doesn't make for good warning messages.

- #17378, !1765: Preserve non-void constraints  
  - Should not remove inhabitation candidate stuff just yet, newtypes...
  - Perhpas postpone test until get to RHS (pmc []), and then ask for `not (null (provideEvidence 1 delta))`

- Implement "smarter `CoreMap`"
- Test for "N series" (Matching over a binary tree, like the code generated by `-XDeriveGeneric`)

## Issues

- #15532: Levity polymorphism and ANF  
  - We talked about it with Richard and came to the understanding that it would probably work, but entail refactorings of Core to Core passes which assume they can just let-bind everything.
  - Also we shouldn't worry about it until we need it. But it's a logical next step after we have unlifted datatypes, otherwise there is no chance of code re-use.

## Epics

- Think about how to fix "regression" in T11822  
  - SG bets a smart `CoreMap` would do
- Maybe pattern-match check typed TH quotations? SG doesn't think this is a good idea, because they might not even end up in that form in spliced code.
- Can we check if a clause is uniform? E.g. can be moved around (more or less) freely, up or down.  
  - I think we can, by trying to move up the clause and see if its new Covered set has a non-empty intersection (e.g. overlaps) with the clause that was previously there. Example:  
    ```haskell
    data T = A | B | C
    f (Just False) = ()
    f Nothing      = ()
    f (Just _)     = ()
    ```
    If we try to move the third clause up once, it covers the left over `Just True`. The second clause covers `Nothing`, so  the two clauses don't overlap. If we try to move up the third clause to the top, it suddenly covers `Just _`, which overlaps with `Just False` from the actual first clause. So we may switch second and third clause but not move the third clause to the top.
  - This is very similar to redundancy checking, but in redundancy checking we see if we *completely* overlap the pattern. Here, we see if their Covered sets overlap *at all* instead of seeing if one completely covers the other.
  - I suppose this also has tricky interactions with bottom. But our existing machinery should cover it.

## Roadblocks 

- T9291: unsafePtrEquality checks for STG CSE  
  - `bar x = (Right x, Right x)` gets CPR'd, `$wbar x = (# x, x #)` can't CSE at call site. Fixed testcase with `lazy`, but is that the right thing to do?
- integerConstantFolding: `CONSTANT_FOLDED` on `decodeDoubleInteger`, but gets WW'd because of nested `Int64`
  - We can't really just return the unboxed Int#, because that's platform dependent. BUT we could return Int64# instead
  - Alternatively, inline `decodeIntegerDouble` and recognise the PrimOp, seems like the much saner behavior?!

# On hold

- https://gitlab.haskell.org/ghc/ghc/tree/wip/ext-arity: Rebased Zach's implementation of the extensionality paper  
  - Wait for levity polymorphism and matchability polymorphism to work out

- #915: Specialisation through type classes/defunctionalisation
  - #17592: Specialisation for call patterns is quite unreliable:
    ```hs
    f :: Maybe Bool -> Int -> Int
    f (Just True) 0 = 1
    f (Just True) n = f (Just True) (n-1)
    f _ 0 = 2
    f x n = f x (n-1)
    
    g x n = f (Just x) n
    h n = g True n
    ```
    There are situations in which `g` has not been inlined into `h` by the time SpecConstr runs. SpecConstr will then create two specialisations: One for `(Just True, _)` (`f1`) and one for `(Just _, _)` (`f2`), the former of which is a strict specialisation of the latter. The simplifier will then rewrite the call site in `g` to `f2`. Now, at some point `g` will be inlined and we see the call site `f2 True n`, which we *could* rewrite to `f1`. But all specialisation rules only apply to `f`, so we can't do the rewrite. The solution is simply to attach a derived specialisation rule to `f2`.
  - (Obsolete) Why not do specialisation of recursive functions instead of inlining them, as part of the simplifier? Consider separate pragma `{-# SPECIALISABLE #-}` or something
  - Pros:
    - No complicated and brittle reliance on rewrite rules
    - Like `INLINE`, the pragma is persisted throughout transformations
    - It seems like the logical way to do inlining for recursive functions
  - Cons:
    - Probably quirky for complicated recursion schemes
    - How does this work for rewriting recursive call sites? Seems impossible without RULEs and thus SpecConstr. OK, that won't work

- https://github.com/ghc-proposals/ghc-proposals/pull/43 Or patterns: Potential bachelor's thesis?
  - osa1 ultimately after a long and tedious discussion gave up.
  - Why? What's needed? A formal Specification? Which part? Static or dynamic semantics?
  - Also how much? Whole pattern language or just enough of a fragment to explain or patterns?
  - I see there is https://gitlab.haskell.org/rae/haskell as a starting point, but it seems to focus entirely on static semantics. But probably the document to complete?
  - We talked about it; it's a matter of pushing the proposal forward rather than investing actual elbow grease into an impl.

- !2218 Unlifted data types
  - Wait for BoxedRep !2249

# Done

- !1427: Separate CPR
- !2192: Reflect tree structure of clauses and gaurds in the syntax we check.
