# Agenda

- Paper 
  - Outline:
    - Introduction
    - Problem statement  
      - Complexity of previous impl, maintenance burden
      - No mixing of pattern guards and pattern matching (can be combined with the following point)
      - Bad story for guards in general
      - (Laziness)/Bang patterns/strict fields
      - (Overloaded) literals
      - Pattern synonyms + COMPLETE pragmas
      - New implementation has favorable efficiency, graceful degradation for predictable performance
      - Unsatisfying story around Newtype pattern matches (they are not strict)
      - Long distance info
      - (And all other stuff that was solved as part of GADTs MTM, like type info) 
    - Our solution
      - (GADTs MTM does this by first introducing the concepts intuitively by way of examples, and then giving the formalism in the next chapter. Maybe we should do that, too?)
      - Make a Figure 1
      - Desugaring of source syntax pattern matching to simple guard tree language
      - Pattern-match checking as a function on guard trees, producing uncovered patterns and clause tree annotated with redundancy info
      - Constraint solving/generating inhabitants
    - Possible extensions:
      - Long distance info
      - CoreMap/semantic equality (Just add additional constraint and widen domain of \rep{\Phi})
      - Pattern synonyms + COMPLETE pragmas
      - (Overloaded) literals
      - Newtypes through coercions
      - Maybe regard type info as an extension, too?
    - Implementation
      - Interleaving U and A, skip Delta and add to nabla directly, so that we share work for checking the incoming nabla
      - Throttling for graceful degradation (needs DNF of nablas from previous hack, otherwise actual complexity not easily predictable), argue for soundness
    - Evaluation
      - Check a couple of packages
      - Issues closed?
    - Related Work
      - GADTs MTM (mine their Related Work)
      - Maranget's work; tries to account for laziness, but wrongly so
      - Compare to "Elaborating dependent (co)pattern matching", which is essentially GADTs MTM with more type foo going on
      - Compare to refinement types
      - Compare to "Structural and semantic pattern matching analysis in Haskell", which uses an SMT solver as the oracle in the GADTMTM formalism. We could extend nabla with reasoning about booleans and arithmetic to achieve something similar

- #17676: Consolidate when to apply IO hack. Maybe rename it  
  - Does it make sense to re-use `expr_ok`? It returns `False` for `Tickish` things, for example.
  - Also the check for the right type is already pretty sensitive and will rule out many false positives.

- Newtypes and ⊥ constraints: Is `⊥ ~ NT _` `Disjoint` or `PossiblyOverlap`ping? Probably the latter.
  - But we only ever add `x ~ ⊥` when checking for divergence, after which don't pass the resulting Delta on. Thus we never have to preserve it, because there is no way we would add `x /~ ⊥` *after* we added `x ~ ⊥`.
  - The other way round is very much possible, though. So we need to preserve `x /~ ⊥`. But for newtypes, satisfiability of that constraint depends on the field. so if we have `x ~ NT y`, `x /~ ⊥`, we might not detect that `y ~ ⊥` is impossible! #17725
  -  I think we should rather store the coercion in the `SharedIdEnv`
  - Problem: How to solve `x ~ y` when `r1 <| co1` represents `x` and `r2 <| co2` represents `y`?

- !1427:
  - What is there left to do? For review:
      1. Show `Cpr` module
      2. See how it simplifies `Demand`, but also means we have the bottom of `Divergence` separate from the bottom of `Cpr`
      3. No need for the `TopLevelFlat` in `DmdAnal` anymore. Also gets rid of some trimming logic, which ends up in `CprAnal`. TODO: There is probably room left for more simplification to `DmdAnal`
      4. `WorkWrap` functions need to consider `Divergence` and `CprResult` separately

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

- #17673: Eta-expansion, WW and NOINLINE, #17690: WW for coercions
  - Far future: Have one unified WW for eta expansion (based on `CoreArity`), coercions (no analysis info needed) and unboxing (Strictness/CPR)?

- https://github.com/ghc-proposals/ghc-proposals/pull/43 Or patterns: Potential bachelor's thesis?
  - osa1 ultimately after a long and tedious discussion gave up.
  - Why? What's needed? A formal Specification? Which part? Static or dynamic semantics?
  - Also how much? Whole pattern language or just enough of a fragment to explain or patterns?
  - I see there is https://gitlab.haskell.org/rae/haskell as a starting point, but it seems to focus entirely on static semantics. But probably the document to complete?

- !2218 Unlifted data types

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

- `data PmCt = TyCt .. | TmCt ...`; then `addPmCt :: Delta -> PmCt -> DsM Delta`  
  - Type evidence only in bulk lists, so `addPmCt` doesn't really make sense. It should be `addPmCts`, taking a bag of `PmCt`s.

- When we have `newtype Deltas = Deltas (Bag Delta)`, we can actually use the Covered set as a `Delta` for "long distance info"  
  - No more just positive info and the `computeCovered` duplication can go away
  - Performance-wise, we should be fine with !1752

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

# Separate CPR

- Wiki page: https://gitlab.haskell.org/ghc/ghc/wikis/nested-cpr/split-off-cpr  
  - Collect proper arguments
  - Include the example from https://gitlab.haskell.org/ghc/ghc/merge_requests/1427#note_214697
- !1427: Splits-off CPR analysis from DmdAnal, but still needs work  
  - Delete CPR stuff from DmdAnal
  - Encode `Arity` in CPR signature
  - Measure compile-time impact

# Nested CPR

- Mostly working prototype at https://gitlab.haskell.org/ghc/ghc/tree/wip/nested-cpr-2019

## Roadblocks 

- Nested CPR of DataCon wrappers needs to look at RHS of wrapper (think of `data T = T !(Int, Int)`) 
- Do nested CPR for KindReps/TypeReps/Modules? Horrendeous signatures in T7360 and T8274, but stats only improve overall
- T9291: unsafePtrEquality checks for STG CSE  
  - `bar x = (Right x, Right x)` gets CPR'd, `$wbar x = (# x, x #)` can't CSE at call site. Fixed testcase with `lazy`, but is that the right thing to do?
- integerConstantFolding: `CONSTANT_FOLDED` on `decodeDoubleInteger`, but gets WW'd because of nested `Int64`
  - We can't really just return the unboxed Int#, because that's platform dependent. BUT we could return Int64# instead
  - Alternatively, inline `decodeIntegerDouble` and recognise the PrimOp, seems like the much saner behavior?!

# On hold

- https://gitlab.haskell.org/ghc/ghc/tree/wip/ext-arity: Rebased Zach's implementation of the extensionality paper  
  - Wait for levity polymorphism and matchability polymorphism to work out

# Done

- !2192: Reflect tree structure of clauses and gaurds in the syntax we check.
- #17270: `Origin` annotations should be consistent about TH  
  - Faintly related: #14838, #14899. Should we warn about TH? Probably guard it behind a flag. Off by default? SG thinks so. This code is generated potentially in another library by a different user. Also compiler performance
- #17248, #17376, !1975: Get rid of the special case for `-XEmptyCase`. Fix handling of newtypes because of testsuite failures. Some ground work for proper non-void constraint handling.
- #17357: Fix strictness of pattern synonyms. We came to the agreement that it's not worth the trouble and most useful pattern synonyms are strict anyway.


