# Agenda

# SAT

- #18962, !4553, #9374: Only SAT unfoldings
  - Works as follows:
    1. Do SA transform in Simplifier by attaching the SAT'd defn as an unfolding
    2. Then do callSiteInline that unfolding
    3. SA analysis just before OccurAnal. Pretty simple stuff (150loc)
  - But INLINABLE will override the unfolding with a stable one (I think?). Should we SAT that unfolding? See for example `GHC.Utils.FV.mapUnionFV`. It seems that INLINABLE is quite useless, it doesn't even specialise or anything. But zapping SA info for now.
  - We can try to "solve" stream fusion this way. See [the stream-fusion paper, section 7.2 "Static Argument Transformation"](http://fun.cs.tufts.edu/stream-fusion.pdf). The key missing features:
    - Managed to optimise that example, simply by SA analysing each binding of the mutually recursive group in isolation and then taking care not make specialisable functions loop-breakers
    - But running into tick-exhaustions on `>>=` on `CmdM`, so I opened some unwanted back door. How to debug?

# Demand Analysis

- #18907 Product demands

- #18349 Trimming of DmdAnal results.
  * !3466 (merged) fixes #18304, but Andreas pointed out a shortcoming; !3558 (merged) fixes that; but we need a regression test for the latter; then close #18304
  * !3516 is a failed attempt to break the loop.
  * Sebastian thinks he has a better way to detect potentially-recursive type constructors

- #14620 join points [this comment](https://gitlab.haskell.org/ghc/ghc/-/issues/14620#note_315900)

- #19050 refactor demand lattice so that it has fewer equalities

- #19016 better syntax for demand signatures

- #14816, [this comment](https://gitlab.haskell.org/ghc/ghc/-/issues/14816#note_315980)
  - Drop `reuseEnv` in DmdAnal, check `lazy_fvs` for equality.

- #18885: Make product strictness demands relative
  - In adding hack after hack, I felt less and less confident that it works.
  - I think we only want the product demand to apply relatively, when the outer
    cardinality is lazy (e.g. 0). See
    https://gitlab.haskell.org/ghc/ghc/-/issues/18885#note_315189
    for a summary.

- #18983: absent unlifted coercions
  - Unblocked: Widen scope of RubbishLit

- #18927: Use `SmallArray#`
  - I have a handy small library now, just have to use it
  - But I got distracted by trying to solve list fusion, again...
  - Anyway: I think we want an explicit fusion framework exposed as maybe Data.Stream.FB
  - Stalled because I need a PrimOp introduced in 8.10

# Nested CPR

Main ticket: #18174, MR !1866. Blocked on #18894

- Interleave CPR and Termination analysis or not? See also https://gitlab.haskell.org/ghc/ghc/-/merge_requests/1866#note_304163
- Also my "speculation analysis" proposal. Nested CPR could be made more aggressive by looking at call sites strictness. But that probably needs a refactoring of `Demand`.

# Eta expansion

- #18993: regression in 9.1 due to eta-expansion

- #18793: Arity analysis does only very naive fixed-point iteration
  - Afterwards, we can discuss whether it makes sense to also store the results of calling `arityType` on local bindings in the signature environment, which would fix the PAP scenario in #18793
  - What does the termination analysis compute? Higher-order `exprOkForSpeculation`? What is the difference to `exprIsCheap`? They conincide for primops. Is diverging stuff cheap? Should we have `myExprOkForSpeculation` in arity analysis?

- #18231: eta expansion. Mysteries remain.
  - In particular, we wondered whether (or when, rather) `etaExpand` has to expose lambdas manifestly. Makes a difference for PAPs (special case: trivial exprs?)
  - We investigated call sites of `etaExpand` and concluded that the only call site that really needs lambdas manifestly is CoreToStg.Prep
  - On inlining PAPs: Makes sense operationally (so do it before STG), but keeping PAPs makes bindings much more likely to inline
  - (Apparently, CoreToStg.Prep has its own eta expander)
  - SPJ: "in mkLam I think it'd be fine not to eta-expand a trivial exprssion" (despite Note [Eta expanding lambdas])
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

# PmCheck

- #14422, #18277, !3959: Disattach COMPLETE pragmas from TyCons
  - This allows "polymorphic" use, for example `pattern P :: C f => f a`, `{-#
    COMPLETE P #-}`, like what we had wanted for the now extinct `LL` pattern
    of TTG
  - But doesn't actually fix #14422, which actually wants disambiguation by
    type signatures. Also a user complains there.

# Pattern-match checking

# Ideas

- Implement "smarter `CoreMap`"
- Test for "N series" (Matching over a binary tree, like the code generated by `-XDeriveGeneric`)

# Issues

- #19001: float-out and case-expressions. Quick win here?
- #17900, !3013: primop effects
- #17881, #17896: eta reduction (based on usage Demand)
- #18174, !1866: Nested CPR. See above
- #15532: Levity polymorphism and ANF
  - We talked about it with Richard and came to the understanding that it would probably work, but entail refactorings of Core to Core passes which assume they can just let-bind everything.
  - Also we shouldn't worry about it until we need it. But it's a logical next step after we have unlifted datatypes, otherwise there is no chance of code re-use.

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
  - Wait for BoxedRep !4612

# Done

- !1427: Separate CPR
- !2192: Reflect tree structure of clauses and gaurds in the syntax we check.
- #17340, !2938: Detecting redundant bangs: A new extension for LYGs! Inspires need for unlifted types (#18249)
- #18341, !3633: Strict fields are unlifted

