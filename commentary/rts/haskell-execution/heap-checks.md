# Heap and Stack checks


## General overview

When allocating a heap object, we bump `Hp` and compare to `HpLim`. If the test fails we branch to one of the helpers in `rts/HeapStackCheck.cmm`.  Usually this code tests an interrupt flag (to see if execution should be brought tidily to a halt); grabs the next block of allocation space; makes `Hp` point to it and `HpLim` to its end; and returns.  If there are no more allocation-space blocks, garbage collection is triggered.

Source files: [rts/HeapStackCheck.cmm](https://gitlab.haskell.org/ghc/ghc/blob/master/rts/HeapStackCheck.cmm)

-------

## Improving Heap Check Placement

Deciding whether to a put heap in case alternative branch is quite a delicate process.  

**Relevant Tickets:**

  * Improving Placement of Heap Checks - Avoiding Slowdowns in Hot Code #16064
  * Optimization: eliminate unnecessary heap check in recursive function #1498
  * Place heap checks common in case alternatives before the case #8326. There is good discussion in this ticket.
  * Eliminate redundant heap allocations/deallocations #12231
  * Extending the idea to stack checks: [this comment](https://gitlab.haskell.org/ghc/ghc/issues/14791#note_150481) in #14791
  * Optimisation: eliminate unnecessary heap check in recursive function #1498

### Current strategy 

The `GcPlan`
datatype from [GHC.StgToCmm.Expr](https://gitlab.haskell.org/ghc/ghc/-/blob/master/compiler/GHC/StgToCmm/Expr.hs) embodies this decision. Given this program:

```
  f = \x -> let y = blah
            in case <scrut> of
                  0#      -> <rhs1>
                  DEFAULT -> <rhs2>
```
Things that affect this decision:
 * If the scrutinee is a boolean condition then do the heap check before - See `Note [GC for conditionals]`.
 * If the scrutinee `<scrut>` requires any non-trivial work, we MUST `GcInAlts`. For example if 
   `<scrut>` was `(g x)`, then calling `g` might result in lots of allocation, so any heap check done 
   at the start of `f` is irrelevant to the branches. They must do their own checks.
 * If there is just one alternative, then it's always good to do the check before the `case`.
 * If there is heap allocation in the code before the case, then we are going to do a heap-check 
   upstream anyway. In that case, don't do one in the alternatives too. The single check might 
   allocate too much space, but the alternatives that use less space simply move `Hp` back down 
   again, which only costs one instruction (TODO: Does it do it now? Check it)
 * Otherwise, if there is no heap allocation upstream, put heap checks in each alternative. The reasoning
   here was that if one alternative needs heap and the other one doesn't we don't want to pay the 
   runtime for the heap check in the case where the heap-free alternative is taken.

This decision is being made in the code that compiles case expressions in [GHC.StgToCmm.Expr](https://gitlab.haskell.org/ghc/ghc/-/blob/master/compiler/GHC/StgToCmm/Expr):

```haskell
       ; let do_gc  | is_cmp_op scrut  = False  -- See Note [GC for conditionals]
                    | not simple_scrut = True
                    | isSingleton alts = False
                    | up_hp_usg > 0    = False
                    | otherwise        = True
```

If `do_gc` is True, we put heap checks at the start of each branch. If `do_gc` is False, we take the max of the branches, and do the heap check before the `case`. See `Note [Compiling case expressions]` in[GHC.StgToCmm.Expr (https://gitlab.haskell.org/ghc/ghc/-/blob/master/compiler/GHC/StgToCmm/Expr.hs).

---


### Case Study

Now let's consider this program:

```haskell
data X a = X { runX :: !a }

slow :: Int -> Int
slow = \ !i -> runX (go i) where
    go = \ !i -> if i > 0
        then go $! i - 1
        else X i
```

Currently we put a heap check before the `case`. In that particular scenario we do that because
our scrutinee is a PrimOp (see `Note [GC for conditionals]`). The advantage of a single heap check will
do for all alternatives that allocate, which may save code space. (A heap check takes quite a few
instructions). 

Unfortunately, this means that all these heap checks will make our program run slower. Our goal is to get the heap check out of the "hot" path - we would like to put it in alts.

In this particular case we fall into the first branch because our program uses comparison operator (See `Note [GC for conditionals]`).

We can push the heap check into the branch by removing the first guard in `do_gc`:

```
diff --git a/compiler/GHC/StgToCmm/Expr.hs b/compiler/GHC/StgToCmm/Expr.hs
index eb56a6ad09..2265876a4d 100644
--- a/compiler/GHC/StgToCmm/Expr.hs
+++ b/compiler/GHC/StgToCmm/Expr.hs
@@ -430,8 +430,7 @@ cgCase scrut bndr alt_type alts
        ; let ret_bndrs = chooseReturnBndrs bndr alt_type alts
              alt_regs  = map (idToReg platform) ret_bndrs
        ; simple_scrut <- isSimpleScrut scrut alt_type
-       ; let do_gc  | is_cmp_op scrut  = False  -- See Note [GC for conditionals]
-                    | not simple_scrut = True
+       ; let do_gc  | not simple_scrut = True
                     | isSingleton alts = False
                     | up_hp_usg > 0    = False
                     | otherwise        = True
```

We will refer to this patch as `GC_IN_ALTS`.

| HEAD | GC_IN_ALTS |
| ------ | ------ |
| [Generated Cmm](uploads/01cba099560bdd650e3fc4ebc2b219a4/T16064.dump-cmm-HEAD) | [Generated Cmm](uploads/af64f3bb565f0494f2054813b8b9de33/T16064.dump-cmm-GC_IN_ALTS) |
| [Compiling Cabal (Cachegrind stats)](uploads/02bb81013526792b56d0f311956d2d17/cabal-cachegrind-stats-summary-HEAD) | [Compiling Cabal (Cachegrind stats)](uploads/6997d85fe4d7950cb30cd1d2cae0cfc4/cabal-cachegrind-stats-summary-GC_IN_ALTS) |
| [Compiling Cabal (Compiler RTS)](uploads/10b00658dc04d15a77de2aee8dc6d38b/cabal-stats-HEAD) | [Compiling Cabal (Compiler RTS)](uploads/e40570d331453aa7e1f21145dd56bfcb/cabal-stats-GC_IN_ALTS) |
| [Ticky](uploads/73a8ec5e79476f7eb3d17ac2d20590cd/HEAD.ticky) | [Ticky](uploads/7c4b328af52d58c24ed690e4270cbf47/GC_IN_ALTS.ticky) |

| Nofib |
|-------|
|[Run#1](uploads/dfd13b166159e3ae1484f1b7adbcf35b/nofib-analyse-Run_1)|
|[Run#2](uploads/f3571cd85fbb1561c8b3cb91f24125e5/nofib-analyze-Run_2)|


### New Proposed Strategy

It should be possible to decide when to get the heap check out of the "hot" path when:

1. A primitive case with no allocation upstream from it.
1. One alternative that performs no allocation.

Under such circumstances the stack/heap checks can be moved from being performed on every iteration to being performed just prior to the allocation itself.

The idea of the new strategy is:
* If one or more of the case alternatives does not allocate,
* AND upstream does not allocate
* THEN put the heap check in the alternatives

Knowing whether it allocates is pretty simple. Perhaps we could also accurately predict how much it 
allocates, which would reduce the tricky getHeapUsage plumbing in the `FCode` monad. That is quite an 
attractive thought too... The basic idea is that 
 * `Let` allocates
 * `ConApp` (Constructor Application) allocates
 * `OpApp` (Primitive Op/foreign call) allocates if it uses `allocHeapClosure` from [GHC.StgToCmm.Heap](https://gitlab.haskell.org/ghc/ghc/-/blob/master/compiler/GHC/StgToCmm/Expr.hs) 
 * `Case` allocates if the scrutinee allocates; otherwise doesn't allocate if `scrut` is not "simple" 
   (because then evaluating scrut might call unknown code), otherwise does allocate if any branch 
   allocates (and this is where our newly added flag comes in handy)
 * ... And nothing else allocates!

## Implementation Plan

The idea would be to attach to each case alternative a single boolean saying "does this alternative 
allocate?".  Then the code generator could consult that boolean to decide the GCPlan, and we
could try different GC plans very easily.

1) Augment GenStgAlt with extra boolean flag to carry "does it allocate" info
2) Before codegen, in `annTopBindingsFreeVars`, determine for each case alternative in STG
whether it allocates.
3) During code generation use that info, plus info about whether any allocation has taken place prior to 
the case, to decide on the `GcPlan`.

Tricky bit: even if there are multiple branches that allocate, if the branches that do not are the 
hot ones, we still might want to duplicate that heap check into the cold branches. We don't know 
which branches are hot, so we'll have to make do with an approximation.


### Notes of Interest

  * [Heap checks]
  * [Compiling case expressions]
  * [scrut sequel]
  * [Inlining out-of-line primops and heap checks]
  * [alg-alt heap check]
  * [Stack usage]
  * [Single stack check]

[CategoryStub](category-stub)