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


The `GcPlan`
datatype from [GHC.StgToCmm.Expr](https://gitlab.haskell.org/ghc/ghc/-/blob/master/compiler/GHC/StgToCmm/Expr.hs) embodies this decision. Given this program:

```
  f = \x -> let y = blah
            in case <scrut> of
                  0#      -> <rhs1>
                  DEFAULT -> <rhs2>
```
Things that affect this decision:

 * If the scrutinee `<scrut>` requires any non-trivial work, we MUST `GcInAlts`. For example if 
   `<scrut>` was `(g x)`, then calling `g` might result in lots of allocation, so any heap check done 
   at the start of `f` is irrelevant to the branches. They must do their own checks.
 * If there is just one alternative, then it's always good to amalgamate
 * If there is heap allocation in the code before the case, then we are going to do a heap-check 
   upstream anyway. In that case, don't do one in the alternatives too. The single check might 
   allocate too much space, but the alternatives that use less space simply move `Hp` back down 
   again, which only costs one instruction
 * Otherwise, if there is no heap allocation upstream, put heap checks in each alternative. The reasoning
   here was that if one alternative needs heap and the other one doesn't we don't want to pay the 
   runtime for the heap check in the case where the heap-free alternative is taken.

---

Now let's consider this program:

```
data X a = X { runX :: !a }

slow :: Int -> Int
slow = \ !i -> runX (go i) where
    go = \ !i -> if i > 0
        then go $! i - 1
        else X i
```

This will produce the following Cmm(with `-O`):

```
[$wgo1_r2zB_entry() { //  [R2]
     { ... }
     {offset
       c2BJ:  // get argument
           _s2zT::I64 = R2;
           goto c2BB;
       c2BB: // <------------------ HEAP CHECK
           Hp = Hp + 16; 
           if (Hp > HpLim) (likely: False) goto c2BN; else goto c2BM;
       c2BN: // GC
           HpAlloc = 16;
           R2 = _s2zT::I64;
           R1 = $wgo1_r2zB_closure;
           call (stg_gc_fun)(R2, R1) args: 8, res: 0, upd: 8;
       c2BM: // i > 0
           if (%MO_S_Le_W64(_s2zT::I64, 0)) goto c2BH; else goto c2BI;
       c2BH: // <------------------ ELSE branch - return here
           I64[Hp - 8] = GHC.Types.I#_con_info;
           I64[Hp] = _s2zT::I64;
           R1 = Hp - 7;
           call (P64[Sp])(R1) args: 8, res: 0, upd: 8;
       c2BI: // true branch (hot path)
           Hp = Hp - 16;
           _s2zT::I64 = _s2zT::I64 - 1;
           goto c2BB;
     }
 }
```

Currently we put a heap check before the `case`. In that particular scenario we do that because
our scrutinee is a PrimOp (see `Note [GC for conditionals]`). Advantage: a single heap check will
do for all alternatives that allocate, which may save code space. (A heap check takes quite a few
instructions). There is an illuminating discussion in `Note [Compiling case expressions]` in 
[GHC.StgToCmm.Expr](https://gitlab.haskell.org/ghc/ghc/-/blob/master/compiler/GHC/StgToCmm/Expr.hs).

Our goal is to get the heap check out of the "hot" path - we would like to put it in the `DEFAULT`
branch only; the 1# branch does not allocate.

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
allocate (at all)?".  Then the code generator could consult that boolean to decide the GCPlan, and we
could try different GC plans very easily.

1) Augment GenStgAlt with extra boolean flag to carry "does it allocate" info
2) Before codegen, perhaps in `annTopBindingsFreeVars`, determine for each case alternative in STG
whether it allocates.
3) During codegen, use that info, plus info about whether any allocation has taken place prior to 
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