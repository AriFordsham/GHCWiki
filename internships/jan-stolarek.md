# Jan Stolarek's internship notes

# Loopification


Some points made by Geoffrey:

- In the past, LLVM could not recognize all loops output by the LLVm back end as loops. Perhaps that has changed.
- Answering the question "What does loopification do that isn't already being done?" would still be useful
- So figuring out how to make LLVM recognize more loops would be good.
- if you write a simple, tight loop in Haskell of the sort a C compiler would vectorize, will LLVm vectorize it? If not, why?


Some points made by Austin Seipp:

- i took the time to implement a half-assed almost-working loopification pass a few months ago. the sinking pass by Simon is what really does a huge amount of the optimizations Kryzsztof's thesis attacked differently. but i think doing loopification could maybe lead to identifying things like loop invariant expressions. it can't bootstrap the compiler with it (JS: it = Austin's patch). i think whenever i tie the knot in the new graph, i don't abandon parts of the old CmmNode, which then causes dead labels to hang around
- oh, yeah, and as i noted in the commit message, you have to be careful when ordering those optimizations around. this is obviously only a valid transform pre-CPS. also you have to run a block elimination passes, otherwise things can happen where work can get duplicated into empty blocks
-  i think another problem is that the new codegen doesn't always discard empty basic blocks which pisses off the native code generator (see [\#7574](https://gitlab.haskell.org//ghc/ghc/issues/7574) ) so we need a little refactoring to handle that correctly, too, by being able to do SCC passes from any particular node
- i think that the fix for [\#7574](https://gitlab.haskell.org//ghc/ghc/issues/7574) is probably pretty easily actually, it just requires shuffling things around. oh, and to be clear it's not \*empty\* basic blocks, it's \*unreachable\* basic blocks that make the codegen mad

[\#7574](https://gitlab.haskell.org//ghc/ghc/issues/7574) bug can be triggered with `./inplace/bin/ghc-stage2 -c -no-hs-main -fasm -O2 ./testsuite/tests/llvm/should_compile/T7571.cmm`

# Back-end notes

## Some interesting tickets

- [\#1498](https://gitlab.haskell.org//ghc/ghc/issues/1498) - Optimisation: eliminate unnecessary heap check in recursive function. 
- [\#1600](https://gitlab.haskell.org//ghc/ghc/issues/1600) - Optimisation: CPR the results of IO
- [\#2289](https://gitlab.haskell.org//ghc/ghc/issues/2289) - Needless reboxing of values when returning from a tight loop
- [\#2387](https://gitlab.haskell.org//ghc/ghc/issues/2387) - Optimizer misses unboxing opportunity
- [\#5567](https://gitlab.haskell.org//ghc/ghc/issues/5567) - LLVM: Improve alias analysis / performance [BackEndNotes](back-end-notes#heap/stack-checks) page has some discussion of this.
- [\#7198](https://gitlab.haskell.org//ghc/ghc/issues/7198) - New codegen more than doubles compile time of T3294

## Notes on the wiki

- [Commentary/Compiler/NewCodeGen/Cleanup](commentary/compiler/new-code-gen/cleanup)
- [Commentary/Compiler/NewCodeGenStupidity](commentary/compiler/new-code-gen-stupidity)
- [ cmm-notes](http://darcs.haskell.org/ghc/compiler/cmm/cmm-notes)

# Various clean-up tasks

## Cleaning up the STG -\>Cmm pass


When generating Cmm from STG there is some [SRT information](commentary/rts/storage/gc/ca-fs) being generated. It is not used and has to be rebuilt anyway after converting to CPS Cmm. Below are some random notes and pieces of code that might related to this:
  

- Cmm conversions in the compiler pipeline: `main/HscMain.hs` has `tryNewCodeGen` (l. 1300), which first calls `StgCmm.codegen` and then passes the generated Cmm to `cmmPipeline` function from `cmm/CmmPipeline.hs`. According to Austin Seipp `cpsTop` in `cmm/CmmPipeline.hs` takes care of converting to CPS: "yeah, CmmPipeline does take care of it. it's partially cpsTop that does it, and doSRTs elaborates the top-level info tables and stuff beyond that but mostly cpsTop. i think your general turning point is after the stack layout and stack pointer manifestation". 


This code in `cmm/Cmm.hs` that might be relevant (or not):

```wiki
-- (line 141 and onwards)
-- | Info table as a haskell data type
data CmmInfoTable
  = CmmInfoTable {
      cit_lbl  :: CLabel, -- Info table label
      cit_rep  :: SMRep,
      cit_prof :: ProfilingInfo,
      cit_srt  :: C_SRT
    }

data ProfilingInfo
  = NoProfilingInfo
  | ProfilingInfo [Word8] [Word8] -- closure_type, closure_desc

-- C_SRT is what StgSyn.SRT gets translated to...
-- we add a label for the table, and expect only the 'offset/length' form

data C_SRT = NoC_SRT
           | C_SRT !CLabel !WordOff !StgHalfWord {-bitmap or escape-}
           deriving (Eq)

needsSRT :: C_SRT -> Bool
needsSRT NoC_SRT       = False
needsSRT (C_SRT _ _ _) = True
```

## Random code

- `main/HscMain.lhs:1300`\`. Is:

```wiki
| otherwise
  = {-# SCC "cmmPipeline" #-}
    let initTopSRT = initUs_ us emptySRT in

    let run_pipeline topSRT cmmgroup = do
          (topSRT, cmmgroup) <- cmmPipeline hsc_env topSRT cmmgroup
          return (topSRT,cmmgroup)

    in do topSRT <- Stream.mapAccumL run_pipeline initTopSRT ppr_stream1
          Stream.yield (srtToData topSRT)
```


The `<- / return` sequence in the definition of `run_pipeline` can be eliminated, which allows to remove the `do` notation, which allows to do eta-reduction, which (finally) allows to remove the `run_pipeline` binding and using `(cmmPipeline hsc_env)` instead:

```wiki
| otherwise
  = {-# SCC "cmmPipeline" #-}
    let initTopSRT = initUs_ us emptySRT
    in do topSRT <- Stream.mapAccumL (cmmPipeline hsc_env) initTopSRT ppr_stream1
          Stream.yield (srtToData topSRT)
```

- `cmm/CmmLive.hs:106`. This function is not used (unless there is some auto-generated code that uses it, but I doubt it):

```wiki
removeDeadAssignments :: DynFlags -> CmmGraph
                      -> UniqSM (CmmGraph, BlockEnv CmmLocalLive)
```


It is however referenced in some of the comments!

- `cmm/CmmRewriteAssignments.hs` is not used at all?
- `cmm/CmmUtils.hs`, function `toBlockListEntryFirst` - perhaps it would be safer to return a tuple in this case? This would probably make the invariant more explicit.

## Wiki

- [NewCodeGenPipeline](commentary/compiler/new-code-gen-pipeline) has some outdated sections in the Cmm pipeline description: Add spill/reload, Rewrite assignments. So far I only marked them as OUTDATED
- [NewCodeGenModules](commentary/compiler/new-code-gen-modules) - mostly outdated. Mentioned data types and modules no longer exist.

# Various stuff


Tickets that I could potentially look into:

- [\#4101](https://gitlab.haskell.org//ghc/ghc/issues/4101) - Primitive constant unfolding
- [\#3070](https://gitlab.haskell.org//ghc/ghc/issues/3070) - floor(0/0) should not be defined
- [\#3676](https://gitlab.haskell.org//ghc/ghc/issues/3676) - realToFrac doesn't sanely convert between floating types
- [\#3744](https://gitlab.haskell.org//ghc/ghc/issues/3744) - Comparisons against minBound/maxBound not optimised
- [\#5615](https://gitlab.haskell.org//ghc/ghc/issues/5615) - ghc produces poor code for `div` with constant powers of 2.
- [\#7116](https://gitlab.haskell.org//ghc/ghc/issues/7116) - Missing optimisation: strength reduction of floating-point multiplication
- [\#7858](https://gitlab.haskell.org//ghc/ghc/issues/7858) - Fix definitions of abs/signum for Floats/Doubles.


Other things to do:

- improve Cmm dumping. I think that during pretty-printing of Cmm labels of the entry code of a closure are refered to as "info_table", while the label of the whole closure is refered to as "entry". This is confusing.
- investigate opportunities for improving heap checks. An idea: if a worker knows its heap requirements it could pass them to the caller, thus avoiding the heap check. A question: how much time do we really spend on heap checks?


Some LLVM notes that may be useful:

- [LLVM Alias Notes](commentary/compiler/backends/llvm/alias)
- [ David Terei's LLVM blog post](http://blog.davidterei.com/2011/09/ghc-project-for-all.html)
- [ Max Bolingbroke's LLVM blog entry](http://blog.omega-prime.co.uk/?p=135)

# Github repos

- [ GHC](https://github.com/jstolarek/ghc)
- [ testsuite](https://github.com/jstolarek/testsuite)
- [ packages-array](https://github.com/jstolarek/packages-array)
- [ packages-base](https://github.com/jstolarek/packages-base)
- [ packages-ghc-prim](https://github.com/jstolarek/packages-ghc-prim)
- [ packages-integer-gmp](https://github.com/jstolarek/packages-integer-gmp)
- [ packages-integer-simple](https://github.com/jstolarek/packages-integer-simple)
- [ packages-primitive](https://github.com/jstolarek/packages-primitive)


Unboxed Booleans ([\#6135](https://gitlab.haskell.org//ghc/ghc/issues/6135)) work is in all 8 repos on branch `bool-primops-vX`, where `X` is a number. `X` is increased after rebasing on top of new HEAD (I'm doing this to avoid upstream rebasing).


Loopification work is in main GHC repo on branch [ js-loopification](https://github.com/jstolarek/ghc/tree/js-loopification).
