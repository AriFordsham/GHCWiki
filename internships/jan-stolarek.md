# Jan Stolarek's internship notes

# Wise people say…


Geoffrey:

- In the past, LLVM could not recognize all loops output by the LLVm back end as loops. Perhaps that has changed.
- Answering the question "What does loopification do that isn't already being done?" would still be useful
- So figuring out how to make LLVM recognize more loops would be good.
- if you write a simple, tight loop in Haskell of the sort a C compiler would vectorize, will LLVm vectorize it? If not, why?


Austin Seipp:

- i took the time to implement a half-assed almost-working loopification pass a few months ago. the sinking pass by Simon is what really does a huge amount of the optimizations Kryzsztof's thesis attacked differently. but i think doing loopification could maybe lead to identifying things like loop invariant expressions. it can't bootstrap the compiler with it (JS: it = Austin's patch). i think whenever i tie the knot in the new graph, i don't abandon parts of the old CmmNode, which then causes dead labels to hang around
- oh, yeah, and as i noted in the commit message, you have to be careful when ordering those optimizations around. this is obviously only a valid transform pre-CPS. also you have to run a block elimination passes, otherwise things can happen where work can get duplicated into empty blocks
-  i think another problem is that the new codegen doesn't always discard empty basic blocks which pisses off the native code generator (see [\#7574](https://gitlab.haskell.org//ghc/ghc/issues/7574) ) so we need a little refactoring to handle that correctly, too, by being able to do SCC passes from any particular node
- i think that the fix for [\#7574](https://gitlab.haskell.org//ghc/ghc/issues/7574) is probably pretty easily actually, it just requires shuffling things around. oh, and to be clear it's not \*empty\* basic blocks, it's \*unreachable\* basic blocks that make the codegen mad


Simon Marlow:

- CmmSink removes dead assignments (though not in loops), which is why it's commented out.  A single removeDeadAssigments pass costs about 5% of compilation time, and in the vast majority of code does nothing over what CmmSink already does.
- **PLEASE make sure that you're carefully measuring compilation time when making changes to the code generator**.  Expensive optimisations need to go in -O2 (at least).

# Back-end notes

## Various notes to self

- Does it make sense to create a separate flag for every Cmm optimisation I add? After all they are designed to work together
- I need to remember to cerfully choose at which optimization levels my Cmm passes are enabled
- Here's an interesting bit from `CoreToStg.lhs`: "a dead variable's stack slot (if it has one): should be stubbed to avoid space leaks"
- During stack layout phase we generates stores of variables live across calls. This could lead to potentially unnecessary stores and loads. The question is whether we avoid that? If we do then how do that?

## Some interesting tickets

- [\#605](https://gitlab.haskell.org//ghc/ghc/issues/605) - Optimisation: strict enumerations
- [\#1498](https://gitlab.haskell.org//ghc/ghc/issues/1498) - Optimisation: eliminate unnecessary heap check in recursive function. 
- [\#1600](https://gitlab.haskell.org//ghc/ghc/issues/1600) - Optimisation: CPR the results of IO
- [\#2289](https://gitlab.haskell.org//ghc/ghc/issues/2289) - Needless reboxing of values when returning from a tight loop
- [\#2387](https://gitlab.haskell.org//ghc/ghc/issues/2387) - Optimizer misses unboxing opportunity
- [\#2450](https://gitlab.haskell.org//ghc/ghc/issues/2450) - Data.Complex.magnitude squares using <sup>(2 :: Int), which is slow
  </sup>
- [\#2731](https://gitlab.haskell.org//ghc/ghc/issues/2731) - Avoid unnecessary evaluation when unpacking constructors
- [\#2823](https://gitlab.haskell.org//ghc/ghc/issues/2823) - Another arity expansion bug
- [\#4470](https://gitlab.haskell.org//ghc/ghc/issues/4470) - Loop optimization: identical counters
- [\#4937](https://gitlab.haskell.org//ghc/ghc/issues/4937) - Remove indirections caused by sum types, such as Maybe
- [\#5567](https://gitlab.haskell.org//ghc/ghc/issues/5567) - LLVM: Improve alias analysis / performance [BackEndNotes](back-end-notes#heap/stack-checks) page has some discussion of this.
- [\#7198](https://gitlab.haskell.org//ghc/ghc/issues/7198) - New codegen more than doubles compile time of T3294
- [\#7574](https://gitlab.haskell.org//ghc/ghc/issues/7574) - Register allocator chokes on certain branches with literals (bug can be triggered with `./inplace/bin/ghc-stage2 -c -no-hs-main -fasm -O2 ./testsuite/tests/llvm/should_compile/T7571.cmm`)
- [\#8048](https://gitlab.haskell.org//ghc/ghc/issues/8048) - Register spilling produces ineffecient/highly contending code

## Notes on the wiki

- [Commentary/Compiler/NewCodeGen/Cleanup](commentary/compiler/new-code-gen/cleanup)
- [Commentary/Compiler/NewCodeGenStupidity](commentary/compiler/new-code-gen-stupidity)
- [ cmm-notes](http://darcs.haskell.org/ghc/compiler/cmm/cmm-notes)

# Various clean-up tasks

## Cmm clean-up

- remove unused CmmRewriteAssignments
- `cmm/CmmLive.hs:106`. This function is not used:

```wiki
removeDeadAssignments :: DynFlags -> CmmGraph
                      -> UniqSM (CmmGraph, BlockEnv CmmLocalLive)
```


It is however referenced in some of the comments. I might be able to use it for my dead assignment removal. Simon PJ notes: ", we want to eliminate dead assignments to stack locations too, so the liveness info need to be augmented with stack areas.
"

- Cmm dumping could be improved. Right now it dumps all optimisation passes for one fragment of Cmm code, then for next fragment and so on. It would be more convinient to dump whole Cmm code after each pass. I'm not sure if that's possible with the current pipeline design. It seems that Stg-\>Cmm pass is intentionally design to produce Cmm code incrementally (via Stream) and I suspect that this might be the reason why the code is processed incrementally.
- Simon M. says: The CmmSink pass before stack layout is disabled because I never got around to measuring it to determine whether it is a good idea or not. By all means do that!

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

- `cmm/CmmUtils.hs`, function `toBlockListEntryFirst` - perhaps it would be safer to return a tuple in this case? This would probably make the invariant more explicit.

## Wiki

- [NewCodeGenPipeline](commentary/compiler/new-code-gen-pipeline) has some outdated sections in the Cmm pipeline description: Add spill/reload, Rewrite assignments. So far I only marked them as OUTDATED
- [NewCodeGenModules](commentary/compiler/new-code-gen-modules) - mostly outdated. Mentioned data types and modules no longer exist.

# Various stuff


Tickets that I could potentially look into:

- [\#3070](https://gitlab.haskell.org//ghc/ghc/issues/3070) - floor(0/0) should not be defined
- [\#3676](https://gitlab.haskell.org//ghc/ghc/issues/3676) - realToFrac doesn't sanely convert between floating types
- [\#3744](https://gitlab.haskell.org//ghc/ghc/issues/3744) - Comparisons against minBound/maxBound not optimised
- [\#4101](https://gitlab.haskell.org//ghc/ghc/issues/4101) - Primitive constant unfolding
- [\#5615](https://gitlab.haskell.org//ghc/ghc/issues/5615) - ghc produces poor code for `div` with constant powers of 2.
- [\#7116](https://gitlab.haskell.org//ghc/ghc/issues/7116) - Missing optimisation: strength reduction of floating-point multiplication
- [\#7858](https://gitlab.haskell.org//ghc/ghc/issues/7858) - Fix definitions of abs/signum for Floats/Doubles.
- [\#8072](https://gitlab.haskell.org//ghc/ghc/issues/8072) - Optimizations change result of div for Word


Other things to do:

- investigate opportunities for improving heap checks. An idea: if a worker knows its heap requirements it could pass them to the caller, thus avoiding the heap check. A question: how much time do we really spend on heap checks?


Some LLVM notes that may be useful:

- [LLVM Alias Notes](commentary/compiler/backends/llvm/alias)
- [ David Terei's LLVM blog post](http://blog.davidterei.com/2011/09/ghc-project-for-all.html)
- [ Max Bolingbroke's LLVM blog entry](http://blog.omega-prime.co.uk/?p=135)
- [ Implementation of various LLVM optimisations using Hoopl](https://github.com/mlite/HsLlvm) - **this seems very relevant**

# Github repos

- [ GHC](https://github.com/jstolarek/ghc)
- [ testsuite](https://github.com/jstolarek/testsuite)
- [ packages-array](https://github.com/jstolarek/packages-array)
- [ packages-base](https://github.com/jstolarek/packages-base)
- [ packages-ghc-prim](https://github.com/jstolarek/packages-ghc-prim)
- [ packages-integer-gmp](https://github.com/jstolarek/packages-integer-gmp)
- [ packages-integer-simple](https://github.com/jstolarek/packages-integer-simple)
- [ packages-primitive](https://github.com/jstolarek/packages-primitive)

- [ test programs](https://github.com/jstolarek/ghc-tests)


Unboxed Booleans ([\#6135](https://gitlab.haskell.org//ghc/ghc/issues/6135)) work is in all 8 repos on branch `bool-primops-vX`, where `X` is a number. `X` is increased after rebasing on top of new HEAD (I'm doing this to avoid upstream rebasing).


Loopification work is in main GHC repo on branch `js-loopification-vX`.
