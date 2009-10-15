# GHC's glorious new code generator


This page summarises work that Norman Ramsey, Simon M, Simon PJ, and John Dias are doing on re-architecting GHC's back end.  Our plan is as follows:

- **Step 1**: drain the "Rep swamp".  This is a change of data representation that pervades the compiler, including lots and lots of tiny changes in the existing native code generators.  It's done (see [Commentary/Compiler/BackEndTypes](commentary/compiler/back-end-types)), and tested, but not yet committed to the HEAD.

- **Step 2**: Replace the existing Stg to Cmm code generator (a very complex and inflexible pass) with a new modular pipeline. The output of this pipeline is fed to the existing, un-modified code generators.  The design of the new pipeline is here: [Commentary/Compiler/NewCodeGenPipeline](commentary/compiler/new-code-gen-pipeline).

- **Step 3**: Expand the capability of the new pipeline so that it does native code generation too, and we can ultimately discard the existing code generators.  The design of this stage is here: [Commentary/Compiler/IntegratedCodeGen](commentary/compiler/integrated-code-gen)


In timescale terms it looks like this:

- GHC 6.10 will have nothing new at all

- Immediately after the code fork for 6.10 we'll commit the new stuff for Step 1 and Step 2 to the HEAD.  

- By the end of 2008 (and probably much earlier) we hope to be using the Step 2 pipeline in anger, and can discard the existing code generator entirely.  To be fair, at this point you probably won't see any performance improvements; indeed compilation could be a bit slower.  But the pipeline will be far more modular and flexible.

- Work on Step 3 will proceed in 2009, but at a slower pace because John's internship ends in Oct 2008.

- At the same time, others can help!  In particular, Cmm-to-Cmm optimisations will be easy.  And some of them really should yield performance improvements.

## Bugs


Bug list (code-gen related bugs that we may be able to fix):

- [\#1498](https://gitlab.haskell.org//ghc/ghc/issues/1498) (avoid redundant heap check on the fast path)
- [\#3552](https://gitlab.haskell.org//ghc/ghc/issues/3552) (unreachable code)
- [\#3462](https://gitlab.haskell.org//ghc/ghc/issues/3462) (a feature)
- [\#2249](https://gitlab.haskell.org//ghc/ghc/issues/2249)
- [\#2253](https://gitlab.haskell.org//ghc/ghc/issues/2253)
- [\#2289](https://gitlab.haskell.org//ghc/ghc/issues/2289)

## Loose ends as at October 2008


Testing

- Check nofib performance regressions (one or two allocation (odd)); several on time.
- Compile libraries and test


Short term

- Nuke `CmmCPSGen`, `CmmCPS`, and the two `Maybe`s in `CmmInfo` data type.
- Remove `optionallyConvertAndOrCPS` from main pipeline.
- Fix if-then-else special case in `StgCmmExpr`
- Perhaps some obvious CSE opportunities
- We only use one GC entry point, but there should be a bunch of canned ones.

- To `Convention` add `StaticNative` (which does not use R1), and use it appropriately.  Replace `GC` by `gc = Native`.
- Allow .cmx files to come in *before* the new pipeline, to aid graceful migration of RTS.


Larger

- Migrate RTS to use procedures
- Explore new calling conventions


Tidying up 

- Get rid of SRTs and live-variable info from STG, and from the Core-to-Stg phase.
- Do not split proc-points into separate `CmmProc`. Not a trivial change, because it involves attaching info tables to blocks, not just to `CmmProc`s.
- Nuke old code gen, and associated Cmm cruft
- Simplify dataflow framework, by doing deep rewriting only.  If possible kill `LastExit` (Simon's favourite hate).

## Notes about the state of play in August, 2008


These notes are largely out of date, but I don't want to dump them till we're sure that we've sucked all the juice out of them.
 

- Code generator: first draft done.
- Control-flow opt: simple ones done

  - Common block elimination: done
  - Block concatenation: done
- Adams optimisation: currently done in [compiler/cmm/CmmProcPointZ.hs](/trac/ghc/browser/ghc/compiler/cmm/CmmProcPointZ.hs).  The Adams optimization should be divorced from this module and replaced with common-block elimination, to be done after the proc-point transformation.  In principle this combination may be slightly less effective than the current code, since the selection of proc-point protocols is guided by Adams's criteria, but NR thinks it will be easy to get the common, important cases nailed.  Note that the Adams optimization is designed to avoid generating extra continuations in the case of [heap checks after a function call](commentary/compiler/cps#branches-to-continuations).
- Proc-point analysis and transformation: working, although there is plenty of room for experimentation with the calling conventions at proc points.  In practice NR recommends the following procedure:

  - All optional proc points to be generated with no parameters (all live variables on the stack)
  - This situation to be remedied when the code generator is reorganized along the lines NR proposed in July 2007, i.e., the register allocator runs on C-- with calls (as opposed to C-- with jumps only) and therefore *before* proc-point analysis
- Bypassing proc-point analysis for native back ends: not done.
- Add spill/reload: Implemented to NR's satisfaction in [compiler/cmm/CmmSpillReload.hs](/trac/ghc/browser/ghc/compiler/cmm/CmmSpillReload.hs), with the proviso that spilling is done to *abstract* stack slots rather than real stack positions (see comments below on stack-slot allocation)
- Stack slot allocation: implemented with a greedy algorithm. There is room for improvement here.
- Make stack explicit: done.
- Split into multiple CmmProcs: done.


ToDo: main issues

- How do we write continuations in the RTS?  E.g. the update-frame continuation?  Michael Adams had a syntax with two sets of parameters, the the ones on the stack and the return values.

- Review code gen for calls with lots of args.  In the existing codegen we push magic continuations that say "apply the return value to N more args".  Do we want to do this?  ToDo: how rare is it to have too many args?

- Figure out how PAPs work.  This may interact with the GC check and stack check at the start of a function call.  The concern is how to enter the garbage collector with an infotable that properly describes the live variables. Now that we generate info tables on demand at the end of the pipeline, we can enter the gc with a regular procedure call and expect that the proper info table will be generated.

- Was there something about sinking spills and hoisting reloads?


ToDo: small issues

- Shall we rename Branch to GoTo?!
- Change the C-- parser (which parses RTS .cmm files) to directly construct `CmmGraph`.  
- (SLPJ) See let-no-escape todos in `StgCmmExpr`.
