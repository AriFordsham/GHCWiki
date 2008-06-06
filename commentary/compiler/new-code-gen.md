# Material about the new code generator


This page summarises work that Norman Ramsey, Simon M, Simon PJ, and John Dias are doing on re-architecting GHC's back end. You may want to see the [short-term view of the pipeline](commentary/compiler/new-code-gen-pipeline).


Bug list (code-gen related bugs that we may be able to fix):

- [\#2249](https://gitlab.haskell.org//ghc/ghc/issues/2249)
- [\#2253](https://gitlab.haskell.org//ghc/ghc/issues/2253)


Status:

- The Rep swamp is drained: see [Commentary/Compiler/BackEndTypes](commentary/compiler/back-end-types)
- Code generator: first draft done.
- Control-flow opt: simple ones done

  - Common block elimination: done
  - Block concatenation: done
- Adams optimisation: currently done in [compiler/cmm/CmmProcPointZ.hs](/trac/ghc/browser/ghc/compiler/cmm/CmmProcPointZ.hs), which is incomplete because it does not insert the correct CopyOut nodes.  The Adams optimization should be divorced from this module and replaced with common-block elimination, to be done after the proc-point transformation.  In principle this combination may be slightly less effective than the current code, since the selection of proc-point protocols is guided by Adams's criteria, but NR thinks it will be easy to get the common, important cases nailed.
- Proc-point analysis and transformation: 'working' but largely untested.  There is still no coherent plan for calling conventions, and the lack of such a plan prevents the completion of proc-point analysis, as in principle it should come up with a calling convention for each freely chosen proc point.  In practice NR recommends the following procedure:

  - All optional proc points to be generated with no parameters (all live variables on the stack)
  - This situation to be remedied when the code generator is reorganized along the lines NR proposed in July 2007, i.e., the register allocator runs on C-- with calls (as opposed to C-- with jumps only) and therefore *before* proc-point analysis
- Add spill/reload: Implemented to NR's satisfaction in [compiler/cmm/CmmSpillReload.hs](/trac/ghc/browser/ghc/compiler/cmm/CmmSpillReload.hs), with the proviso that spilling is done to *abstract* stack slots rather than real stack positions (see comments below on stack-slot allocation)
- Stack slot allocation: nothing here but some broken bits and pieces.  Progress in this arena is blocked by the lack of a full understanding of how to do stack-frame layout and how to deal with calling conventions.  NR proposes that life would be simplified if *all* calls downstream from the Cmm converter were to be parameterless---the idea being to handle the calling conventions *here* and to put arguments and results in their conventional locations. John has done much of the work here already; the remaining bit is the actual layout of the stack slots.
- Make stack explicit: done.
- Split into multiple CmmProcs: mostly done, just a bit of patching up remains.


Norman's plan

1. New code to check invariants of output from [compiler/cmm/ZipDataflow.hs](/trac/ghc/browser/ghc/compiler/cmm/ZipDataflow.hs)
1. Finish debugging [compiler/cmm/ZipDataflow.hs](/trac/ghc/browser/ghc/compiler/cmm/ZipDataflow.hs).
1. Use Simon PJ's 'common-blockifier' (which does not exist!!!) to move the Adams optimization outside [compiler/cmm/CmmProcProintZ.hs](/trac/ghc/browser/ghc/compiler/cmm/CmmProcProintZ.hs)
1. ProcPointZ does not insert `CopyOut` nodes; this omission must be rectified and will require some general infrastructure for inserting predecessors.
1. Simple optimizations on `CopyIn` and `CopyOut` may be required
1. Define an interface for calling conventions and invariants for the output of frame layout \[will require help from Simon M\]
1. Stack layout
1. Glue the whole pipeline together and make sure it works.


Items 1-5 look like a few days apiece. Items 6 and 7 are more scary...


ToDo: main issues

- SRTs simply record live global variables.  So we should use the same live-variable framework as for live local variables.  That means we must be able to identify which globals are SRT-able.  What about compression/encoding schemes?

- How do we write continuations in the RTS?  E.g. the update-frame continuation?  Michael Adams had a syntax with two sets of parameters, the the ones on the stack and the return values.

- Review code gen for calls with lots of args.  In the existing codegen we push magic continuations that say "apply the return value to N more args".  Do we want to do this?  ToDo: how rare is it to have too many args?

- Figure out how PAPs work.  This may interact with the GC check and stack check at the start of a function call.

- How do stack overflow checks work?  (They are inserted by the CPS conversion, and must not generate a new info table etc.)

- Was there something about sinking spills and hoisting reloads?


ToDo: small issues

- Shall we rename Branch to GoTo?!
- Where is the "push new continuation" middle node? 
- Change the C-- parser (which parses RTS .cmm files) to directly construct `CmmGraph`.  
- (SLPJ) See let-no-escape todos in `StgCmmExpr`.

## The new Cmm data type


There is a new Cmm data type:

- [compiler/cmm/ZipCfg.hs](/trac/ghc/browser/ghc/compiler/cmm/ZipCfg.hs) contains a generic zipper-based control-flow graph data type.  It is generic in the sense that it's polymorphic in the type of **middle nodes** and **last nodes** of a block.  (Middle nodes don't do control transfers; last nodes only do control transfers.)  There are extensive notes at the start of the module.

  The key types it defines are:

  - Block identifiers: `BlockId`, `BlockEnv`, `BlockSet`
  - Control-flow blocks: `Block`
  - Control-flow graphs: `Graph`
- **`ZipDataFlow`** contains a generic framework for solving dataflow problems over `ZipCfg`.
- **[compiler/cmm/ZipCfgCmmRep.hs](/trac/ghc/browser/ghc/compiler/cmm/ZipCfgCmmRep.hs)** instantiates `ZipCfg` for Cmm, by defining types `Middle` and `Last` and using these to instantiate the polymorphic fields of `ZipCfg`.  It also defines a bunch of smart constructor (`mkJump`, `mkAssign`, `mkCmmIfThenElse` etc) which make it easy to build `CmmGraph`.
- **`CmmExpr`** contains the data types for Cmm expressions, registers, and the like.  It does not depend on the dataflow framework at all.

---

## Runtime system

- **Garbage collector entry points**: see `Note [Heap checks]` in `StgCmmHeapery`.

- **PAPs**

- **Update frames** and **exception handling**.  Also STM frames.

- **Primitives** can be rewritten:

  - Use parameters
  - In a few cases, use native calls (notably eval)
