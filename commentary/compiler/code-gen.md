# Code Generator


This page describes code generator ("codegen") in GHC. It is meant to reflect current state of the implementation. If you notice any inacurracies please update the page (if you know how) or complain on ghc-devs.

## A brief history of code generator


You might ocasionally hear about "old" and "new" code generator. GHC 7.6 and earlier used the old code generator. New code generator was being developed since 2007 and it was [enabled by default on 31 August 2012](/trac/ghc/changeset/832077ca5393d298324cb6b0a2cb501e27209768/ghc) after the release of GHC 7.6.1. The first stable GHC to use the new code generator is 7.8.1 released in early 2014. The commentary on the old code generator can be found [here](commentary/compiler/old-code-gen). Notes from the development process of the new code generator are located in a couple of pages on the wiki - go to [Index](title-index) and look for pages starting with "NewCodeGen".


There are some plans for the future development of code generator. One plan is to expand the capability of the pipeline so that it does native code generation too so that existing backends can be discarded - see [IntegratedCodeGen](commentary/compiler/integrated-code-gen) for discussion of the design. It is hard to say if this will ever happen as currently there is no work being done on that subject and in the meanwhile there was an alternative proposal to [replace native code generator with LLVM](commentary/compiler/backends/llvm/replacing-ncg).

## Overview


The goal of the code generator is to convert program from [STG](commentary/compiler/generated-code) representation to [Cmm](commentary/compiler/cmm-type) representation. STG is a functional language with explicit stack. Cmm is a low-level imperative language - something between C and assembly - that is suitable for machine code generation. Note that terminology might be a bit confusing here: the term "code generator" refers to STG-\>Cmm pass but it may also be used to refer to the Cmm-\>assembly pass. Referring to the latter as the "backend" eliminates the confusion.


The top-most entry point to the codegen is located in [compiler/main/HscMain.hs](/trac/ghc/browser/ghc/compiler/main/HscMain.hs) in the `tryNewCodegen` function. Code generation now has three stages:

1. Convert STG to Cmm with implicit stack, and native Cmm calls. This whole stage lives in [compiler/codeGen](/trac/ghc/browser/ghc/compiler/codeGen) directory with the entry point being `codeGen` function in [compiler/codeGen/StgCmm.hs](/trac/ghc/browser/ghc/compiler/codeGen/StgCmm.hs) module.
1. Optimise the Cmm, and CPS-convert it to have an explicit stack, and no native calls. This lives in [compiler/cmm](/trac/ghc/browser/ghc/compiler/cmm) directory with the `cmmPipeline` function from [compiler/cmm/CmmPipeline.hs](/trac/ghc/browser/ghc/compiler/cmm/CmmPipeline.hs) module being the entry point. This is described below in full detail.
1. Feed the CPS-converted Cmm to the existing, unmodified native code generators. This is done by `codeOutput` function ([compiler/main/CodeOutput.lhs](/trac/ghc/browser/ghc/compiler/main/CodeOutput.lhs) called from `hscGenHardCode` after returning from `tryNewCodegen`.

## Structure of Cmm pipeline


The first two steps are described in more detail here:

- **Code generator** converts STG to `CmmGraph`.  Implemented in `StgCmm*` modules (in directory `codeGen`). 

  - `Cmm.CmmGraph` is pretty much a Hoopl graph of `CmmNode.CmmNode` nodes. Control transfer instructions are always the last node of a basic block.
  - Parameter passing is made explicit; the calling convention depends on the target architecture.  The key function is `CmmCallConv.assignArgumentsPos`. 

    - Parameters are passed in virtual registers R1, R2 etc. \[These map 1-1 to real registers.\] 
    - Overflow parameters are passed on the stack using explicit memory stores, to locations described abstractly using the [''Stack Area'' abstraction.](commentary/compiler/stack-areas).   
    - Making the calling convention explicit includes an explicit store instruction of the return address, which is stored explicitly on the stack in the same way as overflow parameters. This is done (obscurely) in `MkGraph.mkCall`.

- **Simple control flow optimisation**, implemented in `CmmContFlowOpt`.  It's called both at the beginning and end of the pipeline.

  - Branch chain elimination.
  - Remove unreachable blocks.
  - Block concatenation.  branch to K; and this is the only use of K.  

- **More control flow optimisations**.

  - Common Block Elimination (like CSE). This essentially implements the Adams optimisation, we believe.
  - Consider (sometime): block duplication.  branch to K; and K is a short block.  Branch chain elimination is just a special case of this.

- **Proc-point analysis** and **transformation**, implemented in `CmmProcPoint`. The transformation part adds a function prologue to the front of each proc-point, following a standard entry convention.

  - The analysis produces a set of `BlockId` that should become proc-points
  - The transformation inserts a function prologue at the start of each proc-point, and a function epilogue just before each branch to a proc-point.

- **Remove dead assignments and stores**, implemented in `CmmLive`, removes assignments to dead variables and things like ``a = a`` or ``I32\[Hp\] = I32\[Hp\]``. The latter may more appropriately be done in a general optimization pass, as it doesn't take advantage of liveness information.

- **Figure out the stack layout**, implemented in `CmmStackLayout`.

  - Each variable 'x', and each proc-point label 'K', has an associated *Area*, written SS(x) and SS(k) resp, that names a contiguous portion of the stack frame.  
  - The stack layout pass produces a mapping of: *(`Area` -\> `StackOffset`)*. For more detail, see [the description of stack layout.](commentary/compiler/stack-areas#laying-out-the-stack)
  - A `StackOffset` is the byte offset of a stack slot from the old end (high address) of the frame.  It doesn't vary as the physical stack pointer moves.

- **Manifest the stack pointer**, implemented in `CmmStackLayout`.  Once the stack layout mapping has been determined, a second pass walks over the graph, making the stack pointer, `Sp` explicit. Before this pass, there is no `Sp` at all.  After this, `Sp` is completely manifest.

  - replacing references to `Areas` with offsets from `Sp`.
  - adding adjustments to `Sp`.

- **Split into multiple CmmProcs**, implemented in `CmmProcPointZ`.  At this point we build an info-table for each of the CmmProcs, including SRTs.  Done on the basis of the live local variables (by now mapped to stack slots) and live CAF statics.

  - `LastCall` and `LastReturn` nodes are replaced by `Jump`s.

- **Build info tables**, implemented in `CmmBuildInfoTables`..  

  - Find each safe `MidForeignCall` node, "lowers" it into the suspend/call/resume sequence (see `Note [Foreign calls]` in `CmmNode.hs`.), and build an info table for them.
  - Convert the `CmmInfo` for each `CmmProc` into a `[CmmStatic]`, using the live variable information computed just before "Figure out stack layout".  

## Dumping Cmm

## Hoopl


write about Hoopl (link paper, mention which modules are implemented with Hoopl)
