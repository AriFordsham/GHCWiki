## Historical page


This page stores historical information about Cmm Pipeline in the new code generator. This description has been updated and is maintained on the [Code Generator](commentary/compiler/code-gen) page. This page has also historical notes about Adams optimisation. That optimisation is also described in Note \[sharing continuations\] in [compiler/GHC/StgToCmm/Monad.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/StgToCmm/Monad.hs) and probably deserves its own wiki page.

# Design of the new code generator


This page contains notes about the design of the new code generator.
See also: [overview of the module structure in the new code generator](commentary/compiler/new-code-gen-modules).

## Overview


Code generation now has three stages:

1. Convert STG to Cmm, with implicit stack implicit, and native Cmm calls.
1. Optimise the Cmm, and CPS-convert it to have an explicit stack, and no native calls.
  This part of the pipeline is stitched together in `cmm/CmmPipeline.hs`.
1. Feed the CPS-converted Cmm to the existing, unmodified native code generators.


Ultimately our plan is to expand the capability of the new pipeline so that it does native code generation too, and we can ultimately discard the existing code generators.  The design of this stage is here: [Commentary/Compiler/IntegratedCodeGen](commentary/compiler/integrated-code-gen)

## The Cmm pipeline


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

- **(OUTDATED - CmmSpillReload does not exist any more)** **Add spill/reload**, implemented in `CmmSpillReload`, to spill live C-- variables before a call and reload them afterwards.  The spill and reload instructions are simply memory stores and loads respectively, using symbolic stack offsets (see [stack layout](commentary/compiler/stack-areas#laying-out-the-stack)).  For example, a spill of variable 'x' would look like `Ptr32[SS(x)] = x`.

  - `dualLivenessWithInsertion` does two things:

    - Spills at the definition of any variable that is subequently live across a call (uses a backward analysis)
    - Adds a reload at each return (or proc) point

    At this point, no (`LocalReg`) variables are live across a call.
  - TODO avoid  `f();g()` turning into `spill x; f(); reload x; spill x; g(); reload x`.

- **(OUTDATED - CmmRewriteAssignments is not used any more)** **Rewrite assignments** (assignments to local regs, that is, not stores). 

  - Convert graph to annotated graph whose nodes are `CmmRewriteAssignments.WithRegUsage`.  Specifically, `CmmAssign` is decorated with a flag `RegUsage` saying whether it is used once or many times.
  - Sink or inline assignments nearer their use points
  - Do constant mach-op folding. This is done in this phase, because folded mach-ops can be inlined, and inlining exposes opportunities for mach-op folding.

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

### Branches to continuations and the "Adams optimisation"


A GC block for a heap check after a call should only take one or two instructions.
However the natural code:

```wiki
    ...put params in R1 R2 etc...
    call foo returns to L
 L: r = R1   -- get return value
    goto M
 M: Hp = Hp + 20
    if (Hp > HpLim) { call do_gc returns to K;
                   K: goto M; }
```


The label M is the head of the call-gc-and-try-again loop.
If we do this, we'll generate two info tables, one for L and one for K.


We can do better like this:

```wiki
    ...put params in R1 R2 etc...
    call foo returns to L
 L: r = R1
    goto M
 M: Hp = Hp + 20
    if (Hp > HpLim) { R1 = r
                      call do_gc_p returns to K;
                   K: r = R1; goto M; }
```


Now the `do_gc_p` call has the same return signature as `foo`
and can use the same continuation, thus:

```wiki
    ...put params in R1 R2 etc...
    call foo returns to L
 L: r = R1
    goto M
 M: Hp = Hp + 20
    if (Hp > HpLim) { R1 = r
                      call do_gc_p returns to L }
```


Now we can coalesce the uniquely-used block M into L, thus:

```wiki
    ...put params in R1 R2 etc...
    call foo returns to L
 L: r = R1
    Hp = Hp + 20
    if (Hp > HpLim) { R1 = r
                      call do_gc_p returns to L }
```


(A call followed by a `goto` thus gets optimized down to just the call.)


Now things are good.  Simple common block elimination (CBE) will common up K and L, so both calls share the same info table.

## Runtime system

- **Garbage collector entry points**: see `Note [Heap checks]` in `StgCmmHeapery`.

- **PAPs**


 


- **Update frames** and **exception handling**.  Also STM frames.

- **Primitives** can be rewritten:

  - Use parameters
  - In a few cases, use native calls (notably eval)
