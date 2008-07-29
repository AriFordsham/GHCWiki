---

## The pipeline: Make the new code generator work with the existing native codeGen

- **Code generator** converts STG to `CmmGraph`.  Implemented in `StgCmm*` modules (in directory `codeGen`). 

  - Parameter passing and stack adjustments are made explicit using the [''Stack Area'' abstraction.](commentary/compiler/stack-areas)
  - That includes a store of the return address.
  - No `CopyIn`, `CopyOut` nodes any more; instead "smart constructors" lower the calling convention to loads/stores/register transfers, using stack area abstraction.
  - But we still have `LastCall`, `LastReturn`, `LastBranch`, `LastJump` as `Last` nodes.
  - TODO Use the proper calling conventions (post Rep Swamp).

- **Simple control flow optimisation**, implemented in `CmmContFlowOpt`:

  - Branch chain elimination.
  - Remove unreachable blocks.
  - Block concatenation.  branch to K; and this is the only use of K.  
  - Common Block Elimination (like CSE). This essentially implements the Adams optimisation, we believe.
  - Consider (sometime): block duplication.  branch to K; and K is a short block.  Branch chain elimination is just a special case of this.

- **Proc-point analysis** and **transformation**, implemented in `CmmProcPointZ`.  (Adams version is `CmmProcPoint`.) The transformation part adds a function prologue to the front of each proc-point, following a standard entry convention.

  - The analysis produces a set of `BlockId` that should become proc-points
  - The transformation inserts a function prologue at the start of each proc-point, and a function epilogue just before each branch to a proc-point.

- **Add spill/reload**, implemented in `CmmSpillReload`, to spill live C-- variables before a call and reload them afterwards.  The spill and reload instructions are simply memory stores and loads respectively, using symbolic stack offsets (see [stack layout](commentary/compiler/stack-areas#laying-out-the-stack)).  For example, a spill of variable 'x' would look like `Ptr32[SS(x)] = x`.

- **Figure out the stack layout**

  - Each variable 'x', and each proc-point label 'K', has an associated *Area*, written SS(x) and SS(k) resp, that names a contiguous portion of the stack frame.  
  - The stack layout pass produces a mapping of: *(`Area` -\> `StackOffset`)*. For more detail, see [the description of stack layout.](commentary/compiler/stack-areas#laying-out-the-stack)
  - A `StackOffset` is the byte offset of a stack slot from the old end (high address) of the frame.  It doesn't vary as the physical stack pointer moves.

- **Manifest the stack pointer**.  Once the stack layout mapping has been determined, a second pass walks over the graph, making the stack pointer, `Sp` explicit. Before this pass, there is no `Sp` at all.  After this, `Sp` is completely manifest.

  - replacing references to `Areas` with offsets from `Sp`.
  - adding adjustments to `Sp`.

- **Split into multiple CmmProcs**.  At this point we build an info-table for each of the CmmProcs, including SRTs.  Done on the basis of the live local variables (by now mapped to stack slots) and live CAF statics.

  - `LastCall` and `LastReturn` nodes are replaced by `Jump`s.

**The Adams optimisation** is done by stuff above.  Given:

```wiki
  call f returns to K
  K: CopyIn retvals; goto L
  L: <code>
```

>
> transform to 
>
> ```wiki
>   call f returns to L
>   L : CopyIn retvals; <code>
> ```
>
> *and* move `CopyOut` into L's other predecessors.  ToDo: explain why this is a good thing.  In fact Common Block Elimination does this, we think.
