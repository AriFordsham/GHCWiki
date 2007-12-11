# Material about the new code generator


This page summarises work that Norman Ramsey, Simon M, and Simon PJ are doing on re-architecting GHC's back end.


Status:

- The Rep swamp is drained: see [Commentary/Compiler/BackEndTypes](commentary/compiler/back-end-types)
- Code generator: first draft done.
- Control-flow opt: simple ones done

  - Common block elmination: to do
  - Block concatenation: to do
- Adams optimisation: currently done somewhere but not modularly.  I think.
- Proc-point analysis and transformation: done?
- Add spill/reload: done?
- Stack slot alloction?
- Make stack explicit: to do
- Split into multiple CmmProcs: to do


Norman's plan

1. New code to check invariants of output from `ZipDataflow`
1. Finish debugging `ZipDataflow`
1. Use Simon PJ's 'common-blockifier' to move the Adams optimization outside `CmmProcProintZ`
1. ProcPointZ does not insert `CopyOut` nodes; this omission must be rectified and will require some general infrastructure for inserting predecessors.
1. Simple optimizations on `CopyIn` and `CopyOut` may be required
1. Define an interface for calling conventions and invariants for the output of frame layout \[will require help from Simon M\]
1. Stack layout
1. Glue the whole pipeline together and make sure it works.


Items 1-5 look like a few days apiece. Items 6 and 7 are more scary...


ToDo: main issues

- SRTs simply record live global variables.  So we should use the same live-variable framework as for live local variables.  That means we must be able to identify which globals are SRT-able.  What about compression/encoding schemes?

- Draining the Rep swamp.

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

- **`ZipCfg`** contains a generic zipper-based control-flow graph data type.  It is generic in the sense that it's polymorphic in the type of **middle nodes** and **last nodes** of a block.  (Middle nodes don't do control transfers; last nodes only do control transfers.)  There are extensive notes at the start of the module.

  The key types it defines are:

  - Block identifiers: `BlockId`, `BlockEnv`, `BlockSet`
  - Control-flow blocks: `Block`
  - Control-flow graphs: `Graph`
- **`ZipDataFlow`** contains a generic framework for solving dataflow problems over `ZipCfg`.
- **`ZipCfgCmm`** instantiates `ZipCfg` for Cmm, by defining types `Middle` and `Last` and using these to instantiate the polymorphic fields of `ZipCfg`.  It also defines a bunch of smart constructor (`mkJump`, `mkAssign`, `mkCmmIfThenElse` etc) which make it easy to build `CmmGraph`.
- **`CmmExpr`** contains the data types for Cmm expressions, registers, and the like.  It does not depend on the dataflow framework at all.

---

## The pipeline

- **Code generator** converts STG to `CmmGraph`.  Implemented in `StgCmm*` modules (in directory `codeGen`).

- **Simple control flow optimisation**, implemented in `CmmContFlowOpt`:

  - Branch chain elimination
  - Remove unreachable blocks
  - TODO block concatenation.  branch to K; and this is the only use of K.
  - Consider: block duplication.  branch to K; and K is a short block.  Branch chain elimination is just a special case of this.
  - TODO Common block elimination (like CSE). This makes something else significantly simpler.  (**ToDo**: what?).

- **The Adams optimisation**.  Given:

  ```wiki
    call f returns to K
    K: CopyIn retvals; goto L
    L: <code>
  ```

  transform to 

  ```wiki
    call f returns to L
    L : CopyIn retvals; <code>
  ```

  *and* move `CopyOut` into L's other predecessors.  ToDo: explain why this is a good thing.

- **Proc-point analysis** and **transformation**, implemented in `CmmProcPointZ`.  (Adams version is `CmmProcPoint`.) The transfomation part adds a `CopyIn` to the front of each proc-point, which expresses the idea that proc-points use a standard entry convention. 

- **Add spill/reload**, implemented in `CmmSpillReload`, to spill live C-- variables before a call and reload them afterwards.  The middle node of the result is `Middle` (from `ZipCfgCmm` extended with `Spill` and `Reload` constructors.  
  Invariant: (something like) all variables in a block are gotten from `CopyIn` or `Reload`. 

- **Stack slot layout**.  Build inteference graph for variables live across calls, and allocate a stack slot for such variables.  That is, stack slot allocation is very like register allocation.

- **Make the stack explicit**. 

  - Convert `CopyIn`, `CopyOut`, `Spill`, `Reload` to hardware-register and stack traffic.
  - Add stack-pointer adjustment instructions.
  - Avoid memory traffic at joins. (What does this mean?)

- **Split into multiple CmmProcs**.

---

## Runtime system

- **Garbage collector entry points**: see `Note [Heap checks]` in `StgCmmHeapery`.

- **PAPs**

- **Update frames** and **exception handling**.  Also STM frames.

- **Primitives** can be rewritten:

  - Use parameters
  - In a few cases, use native calls (notably eval)
