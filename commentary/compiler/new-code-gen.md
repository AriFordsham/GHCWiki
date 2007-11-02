# Material about the new code generator


This page summarises work that Norman Ramsey, Simon M, and Simon PJ are doing on re-architecting GHC's back end.

## The new Cmm data type


There is a new Cmm data type:

- **`ZipCfg`** contains a generic zipper-based control-flow graph data type.  It is generic in the sense that it's polymorphic in the type of **middle nodes** and **last nodes** of a block.  (Middle nodes don't do control transfers; last nodes only do control transfers.)  There are extensive notes at the start of the module.

  The key types it defines are:

  - Block identifiers: `BlockId`, `BlockEnv`, `BlockSet`
  - Control-flow blocks: `Block`
  - Control-flow graphs: `Graph`
- **`ZipCfgCmm`** instantiates `ZipCfg` for Cmm, by defining types `Middle` and `Last` and using these to instantiate the polymorphic fields of `ZipCfg`.  It also defines a bunch of smart constructor (`mkJump`, `mkAssign`, mkCmmIfThenElse` etc) which make it easy to build `CmmGraph\`.


Todo list:

- Get rid of `CmmFormals` on `LastJump` and `LastCall` in `ZipCfgCmm` in favour of `CopyIn` and `CopyOut`.
- Change the C-- parser (which parses RTS .cmm files) to directly construct `CmmGraph`.  

## The pipeline

- Code generator converts STG to `CmmGraph`.

- **Simple control flow optimisation**, implemented in `CmmContFlowOpt`:

  - Branch chain elimination
  - Remove unreachable blocks
  - TODO block concatenation.  branch to K; and this is the only use of K.
  - Consider: block duplication.  branch to K; and K is a short block.  Branch chain elimination is just a special case of this.

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
