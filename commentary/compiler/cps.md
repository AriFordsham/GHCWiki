# CPS Conversion


This part of the compiler is now merged in ghc-HEAD.

## Overview


This pass takes Cmm with native proceedure calls and an implicit stack and produces Cmm with only tail calls implemented as jumps and an explicit stack.  In a word, it does CPS conversion.  (All right, so that's two words.)

## Design Aspects

- Proc-Point Analysis
- Calling Conventions
- Live Value Analysis
- Stack Layout

## Simple Design

- Split blocks into multiple blocks at function calls

  - TODO eliminate extra jump at block ends when there is already a jump at the end of the call
- Do liveness analysis
- Split every block into a separate function
- Pass all live values as parameters (probably slow)

  - Must arrange for both the caller and callee to know argument order

    - Simple design: callee just chooses some order and all callers must comply
  - Eventually could be passed implicitly but keeping things explicit makes things easier
  - Evantually could use a custom calling convention
  - Actual syntax is probably virtual.  (I.e. in an external table, not in actual syntax because that would require changes to the type for Cmm code)

    - Input code:

      ```wiki
      f {
        y = 1;
        z = 2;
        x = call g(a, b); // y, z live
        return x+y+z;
      }
      ```
    - Output code:

      ```wiki
      f {
        y = 1;
        z = 2;
        push_continuation h [y, z]; // Probably virtual
        jump g(a, b);
      }

      foreign "ret" h(x) {
        (y, z) = expand_continuation; // Probably virtual
        return x+y+z;
      } 
      ```
- Save live values before a call in the continuation

  - Must arrange for both the caller and callee to know field order

    - Simple design: callee just chooses some order and all callers must comply
  - Eventually needs to be optimized to reduce continuation shuffling

    - Can register allocation algorithms be unified with this into one framework?

## To be worked out

- The continuations for `f` and `g` are different.

  ```wiki
  if (test) {
    x = f();
  } else {
    y = g();
  }
  ```

  - Could make a for each that shuffles the arguments into a common format.
  - Could make one branch primary and shuffle the other to match it, but that might entail unnecessary memory writes.

## Pipeline

- CPS

  - Make closures and stacks manifest
  - Makes all calls are tail calls
- Parameter Elimination

  - Makes calling convention explicit
  - For externally visible functions calling conventions is machine specific, but not backend specific because functions compiled from different backends must be be able to call eachother
  - For local functions calling convention can be left up to the backend because it can take advantage of register allocation.

    - However, the first first draft will specify the standard calling convention for all functions even local ones because:

      - It's simpler
      - The C code generator can't handle function parameters because of the Evil Mangler
      - The NCG doesn't yet understand parameters

## TODO

- Downstream

  - Argument passing convention
  - Stack check

    - Needs some way to synchronize the branch label with the heap check
- Midstream

  - Support `switch` (needed by rts/Apply.cmm)
  - More factoring and cleanup/documentation
  - Wiki document the designed choosen
  - Better stack slot selection
  - Foreign function calls
  - Garbage collector
  - Proc points

    - May cause new blocks
    - May cause new functions
    - Lives could be passes either on stack or in arguments
  - Proc grouping of blocks
- Upstream

  - Have `codeGen` emit C-- with functions.

## Current Pipeline

### `cmmToRawCmm`


The `Cmm`/`parseCmmFile` pipeline and the `Stg`/`codeGen` pipeline
can each independantly use the CPS pass.
However, they currently bypass it untill the CPS code becomes stablized,
but they must both use the `cmmToRawCmm` pass.
This pass converts the header on each function from a `CmmInfo`
to a `[CmmStatic]`.

## Non-CPS Changes

- Cmm Syntax Changes

  - The returns parameters of a function call must be surrounded by parenthesis.
    For example

    ```wiki
    foreign "C" fee ();
    (x) = foreign "C" foo ();
    (x, y) = foreign "C--" bar ();
    ```

    This is simply to avoid shift-reduce conflicts with assignment.
    Future revisions to the parser may eliminate the need for this.

- Variable declarations may are annotated to indicate
  whether they are GC followable pointers.

  ```wiki
  W_ x; // Not GC followable
  "ptr" W_ y, z; // Both GC followable
  ```
- The bitmap of a `INFO_TABLE_RET` is now specified using
  a parameter like syntax.

  ```wiki
  INFO_TABLE_RET(stg_ap_v, RET_SMALL) { ... } // No args
  INFO_TABLE_RET(stg_ap_d, RET_SMALL, D_ unused1) { ... } // Single double arg
  INFO_TABLE_RET(stg_ap_np, RET_SMALL, W_ non_ptr, "ptr" W_ pointer) { ... }
    // Pointerhood indicated by "ptr" annotation
  ```

  Note that these are not real parameters, they are the stack layout
  of the continuation.  Also, until the CPS algorithm
  gets properly hooked into the `Cmm` path the parameter names are not used.
- The return values of a function call may only be `LocalReg`.
  This is due to changes in the `Cmm` data type.

- Cmm Data Type Changes

  - The return parameters of a `CmmCall` are `LocalReg` instead of `CmmReg`.
    This is because a `GlobalReg` doesn't have a well defined pointerhood,
    and the return values will become parameters to continuations where
    their pointerhood will be needed.
  - The type of info tables is now a separate parameter to `GenCmmTop`

    - Before

      ```wiki
      data GenCmmTop d i
        = CmmProc [d] ...
        | CmmData Section [d]
      ```
    - After

      ```wiki
      data GenCmmTop d h i
        = CmmProc h ...
        | CmmData Section [d]
      ```

      This is to support using either `CmmInfo` or `[CmmStatic]`
      as the header of a `CmmProc`.
    - Before info table conversion use `Cmm`

      ```wiki
      type Cmm = GenCmmTop CmmStatic CmmInfo CmmStmt
      ```
    - After info table conversion use `RawCmm`

      ```wiki
      type RawCmm = GenCmmTop CmmStatic [CmmStatic] CmmStmt
      ```

      Same for `CmmTop` and `RawCmmTop`.
  - New type aliases `CmmActuals`, `CmmFormals` and `CmmHintFormals`.
    Respectively these are the actual parameters of a function call,
    the formal parameters of a function, and the
    return results of a function call with pointerhood annotation
    (CPS may convert these to formal parameter of the call's continuation).

## Notes

- Changed the parameter to a `CmmTop` to be `CmmFormals` instead of `[LocalReg]`

  - `CmmFormals` are `[(CmmReg,MachHint)]`
  - This field seems to not have been being used; it only require a type change
- GC can be cleaned up b/c of the CPS

  - Before

    ```wiki
    f (x, y) {
      if (Hp + 5 > HpLim) {
        jump do_gc;
        // do_gc looks up the function type in it's info table
        // and saves a "RET_FUN" frame on the stack
        // RET_FUN fames are different than every other frame
        // in that the info table isn't on the RET_FUN code
        // but on the slot under RET_FUN which is filled with 'f'
      }
    }
    g (a, b, c, d, e) {
      if (Hp + 5 > HpLim) {
        jump do_gc;
        // Gen-calls have a special tag
      }
    }
    ```
  - After

    ```wiki
    f (f, x, y) {
      if (Hp + 5 > HpLim) {
        jump do_gc_pp(f, x, y); // becomes jump do_gc_pp;
        // Tail call, note that arguments are in same order
        // as the arguments to 'f'
        // Also can skip the table lookup and
        // do_gc_pp can be high-level C-- and save us the
        // work of saving f, x, and y
      }
    }
    g (g, a, b, c, d, e) {
      if (Hp + 5 > HpLim) {
        call do_gc(g, a, b, c, d, e);
        jump g(g, a, b, c, d, e);
        // The general form will generate
        // a custom continuation
      }
    }
    ```

- We need the NCG to do aliasing analysis.  At present the CPS pass will generate the following, and will assume that the NCG can figure out when the loads and stores can be eliminated.  (The global saves part of a `CmmProc` is dead b/c of this.)

  ```wiki
  foo () {
    // Parameters in regs
    a = R1;
    b = R2;
    // Parameters on stack
    c = [Sp-8];
    d = [Sp-4];
    // Saved live variables
    e = [Sp+4];
    f = [Sp+8];

    /*
    ...
    Misc code that might mutate variables
    or branch or loop or any other evil thing
    ...
    */

    // A tail call (there could be multiple blocks that have this)
    a = R1;
    b = R2;
    // Parameters on stack
    c = [Sp-8];
    d = [Sp-4];
    // Saved live variables
    e = [Sp+4];
    f = [Sp+8];
    
  }
  ```

- Simple calls

  - Before

    ```wiki
    f(..., z, ...) {
      ...
      r = f(x, y);
      ...
      ... = z;
      ... = r;
    }
    ```
  - Output of CPS

    ```wiki
    f() {
      z=R1
      ...
      ... = z;
      ...
      R1 = x;
      R2 = y;
      call f;
      r = R1
      ...
      ... = z;
      ... = r;
    }
    ```
  - Optimization by the NCG

    ```wiki
    f() {
      ...
      ... = R1;
      ...
      z = R1;
      R1 = x;
      R2 = y;
      call f;
      ...
      ... = z;
      ... = R1;
    }
    ```

## Loopholes


There are a number of deviations from what one might expect from a CPS algorithm
due to the need to encode existing optimizations and idioms.

### GC Blocks


For obvious reasons,
the stack used by GC blocks does not count tward the maximum amount of stack
used by the function.


This loophole is overloaded by the GC **functions** so they don't create their own
infinite loop.  The main block is marked as being the GC block so its stack usage
doesn't get checked.

### Update Frames


Update frame have to be pushed onto the stack at the begining of an update function.
We could do this by wrapping the update function inside another function
that just does the work of calling that other function,
but since updates are so common we don't want to pay the cost
of that extra jump.
Thus a function can be annotated with a frame that should be pushed on entry.


Note that while the frame is equivalent to a tail call at the end of the function,
the frame must be pushed at the beginning of the function because parts of the blackhole
code look for these update frames to determine what thunks are under evaluation.

### User defined continuations


Pushing an update frame on the stack requires the ability to define a
function that will pull that frame from the stack and have access to any
values within the frame.  This is done with user-defined continuations.

### Branches to continuations


A GC block for a heap check after a call should only take one or two instructions.
However the natural code:

```wiki
  r = foo(1, 2);
  goto L;
 L:
  if (Hp < HpLim) { do_gc(); goto L; }
```


would generate a trivial continuation for the `do_gc` call as
well as a trivial continuation for the `foo` call that just calls
the proc point `L`.


We solve this by changing the syntax to

```wiki
  r = foo(1, 2);
  goto L;
 L:
  if (Hp < HpLim) { r = do_gc_p(r); goto L; }
```


Now the `do_gc_p` call has the same return signature as `foo`
and can use the same continuation.
(A call followed by a `goto` thus gets optimized down to just the call.)

## Not in Scope of Current Work


Improvements that could be made but that will not be implemented durring the curent effort.

### Static Reference Table Handling (SRT)


As it stands, each function and thus each call site must be annotated with a bitmap and
a pointer or offset to the SRT shared by the function.
This does not interact with the stack in any way so it ought to be outside the scope of the CPS
algorithm.
However there is some level of interaction because

1. the SRT information on each call site needs to be attached to the resulting continuation and
1. functions read from a Cmm file might need to be annotated with that SRT info.


The first is a concern for correctness but may be handled by treating the SRT info as opaque data.
The second is a concern for ease of use and thus the likelyhood of mistakes in hand written C-- code.
At the moment it appears that all of the C-- functions in
the runtime system (RTS) use a null SRT so for now we'll just have the CPS algorithm treat the SRT info as opaque.


In the future it would be nice to have a more satisfactory way to handle both these issues.

### Cmm Optimization assumed by CPS


In order to simplify the CPS pass, it makes some assumptions about the optimizer.

- The CPS pass may generate more blocks than strictly necessary.  In particular,
  it might be possible to join together two blocks when the second block is only
  entered by the first block.  This is a simple optimization that needs to be implemented.
- The CPS pass may generate more loads and stores than strictly necessary.  In particular,
  it may load a local register only to store it back to the same stack location a few
  statements later.  There may be intervening branches.  The optimizer
  needs to be extended to eliminate these load store pairs.

## Notes on future development

### Handling GC

```wiki
stg_gc_gen        = mkRtsApFastLabel SLIT("stg_gc_gen")

stg_gc_fun_v      = mkRtsApFastLabel SLIT("stg_gc_fun_v")
stg_gc_fun_n      = mkRtsApFastLabel SLIT("stg_gc_fun_n")
stg_gc_fun_f      = mkRtsApFastLabel SLIT("stg_gc_fun_f")
stg_gc_fun_d      = mkRtsApFastLabel SLIT("stg_gc_fun_d")
stg_gc_fun_l      = mkRtsApFastLabel SLIT("stg_gc_fun_l")
stg_gc_fun_p      = mkRtsApFastLabel SLIT("stg_gc_fun_p")
stg_gc_fun_pp     = mkRtsApFastLabel SLIT("stg_gc_fun_pp")
stg_gc_fun_ppp    = mkRtsApFastLabel SLIT("stg_gc_fun_ppp")
stg_gc_fun_pppp   = mkRtsApFastLabel SLIT("stg_gc_fun_pppp")
stg_gc_fun_ppppp  = mkRtsApFastLabel SLIT("stg_gc_fun_ppppp")
stg_gc_fun_pppppp = mkRtsApFastLabel SLIT("stg_gc_fun_pppppp")

stg_gc_ret_v      = stg_gc_gen -- Conceptually different but practically the same
stg_gc_ret_n      = mkRtsApFastLabel SLIT("stg_gc_ret_n")
stg_gc_ret_f      = mkRtsApFastLabel SLIT("stg_gc_ret_f")
stg_gc_ret_d      = mkRtsApFastLabel SLIT("stg_gc_ret_d")
stg_gc_ret_l      = mkRtsApFastLabel SLIT("stg_gc_ret_l")
stg_gc_ret_p      = mkRtsApFastLabel SLIT("stg_gc_ret_p")
stg_gc_ret_pp     = mkRtsApFastLabel SLIT("stg_gc_ret_pp")
stg_gc_ret_ppp    = mkRtsApFastLabel SLIT("stg_gc_ret_ppp")
stg_gc_ret_pppp   = mkRtsApFastLabel SLIT("stg_gc_ret_pppp")
stg_gc_ret_ppppp  = mkRtsApFastLabel SLIT("stg_gc_ret_ppppp")
stg_gc_ret_pppppp = mkRtsApFastLabel SLIT("stg_gc_ret_pppppp")

call stg_gc_fun_gen() -> stg_gc_gen
jump stg_gc_fun_v(f')
jump stg_gc_fun_p(f', p)
jump stg_gc_fun_n(f', n)

gcFunPattern []                        = Just stg_gc_fun_v
gcFunPattern [x]
  | isNonPtr x && matchRep x NonPtrArg = Just stg_gc_fun_n
  | isNonPtr x && matchRep x LongArg   = Just stg_gc_fun_l
  | isNonPtr x && matchRep x FloatArg  = Just stg_gc_fun_f
  | isNonPtr x && matchRep x DoubleArg = Just stg_gc_fun_d
gcFunPattern xs
  | all (\x -> isPtr x && matchRep x PtrArg)
  = case length xs of
      1 -> Just stg_gc_fun_p
      2 -> Just stg_gc_fun_pp
      3 -> Just stg_gc_fun_ppp
      4 -> Just stg_gc_fun_pppp
      5 -> Just stg_gc_fun_ppppp
      6 -> Just stg_gc_fun_pppppp
      _ -> Nothing
gcFunPattern xs = Nothing

case gcFunPattern formals of
  Nothing -> [stg_gc_gen_call srt, CmmJump target formals]
  Just gc -> [stg_gc_fun_spec_jump gc formals]

stg_gc_gen_call srt =
  CmmCall (CmmForeignCall (CmmLit (CmmLabel stg_gc_gen)) CmmCallConv)
          [] [] (CmmSafe srt)
stg_gc_fun_spec_jump gc formals =
  CmmJump (CmmLit (CmmLabel gc)) (map formal_to_actual formals)

isNonPtr x = kind x == KindNonPtr
matchRep x rep = localRep x == argMachrep rep

call stg_gc_ret_gen() -> stg_gc_gen
call stg_gc_ret_v() -> stg_gc_gen
call p = stg_gc_ret_p(p)
call n = stg_gc_ret_n(n)
```

```wiki
hpStkCheck:
    rts_label | is_fun    = CmmReg (CmmGlobal GCFun)
                                -- Function entry point
              | otherwise = CmmReg (CmmGlobal GCEnter1)
                                -- Thunk or case return
        -- In the thunk/case-return case, R1 points to a closure
        -- which should be (re)-entered after GC

altHeapCheck:
    rts_label PolyAlt = CmmLit (CmmLabel (mkRtsCodeLabel SLIT( "stg_gc_unpt_r1")))
        -- Do *not* enter R1 after a heap check in
        -- a polymorphic case.  It might be a function
        -- and the entry code for a function (currently)
        -- applies it
        --
        -- However R1 is guaranteed to be a pointer

    rts_label (AlgAlt tc) = stg_gc_enter1
        -- Enter R1 after the heap check; it's a pointer
        
    rts_label (PrimAlt tc)
      = CmmLit $ CmmLabel $ 
        case primRepToCgRep (tyConPrimRep tc) of
          VoidArg   -> mkRtsCodeLabel SLIT( "stg_gc_noregs")
          FloatArg  -> mkRtsCodeLabel SLIT( "stg_gc_f1")
          DoubleArg -> mkRtsCodeLabel SLIT( "stg_gc_d1")
          LongArg   -> mkRtsCodeLabel SLIT( "stg_gc_l1")
                                -- R1 is boxed but unlifted: 
          PtrArg    -> mkRtsCodeLabel SLIT( "stg_gc_unpt_r1")
                                -- R1 is unboxed:
          NonPtrArg -> mkRtsCodeLabel SLIT( "stg_gc_unbx_r1")

    rts_label (UbxTupAlt _) = panic "altHeapCheck"

unbxTupleHeapCheck:
    rts_label       = CmmLit (CmmLabel (mkRtsCodeLabel SLIT("stg_gc_ut")))

hpChkGen:
  stg_gc_gen

hpChkNodePointsAssignSp0:
-- a heap check where R1 points to the closure to enter on return, and
-- we want to assign to Sp[0] on failure (used in AutoApply.cmm:BUILD_PAP).
  stg_gc_enter1

stkChkGen:
  stg_gc_gen

stkChkNodePoints:
  stg_gc_enter1

--------
-- hpStkCheck (is_fun)
stg_gc_fun = CmmReg (CmmGlobal GCFun)
-- hpStkCheck (no is_fun), altHeapCheck (AlgAlt), hpChkNodePointsAssignSp0, stkChkNodePoints
stg_gc_enter1 = CmmReg (CmmGlobal GCEnter1)
-- hpChkGen, stkChkGen
stg_gc_gen = CmmLit (CmmLabel (mkRtsCodeLabel SLIT("stg_gc_gen")))
-- unbxTupleHeapCheck
stg_gc_ut = CmmLit (CmmLabel (mkRtsCodeLabel SLIT("stg_gc_ut")))

-- altHeapCheck (PrimAlt)
--  VoidArg
stg_gc_noregs = CmmLit (CmmLabel (mkRtsCodeLabel SLIT( "stg_gc_noregs")))
--  FloatArg
stg_gc_f1 = CmmLit (CmmLabel (mkRtsCodeLabel SLIT( "stg_gc_f1")))
--  DoubleArg
stg_gc_d1 = CmmLit (CmmLabel (mkRtsCodeLabel SLIT( "stg_gc_d1")))
--  LongArg
stg_gc_l1 = CmmLit (CmmLabel (mkRtsCodeLabel SLIT( "stg_gc_l1")))
--  PtrArg and PolyAlt
stg_gc_unpt_r1 = CmmLit (CmmLabel (mkRtsCodeLabel SLIT( "stg_gc_unpt_r1")))
--  NonPtrArg
stg_gc_unbx_r1 = CmmLit (CmmLabel (mkRtsCodeLabel SLIT( "stg_gc_unbx_r1")))
```