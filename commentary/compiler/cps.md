# CPS Conversion


This part of the compiler is still under construction and it not presently in ghc-HEAD.
These notes are to document it for when it does get merged in.

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
  - Could make one branch primary and shuffle the other to match it, but that might entail unnessisary memory writes.

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

- We need the NCG to do aliasing analysis.  At present the CPS pass will generate the following, and will assume that the NCG can figure out when the loads and stores can be eliminated.

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