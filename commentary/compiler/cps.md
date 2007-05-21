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
