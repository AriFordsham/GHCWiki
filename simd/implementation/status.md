## Current Status



**As of 26 June 2017**


### Tickets

See the ~SIMD label.


### Vector types


Vectors of the following types are implemented: `Int32`, `Int64`, `Float`, and `Double`. These types and their associated primops can be found in [`GHC.Prim`](https://downloads.haskell.org/~ghc/8.0.2/docs/html/libraries/ghc-prim-0.5.0.0/GHC-Prim.html#g:28).

### Fixed and variable sized vectors


For each type, currently only one vector width is implemented, namely the width that is appropriate for SSE2. This means that vectors are currently all 16 bytes in size.

## Code generators


Only the LLVM code generator (i.e. `-fllvm`) is supported. However, work is [afoot](https://github.com/Abhiroop/ghc-1/tree/wip/simd-ncg-support) to add support to the NCG as well.

## Cmm layer


Our `CmmType` representation for vectors differs slightly from the proposal. See [cmm/CmmType.hs](https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/cmm/CmmType.hs?rev=e42746d07239888c74e937046fadf93655b44b65#L42).


See [cmm/CmmMachOp.hs](https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/cmm/CmmMachOp.hs?rev=e42746d07239888c74e937046fadf93655b44b65#L106) for the new vector MachOps.

## Core layer


The implementation differs from the proposal in its naming scheme. We wanted to avoid overloading the term "vector," so, e.g., a 4-wide SIMD vector of `Float#`s is a `FloatX4#`.


See [compiler/prelude/primops.txt.pp](https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/prelude/primops.txt.pp?rev=e42746d07239888c74e937046fadf93655b44b65#L1935) for the new primops. Not everything in the proposal is implemented, but we do have a useful subset.

## Native vector sizes


This is unimplemented. Instead we define a higher-level `Multi` data family whose instance is platform-dependent. For example, a `Multi Int` is represented using an `Int32X4#` on a 32-bit platform, and by a `Int64X2#` on a 64-bit platform.

## ABIs and calling conventions


Integrating variable-sized vectors with GHC's calling convention is a challenge. How many new registers do we add? Do we add registers for each vector type? The correct approach is unclear, so the current implementation passes all SIMD vectors on the stack.

### Memory alignment for vectors



The implementation does not attempt to align memory containing SIMD vectors. SIMD vector loads and stores do not assume alignment.


### Other resources of interest


- This `ghc-devs` discussion: [https://mail.haskell.org/pipermail/ghc-devs/2017-March/013899.html](https://mail.haskell.org/pipermail/ghc-devs/2017-March/013899.html)
