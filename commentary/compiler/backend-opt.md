# Planned Backend Optimizations


This page gathers together a list of ideas and plans for improving the backend of the compiler. Any progress made will also be noted here.

## Removal of Proc Point Splitting


The reason we needed proc-point splitting for the LLVM backend is detailed [here](commentary/compiler/backends/llvm/wip#get-rid-of-proc-point-splitting).

#### Rationale


Ideally, we would expose an entire Cmm function to LLVM for better optimization and machine code generation, instead of breaking the functions apart just to grab a valid label.
The proc-point splitting is also a rather ugly transformation we would like to remove from the pipeline.

#### Challenges


The address of a basic block is [ not a well-defined concept](http://llvm.org/docs/LangRef.html#addresses-of-basic-blocks) outside of an LLVM function.
There has been no movement on this in LLVM due to the key assumption made by LLVM's IR that all basic blocks have *known* predecessors, forming a complete control-flow graph amongst blocks.
An escaping block label opens up the possibility of an edge from an unknown location.
This is why an indirect branch instruction must conservatively list [ all possible block targets](http://llvm.org/docs/LangRef.html#i-indirectbr).

#### Proposal


The current proposal can be found [ here.](http://lists.llvm.org/pipermail/llvm-dev/2017-April/112144.html)

## Improving Heap Checks


See [\#8905](https://gitlab.haskell.org//ghc/ghc/issues/8905) and [\#12231](https://gitlab.haskell.org//ghc/ghc/issues/12231) and \[Compiling case expressions\] in StgCmmExpr

### Other Performance Bugs To Consider

- [\#12798](https://gitlab.haskell.org//ghc/ghc/issues/12798)
- [\#12808](https://gitlab.haskell.org//ghc/ghc/issues/12808)