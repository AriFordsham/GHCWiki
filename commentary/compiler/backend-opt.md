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


All values that are live at a return point are explicitly passed as arguments in register; we make no use of the built-in stack.
Thus, our core problem of jumping to a block label in LLVM is that we do not know which registers should hold live values, because we cannot pin registers within a function, and cannot assign a register convention to values incoming to a block.
  
Adding the ability to specify which registers to use when passing each value to a block should remove the need for splitting.


The current proposal direction is:

- Add annotations to phi nodes to specify which register to use for that value when it is passed to the block.
- Ensure that whenever a block address escapes a function, optimization & analysis in LLVM makes conservative decisions about that block (e.g., do not destroy it).


Other ideas include playing with [ deoptimization bundles](http://llvm.org/docs/LangRef.html#deoptimization-operand-bundles) or [ landing pads](http://llvm.org/docs/LangRef.html#i-landingpad)

TODO Do further investigation to find the path of least resistance to extend LLVM, compiling a proposal write-up.

TODO Float the proposal on the LLVM developers list to get feedback.

## Improving Heap Checks


See [\#8905](https://gitlab.haskell.org//ghc/ghc/issues/8905) and [\#12231](https://gitlab.haskell.org//ghc/ghc/issues/12231) and \[Compiling case expressions\] in StgCmmExpr

### Other Performance Bugs To Consider

- [\#12798](https://gitlab.haskell.org//ghc/ghc/issues/12798)
- [\#12808](https://gitlab.haskell.org//ghc/ghc/issues/12808)