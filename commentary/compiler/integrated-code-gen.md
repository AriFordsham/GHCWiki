
The long-term plan for reworking GHC's back end is to produce an ``Integrated Code Generator*, which will break down the barrier between the machine-independent code generator (CPS conversion, stack layout, etc) and the native-code generators (instruction selection, calling conventions, register allocation -- including spilling to the C stack, etc). The goal is to simplify the back ends by reducing code duplication and to improve the quality of the generated code by making machine-specific decisions (such as register usage) using knowledge of the actual target machine.
*


Philosophy


The main infrastructure of the back end may be complicated in some cases, but the  the interface for extending a back end should be as simple as possible. For example, the implementation of the dataflow framework is quite complicated. But we can use the framework to write a new optimization by simply writing down the dataflow transfer functions that are found in standard compiler textbooks. Better yet, we can write combined ``superoptimizations* with no more effort than writing the dataflow transfer functions for each individual optimization.
*


Pipeline

1. Convert from STG to a flat representation of C--: Stg -\> Cmm
1. Build control-flow graph: Cmm -\> ZGraphCmm\<stack slots, compile-time constants\>

  - Converts the flat representation to a control-flow graph, with Cmm statements representing instructions in the basic blocks.
  - Implements calling conventions for call, jump, and return instructions: all parameter passing is turned into data-movement instructions (register-to-register move, load, or store), and stack-pointer adjustments are inserted. After this point, calls, returns, and jumps are just control-transfer instructions -- the parameter passing has been compiled away.
  - How do we refer to locations on the stack when we haven't laid it out yet? The compiler names a stack slot using the idea of a "late compile-time constant," which is just a symbolic constant that will be replaced with an actual stack offset when the stack layout is chosen.
1. Code expansion (instruction selection): ZGraph Cmm\<stack slots, compile-time constants\> -\> ZGraph Instrs\<stack slots, compile-time constants\>

  - Expands each Cmm instruction into a series of instructions. The representation of an instruction can be chosen by the back end. In some compilers (vpo, gcc, QC--), machine instructions are represented using RTLs. But Machine SUIF uses a target-specific, abstract representation that must satisfy a well-defined interface (i.e. by using a typeclass). It would be nice to support both.
1. Optimizer: ZGraph Instrs\<stack slots, compile-time constants\> -\> ZGraph Instrs\<stack slots, compile-time constants\>


Implicit in this pipeline:

- Besides the expander, (parts of) the optimizer, and the code emitter, the rest of the passes should work on any chosen representation of instructions. Typeclasses are our friends.
