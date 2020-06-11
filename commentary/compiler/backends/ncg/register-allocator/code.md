## Register Allocator Code



The register allocator code is split into two main sections, the register allocator proper and a generic graph coloring library. The graph coloring library is also used by the Stg-\>Cmm converter.


### The register allocator


- [compiler/GHC/CmmToAsm/Reg/Liveness.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/CmmToAsm/Reg/Liveness.hs) 

  Defines `LiveInstr` and `LiveCmmTop` which carry native machine instructions annotated with register liveness information. It also provides functions to annotate native code (`NatCmmTop`) with this liveness information, and to slurp out sets of register conflicts for feeding into the coloring allocator.

- [compiler/GHC/CmmToAsm/Reg/Graph.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/CmmToAsm/Reg/Graph.hs)

  Defines `regAlloc`, the main driver function for the graph coloring allocator. The driver accepts `LiveCmmTop`s which use virtual regs, and produces`NatCmmTops` which use real machine regs. This module also provides functions to help build and deep seq the register conflict graph.

- [compiler/GHC/CmmToAsm/Reg/Linear.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/CmmToAsm/Reg/Linear.hs) 

  Defines the linear scan allocator. Its interface is identical to the coloring allocator.

- [compiler/GHC/CmmToAsm/Instr.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/CmmToAsm/Instr.hs) 

  Defines the register information function, `regUsage`, which takes a set of real and virtual registers and returns the actual registers used by a particular `Instr`; register allocation is in AT&T syntax order (source, destination), in an internal function, `usage`; defines the `RegUsage` data type

- [compiler/GHC/CmmToAsm/Reg/Graph/SpillCost.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/CmmToAsm/Reg/Graph/SpillCost.hs) 

  Defines `chooseSpill` which is responsible for selecting a virtual reg to spill to the stack when not enough real regs are available.

- [compiler/GHC/CmmToAsm/Reg/Graph/Spill.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/CmmToAsm/Reg/Graph/Spill.hs) 

  Defines `regSpill` which takes `LiveCmmTop`s and inserts spill/reload instructions virtual regs that wouldn't fit in real regs. `regSpill`'s strategy is to simply inserts spill/reloads for every use/def of a particular virtual reg. This inefficient code is cleaned up by the spill cleaner after allocation.


- [compiler/GHC/CmmToAsm/Reg/Graph/SpillClean.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/CmmToAsm/Reg/Graph/SpillClean.hs) 

  The spill cleaner is run after real regs have been allocated. It erases spill/reload instructions inserted by `regSpill` that weren't strictly nessesary.

- [compiler/GHC/CmmToAsm/Reg/Graph/Stats.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/CmmToAsm/Reg/Graph/Stats.hs) 

  Defines data types and pretty printers used for collecting statistics and debugging info from the coloring allocator.

### Graph coloring


- [compiler/GHC/Data/Graph/Base.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/Data/Graph/Base.hs) 

  Defines the basic `Graph`, `Node` and `Triv` types used by the coloring algorithm.

- [compiler/GHC/Data/Graph/Color.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/Data/Graph/Color.hs) 

  Defines the function `colorGraph` which is responsible for assigning colors (real regs) to nodes (virtual regs) in the register conflict graph.

- [compiler/GHC/Data/Graph/Ops.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/Data/Graph/Ops.hs) 

  Defines functions to perform basic operations on the graphs such as adding, deleting, and coalescing nodes.

- [compiler/GHC/Data/Graph/Ppr.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/Data/Graph/Ppr.hs) 

  Defines functions for pretty print graphs in human readable-ish and graphviz format.

### Miscellanea


- [compiler/GHC/CmmToAsm/Reg/Graph/Coalesce.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/CmmToAsm/Reg/Graph/Coalesce.hs) 

  Defines a function `regCoalesce` that does aggressive coalescing directly on `LiveCmmTops`, without using the graph. This isn't used at the moment but has been left in incase we want to rejig the allocator when the new CPS converter comes online.

- [compiler/GHC/CmmToAsm/Reg/Graph/Base.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/CmmToAsm/Reg/Graph/Base.hs) 

  Defines utils for calculating whether a register in the conflict graph is trivially colorable, in a generic way which handles aliasing between register classes. This module is not used directly by GHC.

- [compiler/GHC/CmmToAsm/Reg/Graph/X86.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/CmmToAsm/Reg/Graph/X86.hs) 

  Contains a description of the aliasing constraints between the register sets on x86. This module is not used directly by GHC.
