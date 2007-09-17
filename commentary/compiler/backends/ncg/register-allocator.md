# The NCG Register Allocator

## Overview

TODO what a register allocator is responsible for.


GHC currently provides two register allocation algorithms, simple linear scan and graph coloring. Although some of the code is shared between the two, as we only want to maintain a single algorithm, support for linear scan will be removed in a subsequent version.


In the meantime, there are three options for register allocation:

- Linear allocator
  The linear allocator is currently turned on by default. This is what you get when you compile with -fasm. The linear allocator does a single pass through the code, allocating registers on a first-come-first-served basis. It is quick, and does a reasonable job for code with little register pressure. It has no look-ahead. If say, a particular register will be clobbered by a function call, it does not know to avoid allocating to that register in the code before the call - and subsequently inserts more spill/reload instructions than the other algorithms.

- Graph coloring allocator
  Enabled with `-fregs-graph`. The graph coloring algorithm operates on the native code for a whole function at a time. From each function it extracts a register conflict graph which represents which virtual regs are in use (live) at the same time, and thus cannot share a real reg. It tries to assign real regs (represented as colors) to the nodes so that no two adjacent nodes share the same color. This algorithm tends to do better than the linear allocator because the conflict graph helps it avoid the look-ahead problem. The coloring allocator also tries to allocate the source and destination of register-to-register move instruction to the same real reg. This is done by coalescing (merging) move-related nodes. If this succeeds then the move instruction can be erased.

- Graph coloring with iterative coalescing
  Enabled with `-fregs-iterative`. Iterative coalescing is an improvement over regular graph coloring whereby coalescing passes are interleaved with coloring passes. Iterative coalescing does a better job than regular graph coloring, but is slower.

## Code


The register allocator code is split into two main sections, the register allocator proper and a generic graph coloring library. The graph coloring library is also used by the Stg-\>Cmm converter.

### The register allocator

- [compiler/nativeGen/RegLiveness.hs](/trac/ghc/browser/ghc/compiler/nativeGen/RegLiveness.hs)

  Defines `LiveInstr` and `LiveCmmTop` which carry native machine instructions annotated with register liveness information. It also provides functions to annotate native code (`NatCmmTop`) with this liveness information, and to slurp out sets of register conflicts for feeding into the coloring allocator.

- [compiler/nativeGen/RegAllocColor.hs](/trac/ghc/browser/ghc/compiler/nativeGen/RegAllocColor.hs)

  Defines `regAlloc`, the main driver function for the graph coloring allocator. The driver accepts `LiveCmmTop`s which use virtual regs, and produces`NatCmmTops` which use real machine regs. This module also provides functions to help build and deep seq the register conflict graph.

- [compiler/nativeGen/RegAllocLinear.hs](/trac/ghc/browser/ghc/compiler/nativeGen/RegAllocLinear.hs)

  Defines the linear scan allocator. Its interface is identical to the coloring allocator.

- [compiler/nativeGen/RegAllocInfo.hs](/trac/ghc/browser/ghc/compiler/nativeGen/RegAllocInfo.hs)

  Defines the register information function, `regUsage`, which takes a set of real and virtual registers and returns the actual registers used by a particular `Instr`; register allocation is in AT&T syntax order (source, destination), in an internal function, `usage`; defines the `RegUsage` data type

- [compiler/nativeGen/RegSpillCost.hs](/trac/ghc/browser/ghc/compiler/nativeGen/RegSpillCost.hs)

  Defines `chooseSpill` which is responsible for selecting a virtual reg to spill to the stack when not enough real regs are available.

- [compiler/nativeGen/RegSpill.hs](/trac/ghc/browser/ghc/compiler/nativeGen/RegSpill.hs)

  Defines `regSpill` which takes `LiveCmmTop`s and inserts spill/reload instructions virtual regs that wouldn't fit in real regs. `regSpill` simply inserts spill/reloads for every use/def of a particular virtual reg.

- [compiler/nativeGen/RegSpillClean.hs](/trac/ghc/browser/ghc/compiler/nativeGen/RegSpillClean.hs)

  The spill cleaner is run after real regs have been allocated and erases spill/reload instructions inserted by `regSpill` that weren't strictly nessesary.

- [compiler/nativeGen/RegAllocStats.hs](/trac/ghc/browser/ghc/compiler/nativeGen/RegAllocStats.hs)

  Defines data types and pretty printers used for collecting statistics and debugging info from the coloring allocator.

### Graph coloring

- [compiler/util/GraphBase.hs](/trac/ghc/browser/ghc/compiler/util/GraphBase.hs)

  Defines the basic `Graph`, `Node` and `Triv` types used by the coloring algorithm.

- [compiler/util/GraphColor.hs](/trac/ghc/browser/ghc/compiler/util/GraphColor.hs)

  Defines the function `colorGraph` which is responsible for assigning colors (real regs) to nodes (virtual regs) in the register conflict graph. For more detail see Commentary/Compiler/GraphColoring Graph Coloring?.

- [compiler/util/GraphOps.hs](/trac/ghc/browser/ghc/compiler/util/GraphOps.hs)

  Defines functions to perform basic operations on the graphs such as adding, deleting, and coalescing nodes.

- [compiler/util/GraphPps.hs](/trac/ghc/browser/ghc/compiler/util/GraphPps.hs)

  Defines functions for pretty print graphs in human readable-ish and graphviz format.

### Miscellanea

- [compiler/nativeGen/RegCoalesce.hs](/trac/ghc/browser/ghc/compiler/nativeGen/RegCoalesce.hs)

  Defines a function `regCoalesce` that does aggressive coalescing directly on `LiveCmmTops`, without using the graph. This isn't used at the moment but has been left in incase we want to rejig the allocator when the new CPS converter comes online.

- [compiler/nativeGen/RegArchBase.hs](/trac/ghc/browser/ghc/compiler/nativeGen/RegArchBase.hs)

  Defines utils for calculating whether a register in the conflict graph is trivially colorable, in a generic way which handles aliasing between register classes. This module is not used directly by GHC. TODO link to description of algorithm.

- [compiler/nativeGen/RegArchX86.hs](/trac/ghc/browser/ghc/compiler/nativeGen/RegArchX86.hs)

  Contains a description of the aliasing constraints between the register sets on x86. This module is not used directly by GHC.
