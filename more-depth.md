# Areas We'd Like To See Covered In More Depth

- The build system (for example)
- It would be quite interesting to hear what parts of GHC are changing rapidly and what parts seem quite stable.  Also, into which corners of the compiler do the righteous fear to tread?  (The evil mangler?)
- It is unclear how binding group analysis influences the use of 1 abstract syntax (HsSyn): how is the required ordering and grouping implemented, how does it affect error messages? 
- How does this Cabal thing work?
- How to read a dailysnapshot build log.
- Does ghc compile with VisualHaskell yet?
- In the compiler pipeline, please explain a little more about the invariants as a program flows through the pipeline.  For example, toward the back end, it would be useful to know at what point the following become known:

  - layout of heap objects
  - explicit heap pointer (allocation pointer)
  - explicit stack pointer
- What optimizations happen in the simplifier? How do they work?
- The GHC API and implementation of GHCi
- A breakdown of the size of the various parts of the run-time system
