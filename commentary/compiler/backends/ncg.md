# Native Code Generator


For other information related to this page, see:

- [ the Old GHC Commentary: Native Code Generator](http://www.cse.unsw.edu.au/~chak/haskell/ghc/comm/the-beast/ncg.html) page (comments regarding Maximal Munch and register allocation optimisations are mostly still valid)
- [BackEndNotes](back-end-notes) for optimisation ideas regarding the current NCG

### Overview: Files, Parts


After GHC has produced Cmm (use -ddump-cmm or -ddump-opt-cmm to view), the Native Code Generator (NCG) transforms [Cmm](commentary/compiler/cmm-type) into architecture-specific assembly code.  The NCG is located in [compiler/nativeGen](/trac/ghc/browser/ghc/compiler/nativeGen) and is separated into eight modules:

- [compiler/nativeGen/AsmCodeGen.lhs](/trac/ghc/browser/ghc/compiler/nativeGen/AsmCodeGen.lhs)

  top-level module for the NCG, imported by [compiler/main/CodeOutput.lhs](/trac/ghc/browser/ghc/compiler/main/CodeOutput.lhs); also defines the Monad for optimising generic Cmm code, `CmmOptM`
- [compiler/nativeGen/MachCodeGen.hs](/trac/ghc/browser/ghc/compiler/nativeGen/MachCodeGen.hs)

  generates architecture-specific instructions (a Haskell-representation of assembler) from Cmm code
- [compiler/nativeGen/MachInstrs.hs](/trac/ghc/browser/ghc/compiler/nativeGen/MachInstrs.hs)

  contains data definitions and some functions (comparison, size, simple conversions) for machine instructions, mostly carried out through the `Instr` data type, defined here
- [compiler/nativeGen/NCGMonad.hs](/trac/ghc/browser/ghc/compiler/nativeGen/NCGMonad.hs)

  defines the the main monad in the NCG: the Native code Machine instruction Monad, `NatM`, and related functions.  *Note: the NCG switches between two monads at times, especially in `AsmCodeGen`: `NatM` and the `UniqSM` Monad used throughout the compiler.*
- [compiler/nativeGen/PositionIndependentCode.hs](/trac/ghc/browser/ghc/compiler/nativeGen/PositionIndependentCode.hs)

  handles generation of position independent code and issues related to dynamic linking in the NCG; related to many other modules outside the NCG that handle symbol import, export and references, including `CLabel`, `Cmm`, `codeGen` and the RTS, and the Mangler
- [compiler/nativeGen/PprMach.hs](/trac/ghc/browser/ghc/compiler/nativeGen/PprMach.hs)

  Pretty prints machine instructions (`Instr`) to assembler code (currently readable by GNU's `as`), with some small modifications, especially for comparing and adding floating point numbers on x86 architectures
- [compiler/nativeGen/RegAllocInfo.hs](/trac/ghc/browser/ghc/compiler/nativeGen/RegAllocInfo.hs)

  defines the main register information function, `regUsage`, which takes a set of real and virtual registers and returns the actual registers used by a particular `Instr`; register allocation is in AT&T syntax order (source, destination), in an internal function, `usage`; defines the `RegUsage` data type
- [compiler/nativeGen/RegisterAlloc.hs](/trac/ghc/browser/ghc/compiler/nativeGen/RegisterAlloc.hs)

  one of the most complicated modules in the NCG, `RegisterAlloc` manages the allocation of registers for each *basic block* of Haskell-abstracted assembler code: management involves *liveness* analysis, allocation or deletion of temporary registers, *spilling* temporary values to the *spill stack* (memory) and many optimisations.  *Note: much of this detail will be described later; **basic block** is defined below.*


and one header file:

- [compiler/nativeGen/NCG.h](/trac/ghc/browser/ghc/compiler/nativeGen/NCG.h)

  defines macros used to separate architecture-specific code in the Haskell NCG files; since GHC currently only generates machine code for the architecture on which it was compiled (GHC is not currently a cross-compiler), the Haskell NCG files become considerably smaller after preprocessing; ideally all architecture-specific code would reside in separate files and GHC would have them available to support cross-compiler capabilities.


The NCG has **machine-independent**  and **machine-dependent** parts.  


The **machine-independent** parts relate to generic programming, especially for optimisations, and Cmm.  The main machine-independent parts begin with *Cmm blocks.*  A *Cmm block* is roughly parallel to a Cmm function or procedure in the same way as a compiler may generate a C function into an assembler function composed of smaller *basic blocks* separated by branches (jumps).  *Cmm block*s are held as lists of `Cmm` statements (`[CmmStmt]`, defined in [compiler/cmm/Cmm.hs](/trac/ghc/browser/ghc/compiler/cmm/Cmm.hs), or the `type` synonym `CmmStmts`, defined in [compiler/cmm/CmmUtils.hs](/trac/ghc/browser/ghc/compiler/cmm/CmmUtils.hs)).  A machine-specific (assembler) instruction is represented as a `Instr`.

1. each Cmm block is lazily converted to abstract machine instructions (`Instr`) operating on an infinite number of registers--since the NCG Haskell files only contain instructions for the host computer on which GHC was compiled, these `Instr` are machine-specific;
1. for each *basic block* (a, contiguous block of instructions with no branches (jumps) in each *`Cmm` block*), real registers are lazily allocated based on the number of available registers on the target machine (say, 32 integer and 32 floating-point registers on the PowerPC architecture).
  *Note*: if a basic block simultaneously requires more registers than are available on the target machine and the temporary variable needs to be used (would sill be *live*) after the current instruction, it will be moved (*spilled*) into memory; and,
1. each Cmm block is optimised by reordering its basic blocks from the original order (the `Instr` order from the `Cmm`) to minimise the number of branches between basic blocks, in other words, by maximising fallthrough of execution from one basic block to the next.


The **machine-dependent** parts generally cover :

1. the number and kinds of registers available
1. 