# Native Code Generator (NCG)


For other information related to this page, see:

- [ the Old GHC Commentary: Native Code Generator](http://darcs.haskell.org/ghc/docs/comm/the-beast/ncg.html) page (comments regarding Maximal Munch and register allocation optimisations are mostly still valid)
- [BackEndNotes](back-end-notes) for optimisation ideas regarding the current NCG
- The New GHC Commentary Cmm page: [The Cmm language](commentary/compiler/cmm-type) (the NCG code works from Haskell's implementation of C-- and many optimisations in the NCG relate to Cmm)
- [The register allocator](commentary/compiler/backends/ncg/register-allocator).

### Overview: Files, Parts


After GHC has produced [Cmm](commentary/compiler/cmm-type) (use -ddump-cmm or -ddump-opt-cmm to view), the Native Code Generator (NCG) transforms Cmm into architecture-specific assembly code.  The NCG is located in [compiler/nativeGen](/trac/ghc/browser/ghc/compiler/nativeGen) and is separated into eight modules:

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

  one of the most complicated modules in the NCG, `RegisterAlloc` manages the allocation of registers for each *basic block* of Haskell-abstracted assembler code: management involves *liveness* analysis, allocation or deletion of temporary registers, *spilling* temporary values to the *spill stack* (memory) and many optimisations.  *See [The Cmm language](commentary/compiler/cmm-type) for the definition of a *basic block* (in Haskell, *`type CmmBasicBlock =  GenBasicBlock CmmStmt`*).*


and one header file:

- [compiler/nativeGen/NCG.h](/trac/ghc/browser/ghc/compiler/nativeGen/NCG.h)

  defines macros used to separate architecture-specific code in the Haskell NCG files; since GHC currently only generates machine code for the architecture on which it was compiled (GHC is not currently a cross-compiler), the Haskell NCG files become considerably smaller after preprocessing; ideally all architecture-specific code would reside in separate files and GHC would have them available to support cross-compiler capabilities.


The NCG has **machine-independent**  and **machine-dependent** parts.  


The **machine-independent** parts relate to generic operations, especially optimisations, on Cmm code.  The main machine-independent parts begin with *Cmm blocks.*  (A *Cmm block* is a compilation unit of Cmm code, a file.  See [The Cmm language](commentary/compiler/cmm-type) for a discussion of what a *Cmm block* is but note that *Cmm* is a type synonym for `GenCmmTop CmmStatic CmmStmt`.)  A machine-specific (assembler) instruction is represented as a `Instr`.  The machine-independent NCG parts:

1. optimise each Cmm block by reordering its basic blocks from the original order (the `Instr` order from the `Cmm`) to minimise the number of branches between basic blocks, in other words, by maximising fallthrough of execution from one basic block to the next.
1. lazily convert each Cmm block to abstract machine instructions (`Instr`) operating on an infinite number of registers--since the NCG Haskell files only contain instructions for the host computer on which GHC was compiled, these `Instr` are machine-specific; and,
1. lazily allocate real registers for each basic block, based on the number of available registers on the target (currently, only the host) machine; for example, 32 integer and 32 floating-point registers on the PowerPC architecture.  The NCG does not currently have support for SIMD registers such as the vector registers for Altivec or any variation of SSE.
  *Note*: if a basic block simultaneously requires more registers than are available on the target machine and the temporary variable needs to be used (would sill be *live*) after the current instruction, it will be moved (*spilled*) into memory.


The **machine-dependent** parts:

1. define the abstract (Haskell) assembler `Instr` for the target (host) machine and convert every Cmm block into it;
1. define, manage and allocate the real registers available on the target system;
1. pretty-print the Haskell-assembler to GNU AS (GAS) assembler code
