# Native Code Generator

### Overview: Files, Phases


After GHC has produced Cmm (use -ddump-cmm or -ddump-opt-cmm to view), the Native Code Generator (NCG) transforms [Cmm](commentary/compiler/cmm-type) into architecture-specific assembly code.  The NCG is located in [compiler/nativeGen](/trac/ghc/browser/ghc/compiler/nativeGen) and is separated into eight modules:

- [compiler/nativeGen/AsmCodeGen.lhs](/trac/ghc/browser/ghc/compiler/nativeGen/AsmCodeGen.lhs)
- [compiler/nativeGen/MachCodeGen.hs](/trac/ghc/browser/ghc/compiler/nativeGen/MachCodeGen.hs)
- [compiler/nativeGen/MachInstrs.hs](/trac/ghc/browser/ghc/compiler/nativeGen/MachInstrs.hs)
- [compiler/nativeGen/NCGMonad.hs](/trac/ghc/browser/ghc/compiler/nativeGen/NCGMonad.hs)
- [compiler/nativeGen/PositionIndependentCode.hs](/trac/ghc/browser/ghc/compiler/nativeGen/PositionIndependentCode.hs)
- [compiler/nativeGen/PprMach.hs](/trac/ghc/browser/ghc/compiler/nativeGen/PprMach.hs)
- [compiler/nativeGen/RegAllocInfo.hs](/trac/ghc/browser/ghc/compiler/nativeGen/RegAllocInfo.hs)
- [compiler/nativeGen/RegisterAlloc.hs](/trac/ghc/browser/ghc/compiler/nativeGen/RegisterAlloc.hs)


and one header file:

- [compiler/nativeGen/NCG.h](/trac/ghc/browser/ghc/compiler/nativeGen/NCG.h)


The NCG runs through two main phases: a **machine-independent** phase and a **machine-dependent** phase.  


The **machine-independent** phase begins with *Cmm blocks.*  A *Cmm block* is roughly parallel to a Cmm function or procedure in the same way as a compiler may generate a C function into a block of assembler instructions.  *Cmm block*s are held as lists of `Cmm` statements (`[CmmStmt]`, defined in [compiler/cmm/Cmm.hs](/trac/ghc/browser/ghc/compiler/cmm/Cmm.hs), or `type CmmStmts`, defined in [compiler/cmm/CmmUtils.hs](/trac/ghc/browser/ghc/compiler/cmm/CmmUtils.hs)).  A machine-specific (assembler) instruction is represented as a `Instr`, defined in [compiler/nativeGen/MachInstrs.hs](/trac/ghc/browser/ghc/compiler/nativeGen/MachInstrs.hs). During this phase:

1. each Cmm block is *lazily* converted to machine-specific instructions (`Instr`) operating on an infinite number of registers;
1. for each *basic block* (a, contiguous block of instructions with no branches (jumps) in each *`Cmm` block*), real registers are *lazily* allocated based on the number of available registers on the target machine (say, 32 integer and 32 floating-point registers on the PowerPC architecture),
  if a basic block simultaneously requires more registers than are available on the target machine and the temporary variable needs to be used (would sill be *live*) after the current instruction, it will be moved (*spilled*) into memory; and,
1. each Cmm block is optimised by reordering its basic blocks from the original order (the `Instr` order from the `Cmm`) to minimise the number of branches between basic blocks, in other words, by maximising fallthrough of execution from one basic block to the next.
