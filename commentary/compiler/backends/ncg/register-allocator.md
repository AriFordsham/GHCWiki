# The NCG Register Allocator

## Overview


The register allocator is split into two sections, the register allocator proper in nativeGen/Reg\*.hs and a generic graph coloring library in utils/Graph\*.hs.

### The register allocator

- [compiler/nativeGen/RegAllocColor.hs](/trac/ghc/browser/ghc/compiler/nativeGen/RegAllocColor.hs)

- [compiler/nativeGen/RegAllocLinear.hs](/trac/ghc/browser/ghc/compiler/nativeGen/RegAllocLinear.hs)

- [compiler/nativeGen/RegAllocStats.hs](/trac/ghc/browser/ghc/compiler/nativeGen/RegAllocStats.hs)

- [compiler/nativeGen/RegArchBase.hs](/trac/ghc/browser/ghc/compiler/nativeGen/RegArchBase.hs)

- [compiler/nativeGen/RegArchX86.hs](/trac/ghc/browser/ghc/compiler/nativeGen/RegArchX86.hs)

- [compiler/nativeGen/RegCoalesce.hs](/trac/ghc/browser/ghc/compiler/nativeGen/RegCoalesce.hs)

- [compiler/nativeGen/RegLiveness.hs](/trac/ghc/browser/ghc/compiler/nativeGen/RegLiveness.hs)

- [compiler/nativeGen/RegSpillClean.hs](/trac/ghc/browser/ghc/compiler/nativeGen/RegSpillClean.hs)

- [compiler/nativeGen/RegSpillCost.hs](/trac/ghc/browser/ghc/compiler/nativeGen/RegSpillCost.hs)

- [compiler/nativeGen/RegSpill.hs](/trac/ghc/browser/ghc/compiler/nativeGen/RegSpill.hs)

- [compiler/nativeGen/RegAllocInfo.hs](/trac/ghc/browser/ghc/compiler/nativeGen/RegAllocInfo.hs)

### Graph coloring

- [compiler/util/GraphBase.hs](/trac/ghc/browser/ghc/compiler/util/GraphBase.hs)

- [compiler/util/GraphColor.hs](/trac/ghc/browser/ghc/compiler/util/GraphColor.hs)

- [compiler/util/GraphOps.hs](/trac/ghc/browser/ghc/compiler/util/GraphOps.hs)

- [compiler/util/GraphPps.hs](/trac/ghc/browser/ghc/compiler/util/GraphPps.hs)