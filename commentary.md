## 0. Overview (SPJ)

- [The overall organisation of GHC](commentary/organisation)
- [Source Tree Roadmap](commentary/source-tree), and [module dependencies](module-dependencies)
- [The compilation pipeline](commentary/pipeline)
- [Coding guidelines](commentary/coding-style)

## 1. Building (SM)

- Getting the code (do a live build...)
- Setting up the build
- Building
- tweaking & recompiling, etc.
- how libraries/packages are built
- ghc-inplace vs. installed ghc, how does it run inplace (find its bits)
- how to "use" the build system and common tasks, pointer to docs
- using/extending the testsuite

## 2. The Compiler

- Roadmap: [compiling a single module](commentary/compiler/hsc-main)
- Source-language syntax: [HsSyn](commentary/compiler/hs-syn-type)
- Names: [RdrName and OccName](commentary/compiler/rdr-name-type), [Name](commentary/compiler/name-type)
- [Entities](commentary/compiler/entity-types): variables, type constructors, data constructors, and classes.
- Types: Type
- [The core language](commentary/compiler/core-syn-type).
- Cmm.
- SPJ renamer, typechecker, desugarer, core-\>core
- SPJ ModIface, ModDetails, ModGuts
- SPJ Core-\>CorePrep-\>Stg-\>Cmm
- SM PrimOPs: primops.txt.pp - what is generated from it?
- SM GHC API
- SM HscMain upwards: ModSummary, Finder, upsweep, downsweep,

## 3. Back end (SM)

-  SM execution model STG + eval/apply
-  SM Cmm-\>NCG
-  SM Cmm-\>BCO  (simple compilation scheme, no primops: GHC.PrimopWrappers)
-  SM Cmm-\>C

## 4. [The Runtime System](commentary/rts)