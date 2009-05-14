# The DPH packages


This page describes the packages (aka libraries) that form part of DPH, enumerates all the wyas in which they differ from "normal" GHC packages, and says what stuff lives where.

## What packages there are


DPH comes with the following blobs of source code:

- `dph/dph-prim-seq` implements the DPH Primitive Interface for sequential machines.  Compiling it yields package `dph-prim-seq`.

- `dph/dph-prim-par` implements the DPH Primitive Interface for parallel machines.  Compiling it yields package `dph-prim-seq`.

- `dph/dph-base` implements the DPH User Interface, by importing the DPH Primitive Interface. The same source code can be compiled in two ways: 

  - Importing `dph-prim-seq`, generating package `dph-base-seq`
  - Importing `dph-prim-seq`, generating package `dph-base-seq`

**SLPJ: what is dph-common?**


The DPH libraries (in particular `dph-base`) use Template Haskell, so they can only be compiled with a stage2 compiler.

## How the DPH packages are coupled to GHC


GHC knows about DPH as follows.  A single flag `-dph` switches on the following:

- Adds `-package dph` (**SLPJ: correct?**), so that the user can `import Data.Array.Parallel`.  And so that the right package gets linked in the link step.

- Runs a special pass called the **vectoriser**.  This pass generates code that mentions (by Original Name) various functions defined in `dph-prim-seq` or `dph-prim-par` (depending on the compiler flag used).  So if you change where a function is defined in `dph-prim-*`, or the name of the function, you have to make a corresponding change in GHC.

**SLPJ: is it correct that GHC only generates Names in dph-prim?  If not, could it be made true?**

## Array library of flat and segmented operations

**TODO** Here we need to document the structure of the current implementation with subpages for the more complicated aspects (e.g., representation types, distributed types, and gangs).
