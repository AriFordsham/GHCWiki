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

- Runs a special pass called the **vectoriser**.  This generates a close coupling between the vectoriser and the library:

  - The vectoriser generates code that mentions (by Original Name) various functions defined in `dph-prim-seq` or `dph-prim-par` (depending on the compiler flag used).  So if you change where a function is defined in `dph-prim-*`, or the name of the function, you have to make a corresponding change in GHC.
  - The vectoriser knows quite a lot about the internal working of the library. For instance, it knows about the array representation.
  - Parts of the library are vectorised and since these are low-level parts, they rely on being vectorised in particular ways. This means that a particular version of the library will only work correctly with a particular version of the vectoriser and vice versa.  **SLPJ: can you be more precise here?  Which packages are vectorised?  What do you meean by "particular ways"?**

**SLPJ: is it correct that GHC only generates Names in dph-prim?  If not, could it be made true?**

## DPH and ways


When compiling a module with `-dph`, its imported modules must also have been compiled with `-dph`.  It's a bit like profiling; so maybe compiling with `-dph` should count as another "way".  This is an unresolved issue.  Compiling the entire `base` package (say) with `-dph` might well be overkill; for example, we don't want to vectorise the IO library.


At the moment, we finesse this problem by simply requiring that the user solves it; and hence we do not use any `base` package functions in vectorised code.

## Array library of flat and segmented operations

**TODO** Here we need to document the structure of the current implementation with subpages for the more complicated aspects (e.g., representation types, distributed types, and gangs).
