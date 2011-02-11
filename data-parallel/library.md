# The DPH packages


This page describes the packages (aka libraries) that form part of DPH, enumerates all the wyas in which they differ from "normal" GHC packages, and says what stuff lives where.

## What packages there are


DPH comes with the following blobs of source code:

- `dph/dph-prim-seq` implements the DPH Primitive Interface for sequential machines.  Compiling it yields package `dph-prim-seq`.  It depends on the packages `vector` and `primitive`.

- `dph/dph-prim-par` implements the DPH Primitive Interface for parallel machines.  Compiling it yields package `dph-prim-seq`.

- `dph/dph-base` implements the DPH User Interface, by importing the DPH Primitive Interface. The same source code can be compiled in two ways: 

  - Importing `dph-prim-seq`, generating package `dph-base-seq`
  - Importing `dph-prim-seq`, generating package `dph-base-seq`

**SLPJ: what is dph-common?**


The DPH libraries (in particular `dph-base`) use Template Haskell, so they can only be compiled with a stage2 compiler.

## How the DPH packages are coupled to GHC


GHC knows about DPH as follows.

- The flags `-fdph-seq` and `-fdph-par` add `-package dph-seq` and `-package-dph-par`, respectively, so that the user can `import Data.Array.Parallel` and friends.  And so that the right package gets linked in the link step.

- The flag `-fvectorise` runs a special pass called the **vectoriser**. 

  - The vectoriser generates code that mentions (by Original Name) various functions defined in `dph-prim-seq` or `dph-prim-par` (depending on the compiler flag used).  So if you change where a function is defined in `dph-prim-*`, or the name of the function, you have to make a corresponding change in GHC.
  - The vectoriser knows quite a lot about the internal working of the library. For instance, it knows about the array representation.
  - Parts of the library are vectorised and since these are low-level parts, they rely on being vectorised in particular ways. This means that a particular version of the library will only work correctly with a particular version of the vectoriser and vice versa.  **SLPJ: can you be more precise here?  Which packages are vectorised?  What do you meean by "particular ways"?**

**SLPJ: is it correct that GHC only generates Names in dph-prim?  If not, could it be made true?**

## Array library of flat and segmented operations

**TODO** Here we need to document the structure of the current implementation with subpages for the more complicated aspects (e.g., representation types, distributed types, and gangs).  Here a dump of an old description of the library structure (which may be somewhat out of date):

```wiki
Data.Array.Parallel.Lifted
	... the final user types ...

Data.Array.Parallel.Unlifted
  Re-exports Data.Array.Parallel.Unlifted.Parallel
	     Data.Array.Parallel.Unlifted.Distributed
	     Data.Array.Parallel.Unlifted.Segmented
	     Data.Array.Parallel.Unlifted.Flat

Data.Array.Parallel.Unlifted.Parallel
  Parallel operations over UArrs.  No new data types!
  These operations each
	- convert the incoming (UArr t) to a Dist (UArr t)
	- run the operation in parallel using a gang
	- convert the result back to a (UArr t')
  Plus fusion rules, of course!

Data.Array.Parallel.Unlifted.Distributed
  Logically: type Dist a = Array GangMember a
	That is, one 'a' per gang member
  Mutable version: MDist

  Element types: unboxed, and products, (), 
		 *and* UArr, SUArr, Segd.
 
Data.Array.Parallel.Unlifted.Segmented
  Provides SUArr, which are segmented UArrs with exactly one nesting level
  Logically: type SUArr a = UArr (UArr a)
  Element types: unboxed, and products and ().

  Again, a mutable version MSUArr is defined internally.

Data.Array.Parallel.Unlifted.Flat
  Provides immutable (UArr) arrays of unboxed
  values, and strict products thereof.

  Simply lifts BUArr to work over strict products (incl unit).
  Internally to Flat, we also define mutable (MUArr) arrays, 
  but they aren't exported.

	.UArr: representation
	.Loop: main loop combinator over UArr, loopU
	.Fusion: fusion rules for loopU
	.Combinators: instantiate loopU (to get fold, scan, map etc)
  
  The exported maps and folds over these arrays are 
  purely sequential

Data.Array.Parallel.Arr.BUArr
  Arrays of *unboxed* values, immutable (BUArr t) and mutable (MBUArr
  t), indexed by Int.  Supports slicing (= sub-segment) operations.

  ToDo: combine with UArray and STUArray? (But they are indexed by Ix.)

  ToDo: a common pattern (e.g. when filtering) is that we allocate a
  big mutable array, *partially* fill it, and then want to shrink to
  fixed size when freezing it. (This is needed in Don's ByteString
  library too.)


Data.Array.Parallel.Base.Fusion
  Specialised combining functions that specialise loopU to be map, fold, etc
  They are also useful for loopBU; hence not in Unlifted.Flat
  
Data.Array.Parallel.Arr.BBArr
  Similar to BUArr, but for strict, boxed values.  
  Representation: Array, STArray.
  Main application: array of UArrs in Distibuted.Types
```