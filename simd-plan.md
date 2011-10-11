## Introduction


This text documents the implementation stages / components for adding SIMD support to GHC for the LLVM Back-end.  The overriding requirements and architecture for the project are located back at the [ SIMD LLVM Page](http://hackage.haskell.org/trac/ghc/wiki/SimdLlvm).


Based on that design, the high-level tasks that must be accomplished include the following:

1. Add new PrimOps to allow Haskell to make use of Vectors
1. Add new MachOps to Cmm to communicate use of Vectors
1. Modify the LLVM Code Generator to translate Cmm to LLVM vector instructions
1. Demonstrate use of PrimOps from Haskell program
1. Modify Vector Library
1. Modify DPH Libraries
1. Arrange that the other Code Generators continue to generate non SIMD code


Introduction of SIMD support to GHC will occur in stages to demonstrate the entire “vertical” stack is functional:

1. Introduce “Double” PrimOps (as necessary to run an example showing SIMD usage in the LLVM)
1. Add appropriate Cmm support for the Double primtype / primop subset
1. Modify the LLVM Code Generator to support the Double vectorization
1. Demonstrate the PrimOps and do limited performance testing to ensure SIMD is functional
1. Modify Vector Libraries to make use of new PrimOps
1. Modify the DPH Libraries
1. Higher level examples using the above libraries
1. Build out the remaining PrimOps
1. Demonstrate full stack
1. Test remaining code generators

## Current Open Questions


These clearly won't be all of the questions I have, there is a substantial amount of work that goes through the entire GHC compiler stack before reaching the LLVM instructions.

## Add new PrimOps


Adding the new PrimOps is relatively straight-forward, but a  substantial number of LOC will be added to achieve it.  Most of this code is cut/paste "like" with minor type modifications.


Background: The following articles can aid in getting the work done:

- [ Primitive Operations (PrimOps)](http://hackage.haskell.org/trac/ghc/wiki/Commentary/PrimOps)
- [ Adding new primitive operations to GHC Haskell](http://hackage.haskell.org/trac/ghc/wiki/AddingNewPrimitiveOperations)
- Some guidelines for addition?, at least until I find something on the Wiki


The steps to be undertaken are:

1. Modify ./compiler/prelude/primops.txt.pp

  1. Add the following vector length constants as Int\# types

    - intVecLen, intVec8Len, intVec16Len, intVec32Len, intVec64Len, wordVecLen, wordVec8Len, wordVec16Len, wordVec32Len, wordVec64Len, floatVecLen, and doubleVecLen, 
  1. Then add the following primtypes:

    - Int : IntVec\#, Int8Vec\#, Int16Vec\#, Int32Vec\#, Int64Vec\#
    - Word : WordVec\#, Word8Vec\#, Word16Vec\#, Word32Vec\#, Word64Vec\#
    - Float : FloatVec\#
    - Double : DoubleVec\#
  1. Add the following primops associated with the above primtypes.   The ops come in groups associated with the types above, for example for IntVec\#’s we get the following family for the “plus” operation alone:

    - plusInt8Vec\# :: Int8Vec\# -\> Int8Vec\# -\> Int8Vec\#
    - plusInt16Vec\# :: Int16Vec\# -\> Int16Vec\# -\> Int16Vec\#
    - plusInt32Vec\# :: Int32Vec\# -\> Int32Vec\# -\> Int32Vec\#
    - plusInt64Vec\# :: Int64Vec\# -\> Int64Vec\# -\> Int64Vec\#
  1. Repeat this for the following set of operations on IntVec\#’s of various lengths, note that the signatures are summarized informally in parentheses behind the operation:        

    - plusIntVec\#, (signature :: Int8Vec\# -\> Int8Vec\# -\> Int8Vec\#)
    - minusIntVec\#, 
    - timesIntVec\#, 
    - quotIntVec\#, 
    - remIntVec\#
    - negateIntVec\# (signature :: IntVec\# -\> IntVec\#)
    - uncheckedIntVecShiftL\# (signature :: IntVec\# -\> Int\# -\> IntVec\#)
    - uncheckedIntVecShiftRA\#, 
    - uncheckedIntVecShiftRL\# 
  1. For the Word vectors we similarly introduce:

    - plusWordVec\#, minusWordVec\#, timesWordVec\#, quotWordVec\#, remWordVec\#, negateWordVec\#, andWordVec\#, orWordVec\#, xorWordVec\#, notWord\#, uncheckedWordVecShiftL\#, uncheckedWordVecShiftRL\#
  1. Float

    - plusFloatVec\#, minusFloatVec\#, timesFloatVec\#, quotFloatVec\#, remFloatVec\#, negateFloatVec\#, expFloatVec\#, logFloatVec\#, sqrtFloatVec\#<sub> sinFloatVec\#, cosFloatVec\#, tanFloatVec\#, asinFloatVec\#, acosFloatVec\#, atanFloatVec\#, sinhFloatVec\#, coshFloatVec\#, tanhFloatVec\#
      </sub>
  1. Double

    - plusDoubleVec\#, minusDoubleVec\#, timesDoubleVec\#, quotDoubleVec\#, remDoubleVec\#, negateDoubleVec\#, expDoubleVec\#, logDoubleVec\#, sqrtDoubleVec\#, sinDoubleVec\#, cosDoubleVec\#, tanDoubleVec\#, asinDoubleVec\#, acosDoubleVec\#, atanDoubleVec\#, sinhDoubleVec\#, coshDoubleVec\#, tanhDoubleVec\#
1. Modify ./compiler/codeGen/CgPrimOp.hs, code for each primop (above) must be added to complete the primop addition.

  1. The code, basically, links the primops to the Cmm MachOps (that, in turn, are read by the code generators)
  1. It looks like some Cmm extensions will have to be added to ensure alignment and pass vectorization information onto the back ends, the necessary MachOps will be determined after the first vertical stack is completed (using the "Double" as a model).  There may be some reuse from the existing MachOps.  There is some discussion to these extensions (or similar ones) on the original [ Patch 3557 Documentation](http://hackage.haskell.org/trac/ghc/ticket/3557)

## Add new MachOps to Cmm code


It may make more sense to add the MachOps to Cmm prior to implementing the PrimOps (or at least before adding the code to the CgPrimOp.hs file).  There is a useful [ Cmm Wiki Page](http://hackage.haskell.org/trac/ghc/wiki/Commentary/Compiler/CmmType#AdditionsinCmm) available to aid in the definition of the new Cmm operations.


The primary files that are involved in adding Cmm instructions are:

1. Modify CmmExpr.hs


Some existing Cmm instructions may be able to be reused, but there will have to be additional instructions added to account for vectorization primitives.  I'm still a little curious why these new instructions should not be built as out-of-line PrimOps.  It should be noted that the reference for adding primops appears to be wildly out of data for adding out-of-line primops.

## Modify LLVM Code Generator


Take the MachOps in the Cmm definition and translate correctly to the corresponding LLVM instructions.  LLVM code generation is in the /compiler/llvmGen directory.  The following will have to be modified (at a minimum):

- /compiler/llvmGen/Llvm/Types.hs - add the MachOps from Cmm and how they bridge to the LLVM vector operations
- /compiler/llvmGen/LlvmCodeGen/CodeGen.hs - This is the heart of the translation from MachOps to LLVM code.   Possibly significant changes will have to be added.

- Remaining /compiler/llvmGen/\* - Supporting changes


Once the LLVM Code Generator is modified to support Double instructions, tests can be run to ensure the “bottom half” of the stack works.

## Example: Demonstrate SIMD Operation


Once the Code Generator, PrimOps and Cmm are modified, we should be able to demonstrate performance scenarios.  The simplest example to use for demonstrating performance is to time vector additions and multiplications using the new vectorized instruction set against a similar addition or multiplication using another PrimOp.


The following two simple programs should demonstrate the difference in performance.  The program using the PrimOps *should* improve performance approaching 2x (Doubles are 64bit and SSE provides two 64bit registers).


Simple usage of the new instructions to add to vectors of doubles:

**Question:**  How does one create one of the new PrimOp types to test prior to testing the vector add operations?  This is going to have to be looked at a little ... the code should basically create a vector and then insertDoubleVec\# repeatedly to populate the vector.  Without the subsequent steps done, this will have to be "hand" done without additional operations defined.  Here is the response from Manuel to expand on this:  I am not quite sure what the best approach is. The intention in LLVM is clearly to populate vectors using the 'insertIntVec\#' etc functions. However, in LLVM you can just use an uninitialised register and insert elements into a vector successively. We could provide a vector "0" value in Haskell and insert into that. Any other ideas?

```wiki
	let x = ????
	let y = ???
	plusDoubleVec# x y
```


Using simple lists to achieve the same operation:

```wiki
	let x = [1,2,3,4]
	let y = [2,3,4,5]
	zipWith (+) x y
```


The above can be repeated with any of the common operations (multiplication, division, subtraction).  This should be sufficient with large sized vectors / lists to illustrate speedup.


(Note that over time and several generations of the integration, one would hope that the latter path would be “optimized” into SIMD instructions)

## Modify Vector Libraries and Vector Compiler Optimization (Pragmas and such)


Once we've shown there is speed-up for the lower portions of the compiler and have quantified it, the upper half of the stack should be optimized to take advantage of the vectorization code that was added to the PrimOps and Cmm.  There are two primary locations this is handled, in the compiler (compile/vectorize) code that vectorizes modules post-desugar process.  This location handles the VECTORISE pragmas as well as implicit vectorization of code.  The other location that requires modification is the Vector library itself.

1. Modify the Vector library /libraries/vector/Data to make use of PrimOps where possible and adjust VECTORISE pragmas if necessary

  - Modify the existing Vector code
  - We will likely also need vector versions of array read/write/indexing to process Haskell arrays with vector operations (this may need to go into compiler/vectorise)
  - Use the /libraries/vector/benchmarks to test updated code, look for

    - slowdowns - vector operations that cannot benefit from SIMD should not show slowdown
    - speedup - all performance tests that make use of maps for the common operators (+, -, \*, etc..) should benefit from the SIMD speedup
1. Modify the compiler/vectorise code to adjust pragmas and vectorization post-desugar process.  These modifications may not need to be made on the first pass through the code, more evaluation is necessary.

  - /compiler/vectorise/Vectorise.hs
  - /compiler/vectorise/Vectorise/Env.hs
  - /compiler/vectorise/Vectorise/Type/Env.hs


Once the benchmarks show measurable, reproducible behavior, move onto the DPH libraries.  Note that a closer inspection of the benchmarks in the /libraries/vector/benchmarks directory is necessary to ensure they reflect code that will be optimized with the use of SIMD instructions.  If they are not appropriate, add code that demonstrates SIMD speed-up appropriately.

## Modify DPH Libraries


The DPH libraries have heavy dependencies on the previous vectorization modification step (modifying the Vector libraries and the compiler vector options and post-desugar vectorization steps).  The DPH steps should not be undertaken without significant performance improvements illustrated in the previous steps.

1. The primary changes for DPH are in /libraries/dph/dph-common/Data/Array/Parallel/Lifted/\*
1. VECTOR SCALAR is also heavily used in /libraries/dph/dph-common/Data/Array/Parallel/Prelude, these should be inspected for update as well (Double.hs, Float.hs, Int.hs, Word8.hs)
1. Modify pragmas as necessary based on changes made above

**Note to Self:** Determine if the VECTORISE pragmas need adjustment or enhancement (based on previous steps)

## Ensure Remaining Code Generators Function Properly


There are really two options on the remaining code generators:

- Modify each code generator to understand the new Cmm instructions and restore them to non-vectorized instructions
- Add a compiler step that that does a pre-pass and replaces all "length = 1" vectors and operations on them by the corresponding scalar type and operations


The latter makes sense in that it is effective on all code generators, including the LLVM code generator.  Vectors of length = 1 should not be put through SIMD instructions to begin with (as they will incur substantial overhead for no return).


To make this work, a ghc compiler flag must be added that forces all vector lengths to 1 (this will be required in conjunction with any non-LLVM code generator).  A user can also use this option to turn off SIMD optimization for LLVM.

- Add the ghc compiler option: --vector-length=1
- Modify compiler/vectorise to recognize the new option or add this compiler pass as appropriate

## Reference Discussion Threads

```wiki
From: Manual Chakravarty
Q: Should the existing pure Vector libraries (/libraries/vector/Data/*) be modified to use the vectorized code as a first priority, wait until DPH (/libraries/dph/) is modified, or leave the Vector library as is?

A: The DPH libraries ('dph-*') are based on the 'vector' library — i.e., for DPH to use SIMD instruction, we must modify 'vector' first.

Q: How does one create one of the new Vector Types in a Haskell program (direct PrimOp?, for testing ... let x = ????)

A: I am not quite sure what the best approach is. The intention in LLVM is clearly to populate vectors using the 'insertIntVec#' etc functions. However, in LLVM you can just use an uninitialised register and insert elements into a vector successively. We could provide a vector "0" value in Haskell and insert into that. Any other ideas?

A: I just realised that we need vector version of the array read/write/indexing operations as well to process Haskell arrays with vector operations.

Q: One discussion point was that the "Vector Lengths" should be "Set to 1" for non LLVM code generation, where does this happen? On my first survey of the code, it seems that the code generators are partitioned from the main body of code, implying that each of the code generators will have to be modified to account for the new Cmm MachOps? and properly translate them to non-vectorized instructions.

A: Instead of doing the translation for every native code generator separately, we could have a pre-pass that replaces all length = 1 vectors and operations on them by the corresponding scalar type and operation.  Then, the actual native code generators wouldn't need to be changed.

A: The setting of the vector length to 1 needs to happen in dependence on the command line options passed to GHC — i.e., if a non-LLVM backend is selected.

Q: Can we re-use any of the existing MachOps? when adding to Cmm?

A: I am not sure.
```