# Using SIMD instructions in GHC

**Goal**: improve program running times by taking advantage of CPU's SIMD vector instructions.

**How**: by extending GHC to generate code using SIMD vector instructions and by modifying libraries as necessary.


This page describes the issues involved and a design for implementing SIMD vector support in GHC.

## Introduction


We are interested in the SIMD vector instructions on current and future generations of CPUs. This includes SSE and AVX on x86/x86-64 and NEON on ARM chips (targets like GPUs or FPGAs are out of scope for this project). These SIMD vector instruction sets are broadly similar in the sense of having relatively short vector registers and operations for various sizes of integer and/or floating point operation. In the details however they have different capabilities and different vector register sizes.


We therefore want a design for SIMD support in GHC that will let us efficiently exploit current vector instructions but a design that is not tied too tightly to one CPU architecture or generation. In particular, it should be possible to write portable Haskell programs that use SIMD vectors. This implies that we need fallbacks for cases where certain types or operations are not supported directly in hardware.


On the other hand, we want to be able to write programs for maximum efficiency that exploit the native vector sizes, preferably while remaining portable. For example, algorithms on large variable length vectors are in principle agnostic about the size of the primitive vector operations.


Finally, we want a design that is not too difficult or time consuming to implement.

### Existing SIMD instruction sets


Intel and AMD CPUs use the [ SSE family](http://en.wikipedia.org/wiki/Streaming_SIMD_Extensions) of extensions and, more recently (since Q1 2011), the [ AVX](http://en.wikipedia.org/wiki/Advanced_Vector_Extensions) extensions.  ARM CPUs (Cortex A series) use the [ NEON](http://www.arm.com/products/processors/technologies/neon.php) extensions. PowerPC and SPARC have similar vector extensions. Variations between different families of SIMD extensions and between different family members in one family of extensions include the following:

<table><tr><th>**Register width**</th>
<td>
SSE registers are 128 bits, whereas AVX registers are 256 bits, but they can also still be used as 128 bit registers with old SSE instructions. NEON registers can be used as 64-bit or 128-bit register.
</td></tr>
<tr><th>**Register number**</th>
<td>
SSE sports 8 SIMD registers in the 32-bit i386 instruction set and 16 SIMD registers in the 64-bit x84_64 instruction set. (AVX still has 16 SIMD registers.) NEON's SIMD registers can be used as 32 64-bit registers or 16 128-bit registers.
</td></tr>
<tr><th>**Register types**</th>
<td>
In the original SSE extension, SIMD registers could only hold 32-bit single-precision floats, whereas SSE2 extend that to include 64-bit double precision floats as well as 8 to 64 bit integral types. The extension from 128 bits to 256 bits in register size only applies to floating-point types in AVX. This is expected to be extended to integer types in AVX2, but in AVX, SIMD operations on integral types can only use the lower 128 bits of the SIMD registers. NEON registers can hold 8 to 64 bit integral types and 32-bit single-precision floats.
</td></tr>
<tr><th>**Alignment requirements**</th>
<td>
SSE requires alignment on 16 byte boundaries. With AVX, it seems that operations on 128 bit SIMD vectors may be unaligned, but operations on 256 bit SIMD vectors needs to be aligned to 32 byte boundaries. NEON suggests to align SIMD vectors with *n*-bit elements to *n*-bit boundaries.
</td></tr></table>

### SIMD/vector support in other compilers


Both GCC and LLVM provide some low-level yet portable support for SIMD vector types and operations.


GCC provides [ vector extensions](http://gcc.gnu.org/onlinedocs/gcc/Vector-Extensions.html) to C where the programmer may define vector types of a fixed size. The standard C `+`, `-`, `*` etc operators then work on these vector types. GCC implements these operations using whatever hardware support is available. Depending on the requested vector size GCC uses native vector registers and instructions, or synthesises large requested vectors using smaller hardware vectors. For example it can generate code for operating on vectors of 4 doubles by using SSE2 registers and operations which only handle vectors of doubles of size 2.


The LLVM compiler tools targeted by GHC's [LLVM backend](commentary/compiler/backends/llvm) support a generic [ vector type](http://llvm.org/docs/LangRef.html#t_vector) of arbitrary, but fixed length whose elements may be any LLVM scalar type. In addition to three [ vector operations](http://llvm.org/docs/LangRef.html#vectorops), LLVM's operations on scalars are overloaded to work on vector types as well. LLVM compiles operations on vector types to target-specific SIMD instructions, such as those of the SSE, AVX, and NEON instruction set extensions. As the capabilities of the various versions of SSE, AVX, and NEON vary widely, LLVM's code generator maps operations on LLVM's generic vector type to the more limited capabilities of the various hardware targets.

## General plan


We need to implement support for vectors in several layers of GHC + Libraries, from bottom to top:

- code generators (NCG, LLVM)
- Cmm
- Haskell/Core primops
- Some strategy for making use of vector primops, e.g. DPH or Vector lib

### Fixed and variable sized vectors


The hardware supports only small fixed sized vectors. High level libraries would like to be able to use arbitrary sized vectors. Similar to the design in GCC and LLVM we provide primitive Haskell types and operations for fixed-size vectors. The task of implementing variable sized vectors in terms of fixed-size vector types and primops is left to the next layer up (DPH, vector lib).


That is, in the core primop layer and down, vector support is only for fixed-size vectors. The fixed sizes will be only powers of 2 and only up to some maximum size. The choice of maximum size should reflect the largest vector size supported by the current range of CPUs (e.g. 256bit with AVX).

### Fallbacks


The portabilty strategy relies on fallbacks so that we can implement large vectors on machines with only small vector registers, or no vector support at all (either none at all, or none for that type, e.g. only support for integer vectors not floating point, or only 32bit floats not doubles).


The obvious approach is a transformation to synthesize larger vector types and operations using smaller vector operations or scalar operations. This synthesisation could plausible be done at the core, Cmm or code generator layers, however the most natural choice would be as a Cmm -\> Cmm transformation. This approach would reduce or eliminate the burden on code generators by allowing them to support only their architecture's native vector sizes and types, or none at all.


Using fallbacks does pose some challenges for a stable/portable ABI, in particular how vector registers should be used in the GHC calling convention. This is discussed in a later section.

### Code generators


We would not extend the portable C backend to emit vector instructions. It would rely on the higher layers transforming vector operations into scalar operations. The portable C backend is not ABI compatible with the other code generators so there is no concern about vector registers in the calling convention.


The LLVM C library supports vector types and instructions directly. The GHC LLVM backend could be extended to translate vector ops at the CMM level into LLVM vector ops.


The NCG (native code generator) may need at least minimal support for vector types if vector registers are to be used in the calling convention. This would be necessary if ABI compatibility is to be preserved with the LLVM backend. It is optional whether vector instructions are used to improve performance.

## Cmm layer


The Cmm layer will be extended to represent vector types and operations.


The `CmmType` describes the machine-level type of data. It consists of the "category" of data, along with the `Width` in bits.

```wiki
data CmmType = CmmType CmmCat Width
data Width = ...
data CmmCat     -- "Category" (not exported)
   = GcPtrCat   -- GC pointer
   | BitsCat    -- Non-pointer
   | FloatCat   -- Float
```


The current code distinguishes floats, pointer and non-pointer data. These are distinguished primarily either because they need to be tracked separately (GC pointers) or because they live in special registers on many architectures (floats).


For vectors we add two new categories

```wiki
   | VBitsCat  Multiplicity   -- Non-pointer
   | VFloatCat Multiplicity   -- Float

type Multiplicty = Int
```


We keep vector types separate from scalars, rather than representing scalars as having multiplicty 1. This is to limit distruption to existing code paths and also because it is expected that vectors will often need to be treated differently from scalars. Again we distinguish float from integral types as these may use different classes of registers.


Vector operations on these machine vector types will be added to the Cmm `MachOp` type, e.g.

```wiki
data MachOp = 
  ...
  | MO_VF_Add Width Multiplicity
```


For example `MO_VF_Add W64 4` represents vector addition on a length-4 vector of 64bit floats.

## Core layer


We need Haskell data types and Haskell primitive operations for fixed size vectors. In some ways this is a harder problem than representing the vector types and opertions at the Cmm level. In particular, at the Haskell type level we cannot easily parametrise on the vector length.


Our design is to provide a family of fixed size vector types and primitive operations, but not to provide any facility to parametrise this family on the vector length.


Syntax note: here {m} is meta-syntax, not concrete syntax


for width {w} in 8, 16, 32, 64 and "" -- empty for native Int\#/Word\# width
for multiplicity {m} in 2, 4, 8, 16, 32

```wiki
type Int{w}Vec{m}#
type Word{w}Vec{m}#
type FloatVec{m}#
type DoubleVec{m}#
```


It has not yet been decided if we will use a name convention such as:

```wiki
IntVec2#    IntVec4#  IntVec8# ...
Int8Vec2#   ...
Int16Vec2#
...
```


Or if we will add a new concrete syntax to suggest a paramater, but have it really still part of the name, such as:


Syntax note: here \<2\> is concrete syntax

```wiki
IntVec<2>#    IntVec<4>#  IntVec<8># ...
Int8Vec<2>#   ...
Int16Vec<2>#
..
```


Similarly there would be families of primops:

```wiki
extractInt{w}Vec{m}#  :: Int{w}Vec{m}# -> Int# -> Int{w}#
addInt{w}Vec{m}#      :: Int{w}Vec{m}# -> Int{w}Vec{m}# -> Int{w}Vec{m}#
```


From the point of view of the Haskell namespace for values and types, each member of each of these families is distinct. It is just a naming convention that suggests the relationship (with or without the addition of some concrete syntax to support the convention).

### Primop generation and representation


Internally in GHC we can take advantage of the obvious parametrisation within the families of primitive types and operations. In particular we extend GHC's primop.txt.pp machinery to enable us to describe the family as a whole and to generate the members.


For example:

```wiki
paramater <w> Width 8,16,32,64
paramater <m> Multiplicity 2,4,8,16,32

primop VIntAddOp <w> <m> "addInt<w>Vec<m>#" Dyadic
  Int{w}Vec{m}# -> Int{w}Vec{m}# -> Int{w}Vec{m}#
  {doc comments}
```


This would generate a family of primops, and an internal representation using the obvious parameters:

```wiki
data PrimOp = ...
   | IntAddOp
   | VIntQuotOp Width Multiplicity
```

### Optional: primitive int sizes


The same mechanism could be used to handle parametrisation between Int8\#, Int16\# etc. Currently these do not exist as primitive types. The types Int8, Int16 etc are implemented as a boxed native-sized Int\# plus narrowing.


Note that while this change is possible and would make things more uniform it is not essential for vector support.


That is we might have:

```wiki
primtype Int<w>#

primop   IntAddOp <w>    "addInt<w>#"    Dyadic
   Int# -> Int# -> Int#
   with commutable = True
```


generating

```wiki
data PrimOp = ...
   | IntAddOp Width
```


We might want to specify the values \<w\> and \<m\> range over in each operation rather than globally, or override it locally. For example we might want to support Int8 vectors up to size 32 but Double vectors only up to size 8. Or we might want to distinguish the native size or treat it uniformly, e.g.:

```wiki
primtype Int#

primop   IntAddOp    "+#"    Dyadic
   Int# -> Int# -> Int#
   with commutable = True

primtype Int<w>#
  with w = 8,16,32,64

primop   IntAddOp <w>    "addInt<w>#"    Dyadic
   Int# -> Int# -> Int#
   with commutable = True
        w = 8,16,32,64
```


Or we might want some other solution so we can use `+#` as well as `addInt<8>#` since `+<8>#` as an infix operator is more than a bit obscure!

## Sub-architecture challenges

TODO make sure we've made clear our proposed design:


Decision:

- fixed native size vector per arch, not sub-arch, picked as max
- instruction selection is per-module via -msse -mavx
- worker/wrapper with common and specialised ABI including rationale for why this should perform well enough vs compiling everything with one ABI

## Native vector sizes


In addition to various portable fixed size vector types, we would like to have a portable vector type that matches the native register size. This is analogous to the existing integer types that GHC supports. We have Int8, Int16, Int32 etc and in addition we have Int, the size of which is machine dependent (either 32 or 64bit).


As with Int, the rationale is efficiency. For algorithms that could work with a variety of primitive vector sizes it will almost always be fastest to use the vector size that matches the hardware vector register size. Clearly it is suboptimal to use a vector size that is smaller than the native size. Using a larger vector is not nearly as bad as using as smaller one: though it does contribute to register pressure. There is also the difficulty of picking a fixed register size that is always at least as big as the native size on all platforms that are likely to be used and doing so makes less sense as vector sizes on some architectures increases.


Note that the actual size of the native vector size will be fixed per architecture and will not vary based on "sub-architecture" features like SSE vs AVX. We will pick the size to be the maximum of all the sub-architectures. That is we would pick the AVX size for x86-64. The rationale for this is ABI compatibility which is discussed below. In this respect, the IntVec\# is like Int\#, the size of both is crucial for the ABI and is determined by the target platform/architecture.


So we extend our family of vector types with:

```wiki
IntVec#   IntVec2#    IntVec4#  IntVec8# ...
Int8Vec#  Int8Vec2#   ...
Int16Vec# Int16Vec2#
...
```


and there are some top level constants describing the vector size so as to enable their portable use

```wiki
intVecSize :: Int
```


The native-sized vector types are distinct types from the explicit-sized vector types, not type aliases for the corresponding explicit-sized vector. This is to support and encourage portable code.

## ABIs and calling conventions


For each CPU architecture GHC has a calling convention that it uses for all Haskell function calls. The calling convention specifies where function arguments and results are placed in registers and on the stack. Adhering to the calling convention is necessary for correctness. Code compiled using different calling conventions should not be linked together. Note that currently the LLVM and NCG code generators adhere to the same ABI.


The calling convention needs to be extended to take into account the primitive vector types. We have to decide if vectors should be passed in registers or on the stack and how to handle vectors that do not match the native vector register size.


For efficiency it is desirable to make use of vector registers in the calling convention. This can be significantly quicker than copying vectors to and from the stack.


Within the same overall CPU architecture, there are several sub-architectures with different vector capabilities and in particular different vector sizes. The x86-64 architecture supports SSE2 vectors as a baseline which includes pairs of doubles, but the AVX extension doubles the size of the vector registers. Ideally when compiling for AVX we would make use of the larger AVX vectors, including passing the larger vectors in registers.


This poses a major challenge: we want to make use of large vectors when possible but we would also like to maintain some degree of ABI compatibility.


It is worth briefly exploring the option of abandoning ABI compatibility. We could declare that we have two ABIs on x86-64, the baseline SSE ABI and the AVX ABI. We would further declare To generate AVX code you must build all of your libraries using AVX. Essentially this would mean having two complete sets of libraries, or perhaps simply two instances of GHC, each with their own libraries. While this would work and may be satisfactory when speed is all that matters, it would not encourage use of vectors more generally. In practice haskell.org and linux distributions would have to distribute the more compatible SSE build so that in many cases even users with AVX hardware would be using GHC installations that make no use of AVX code. On x86 the situation could be even worse since the baseline x86 sub-architecture used by many linux distributions does not include even SSE2. In addition it is wasteful to have two instances of libraries when most libraries do not use vectors at all.


It it worth exploring options for making use of AVX without having to force all code to be recompiled. Ideally the base package would not need to be recompiled at all and perhaps only have packages like vector recompiled to take advantage of AVX.


Consider the situation where we have two modules Lib.hs and App.hs where App imports Lib The Lib module exports:

```wiki
f :: DoubleVec4# -> Int
g :: (DoubleVec4# -> a) -> a
```


which are used by App. We compile:

```wiki
ghc -msse2 Lib.hs
ghc -mavx  App.hs.
```


There are two cases to consider:

- if the function being called has an unfolding exported from Lib then that unfolding can be compiled in the context of App and can make use of AVX instructions
- alternatively we are dealing with object code for the function which follows a certain ABI


Notice that not only do we need to be careful to call 'f' and 'g' using the right calling convention, but in the case of 'g', the function that we pass as its argument must also follow the calling convention that 'g' will call it with.


One idea is to take a worker/wrapper approach. We would split each function into a wrapper that uses some lowest common denominator calling convention and a worker that uses the best calling convention for the target sub-architecture. For example, the lowest common denominator calling convention might be to pass all vectors on the stack, while the worker convention would use SSE2 or AVX registers.


For App calling Lib.f we start with a call to the wrapper, this can be inlined to a call to the worker at which point we discover that the calling convention will use SSE2 registers. For App calling Lib.g with a locally defined 'h', we would pass the wrapper for 'h' to 'g' and since we assume we have no unfolding for 'g' then this is how it remains: at runtime 'g' will call 'h' through the wrapper for 'h' and so will use the lowest common denominator calling convention.

#### SSE2 code calling AVX code


We might be concerned with the reverse situation where we have A and B, with A importing B:

```wiki
ghc -mavx  B.hs.
ghc -msse2 A.hs
```


That is, a module compiled with SSE2 that imports a module that was compiled with AVX. How can we call functions using AVX registers if we are only targeting SSE2? One option is to note that since we will be using AVX instructions at runtime when we call the functions in B, and hence it is legitimate to use AVX instructions in A also, at least for the calling convention. There may however be some technical restriction to using AVX instructions in A, for example if we decided that we would implement AVX support only in the LLVM backend and not the NCG backend and we chose to compile B using LLVM and A using the NCG. In that case we would have to avoid inlining the wrapper an exposing the worker that uses the AVX calling convention. There are already several conditions that are checked prior to inlining (e.g. phase checks), this would add an additional architecture check.
It may well be simpler however to just implement minimal SSE2 and AVX support in the NCG, even if it is not used for vector operations and simply for the calling convention.

### Types for calling conventions


One of GHC's clever design tricks is that the type of a function in core determines its calling convention. A function in core that accepts an Int is different to one that accepts an Int\#. The use of two different types, Int and Int\# let us talk about the transformation and lets us write code in core for the wrapper function that converts from Int to Int\#.


If we are to take a worker wrapper approach with calling conventions for vectors then we would do well to use types to distinguish the common and special calling conventions. For example, we could define types:

```wiki
FloatSseVec4#
DoubleSseVec2#

FloatAvxVec8#
DoubleAvxVec4#
```


Then we can describe the types for the worker and wrapper for our function

```wiki
f :: DoubleVec4# -> Int
```


This remains the type of the wrapper, which also is still called f. If we compile the module with -msse2 or -mavx then we would get workers with the respective types:

```wiki
f_worker :: (# DoubleSseVec2#, DoubleSseVec2# #) -> Int
```


or

```wiki
f_worker :: DoubleAvxVec4# -> Int
```


Note that in the SSE2 case we have to synthesize a vector of length 4 using native vectors of length 2.


Now it is clearer what the calling convention of the workers are. What is the calling convention of the wrapper?

```wiki
f :: DoubleVec4# -> Int
```


We have said that this is the lowest common denominator calling convention. This might be passing vectors on the stack. But on x86-64 we know we always have SSE2 available, so we might want to use that in our lowest common denominator calling convention. In that case, would it make sense to say that in fact the wrapper 'f' has type:

```wiki
f :: (# DoubleSseVec2#, DoubleSseVec2# #) -> Int
```


This is a plausible design, but it is not necessary to go this way. We can simply declare types like DoubleVec4\# to have a particular calling convention without forcing it to be rewritten in terms of machine-specific types in core.


But it would be plausible to say that types like DoubleVec4\# are ephemeral, having no ABI and must be rewritten by a core -\> core pass to use machine-specific types with an associated ABI.

### Memory alignment for vectors


Many CPUs that support vectors have strict alignment requirements, e.g. that 16 byte vectors must be aligned on 16byte boundaries. On some architectures the requirements are not strict but there may be a performance penalty, or alternative instruction may be required to load unaligned vectors.


Note that the alignment of vectors like DoubleVec4\# has to be picked to fit the maximum required alignment of any sub-architecture. For example while DoubleVec4\# might be synthesized using operations on DoubleSseVec2\# when targeting SSE, the alignment must be picked such that we can use `DoubleAvxVec4#` operations.


It is relatively straightforward to ensure alignment for vectors in large packed arrays. It is also not too great a burden to ensure stack alignment.


A somewhat more tricky problem is alignment of vectors within heap objects, both data constructors and closures. While it is plausible to ban putting vectors in data constructors, it does not seem possible to avoid function closures with vectors saved in the closure's environment record.


We would wish to avoid forcing all heap object to have a stricter alignment since this could waste enormous amounts of memory to padding. We would wish to align only those data constructors and closures that have stricter alignment requirements. This would require runtime fixups for functions allocating heap objects with stricter alignment, and changes to the GC to preserve the alignment when moving heap objects around.


For functions allocating heap objects with stricter alignment, upon entry along with the usual heap overflow check, it would be necessary to test the heap pointer and if necessary to bump it up to achieve the required alignment. Subsequent allocations within that code block would be able to proceed without further checks as any additional padding between allocations would be known statically. The heap overflow check would also have to take into account the possibility of the extra bump.

### GHC command line flags


We would add machine flags such as `-msse2` and `-mavx`. These tell GHC that it is allowed to make use of the corresponding instructions.


For compatibility, the default will remain targeting the base instruction set of the architecture. This is the behaviour of most other compilers. We may also want to add a `-mnative` / `-mdetect` flag that is equivalent to the `-m` flag corresponding to the host machine.


GHC does not support GPUs (and thus SIMD) so far,
however support for vectors is planned.

## See also

- [SIMD LLVM](simd-llvm) A previous (LLVM-specific) iteration of this SIMD proposal.
- [VectorComputing](vector-computing)  A previous proposal to make use of x86 SSE in GHC.
- [ Blog article about Larrabee and Nvidia, MIMD vs. SIMD](http://perilsofparallel.blogspot.com/2008/09/larrabee-vs-nvidia-mimd-vs-simd.html)