# Using SIMD Instructions via the LLVM Backend


The LLVM compiler tools targeted by GHC's [LLVM backend](commentary/compiler/backends/llvm) support a generic [ vector type](http://llvm.org/docs/LangRef.html#t_vector) of arbitrary, but fixed length whose elements may be any LLVM scalar type. In addition to three [ vector operations](http://llvm.org/docs/LangRef.html#vectorops), LLVM's operations on scalars are overloaded to work on vector types as well. LLVM compiles operations on vector types to target-specific SIMD instructions, such as those of the SSE, AVX, and NEON instruction set extensions. As the capabilities of the various versions of SSE, AVX, and NEON vary widely, LLVM's code generator maps operations on LLVM's generic vector type to the more limited capabilities of the various hardware targets.


The SIMD vector extension to GHC proposed here maps to LLVM's vector type in a straight forward manner, which in turn enables us to target a wide range of hardware capabilities. However, GHC's native code generator will simply map SIMD vector operations to ordinary scalar code (in order to avoid having to deal with the complexities of SSE, AVX, NEON, etc).
