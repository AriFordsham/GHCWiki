# memcpy/memmove/memset optimizations

Starting with version 7.2, GHC has three new primitives for copying/setting blocks of memory, corresponding to the C functions `memcpy`, `memmove`, and `memset`. GHC optimizes occurrences of these primitives into efficient unrolled loops when possible and calls the corresponding C functions otherwise.

## Implementation

The primitives are implemented as three [Cmm language](commentary/compiler/cmm-type) [CallishMachOp\`s](commentary/compiler/cmm-type#operators-and-primitive-operations), defined in [compiler/cmm/CmmMachOp.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/cmm/CmmMachOp.hs). The code generator generates calls to these `CallishMachOp`s using three utility functions: `emitMemcpyCall`, `emitMemmoveCall`, and `emitMemsetCall`, defined in [compiler/codeGen/StgCmmPrim.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/codeGen/StgCmmPrim.hs). The helper functions take an extra parameter that indicates the alignment of the arguments, which is used as an optimization hint by the backends.

The reason the primitives are unrolled in the backends, instead of in the code generator, is to allow us to make use of LLVM's `memcpy`/`memmove`/`memset` intrinsics, which LLVM  optimizes well. In the x86/x86-64 backend we unroll the primitives ourselves. The different native code generator backends can also generate more efficient code than a generic case higher up. Currently only the X86 backend unrolls these primitives though, SPARC and !PowerPC both just call the corresponding C functions.

## Unrolling heuristics

We currently only unroll a primitive if the number of required operations is not more than `maxInlineMemcpyInsns` and `maxInlineMemsetInsns` for `memcpy` and `memset` respectively. These values are controlled with command line flags:
* `-fmax-inline-memcpy-insns=⟨n⟩, default: 32`,
* `-fmax-inline-memset-insns=⟨n⟩, default: 32`.

The unrolled primitive uses the widest possible move instruction available on the platform and allowed by the address alignment. This means that with the default value of the flag and word aligned address the threshold is 128 byte and 256 bytes on 32-bit and 64-bit platforms respectively.

### Limitations and possible improvements

Current `memset` and `memcpy` heuristics rely only on the alignment of the address(es) and use only aligned `MOV`s, which is fine for `Array#`s, but can lead to sub-optimal results for `ByteArray#`s. 

For instance, `setByteArray s 0# 24# 1#` will be nicely inlined as
```
movq $72340172838076673,%rcx
movq %rcx,0(%rbx)
movq %rcx,8(%rbx)
movq %rcx,16(%rbx)
```

But `setByteArray# s 1# 23# 0#` since the alignment of the address is just 1 byte when the offset is `1#`, will (sadly) be inlined as
```
movb x23
```

If we could incorporate into `memset`/`memcpy` ops the knowledge that we deal with
```
(baseAddress at known alignment) + offset
```
the latter example could be optimally unrolled into just 5 aligned moves:
```
movb, movw, movl, movq, movq
```

Another option is to ditch alignment requirements altogether and use the widest unaligned move possible. But the performance implications need to be measured.

Another possible improvement is using SSE instructions for even wider moves.

## User API

These primitives aren't directly exposed to the user at this time. Instead the primitives are exposed to the user through a set of primitive operations on boxed arrays and byte arrays:

- `setByteArray#`
- `copyArray#`/`copyByteArray#`/`copyMutableArray#`/`copyMutableByteArray#`.
- `cloneArray#`/`cloneMutableArray#`
- `freezeArray#`
- `thawArray#`

The latter four allow the user to efficiently clone an array without first setting all elements to some dummy element, which would be required to e.g. implement `cloneArray#` in terms of `newArray#` and `copyArray#`. The implementation of these primitive operations are in [compiler/codeGen/StgCmmPrim.hs](https://gitlab.haskell.org/ghc/ghc/tree/master/ghc/compiler/codeGen/StgCmmPrim.hs).

## Test API


The main test for this feature of GHC is `cgrun069`, located under [tests/codeGen/should_run](https://gitlab.haskell.org/ghc/ghc/tree/master/testsuite/tests/codeGen/should_run). The user facing API is tested by `cgrun064` and `cgrun068`. Tests specific to `xxxByteArray#` are under [tests/codeGen/should_gen_asm](https://gitlab.haskell.org/ghc/ghc/tree/master/testsuite/tests/codeGen/should_gen_asm).
