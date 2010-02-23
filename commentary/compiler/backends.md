# GHC Commentary: Backends


After [Cmm](commentary/compiler/cmm-type) has been generated, we have a choice of targets to compile to:

- [The C code generator](commentary/compiler/backends/ppr-c)
- [The native code generator](commentary/compiler/backends/ncg)
- [The LLVM code generator](commentary/compiler/backends/llvm)
- Future: a C-- code generator


These backends are completely interchangeable.  Our preferred route is the native code generator, because the C code generator relies on some serious hackery, namely the [Evil Mangler](commentary/evil-mangler), to get fast tail-calls and other performance tricks.  The Evil Mangler is slated for removal as soon as possible, which would leave us with just the native code generator for optimised compilation, and the C code generator for portable, non-optimised, or unregisterised compilation.


It is likely that only the native code generator will be able to generate position independent code (PIC) which is necessary for dynamic libraries, so once we're doing this the C code generator will be even more deprecated.
