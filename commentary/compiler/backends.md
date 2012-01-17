# GHC Commentary: Backends


After [Cmm](commentary/compiler/cmm-type) has been generated, we have a choice of targets to compile to:

- [The C code generator](commentary/compiler/backends/ppr-c)
- [The native code generator](commentary/compiler/backends/ncg)
- [The LLVM code generator](commentary/compiler/backends/llvm)
- [The GHCi code generator](commentary/compiler/backends/gh-ci)


These backends are completely interchangeable.  Our preferred route is the native code generator.  The C code generator is used for portable, non-optimised, or unregisterised compilation.


It is likely that only the native code generator will be able to generate position independent code (PIC) which is necessary for dynamic libraries, so once we're doing this the C code generator will be even more deprecated.
