# LLVM backend support for ARM


As of August 10 2011, GHC HEAD supports registerised build on ARM when using LLVM backend. You will need to have LLVM 3.0 installed for this to work. If for whatever reasons you cannot use LLVM 3.0 (or 3.0 RC2), you will need to have LLVM HEAD as of July 12 2011 with the patch from here: [http://www.gardas.roznovan.cz/llvm/llvm-2011-07-12.patch](http://www.gardas.roznovan.cz/llvm/llvm-2011-07-12.patch) applied.


You can do this this way:

```wiki
$ svn co http://llvm.org/svn/llvm-project/llvm/trunk llvm-ghc-arm-2011-07-12
$ wget http://www.gardas.roznovan.cz/llvm/llvm-2011-07-12.patch
$ cd llvm-ghc-arm-2011-07-12
$ svn up -r "{2011-07-12}"
$ patch -p1 < ../llvm-2011-07-12.patch
```


Please note that for compilation on native ARM Linux system, you will need to use -O1 optimization level as higher levels produces more buggy LLVM. Configuration and compilation of LLVM might be done in this way:

```wiki
$ mkdir obj
$ cd obj
$ ../llvm-ghc-arm-2011-07-12/configure --enable-optimized
$ make ENABLE_OPTIMIZED=1 OPTIMIZE_OPTION=-O1
```


You don't need to install compiled LLVM, but you will just need to set your PATH to contain llvm-ghc-arm-2011-07-12/Release+Asserts/bin/ path.

## Known issues


The following issues will be addressed in future work:

- The backend currently breaks with LLVM 2011-08-08 (ghc-stage2 segfaults).  This needs to be investigated.  (Does the LLVM backend work on i386 and x86_64 with LLVM 2011-08-08 — i.e., is this ARM specific?)
- ~~Use `-optlc=-mattr=+a9,+vfp3` after extending the properly `DriverPipeline` with a specification of the ARM ISA and ISA extensions (instead of using CPP).~~ -- fixed in GHC HEAD as of August 22 2011
- ~~The above patch to LLVM needs to be submitted upstream, but only after we have decided on the final register use.~~ -- we have given up on changing regs mapping and submitted patch upstream. It was merged for LLVM 3.0 release on 2011-10-24
