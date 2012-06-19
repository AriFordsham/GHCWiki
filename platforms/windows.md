# Windows Support for GHC

## Versions supported


We no longer support Windows 95, 98, ME or NT. We do support 2000, XP and Vista.

## Building 32-bit Windows programs


Current releases of GHC support building 32bit programs on Windows. They ship with a small [ MinGW](http://mingw.org/) system bundled, which provides tools (such as a C compiler, linker and assembler) that are used by the compiler.

## Building 64-bit Windows programs


No current release of GHC supports building 64bit programs on Windows, but there is an [alpha release](http://www.haskell.org/ghc/dist/win64_alpha1/) that does, and it is expected that from GHC 7.6 it will be a supported platform.


It includes a small [ MinGW-w64](http://mingw-w64.sourceforge.net/) system, which provides tools (such as a C compiler, linker and assembler) that are used by the compiler.


There are currently no known bugs that are specific to 64-bit Windows. If you encounter any problems, please [ file a bug](http://hackage.haskell.org/trac/ghc/wiki/ReportABug).

## Building on Windows


Build instructions for Windows are incorporated in the [Building Guide](building).  In particular, here is how to [set up your Windows system for building GHC](building/preparation/windows).
