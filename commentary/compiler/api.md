# GHC Commentary: The GHC API


This section of the commentary describes everything between [HscMain](commentary/compiler/hsc-main) and the front-end; that is, the parts of GHC that coordinate the compilation of multiple modules.

[](/trac/ghc/attachment/wiki/Commentary/Compiler/API/ghc-top.png)


The GHC API is the interface exported by [compiler/main/GHC.hs](/trac/ghc/browser/ghc/compiler/main/GHC.hs).  To compile a Haskell module that uses the GHC API, use the flag `-package ghc` (in GHC 6.6 and later).  GHC itself contains a few front-ends that use the GHC API:

- The "one-shot" mode, where GHC compiles each file on the command line separately (eg. `ghc -c Foo.hs`).  This mode
  is implemented 

- 