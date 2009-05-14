# GHC Commentary: Libraries


All GHC installations contain a set of libraries called the **boot packages**.  They are so called because the boot packages are the ones required for GHC to compile itself (the stage-2 compiler). 


The boot packages, along with the other subcomponents of the GHC build system, are in the file `packages` in a GHC tree. To get a list of them, you can run `make show VALUE=PACKAGES` in a configured GHC build tree.  (This variable is set in `$(TOP)/ghc.mk`.)

- At the root of the hierarchy we have **`ghc-prim`**. As the name implies, this package contains the most primitive types and functions. It only contains a handful of modules, including `GHC.Prim` (which contains `Int#`, `+#`, etc) and `GHC.Bool`, containing the `Bool` datatype.

- Above `ghc-prim` is the **`integer`** package, which provides a definition of the `Integer` type on top of the C `gmp` library. Which functionality is provided in `ghc-prim` is mostly driven by what functionality the `integer` package needs. Unlike all the other libraries, the `integer` package does not live in `libraries/integer`, but `libraries/integer-gmp`. This allows alternate implementations to be used, by defining `INTEGER_LIBRARY=integer-foo` in `mk/build.mk`.

- Next is the **`base`** package. This contains a large number of modules, many of which are in one big cyclic import knot, mostly due to the `Exception` type. This is something that we hope to improve upon, so that base can be split up.

- On top of base are a number of other, more specialised packages, whose purpose is generally clear from their name. If not, you can get more detail from the descriptions in their Cabal files.  Currently these packages are are:

  - `array`
  - `bytestring`
  - `Cabal`
  - `containers`
  - `directory`
  - `extensible-exceptions`
  - `filepath`
  - `haskeline`
  - `haskell98`
  - `hpc`
  - `mtl`
  - `old-locale`
  - `old-time`
  - `packedstring`
  - `pretty`
  - `process`
  - `random`
  - `syb`
  - `template-haskell`
  - `terminfo`
  - `unix`
  - `utf8-string`
  - `Win32`