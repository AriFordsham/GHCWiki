# GHC Commentary: Libraries


All GHC installations contain a set of libraries called the **boot packages**.  They are so called because the boot packages are the ones required for GHC to compile itself (the stage-2 bootstrap compiler). 


The file `libraries/boot-packages` contains the list of boot packages:

- At the root of the tree we have **`ghc-prim`**. As the name implies this package contains the most primitive types and functions. It only contains a handful of modules, including `GHC.Prim` (which contains `Int#`, `+#`, etc) and `GHC.Bool`, containing the `Bool` datatype.

- Above `ghc-prim` is the **`integer`** package, which provides a definition of the `Integer` type on top of the C `gmp` library. Which functionality is provided in `ghc-prim` is mostly driven by what functionality the `integer` package needs. Unlike all the other libraries, the `integer` package does not live in `libraries/integer`, but `libraries/integer-gmp`. This allows alternate implementations to be used, by defining `INTEGER_LIBRARY=integer-foo` in `mk/build.mk`.

- Next is the **`base`** package. This contains a large number of modules, many of which are in one big cyclic import knot, mostly due to the `Exception` type. This is something that we hope to improve upon, so that base can be split up.

- On top of base are a number of other, more specialised packages, whose purpose is generally clear from their name. If not, you can get more detail from the descriptions in their Cabal files.  Currently these packages are are:

  - `array`
  - `bytestring`
  - `Cabal`
  - `containers`
  - `directory`
  - `editline`
  - `filepath`
  - `haskell98`
  - `hpc`
  - `old-locale`
  - `old-time`
  - `packedstring`
  - `pretty`
  - `process`
  - `random`
  - `template-haskell`
  - `unix`
  - `Win32`

> >
> > However the definitive list is in `libraries/boot-packages`