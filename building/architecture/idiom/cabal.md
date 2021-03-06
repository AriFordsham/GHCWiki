# Idiom: interaction with Cabal


Many of the components of the GHC build system are also Cabal
packages, with package metadata defined in a `foo.cabal` file. For the
GHC build system we need to extract that metadata and use it to build
the package. This is done by the program `ghc-cabal` (in `utils/ghc-cabal`
in the GHC source tree). This program reads `foo.cabal` and produces
`package-data.mk` containing the package metadata in the form of
makefile bindings that we can use directly.


We adhere to the following rule: **`ghc-cabal` generates only
makefile variable bindings**, such as

```wiki
  HS_SRCS = Foo.hs Bar.hs
```

`ghc-cabal` never generates makefile rules, macro, macro invocations etc. 
All the makefile code is therefore contained in fixed, editable 
`.mk` files.
