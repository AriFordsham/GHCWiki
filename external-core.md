# The {{[ExternalCore](external-core)}} type


The `ExternalCore` data type is used by GHC to communicate code represented in the [Core](commentary/compiler/core-syn-type) data type with the outside world. It comes with an external syntax, a parser, a pretty printer, and code to convert between Core and External Core. Unfortunately, External Core has not been widely used, and the code has bit-rotted. The recent changes in Core to use [System FC](commentary/compiler/fc) have exacerbated the problem. This page documents the process of getting [ExternalCore](external-core) and Core back in sync.

## Relevant files


The main source files related to External Core:

- [compiler/coreSyn/ExternalCore.hs](/trac/ghc/browser/ghc/compiler/coreSyn/ExternalCore.hs): The definition of the External Core data type.
- [compiler/coreSyn/MkExternalCore.hs](/trac/ghc/browser/ghc/compiler/coreSyn/MkExternalCore.hs): Some code to convert Core to External Core.
- [compiler/coreSyn/PprExternalCore.hs](/trac/ghc/browser/ghc/compiler/coreSyn/PprExternalCore.hs): Some code to pretty-print [ExternalCore](external-core).
- [compiler/parser/LexCore.hs](/trac/ghc/browser/ghc/compiler/parser/LexCore.hs): The lexer for External Core.
- [compiler/parser/ParserCore.hs](/trac/ghc/browser/ghc/compiler/parser/ParserCore.hs): The parser for External Core.
- [compiler/parser/ParserCoreUtils.hs](/trac/ghc/browser/ghc/compiler/parser/ParserCoreUtils.hs): Some additional utility functions used by ParserCore.hs.
- [utils/ext-core/](/trac/ghc/browser/ghc/utils/ext-core/):


Other files that contain some reference to External Core or are otherwise relevant:

- [compiler/coreSyn/PprCore.hs](/trac/ghc/browser/ghc/compiler/coreSyn/PprCore.hs): Some code to pretty-print the Core data type.
- [compiler/hsSyn/HsSyn.hs](/trac/ghc/browser/ghc/compiler/hsSyn/HsSyn.hs): Top-level syntax tree representations for various things GHC can read, including External Core.
- [compiler/main/DriverPhases.hs](/trac/ghc/browser/ghc/compiler/main/DriverPhases.hs): Includes code to decide how to parse things based on file extension.
- [compiler/main/HscMain.hs](/trac/ghc/browser/ghc/compiler/main/HscMain.hs): The main compiler pipeline.

## Documentation

- [ http://research.microsoft.com/\~simonpj/papers/ext-f](http://research.microsoft.com/~simonpj/papers/ext-f): Description of the System FC language which GHC now uses internally.
- [docs/ext-core/](/trac/ghc/browser/ghc/docs/ext-core/): The current documentation for [ExternalCore](external-core), which should eventually become a chapter in the [GHC User's Guide](http://www.haskell.org/ghc/docs/latest/html/users_guide/index.html).
- [http://www.haskell.org/ghc/docs/latest/html/users_guide/ext-core.html](http://www.haskell.org/ghc/docs/latest/html/users_guide/ext-core.html): What the User's Guide currently has to say about External Core.

## Design decisions

TODO

## Tasks

- Update the [ExternalCore](external-core) data type to be compatible with the current Core data type.
- Define an external text representation for [ExternalCore](external-core) (which will probably be simply a minor modification of the old format)
- Update LexCore.hs, ParserCore.y, and ParserCoreUtils.hs to support the new data type and external syntax.
- Update MkExternalCore.hs to support both the current Core and the new [ExternalCore](external-core).
- Update PprExternalCore.hs to print stuff that LexCore and ParserCore can understand.
- Convert the current External Core documentation (in LaTeX) into a chapter (in XML) in the User's Guide.
