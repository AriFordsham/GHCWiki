# The `ExternalCore` type


The `ExternalCore` data type is used by GHC to communicate code represented in the [Core](commentary/compiler/core-syn-type) data type with the outside world. It comes with an external syntax, a parser, a pretty printer, and code to convert between Core and External Core. Unfortunately, External Core has not been widely used, and the code has bit-rotted. The recent changes in Core to use [System FC](commentary/compiler/fc) have exacerbated the problem. This page documents the process of getting External Core and Core back in sync.

## Relevant files


The main source files related to External Core:

- [compiler/coreSyn/ExternalCore.lhs](/trac/ghc/browser/ghc/compiler/coreSyn/ExternalCore.lhs): The definition of the External Core data type.
- [compiler/coreSyn/MkExternalCore.lhs](/trac/ghc/browser/ghc/compiler/coreSyn/MkExternalCore.lhs): Some code to convert Core to External Core.
- [compiler/coreSyn/PprExternalCore.lhs](/trac/ghc/browser/ghc/compiler/coreSyn/PprExternalCore.lhs): Some code to pretty-print External Core.
- [compiler/parser/LexCore.hs](/trac/ghc/browser/ghc/compiler/parser/LexCore.hs): The lexer for External Core.
- [compiler/parser/ParserCore.y](/trac/ghc/browser/ghc/compiler/parser/ParserCore.y): The parser for External Core.
- [compiler/parser/ParserCoreUtils.hs](/trac/ghc/browser/ghc/compiler/parser/ParserCoreUtils.hs): Some additional utility functions used by `ParserCore.hs`.
- [utils/ext-core/](/trac/ghc/browser/ghc/utils/ext-core/): Old code intended as an executable specification of External Core.


Other files that contain some reference to External Core or are otherwise relevant:

- [compiler/coreSyn/PprCore.lhs](/trac/ghc/browser/ghc/compiler/coreSyn/PprCore.lhs): Some code to pretty-print the Core data type.
- [compiler/hsSyn/HsSyn.lhs](/trac/ghc/browser/ghc/compiler/hsSyn/HsSyn.lhs): Top-level syntax tree representations for various things GHC can read, including External Core.
- [compiler/main/DriverPhases.hs](/trac/ghc/browser/ghc/compiler/main/DriverPhases.hs): Includes code to decide how to parse things based on file extension.
- [compiler/main/HscMain.lhs](/trac/ghc/browser/ghc/compiler/main/HscMain.lhs): The main compiler pipeline.

## Documentation

- [ http://research.microsoft.com/\~simonpj/papers/ext-f](http://research.microsoft.com/~simonpj/papers/ext-f): Description of the System FC language which GHC now uses internally.
- [docs/ext-core/](/trac/ghc/browser/ghc/docs/ext-core/): The current documentation for External Core, which should eventually become a chapter in the [GHC User's Guide](http://www.haskell.org/ghc/docs/latest/html/users_guide/index.html).
- [http://www.haskell.org/ghc/docs/latest/html/users_guide/ext-core.html](http://www.haskell.org/ghc/docs/latest/html/users_guide/ext-core.html): What the User's Guide currently has to say about External Core.

## Design decisions

- Probably want to represent all data types as GADTs, even if they can be represented in Haskell 98 form, so that we only have one representation.
- TODO more!

## Tasks

- Update the External Core data type to be compatible with the current Core data type.
- Define an external text representation for External Core (which will probably be simply a minor modification of the old format)
- Update `LexCore.hs`, `ParserCore.y`, and `ParserCoreUtils.hs` to support the new data type and external syntax.
- Update `MkExternalCore.lhs` to support both the current Core and the new External Core.
- Update `PprExternalCore.lhs` to print stuff that `LexCore` and `ParserCore` can understand.
- Convert the current External Core documentation (in LaTeX) into a chapter (in XML) in the User's Guide.

## Miscellaneous notes

- The LaTeX documentation describes PrimOps in some detail. This information is now in the library documentation, so it is probably not needed in the External Core chapter.
- Core now uses `Name` ([compiler/basicTypes/Name.lhs](/trac/ghc/browser/ghc/compiler/basicTypes/Name.lhs)) rather than `IfaceExtName`.
