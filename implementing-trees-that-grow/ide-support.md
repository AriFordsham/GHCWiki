# Possible IDE support using Trees that Grow


In this page, we discuss the possibility of using Trees that Grow to provide alternative `hsSyn` AST versions, which are optimised for providing information to be used in developer tooling.
 


Proposed by Alan Zimmerman


Original discussion on `ghc-devs`: [ https://mail.haskell.org/pipermail/ghc-devs/2018-May/015721.html](https://mail.haskell.org/pipermail/ghc-devs/2018-May/015721.html)

## Motivation


At the moment, if a tool such as `HaRe`[(1)](implementing-trees-that-grow/ide-support#) is used to make changes to the `hsSyn` AST and convert the updated AST back to source, it has to compile with `Opt_KeepRawTokenStream` set, to tell the parser to keep the [ApiAnnotations](api-annotations), and then use the `ghc-exactprint`[(2)](implementing-trees-that-grow/ide-support#) library, and some complex bookkeeping, to make sure the connection between the (modified) API Annotations and the AST is kept intact.


Given that we are already compiling in a different mode, and Trees that Grow is now in the `hsSyn` AST, I propose to move the API Annotations to where they belong, inside the AST.


This will allow the core `ghc-exacprint` functionality to also move into GHC, meaning that the pretty printer can then reproduce the exact source from a `ParsedSource` AST fragment.

(1)[ https://hackage.haskell.org/package/HaRe](https://hackage.haskell.org/package/HaRe)
(2)[ https://hackage.haskell.org/package/ghc-exactprint](https://hackage.haskell.org/package/ghc-exactprint)

## Mechanics

`hsSyn/HsExtension.hs` would be extended to

```
-- | Used as a data type index for the hsSyn ASTdataGhcPass(c ::Pass)dataPass=ParsedProcess|Renamed|TypecheckeddataProcess=Batch|InteractivetypeGhcPs=GhcPass('Parsed'Batch)typeGhcPsI=GhcPass('Parsed'Interactive)
```


So the current `GhcPs` synonym would still indicate the (normal) batch compilation process, and the new one `GhcPsI` reflects the compiler invoked in interactive mode.


Since the key feature of Trees that Grow is that different extensions to the AST can be defined based on the index type used, it means that a set of extension types capturing the requirements for the `ghc-exactprint` capable Api Annotations can be defined.


This means the relevant information is stored directly in the AST, making modification of the AST while preserving layout and comments by tooling much simpler. 

## Phasing


There is potentially more information that can be captured in the AST for IDE support, both in the parsed source AST, as well as the ones after renaming or typechecking.


I propose to make this limited change initially, which has a clear scope, and then review and extend it once complete.

## Longer Term


A longer term goal is to use a modified `happy` to generate a fully incremental parser, which can then be tightly coupled into IDE tooling via HIE [(3)](implementing-trees-that-grow/ide-support#).


In preparation for that, the updated Api Annotations would be defined in a position-independent way, rather than being based on exact line and column positions.


This will probably be based on the approach currently taken in `Coda`[(4)](implementing-trees-that-grow/ide-support#).

(3)[ https://github.com/haskell/haskell-ide-engine](https://github.com/haskell/haskell-ide-engine)
(4)[ https://github.com/ekmett/coda](https://github.com/ekmett/coda)