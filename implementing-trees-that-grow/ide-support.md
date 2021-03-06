# Possible IDE support using Trees that Grow


In this page, we discuss the possibility of using Trees that Grow to provide alternative `hsSyn` AST versions, which are optimised for providing information to be used in developer tooling.
 


Proposed by Alan Zimmerman


Original discussion on `ghc-devs`: [https://mail.haskell.org/pipermail/ghc-devs/2018-May/015721.html](https://mail.haskell.org/pipermail/ghc-devs/2018-May/015721.html)

## Motivation



At the moment, if a tool such as `HaRe`[(1)](implementing-trees-that-grow/ide-support#) is used to make changes to the `hsSyn` AST and convert the updated AST back to source, it has to compile with `Opt_KeepRawTokenStream` set, to tell the parser to keep the [ApiAnnotations](api-annotations), and then use the `ghc-exactprint` [(2)](implementing-trees-that-grow/ide-support#) library, and some complex bookkeeping, to make sure the connection between the (modified) API Annotations and the AST is kept intact.



Given that Trees that Grow is now in the `hsSyn` AST, I propose to move the API Annotations to where they belong, inside the AST.


This will allow the core `ghc-exacprint` functionality to also move into GHC, meaning that the pretty printer can then reproduce the exact source from a `ParsedSource` AST fragment.


The existing API Annotations are only kept if requested, as they impose a space penalty which need not be paid under all circumstances, especially when simply compiling code to generate a library / exe.


A way to avoid this penalty, and to allow the additional information stored to grow relatively freely without having to worry too much about optimising the straight compilation process, is to have two variants of the AST, one for compiling with Api Annotations, one for without, as selected by using the `Opt_KeepRawTokenStream` dynamic flag, as used at present.



This can be achieved by making use of the mechanics listed below. If it turns out that the penalty is moderate, and the additional complexity of having two variants is not worth it, this step need not be taken.



(1) [https://hackage.haskell.org/package/HaRe](https://hackage.haskell.org/package/HaRe) 

(2) [https://hackage.haskell.org/package/ghc-exactprint](https://hackage.haskell.org/package/ghc-exactprint)


## Mechanics



`GHC/Hs/Extension.hs` would be extended to


```
-- | Used as a data type index for the hsSyn AST
data GhcPass (c :: Pass)

data Pass = Parsed Process | Renamed | Typechecked

data Process = WithApiAnnotations | WithoutApiAnnotations

type GhcPs   = GhcPass ('Parsed 'WithoutApiAnnotations)
type GhcPsI  = GhcPass ('Parsed 'WithApiAnnotations)
```


So the current `GhcPs` synonym would still indicate the (normal) batch compilation process without Api Annotations, and the new one `GhcPsI` reflects the compiler invoked to generate the Api Annotations.


Since the key feature of Trees that Grow is that different extensions to the AST can be defined based on the index type used, it means that a set of extension types capturing the requirements for the `ghc-exactprint` capable Api Annotations can be defined.


This means the relevant information is stored directly in the AST, making modification of the AST while preserving layout and comments by tooling much simpler.


There would still be a single parser definition in `Parser.y`, which would make use of functions to add the additional info to the generated source tree, which would be NOPs if the information was not being kept. This is similar to what happens at present with the Api Annotations.

## Phasing


There is potentially more information that can be captured in the AST for IDE support, both in the parsed source AST, as well as the ones after renaming or typechecking.


I propose to make this limited change initially, which has a clear scope, and then review and extend it once complete.

## Longer Term


A longer term goal is to use a modified `happy` to generate a fully incremental parser, which can then be tightly coupled into IDE tooling via HIE [(3)](implementing-trees-that-grow/ide-support#).


In preparation for that, the updated Api Annotations would be defined in a position-independent way, rather than being based on exact line and column positions.



This will probably be based on the approach currently taken in `Coda` [(4)](implementing-trees-that-grow/ide-support#).



(3) [https://github.com/haskell/haskell-ide-engine](https://github.com/haskell/haskell-ide-engine) 

(4) [https://github.com/ekmett/coda](https://github.com/ekmett/coda)


## Even Longer Term



Incremental renaming and type checking.  See related work at [(1)](implementing-trees-that-grow/ide-support#)



(1) [https://arxiv.org/pdf/1805.00155.pdf](https://arxiv.org/pdf/1805.00155.pdf)


