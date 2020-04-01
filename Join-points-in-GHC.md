This page summarises work on join points in GHC

* [Compiling without continuations](https://www.microsoft.com/en-us/research/publication/compiling-without-continuations/) is the main paper that describes join points.

* [The loopification wiki page](https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler/loopification) describes loopification -- I think it's rather out of date.

* Ticket #14152 describes the Exitification transformation, which uses join points.  The relevant module is `GHC.Core.Op.Exitify`.

* The label ~"join points" lists tickets about join points
