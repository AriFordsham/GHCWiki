# The GHC reading list


Suppose you want to start contributing to GHC: what should you read by way of background?  Here is an annotated list.  Please add to it as you come across useful material.


You can ask questions on `ghc-devs@haskell.org`. People are friendly.  See also [working on GHC](working-conventions) and [GHC contributors](contributors).

## General background

- The [GHC Commentary](commentary) is a Wiki that describes GHC's implementation.  It is a Wiki.  That means that you can, and should, fix errors and write new chapters.

- [ The Glasgow Haskell Compiler](http://www.aosabook.org/en/ghc.html), in [ The Architecture of Open Source Applications](http://www.aosabook.org/en/index.html), Volume II, ed Brown & Wilson. This paper gives an up to date (2012) technical overview of GHC.

- Simon PJ's [ home page](http://research.microsoft.com/~simonpj) and [ publications page](http://research.microsoft.com/en-us/um/people/simonpj/papers/papers.html) have lots of relevant papers.  Some key ones appear below but not all.

- Simon PJ's books:

  - [ The Implementation of Functional Programming Languages](http://research.microsoft.com/en-us/um/people/simonpj/papers/slpj-book-1987/index.htm)
  - [ Implementing Functional Languages: a tutorial](http://research.microsoft.com/en-us/um/people/simonpj/papers/pj-lester-book/)
    give useful general background. They are not GHC-specific at all, but they have lots of information about functional-language compilers.

## Types and type inference

- [ Modular type inference with local assumptions](http://haskell.org/haskellwiki/Simonpj/Talk:OutsideIn), Simon Peyton Jones, Dimitrios Vytiniotis, Tom Schrijvers, Martin Suzmann, Journal of Functional Programming, 2011.  This epic 83-page JFP paper brings together, in a single uniform framework, a series of our earlier papers on type inference for type systems involving local constraints, including GADTs and indexed type families.  

- [ Papers about type equalities in GHC's intermediate language](http://research.microsoft.com/en-us/um/people/simonpj/papers/ext-f/)

  - *Equality proofs and deferred type errors*, Simon Peyton Jones, Dimitrios Vytiniotis and Pedro Magalhaes (ICFP 2012).  An exploration of what happens when you take equality proofs seriously in a compiler.
  - *Giving Haskell a promotion*, Brent Yorgey, Stepanie Weirich, Julien Cretin, Simon Peyton Jones, and Dimitrios Vytiniotis (TLDI 2012).  How to (a) add kind polymorphism and (b) promote data types to become data kinds.
  - *System F with Type Equality Coercions*, Martin Sulzmann, Manuel Chakravarty, and Simon Peyton Jones (TLDI 2007).  The first paper about System FC.

- [ Unboxed values as first class citizens](http://research.microsoft.com/en-us/um/people/simonpj/papers/unboxed-values.ps.Z), SL Peyton Jones and J Launchbury, Functional Programming Languages and Computer Architecture (FPCA'91), Boston, LNCS 523, Springer Verlag, Sept 1991, pp636-666. How unboxed data types work in GHC.


Please add: System FC, GADTs, kind polymorphism etc

## Optimisations

- [ A transformation-based optimiser for Haskell](http://research.microsoft.com/en-us/um/people/simonpj/papers/comp-by-trans-scp.ps.gz), SL Peyton Jones and A Santos, Science of Computer Programming 32(1-3), pp3-47, September 1998.  Gives an overview of many of the transformations GHC does on Core.  Andre's [ PhD thesis](http://research.microsoft.com/en-us/um/people/simonpj/papers/santos-thesis.ps.gz) gives more details.

- [ Secrets of the GHC inliner](http://research.microsoft.com/en-us/um/people/simonpj/papers/inlining/index.htm), Simon Peyton Jones and Simon Marlow, Journal of Functional Programming 12(4), July 2002, pp393-434.  Describes how the Simplifier does inlining.

- [ A short cut to deforestation](http://research.microsoft.com/en-us/um/people/simonpj/papers/deforestation-short-cut.ps.Z), A Gill, SL Peyton Jones, J Launchbury, Proc Functional Programming Languages and Computer Architecture (FPCA'93), Copenhagen, June 1993, pp223-232.  The famous foldr/build rule.  Andy's [ PhD thesis](http://research.microsoft.com/en-us/um/people/simonpj/papers/andy-thesis.ps.gz) has more.  

- [ Playing by the rules: rewriting as a practical optimisation technique in GHC](http://research.microsoft.com/en-us/um/people/simonpj/papers/rules.htm), Simon Peyton Jones, Andrew Tolmach and Tony Hoare, Haskell Workshop 2001.  Describes how RULES work, which are heavily used in GHC.

## Back end issues

- [ How to make a fast curry: push/enter vs eval/apply](http://research.microsoft.com/en-us/um/people/simonpj/papers/eval-apply/index.htm), Simon Marlow and Simon Peyton Jones, International Conference on Functional Programming, Snowbird, Sept 2004, pp4-15.

- [ Implementing lazy functional languages on stock hardware: the Spineless Tagless G-machine](http://research.microsoft.com/~simonpj/papers/spineless-tagless-gmachine.ps.gz), SL Peyton Jones, Journal of Functional Programming 2(2), Apr 1992, pp127-202.  The original STG paper but still highly relevant.


Please add: Hoopl, C--.

## Data Parallel Haskell


Please fill in...
