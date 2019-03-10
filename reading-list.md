# The GHC reading list


Suppose you want to start contributing to GHC: what should you read by way of background?  Here is an annotated list.  Please add to it as you come across useful material.


You can ask questions on `ghc-devs@haskell.org`. People are friendly.  See also [working on GHC](working-conventions) and [The GHC Team](team-ghc).

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

- [ Call-pattern Specialisation for Haskell Programs](https://research.microsoft.com/en-us/um/people/simonpj/papers/spec-constr/spec-constr.pdf), Simon Peyton Jones, ICFP 2007. Describes the specialisation optimiser.

- [ Let-floating: moving bindings to give faster programs](http://research.microsoft.com/pubs/67060/float.ps.gz), Simon Peyton Jones, Will Partain, and Andre Santos, ICFP 1996. Describes the let floating and full laziness optimisation passes.

- [ Modular, Higher-Order Cardinality Analysis in Theory and Practice](http://research.microsoft.com/en-us/um/people/simonpj/papers/usage-types/cardinality-popl14.pdf), Ilya Sergey, Dimitrios Vytiniotis, Simon Peyton Jones, POPL 2014. Describes cardinality analysis and optimisations that it enables or improves (eg. let-floating).

- [ Constructed Product Result Analysis for Haskell](http://research.microsoft.com/en-us/um/people/simonpj/papers/cpr/cpr.ps.gz), Clem Baker-Finch, Kevin Glynn, and Simon Peyton Jones, Journal of Functional Programming 14(2), 211–245, March 2004. Describes optimisation that allows to return tuple components in registers (for functions that return tuples).

## Data Parallel Haskell and concurrency

- [ Data Parallel Haskell: a status report](http://www.cse.unsw.edu.au/~chak/papers/data-parallel-haskell.pdf), Manuel M. T. Chakravarty, Roman Leshchinskiy, Simon Peyton Jones, Gabriele Keller, and Simon Marlow. , DAMP 2007: Workshop on Declarative Aspects of Multicore Programming, 2007

- [ Harnessing the Multicores: Nested Data Parallelism in Haskell](http://www.cse.unsw.edu.au/~chak/papers/fsttcs2008.pdf), Simon Peyton Jones, Roman Leshchinskiy, Gabriele Keller, and Manuel M. T. Chakravarty , IARCS Annual Conference on Foundations of Software Technology and Theoretical Computer Science (FSTTCS 2008), IBFI, Schloss Dagstuhl, 2008. 

- [ Vectorisation Avoidance](http://www.cse.unsw.edu.au/~chak/papers/vect-avoid.pdf), Gabriele Keller, Manuel M. T. Chakravarty, Roman Leshchinskiy, Ben Lippmeier, and Simon Peyton Jones, Proceedings of ACM SIGPLAN Haskell Symposium 2012, ACM Press, 2012. 

- [ Work Efficient Higher-Order Vectorisation](http://www.cse.unsw.edu.au/~chak/papers/replicate.pdf), Ben Lippmeier, Manuel M. T. Chakravarty, Gabriele Keller, Roman Leshchinskiy, and Simon Peyton Jones, The 17th ACM SIGPLAN International Conference on Functional Programming, ACM Press, 2012

- [ Runtime Support for Multicore Haskell](http://community.haskell.org/~simonmar/papers/multicore-ghc.pdf) (Simon Marlow, Simon Peyton Jones, Satnam Singh) In ICFP '09: Proceeding of the 14th ACM SIGPLAN International Conference on Functional Programming, Edinburgh, Scotland, August 2009

## Intermediate Representation of GHC (Core & Related)

- [An External Representation for the GHC Core Language](http://www.haskell.org/ghc/docs/6.10.4/html/ext-core/core.pdf) Gives an overview of the semantics and syntax of Core, GHC's internal intermediate representation for Haskell that most of the optimisation work is done on. A good language to understand when starting with GHC.

- [Unboxed Values as First-Class Citizens](http://www.haskell.org/ghc/docs/papers/unboxed-values.ps.gz), Simon L Peyton Jones and John Launchbury, Conference on Functional Programming Languages and Computer Architecture, September 1991. Describe the design of GHC language and internals for handling machine values and boxing / unboxing them as lazy values.

## Code generation and virtual machine

- [ How to make a fast curry: push/enter vs eval/apply](http://research.microsoft.com/en-us/um/people/simonpj/papers/eval-apply/index.htm), Simon Marlow and Simon Peyton Jones, International Conference on Functional Programming, Snowbird, Sept 2004, pp4-15.

- [ Faster laziness using dynamic pointer tagging](http://community.haskell.org/~simonmar/papers/ptr-tagging.pdf) (Simon Marlow, Alexey Rodriguez Yakushev, Simon Peyton Jones) In ICFP '07: Proceedings of the ACM SIGPLAN international conference on Functional programming, Freiburg, Germany, ACM Press, October 2007

- [ Implementing lazy functional languages on stock hardware: the Spineless Tagless G-machine](http://research.microsoft.com/~simonpj/papers/spineless-tagless-gmachine.ps.gz), SL Peyton Jones, Journal of Functional Programming 2(2), Apr 1992, pp127-202.  The original STG paper but still highly relevant.

- [ STG Survival Sheet](http://www.macs.hw.ac.uk/~dsg/gph/docs/StgSurvival.ps.gz), A poor wee soul, 2001 - This is a very old description of STG. This information is now on the wiki in sections about [ RTS](http://hackage.haskell.org/trac/ghc/wiki/Commentary/Rts) and [ STG](http://hackage.haskell.org/trac/ghc/wiki/Commentary/Compiler/GeneratedCode).

- [ Hoopl: A Modular, Reusable Library for Dataflow Analysis and Transformation](http://research.microsoft.com/en-us/um/people/simonpj/papers/c--/hoopl-haskell10.pdf), Norman Ramsey, John Dias, and Simon Peyton Jones. Haskell Symposium 2010

- [ C--: a portable assembly language that supports garbage collection](http://research.microsoft.com/en-us/um/people/simonpj/papers/c--/ppdp.ps.gz), Simon Peyton Jones, Norman Ramsey, and Fermin Reig. Invited talk at PPDP'99. 

- [ A Single Intermediate Language That Supports Multiple Implementations of Exceptions](http://research.microsoft.com/en-us/um/people/simonpj/papers/c--/c--exn-pldi.ps.gz), Norman Ramsey and Simon Peyton Jones, PLDI 2000.

## IO and Related

- [ Tackling the awkward squad: monadic input/output, concurrency, exceptions, and foreign-language calls in Haskell](https://research.microsoft.com/en-us/um/people/simonpj/papers/marktoberdorf/mark.pdf), Simon Peyton Jones. Deals with the incarnation of IO in Haskell and GHC. The history getting to monads, handling exceptions and handling concurrency.

- [Imperative Functional Programming](http://www.haskell.org/ghc/docs/papers/imperative.ps.gz), Simon Peyton Jones, Philip Wadler. POPL,  Jan 1993, pp71-84. Presents Monads as a way of implementing IO in Haskell.

- [Lazy Functional State Threads](http://www.haskell.org/ghc/docs/papers/lazy-functional-state-threads.ps.gz), John Launchbury and Simon Peyton Jones. PLDI 1993. A follow-up on "Imperative Functional Programming" paper.

- [Concurrent Haskell](http://www.haskell.org/ghc/docs/papers/concurrent-haskell.ps.gz), Simon Peyton Jones, Andrew Gordon, Sigbjorn Finne. Deals with the various concurrency constructs in GHC and the Haskell language. E.g., MVars.

- [ Asynchronous Exceptions in Haskell](http://community.haskell.org/~simonmar/papers/async.pdf), Simon Marlow, Simon Peyton Jones, Andrew Moran and John Reppy, 2006.

## The run-time system, garbage collector, profiling, FFI

- [ Parallel Generational-Copying Garbage Collection with a Block-Structured Heap](http://community.haskell.org/~simonmar/papers/parallel-gc.pdf) (Simon Marlow, Tim Harris, Roshan P. James, Simon Peyton Jones) In ISMM '08: Proceedings of the 7th international symposium on Memory management, Tucson, Arizona, ACM, June 2008

- [ Haskell on a Shared-Memory Multiprocessor](http://community.haskell.org/~simonmar/papers/multiproc.pdf) (Tim Harris, Simon Marlow, Simon Peyton Jones) In Haskell '05: Proceedings of the 2005 ACM SIGPLAN workshop on Haskell, pages 49--61, Tallinn, Estonia, ACM Press, September 2005

- [Time and space profiling for non-strict, higher-order functional languages](http://www.haskell.org/ghc/docs/papers/profiling.ps.gz), Patrick M. Sansom and Simon Peyton Jones, POPL 1995.

- [The Concurrent Haskell Foreign Function Interface](http://www.haskell.org/ghc/docs/papers/threads.ps.gz), Wolfgang Thaller. An Addendum to Haskell 98 FFI Report.
