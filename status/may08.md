# GHC Status May 2008


The last six months have been a time of consolidation for GHC.  We have done many of the things described in the last HCAR status report (November 2007), but there are few new headline items to report, so this status report is briefer than usual.  

- **Simple language extensions**

  - Record syntax: wild-card patterns, punning, and field disambiguation
  - View patterns
  - Generalised list comprehensions
  - Quasi-quoting

- **Type-indexed families**.  We learned a lot by writing a paper about the question of type inference in the presence of type families (and existentials, and GADTs): "[ Type checking with open type functions](http://research.microsoft.com/%7Esimonpj/papers/assoc-types)".  The implementation has not quite caught up with the paper and is still incomplete in many ways, but it's a focus of active work.  

- **Parallel garbage collection**. Much implementation work, and a paper for ISMM 2008: "[ Parallel generational-copying garbage collection with a block-structured heap](http://research.microsoft.com/%7Esimonpj/papers/parallel-gc/index.htm)". 

- **Impredicative polymorphism**.  We're not happy with GHC's current implementation of impredicative polymorphism, which is rather complicated and ad hoc.  Dimitrios (with Simon and Stephanie) wrote a paper about a new and better approach: "[ FPH : First-class Polymorphism for Haskell](http://research.microsoft.com/%7Esimonpj/papers/boxy)".  At the same time, Daan Leijen refined his closely-related design: "[ Flexible types: robust type inference for first-class polymorphism](http://research.microsoft.com/users/daan/pubs.html)".  Daan's design has a much simpler implementation, in exchange for an (arguably) less-predicatable specification.  Which of these two should we implement?  Let us know!


Work on the back end has been stalled, but John Dias started a 6-month internship in April, so expect progress on this front!

## Nested data parallelism


We have been working hard on Data Parallel Haskell, especially Roman Leshchinskiy and Gabriele Keller.  It has turned out be be hard to get the entire transformation and optimisation stack to work smoothly, and we have not made progress announcements because we don't want to yell about it until it Actually Works.  But it is the biggest single GHC focus: Roman works on it full time.

*Manuel: more to say.*


We expect to release a working version of Data Parallel Haskell as part of GHC 6.10 (see below).

## Release plans


We plan to release GHC 6.8.3 at the end of May 2008, with many bug-fixes but no new features.


We plan to release GHC 6.10 around the time of ICFP, with significant new features.  The up-to-date list of new stuff is kept at [ http://hackage.haskell.org/trac/ghc/wiki/Status/Releases](http://hackage.haskell.org/trac/ghc/wiki/Status/Releases), but here's a quick summary:

- Simple language extensions (mentioned above)
- Type-indexed families
- Data Parallel Haskell
- Parallel garbage collection
- Extensible exceptions
- Shared libraries
- Improved back end
- Further library reorganisation
