# The GHC Library


This page is meant to serve as a one-stop shop for all papers that directly discuss aspects of GHC and how it is implemented. Naturally, these will fall out of date as GHC evolves, but it is immensely helpful to have the original, long-hand explanations around. If you're aware of work that belongs on this page (including your own, of course!), please by all means add it.

**Note on links:** Because many of these papers are copyrighted by the publishing bodies, we cannot host copies of the papers here. Instead, each entry should have at least one stable link to the publishing body's version and, ideally, a link to a freely available copy. For example, the "System F with Type Equality Coercions" paper links both to [ the version behind the paywall](http://dx.doi.org/10.1145/1190315.1190324) (often via a [ doi](http://doi.org) link) and [ the version on Simon's web page](http://research.microsoft.com/en-us/um/people/simonpj/papers/ext-f/tldi22-sulzmann-with-appendix.pdf). This way, readers can access a PDF easily (via the second link) and we have some hope that the paper will still be discoverable in years' time (via the first link).

## Implemented


The ideas in these papers are implemented and merged into master. 

- **Playing by the Rules: Rewriting as a Practical Optimisation Technique in GHC**. Simon Peyton Jones, Andrew Tolmach, Tony Hoare. Haskell '01. [ CiteSeer](http://citeseer.ist.psu.edu/viewdoc/summary?doi=10.1.1.130.2170)

>
> Introduces `RULES`.

- **Template Meta-programming for Haskell**. Tim Sheard, Simon Peyton Jones. Haskell '02. [ doi](http://dx.doi.org/10.1145/636517.636528)[ pdf](http://research.microsoft.com/en-us/um/people/simonpj/papers/meta-haskell/meta-haskell.pdf)

>
> Introduces Template Haskell.

- **Scrap your Boilerplate: a Practical Design Pattern for Generic Programming**. Ralf Lämmel, Simon Peyton Jones. TLDI '03. [ doi](http://dx.doi.org/10.1145/604174.604179)[ pdfs](http://research.microsoft.com/en-us/um/people/simonpj/papers/hmap/index.htm)

>
> Introduces `Typeable` and `Data`.

- **Associated Types with Class**. Manuel M. T. Chakravarty, Gabriele Keller, Simon Peyton Jones, Simon Marlow. POPL '05. [ doi](http://dx.doi.org/10.1145/1040305.1040306)[ pdf](http://research.microsoft.com/en-us/um/people/simonpj/Papers/assoc-types/assoc.pdf)

>
> Introduces associated data families.

- **Associated Type Synonyms**. Manuel M. T. Chakravarty, Gabriele Keller, Simon Peyton Jones. ICFP '05. [ doi](http://dx.doi.org/10.1145/1086365.1086397)[ pdf](http://research.microsoft.com/en-us/um/people/simonpj/papers/assoc-types/at-syns.pdf)

>
> Introduces associated type families.

- **System F with Type Equality Coercions**. Martin Sulzmann, Manuel Chakravarty, Simon Peyton Jones. TLDI '07. [ doi](http://dx.doi.org/10.1145/1190315.1190324)[ extended pdf](http://research.microsoft.com/en-us/um/people/simonpj/papers/ext-f/tldi22-sulzmann-with-appendix.pdf)

>
> The original paper describing GHC's Core language, also called FC in academic literature.

- **Understanding Functional Dependencies via Constraint Handling Rules**. Martin Sulzmann, Gregory J. Duck, Simon Peyton Jones, Peter J. Stuckey. JFP '07. [ doi](http://dx.doi.org/10.1017/S0956796806006137)[ pdf](http://research-srv.microsoft.com/en-us/um/people/simonpj/papers/fd-chr/jfp06.pdf)

- **Practical Type Inference for Arbitrary-Rank Types**. Simon Peyton Jones, Dimitrios Vytiniotis, Stephanie Weirich, Mark Shields. JFP '07. [ doi](http://dx.doi.org/10.1017/S0956796806006034)[ pdf](http://repository.upenn.edu/cis_papers/315/)[ technical appendix](http://repository.upenn.edu/cis_reports/58/)

>
> Describes type inference for higher-rank types.

- **OutsideIn(X): Modular Type Inference with Local Assumptions**. Dimitrios Vytiniotis, Simon Peyton Jones, Tom Schrijvers, Martin Sulzmann. JFP '11. [ doi](http://dx.doi.org/10.1017/S0956796811000098)[ pdf](http://research.microsoft.com:8082/en-us/um/people/simonpj/papers/constraints/jfp-outsidein.pdf)

>
> Describes the type inference algorithm in detail, focusing on GADTs and type families.

- **Giving Haskell a Promotion**. Brent Yorgey, Stephanie Weirich, Julien Cretin, Simon Peyton Jones, Dimitrios Vytiniotis, José Pedro Magalhães. TLDI '12. [ doi](http://dx.doi.org/10.1145/2103786.2103795)[ pdf](http://research.microsoft.com/en-us/um/people/simonpj/papers/ext-f/promotion.pdf)

>
> This introduces the promoted datatypes, kind polymorphism, and the `Constraint` kind. It includes an updated version of FC.

- **Equality Proofs and Deferred Type Errors: A Compiler Pearl**. Dimitrios Vytiniotis, Simon Peyton Jones, José Pedro Magalhães. ICFP '12. [ doi](http://dx.doi.org/10.1145/2364527.2364554)[ pdf](http://research.microsoft.com/en-us/um/people/simonpj/papers/ext-f/icfp12.pdf)

>
> Introduces deferred type errors, and has some explanation of lifted vs. unlifted equality.

- **Evidence Normalization in System FC**. Dimitrios Vytiniotis, Simon Peyton Jones. RTA '13. [ doi](http://dx.doi.org/10.4230/LIPIcs.RTA.2013.20)[ pdf](http://drops.dagstuhl.de/opus/volltexte/2013/4050/pdf/3.pdf)

>
> Explains the coercion optimizer.

- **Closed Type Families with Overlapping Equations**. Richard A. Eisenberg, Dimitrios Vytiniotis, Simon Peyton Jones, Stephanie Weirich. POPL '14. [ doi](http://dx.doi.org/10.1145/2535838.2535856)[ pdf](http://www.seas.upenn.edu/~sweirich/papers/popl14-axioms.pdf)[ extended version](http://repository.upenn.edu/cis_reports/990/)

>
> Introduces closed type families.

- **Safe Zero-Cost Coercions for Haskell**. Joachim Breitner, Richard A. Eisenberg, Simon Peyton Jones, Stephanie Weirich. ICFP '14. [ doi](http://dx.doi.org/10.1145/2628136.2628141)[ pdf](http://research.microsoft.com/en-us/um/people/simonpj/papers/ext-f/coercible.pdf)[ extended pdf](http://www.seas.upenn.edu/~sweirich/papers/coercible-extended.pdf)

>
> Introduces the `Coercible` mechanism.

- **Partial Type Signatures for Haskell**, Thomas Winant, Dominique Devriese, Frank Piessens, Tom Schrijvers.  PADL 2014 [ pdf](https://lirias.kuleuven.be/bitstream/123456789/423475/3/paper.pdf)[ TR](https://lirias.kuleuven.be/bitstream/123456789/424883/1/CW649.pdf)[ doi](http://dx.doi.org/10.1007/978-3-319-04132-2_2)

>
> Introduces partial type signatures.

## In progress


These papers have an implementation in progress, but have not yet merged (July 2015).

- **System FC with Explicit Kind Equality**. Stephanie Weirich, Justin Hsu, Richard A. Eisenberg. ICFP '13. [ doi](http://dx.doi.org/10.1145/2500365.2500599)[ pdf](http://www.seas.upenn.edu/~sweirich/papers/fckinds.pdf)

>
> Merges types with kinds, allowing promotion of GADTs and type families.

## Proposed


These papers propose extensions to GHC, but have not yet started on an earnest implementation.
