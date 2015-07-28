# The GHC Library


This page is meant to serve as a one-stop shop for all papers that directly discuss aspects of GHC and how it is implemented. Naturally, these will fall out of date as GHC evolves, but it is immensely helpful to have the original, long-hand explanations around. If you're aware of work that belongs on this page (including your own, of course!), please by all means add it.

**Note on links:** Because many of these papers are copyrighted by the publishing bodies, we cannot host copies of the papers here. Instead, each entry should have at least one stable link to the publishing body's version and, ideally, a link to a freely available copy. For example, the "System F with Type Equality Coercions" paper links both to [ the version behind the paywall](http://dx.doi.org/10.1145/1190315.1190324) (often via a [ doi](http://doi.org) link) and [ the version on Simon's web page](http://research.microsoft.com/en-us/um/people/simonpj/papers/ext-f/tldi22-sulzmann-with-appendix.pdf). This way, readers can access a PDF easily (via the second link) and we have some hope that the paper will still be discoverable in years' time (via the first link).

## Implemented


The ideas in these papers are implemented and merged into master. 

- **Playing by the Rules: Rewriting as a Practical Optimisation Technique in GHC**. Simon Peyton Jones, Andrew Tolmach, Tony Hoare. Haskell '01. [ CiteSeer](http://citeseer.ist.psu.edu/viewdoc/summary?doi=10.1.1.130.2170)

>
> Introduces `RULES`.

- **Scrap your Boilerplate: a Practical Design Pattern for Generic Programming**. Ralf Lämmel, Simon Peyton Jones. TLDI '03. [ doi](http://dx.doi.org/10.1145/604174.604179)[ pdfs](http://research.microsoft.com/en-us/um/people/simonpj/papers/hmap/index.htm)

>
> Introduces `Typeable` and `Data`.

- **System F with Type Equality Coercions**. Martin Sulzmann, Manuel Chakravarty, Simon Peyton Jones. TLDI '07. [ doi](http://dx.doi.org/10.1145/1190315.1190324)[ extended pdf](http://research.microsoft.com/en-us/um/people/simonpj/papers/ext-f/tldi22-sulzmann-with-appendix.pdf)

>
> The original paper describing GHC's Core language, also called FC in academic literature.

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

## In progress

## Proposed