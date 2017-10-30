# GHC Status Report (October 2017)


(WIP)

## Major changes in GHC 8.4


GHC 8.4 will continue the focus on stability and performance started in 8.2 and will include a number of internal refactorings.

### Libraries, source language, and type system

-  Phase 2 of the Semigroup-Monoid Proposal

### Compiler

-   A new syntax tree representation based on [ Trees That Grow](http://www.jucs.org/jucs_23_1/trees_that_grow/jucs_23_01_0042_0062_najd.pdf). This will make it easier for external users to l

-   Improved support for cross-compilation

-   A new build system based on the `shake` library. This is the culmination of nearly two years of effort, replacing GHC's old Make-based build system.

-   Improved code generation via exit-point floating. This builds on the join-point work started in GHC 8.2, allowing more functions to be 

### Runtime system

-   Significantly improved Windows support, with improvements in exception handling, linking, crash diagnostics and GHCi responsiveness.

## Development updates and acknowledgments


The past six months have seen a great deal of activity in GHC's infrastructure. This began in the summer on the heels of the 8.2.1 release, with a small group of developers reflecting on the various shortcomings of GHC's current Phabricator-based continuous integration scheme. With the help of Davean this effort grew from a hypothetical reimagining of GHC's continuous integration into a functional Jenkins configuration. This effort revealed and addressed numerous inadequacies in GHC's current test infrastructure. As a result of this work we can now test GHC end-to-end: starting from repository clone, to source distribution tarball, to binary distribution tarball, to completed testsuite run. This allows us to find regressions not just in the compiler itself but also in the sizeable mass of infrastructure dedicated to packaging and deploying it.


While Jenkins served as a good testing ground for improving GHC's test infrastructure, continued work with it revealed a number of issues:

- It lacked support for testing within `msys2` on Windows 
- It offered little support in ensuring build purity and reproducibly configuring build environments
- It required a significant investment of effort to setup, followed by an on-going administrative overhead


Manuel Chakravarty of the newly formed GHC Devops Committee noted these problems and instead proposed that we try a combination of hosted CI services, Appveyor and CircleCI. 


GHC's performance testing infrastructure also saw some attention this summer thanks to Haskell Summer of Code student Jared Weakly. Previously, GHC's performance testsuite would build a variety of test programs, measuring a variety of run-time and compile-time metrics of each. It would then compare each metric against a supplied acceptance window to identify regressions. While this simple approach served us well for years, it suffered from a few shortcomings,

- inherent variability between runs and environment dependence meant that the testsuite would often incorrectly identify commits as regressing
- to reduce these false-positives acceptance window sizes grew over the years, meaning that only large regressions would be identified


In his Summer of Code project, Jared refactored the testsuite to instead simply record the performance metrics resulting from a testsuite run. This new visibility into GHC's testsuite history will allow GHC developers to precisely identify regressing changes, meanwhile freeing maintainers from the need to periodically bump testsuite windows.


In the compiler itself, Shayan Najd and Alan Zimmerman have been working hard on porting the compiler's frontend AST to use the extensibility mechanism proposed in Shayan's "Trees That Grow" paper. This is a significant refactoring that will allow GHC API users to extend the AST for their own purposes, significantly improving the reusability of the structure. Eventually this will allow us to split the AST types out of the `ghc` package, allowing tooling authors, Template Haskell users, and the compiler itself to use the same AST representation.


As always, if you are interested in contributing to any facet of GHC, be it
the runtime system, type-checker, documentation, simplifier, or anything
in between, please come speak to us either on IRC (`#ghc` on
`irc.freeenode.net`) or `ghc-devs@haskell.org`. Happy Haskelling!

## Further reading

-   GHC website:

>
> \<[ https://haskell.org/ghc/](https://haskell.org/ghc/)\>

-   GHC users guide:

>
> \<[ https://downloads.haskell.org/\~ghc/master/users-guide/](https://downloads.haskell.org/~ghc/master/users-guide/)\>

- `ghc-devs` mailing list:

>
> \<[ https://mail.haskell.org/mailman/listinfo/ghc-devs](https://mail.haskell.org/mailman/listinfo/ghc-devs)\>
