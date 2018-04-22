# GHC Status Report (April 2018)


2018 saw GHC's first release under its new accelerated release schedule. GHC 8.4.1 contained 

## Major changes in GHC 8.6

### Libraries, source language, and type system

-  The new `-XNumericUnderscores` extension allows underscores to be used in numeric literals, improving legibility of longer literals.

- The long-awaited `-XBlockArguments` extensions allows `do` and lambda expressions to be used directly as a function argument, eliminating the need for parentheses or an application operator.

- Possibly: The `-XDerivingVia` extension, a proposed relative of `-XGeneralizedNewtypeDeriving` which allows users to derive 

- The `Data.Functor.Contravariant` module from the `contravariant` package has been moved into `base`.

### Compiler

- The compiler's core simplifier now performs significantly more varieties of numeric constant folding.

- Incomplete pattern match warnings are now offered for guards in pattern bindings and `MultiWayIf` alternatives.

- A new syntax tree representation based on [ Trees That Grow](http://www.jucs.org/jucs_23_1/trees_that_grow/jucs_23_01_0042_0062_najd.pdf).
  This will make it easier for external users to add their own annotations to the
  `HsSyn` AST. In future this should allow Shayan Najd to harmonise the GHC
  and Template Haskell ASTs, and for the `ghc-exactprint` annotations to
  move into the GHC parsed AST (Shayan Najd and Alan Zimmerman).

- Further improvements in support for cross-compilation (Moritz Angerman)

- Replacement of the `make`-based build system with Hadrian. Hadrian,
  while being usable in GHC 8.4, should be able to replace `make` in
  nearly all uses. Moreover, it will have significantly better documentation
  and support relocatable installation trees, a feature unavailable in the
  current build system (Andrey Mokhov, Zhen Zhang, Moritz Angerman, Alp
  Mestanogullari)

- Many, many bug fixes.

### Runtime system

- Significantly improved Windows support with a new I/O manager, long file
  path compatibility and dynamic linking support (Tamar Christina).

## Development updates and acknowledgements


GHC is lucky to have a large number of volunteer contributors.


Last year GHC
began accepting GitHub pull requests for small changes, particularly to
documentation. 


Matthías Páll Gissurarson has been adding support for significantly improved
diagnostics messages for typed holes. His 


Ryan Scott has been TODO


Mark Karpov of Tweag I/O has been pushing forward GHC's continuous integration
reboot. Using computational resources generously provided by Google X, GHC will
be moving its continuous integration infrastructure to CircleCI and Appveyor.
This will allow us to more easily produce binary distributions


Boldizsár Németh has been working on improving GHC's plugin story. GHC currently
disables to its recompilation checking when compiling with plugin, dramatically
increasing build times in common situations.


Joachim Breitner has been continuing his work on improving GHC's treatment of
exit join points. TODO


Andreas Klebinger has been working on improving various facets of GHC's backend
code generator. In the past few weeks alone he has contributed performance
optimisations for GHC's C-- pass, improved common subexpression elimination, and
added infrastructure for taking advantage of branch likelihoods.


Michal Terepeta has been performing a variety of refactoring in the backend, moving


Tamar Christina has continued his work on making GHC run great on Windows.
Recently he has been working to finish up a patchset enabling dynamic linking
support on Windows. Tamar is also working on a rework of GHC's Windows IO
manager implementation. The new implementation will take full advantage of
Windows' asynchronous I/O interfaces and should solve dozens of long-standing
tickets.


In addition to contributing valuable code review and bug triaging, Sebastian
Graf has contributed fixes to a variety of issues throughout the compiler,
including fixes to demand analysis, 


Recently Patrick Dougherty dusted off a long-dormant patch making the `ghc-heapview`
package a first-class citizen. This package allows Haskell programs to
introspect the heap


Andrey Mohkov, Zhen Zhang, Moritz Angerman, and Alp Mestanogullari have all been
working on the finishing the last mile of the switch to GHC's new Shake-based
build system, Hadrian.


One of the larger projects in the pipeline for 8.6 is Alan Zimmerman and Shayan Najd's
refactoring of GHC to use the extensible Trees That Grow AST structure. While 


As always, if you are interested in contributing to any facet of GHC,
be it the runtime system, type-checker, documentation, simplifier, or anything in
between, please come speak to us either on IRC (`#ghc` on
`irc.freeenode.net`) or `ghc-devs@haskell.org`. Happy Haskelling!
