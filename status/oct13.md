# GHC Status Report, October 2013


A big event occurred earlier this year in July: the news that Ian Lynagh would be leaving Well-Typed - and consequently, GHC HQ - to move onto new challenges. Ian worked on GHC for 7 years full-time, helped write our new build system, redesigned large parts of the compiler, and fixed innumerable other issues over the course of his work with us. [ According to some statistics](https://github.com/ghc/ghc/graphs/contributors) for the past 7 years or so, Ian quite clearly is one of the biggest contributors we've ever had. And we should take the time to say what is rightfully deserved: **thank you for everything you've done for us, Ian**!


While Ian has not completely disappeared, GHC and Haskell are not his day-job anymore. As a result, this means everyone - including those friends reading this - have a huge opportunity to help continue making GHC even better. Luckily, all the evidence of the past few months points towards a tremendous surge in community involvement. GHC is a community project, and the community is what keeps us afloat. **A great thanks goes to all those who have helped us come so far**!


In light of this, Well-Typed has added two new hires to GHC HQ to keep things moving, and help the development process going forward: **Edsko de Vries** and **Austin Seipp**.


The GHC 7.8 release is in its final stages, and will be released in late November according to our plans. There's a tremendous amount of exciting changes coming very soon, described below.

## Source language and Type System

- **Type natural solver** - Iavor Diatchki implemented a basic constraint solver for the type naturals extension, meaning that GHC can now infer and understand basic identities such as `(x + 2) ~ 5`, which implies `x = 3`.

- **Closed type families** - Richard Eisenberg & co. have implemented support for *closed* type families in GHC, allowing you to write a type family where no instances can be made beyond the ones in the definition. This allows a host of new programs to be expressed, as we now know certain invariants can hold. For example, we may now write:

  ```wiki
  type family Flip p :: *
  type family Flip a where
    Filp Even = Odd
    Flip Odd  = Even
  ```

  to express that `Flip` may have no more instances written, meaning nefarious users can no longer write silly instances like `Flip Bool = Int`.

- **Role support** - Richard Eisenberg implemented support for *role checking* in GHC, fixing a long standing bug where `GeneralizedNewtypeDeriving` could be used to derive unsafe instances for a `newtype`. 

- **New and improved I/O manager** - Earlier this year, Andreas Voellmy and Kazu Yamamoto worked on a host of improvements to our I/O manager, making it scale significantly better on multicore machines. Since then, it's seen some other performance tweaks, and many bugfixes. As a result, the new I/O manager should scale linearly up to about 40 cores. Andreas reports their McNettle Software-defined-network (SDN) implementation can now achieve over *twenty million connections per second*, making it the fastest SDN implementation around - an incredible feat!

- **New Template Haskell** - Geoffrey Mainland implemented support for New Template Haskell, fixing a lot of long-standing bugs with the TH implementation, while making it significantly more expressive, including support for typed quotes, pattern splices and more. This allows us to write for example a typed, staged power function. For details and examples, you can see Geoff's blog\[NewTH1\] and the GHC wiki pages describing the design \[NewTH2\].

- A small menagerie of various other language improvements and extensions, including:

  - `-XOverloadedLists` which allows overloading list literals (Achim Krause, George Giorgidze, Weijers Jeroen)
  - `-XNumDecimals` allowing a compact floating-point syntax for integrals, e.g. `1.2e6 :: Integer` (Shachaf Ben-Kiki)
  - Support for empty case statements (via `-XEmptyCase`)
  - A more liberal version of `-XIncoherentInstances` (Joachim Breitner)
  - A new pragma for specifying the minimal complete definition of a typeclass, via `{-# MINIMAL #-}` (Twan van Laarhoven)

## Back-end and runtime system

- **New code generator** - As previously reported, the New Code Generator is live and switched on by default. There have been a host of bugfixes and stability improvements, meaning it should solid for the 7.8 release.

- **SSE/AVX support** - Geoffrey Mainland implemented support for SSE/AVX intrinsics in the compiler backend, making it possible to exploit hardware-accelerated SIMD operations in your code on Intel/AMD machines. It's currently only enabled for the LLVM backend, however.

- Peter Wortmann spent time earlier this year doing a significant refactoring of the LLVM backend, which means it should be easier in the future to extend the backend and perform long-term maintenance.

- Jan Stolarek had an internship at Microsoft Research during the summer, and as part of this he implemented an array of improvements to the code generator and backend, including a new loopification pass to turn tail-recursive calls into loops, and a refactoring of our `Bool` based primops to return unboxed `Int#` values (making them much faster, with a sizeable performance improvement in some cases \[[PrimBool](prim-bool)\].)

- Simon Marlow implemented support for unloading object code at runtime in the GHC linker. Previously, while GHC's linker could load object code dynamically, there was no facility to unload it - meaning long running applications would continuously suffer a memory leak as they reloaded more code.

- Edward Yang implemented support for running library constructors in GHCi, making it possible to use foreign libraries which depend on constructors being run at load time.

- A set of new primops for all backends, including new atomic memory operations (by Ryan Newton) and support for low-level prefetch instructions in the processor, allowing you to guide cache decisions (by Carter Schonwald.)

## Frontend, build-system, and misc. changes

- **Dynamic-by-default** - In 7.8, we're hoping to make GHCi use the system linker by default on supported platforms, eliminating a host of bugs in the current home-grown linker. Eventually we hope to remove the old linker completely. Until then, GHC now supports compiling files statically and dynamically at the same time (with the `-dynamic-too flag`,) meaning you can switch between static/dynamic builds much more easily.

- **Compiler hooks** - Luite Stegeman and Edsko de Vries did a significant amount of work to improve *hooking* support in the GHC API. This new API makes it possible for users to plug in their own pipeline machinery to the compiler, suitable for implementing new frontend features (like QuasiQuoting hooks) or new backends (like a JavaScript target, as part of the new GHCJS.)

- **Parallel --make** - as part of the haskell.org 2013 GSoC, Patrick Palka implemented a new parallel compilation driver, a long-requested feature. This allows GHC to build multiple modules in parallel when using `--make` by adding a `-j` flag, while having almost no overhead in the single-threaded case.

- **Clang support** - Austin Seipp added support for GHC to use Clang as the C compiler, instead of GCC. While the work is still rough, it should be a boon to modern OS X users, who no longer have GCC available by default.

- **IOS support & cross compilation** - After many years of work by Ian, Stephen Blackheath and friends Luke Ianni and Maxwell Swalding, GHC now has full support for iOS cross-compilation. As of GHC 7.8, you'll really be able to write iOS apps in your favorite programming language!

- **Better ARM support** - Ben Gamari and Austin Seipp have some final changes to make for the ARM story, which should make it significantly more stable and usable, including GHCi support. We hope to have ARMv7 binary releases for GHC 7.8 available for download.

## Future plans


After the 7.8 release, there are some improvements scheduled we plan on integrating:

- **Applicative as a superclass of Monad** - A long-standing proposal, GHC 7.10 will finally make `Applicative` a superclass of `Monad`. GHC 7.8 features warnings to ensure users know where their code will break as a result of this API change.

- **Kinds without data** - Trevor Elliott, Eric Mertens, and Iavor Diatchki have began implementing support for "data kind" declarations, described in more detail on the GHC wiki \[KD\]. The idea is to allow a new form of declaration that introduces a new kind, whose members are described by the (type) constructors in the declaration. This is similar to promoting data declarations, except that no new value-level-constructors are declared, and it also allows the constructors to mention other kinds that do not have corresponding type-level representation (e.g., \*).

- **Overloaded record fields ** - Adam Gundry implemented the overloaded records field proposal as part of the Haskell.org 2013 GSoC. This work will make it possible to not only have overloaded record field names, but also enable a wealth of other nice features, like polymorphic update/lookup, and automatically turning record fields into lens. More detail can be found on the GHC wiki \[ORF\].

- **Pattern synonyms** - Gergo Erdi worked on an implementation of pattern synonyms for GHC, which will finally give us the power to abstract over patterns and give them names. While the design is not final (see the wiki for details\[PS\]), the results look promising, and hopefully fix a long-standing 'abstraction hole' in the term language for Haskell.

- **Explicit Type Application** - Stephanie Weirich, Richard Eisenburg and Hamidhasan Ahmed have been working on adding explicit type applications to GHC. This allows the programmer to specify the \*types\* that should be instantiated for arguments to a function application, where normally they would be inferred. While this capability already exists in FC-pro (indeed, every FC-pro program has function application with explicitly applied types,) it has not been available in Haskell itself. While a lot of the syntax and design is not quite final, there are some details about the design available on the wiki\[TA\].


But we're not sure what else might happen. It's a great time to step up to the plate and do something fun!

# Development updates, joining in and a big Thank You!


In the past several months, GHC has seen a surge of community involvement, and a great deal of new contributors. We now have [37 committers](team-ghc), with **14 added in 2013 alone** - it's an exciting time to work GHC!


Amongst those who have rolled up their sleeves and helped us recently:

- Herbert Valerio Riedel
- Takano Akio
- Reid Barton
- Kazu Yamamoto
- Joachim Breitner
- Carter Schonwald
- Krzysztof Gogolewski
- Jan Stolarek


As ever, there is a ton of stuff in the future for us to do. Don't wait - it might take a while. You should join us instead!

# References


\[NewTH1\] Runtime codegen with typed Template Haskell - [ http://gmainland.blogspot.com/2013/05/type-safe-runtime-code-generation-with.html](http://gmainland.blogspot.com/2013/05/type-safe-runtime-code-generation-with.html)

\[NewTH2\] Major proposed Template Haskell revision - [ http://ghc.haskell.org/trac/ghc/wiki/TemplateHaskell/BlogPostChanges](http://ghc.haskell.org/trac/ghc/wiki/TemplateHaskell/BlogPostChanges)

\[[PrimBool](prim-bool)\] New comparison primitives - [ http://ghc.haskell.org/trac/ghc/wiki/PrimBool](http://ghc.haskell.org/trac/ghc/wiki/PrimBool)

\[KD\] Kinds without Data - [ http://ghc.haskell.org/trac/ghc/wiki/GhcKinds/KindsWithoutData](http://ghc.haskell.org/trac/ghc/wiki/GhcKinds/KindsWithoutData)

\[ORF\] Overloaded record fields - [ http://ghc.haskell.org/trac/ghc/wiki/Records/OverloadedRecordFields/Plan](http://ghc.haskell.org/trac/ghc/wiki/Records/OverloadedRecordFields/Plan)

\[PS\] Pattern synonyms - [ http://ghc.haskell.org/trac/ghc/wiki/PatternSynonyms](http://ghc.haskell.org/trac/ghc/wiki/PatternSynonyms)

\[TA\] Explicit type application - [ http://ghc.haskell.org/trac/ghc/wiki/ExplicitTypeApplication](http://ghc.haskell.org/trac/ghc/wiki/ExplicitTypeApplication)